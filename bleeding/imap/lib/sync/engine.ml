module Mirror = Imap.Mirror
module Uids = Map.Make(Int64)
module Uid_set = Set.Make(Int64)

type error =
  | Client of Imap_eio.Error.t
  | Mirror of Mirror.error
  | Invalid_scope of string
  | Incomplete of string
  | Limit of string
  | Stale_revision
  | Uidvalidity_changed

let pp_error ppf = function
  | Client e -> Imap_eio.Client.pp_error ppf e
  | Mirror _ -> Format.pp_print_string ppf "IMAP mirror consistency error"
  | Invalid_scope s -> Format.fprintf ppf "invalid IMAP scope: %s" s
  | Incomplete s -> Format.fprintf ppf "incomplete IMAP inventory: %s" s
  | Limit s -> Format.fprintf ppf "IMAP scan limit: %s" s
  | Stale_revision -> Format.pp_print_string ppf "IMAP snapshot changed concurrently"
  | Uidvalidity_changed ->
      Format.pp_print_string ppf "mailbox UIDVALIDITY changed"

let ( let* ) result f = match result with Ok x -> f x | Error _ as e -> e
let network = function Ok x -> Ok x | Error e -> Error (Client e)
let mirror = function Ok x -> Ok x | Error e -> Error (Mirror e)
let validation kind = function Ok x -> Ok x | Error message -> Error (kind message)

let validate_scope ~client ~(scope:Mirror.scope) ~mailbox =
  let mode = Imap_eio.Client.mailbox_mode client in
  if scope.encoding <> mode then
    Error (Invalid_scope "mailbox encoding changed")
  else
    let* wire_name = validation (fun s -> Invalid_scope s)
      (Imap.Mailbox_name.encode ~mode mailbox) in
    if wire_name <> scope.raw_name then
      Error (Invalid_scope "mailbox wire name differs from cursor scope")
    else Ok ()

let selected_metadata (info:Imap.Response.select_metadata) =
  let* uidvalidity = validation (fun s -> Incomplete s)
    (Imap.Uidvalidity.of_int64 info.uidvalidity) in
  let* highestmodseq = match info.highestmodseq with
    | None -> Ok None
    | Some value ->
        let* value = validation (fun s -> Incomplete s)
          (Imap.Modseq.of_int64 value) in
        Ok (Some value) in
  Ok ({uidvalidity; uidnext=info.uidnext; highestmodseq;
       nomodseq=info.nomodseq || Option.is_none highestmodseq}:Mirror.selected)

let conflicting_identity =
  Invalid_scope "saved OBJECTID+ binding names another mailbox"

let prepare_object_identity ~client ~store ~scope ~mailbox =
  let has=Imap_eio.Client.has client in
  let offered=has Imap.Capability.Objectid_plus &&
    has Imap.Capability.Enable in
  match Imap_store.object_identity store ~scope with
  | `Conflict -> Error conflicting_identity
  | `Bound _ when not offered ->
    Error (Invalid_scope "saved OBJECTID+ identity cannot be verified")
  | `Unbound when not offered -> Ok false
  | `Unbound | `Bound _ as bound ->
    let* ()=network (Imap_eio.Client.enable_objectid_plus client) in
    let* ()=match bound with
      | `Unbound -> Ok ()
      | `Bound (identity:Imap_store.object_identity) ->
          let* status=network (Imap_eio.Client.status client ~mailbox
            ~items:[Imap.Command.Objectid]) in
          (match status.objectid with
           | Some ids when ids.account_id=Some identity.account_id &&
               ids.mailbox_id=Some identity.mailbox_id ->
               network (Imap_eio.Client.pin_mailbox_objectid client ~mailbox
                 ~account_id:identity.account_id
                 ~mailbox_id:identity.mailbox_id)
           | _ -> Error (Invalid_scope
               "configured mailbox name no longer matches saved OBJECTID+")) in
    Ok true

let guard_bound_mailbox ~client ~store ~scope ~mailbox =
  match Imap_store.object_identity store ~scope with
  | `Unbound -> Ok ()
  | `Conflict -> Error conflicting_identity
  | `Bound _ ->
      let* _=prepare_object_identity ~client ~store ~scope ~mailbox in
      Ok ()

let verify_mutation_destination ~client ~store ~scope ~mailbox =
  match Imap_store.object_identity store ~scope with
  | `Unbound -> Ok ()
  | `Conflict -> Error conflicting_identity
  | `Bound (identity:Imap_store.object_identity) ->
      if not (Imap_eio.Client.is_enabled client
                Imap.Capability.Objectid_plus) then
        Error (Invalid_scope "saved OBJECTID+ identity is not enabled")
      else
        let* status=network (Imap_eio.Client.status client ~mailbox
          ~items:[Imap.Command.Objectid]) in
        (match status.objectid with
         | Some ids when ids.account_id=Some identity.account_id &&
             ids.mailbox_id=Some identity.mailbox_id -> Ok ()
         | _ -> Error (Invalid_scope
             "APPEND destination name no longer matches saved OBJECTID+"))

let observe_selected_identity ~store ~scope info =
  match info.Imap.Response.objectid with
  | Some {account_id=Some account_id;mailbox_id=Some mailbox_id;_} ->
      let identity:Imap_store.object_identity={account_id;mailbox_id} in
      (match Imap_store.observe_object_identity store ~scope identity with
       | `Bound | `Matched -> Ok (Some identity)
       | `Conflict -> Error (Invalid_scope
           "OBJECTID+ identity differs from durable mailbox binding"))
  | _ -> Error (Invalid_scope
      "OBJECTID+ SELECT omitted account/mailbox identity")

(* A first binding would attest a mailbox whose epoch just changed, which
   may be a replacement. Binding waits for a scan that sees the new epoch
   already published. *)
let observe_identity ~objectid_enabled ~store ~scope ~(cursor:Mirror.cursor)
    ~validity info =
  match Imap_store.object_identity store ~scope,cursor.uidvalidity with
  | _ when not objectid_enabled -> Ok None
  | `Unbound,Some previous when previous<>validity -> Ok None
  | _ -> observe_selected_identity ~store ~scope info

let pin_observed_identity ~client ~mailbox = function
  | None -> Ok ()
  | Some (identity:Imap_store.object_identity) ->
      network (Imap_eio.Client.pin_mailbox_objectid client ~mailbox
        ~account_id:identity.account_id ~mailbox_id:identity.mailbox_id)

let row_of_fetch (item : Imap.Response.fetch) =
  match item.uid, item.flags with
  | Some raw_uid, Some raw_flags ->
      let* uid = validation (fun s -> Incomplete s)
        (Imap.Uid.of_int64 raw_uid) in
      let* flags = List.fold_right (fun raw acc ->
        let* rest = acc in
        let* flag = validation (fun s -> Incomplete s)
          (Mail_flag.Imap_flag.of_wire raw) in
        (* \\Recent describes this session, not durable message state. *)
        Ok (match flag with
          | Mail_flag.Imap_flag.Recent -> rest
          | _ -> flag :: rest)) raw_flags (Ok []) in
      let* modseq = match item.modseq with
        | None -> Ok None
        | Some value ->
            let* value = validation (fun s -> Incomplete s)
              (Imap.Modseq.of_int64 value) in
            Ok (Some value) in
      Ok (Some ({uid; flags; modseq} : Mirror.row))
  | _ -> Ok None

let scan ~max_windows ~max_rows ~modseq selected action =
  let upper = action.Mirror.upper_uid in
  let windows = if upper = 0L then 0L else
    Int64.div (Int64.add upper 999L) 1000L in
  if windows > Int64.of_int max_windows then
    Error (Limit "UID range exceeds configured window budget")
  else
    let fetched_count = ref 0 in
    let rec fetch first by_uid =
      if first > upper then Ok by_uid else
      let last = Int64.min upper (Int64.add first 999L) in
      let* fetched = network (Imap_eio.Selected.fetch_metadata_range selected
        ~first ~last ~modseq) in
      let* by_uid = List.fold_left (fun accumulated
          (item : Imap.Response.fetch) ->
        let* by_uid = accumulated in
        let* parsed = row_of_fetch item in
        match parsed with
        | Some row ->
            let raw_uid = Imap.Uid.to_int64 row.uid in
            if not (Uids.mem raw_uid by_uid) then incr fetched_count;
            let by_uid = Uids.add raw_uid row by_uid in
            if !fetched_count > max_rows then
              Error (Limit "metadata row budget exceeded")
            else Ok by_uid
        | None -> Ok by_uid) (Ok by_uid) fetched in
      fetch (Int64.succ last) by_uid
    in
    let* by_uid = fetch 1L Uids.empty in
    let membership_count = ref 0 in
    let rec inventory first found =
      if first > upper then Ok found else
      let last = Int64.min upper (Int64.add first 999L) in
      let* uids = network (Imap_eio.Selected.uid_search_range selected
        ~first ~last) in
      let* found = List.fold_left (fun accumulated uid ->
        let* found = accumulated in
        if uid < first || uid > last then
          Error (Incomplete "SEARCH returned UID outside requested range")
        else if not (Uids.mem uid by_uid) then
          Error (Incomplete "SEARCH found a UID absent from completed FETCH")
        else
          (if not (Uid_set.mem uid found) then incr membership_count;
          let found = Uid_set.add uid found in
          if !membership_count > max_rows then
            Error (Limit "membership row budget exceeded")
          else Ok found)) (Ok found) uids in
      inventory (Int64.succ last) found
    in
    let* found = inventory 1L Uid_set.empty in
    Ok (Uid_set.elements found |> List.map (fun uid -> Uids.find uid by_uid))

let scan_qresync ~max_windows ~max_rows selected action
    (previous : Mirror.snapshot) ~frontier =
  let upper = action.Mirror.upper_uid in
  let windows = if upper = 0L then 0L else
    Int64.div (Int64.add upper 999L) 1000L in
  if windows > Int64.of_int max_windows then
    Error (Limit "UID range exceeds configured window budget")
  else if frontier > upper then
    Error (Incomplete "UID frontier regressed during QRESYNC")
  else
    let old = List.fold_left (fun acc (row : Mirror.row) ->
      Uids.add (Imap.Uid.to_int64 row.uid) row acc)
      Uids.empty (Mirror.rows previous) in
    let row_count = ref (Uids.cardinal old) in
    if !row_count > max_rows then
      Error (Limit "published metadata exceeds row budget")
    else
      let add map (row : Mirror.row) =
        let uid = Imap.Uid.to_int64 row.Mirror.uid in
        let previous = Uids.find_opt uid map in
        let newer = match previous with
          | None -> true
          | Some (old : Mirror.row) ->
              (match old.modseq, row.modseq with
               | Some a, Some b ->
                   Imap.Modseq.to_int64 b >=
                     Imap.Modseq.to_int64 a
               | _ -> true) in
        if not newer then Ok map else (
          if previous = None then incr row_count;
          if !row_count > max_rows then
            Error (Limit "QRESYNC metadata exceeds row budget")
          else Ok (Uids.add uid row map)) in
      let* updates = network (Imap_eio.Selected.select_updates selected) in
      let* by_uid = List.fold_left (fun accumulated update ->
        let* map = accumulated in
        match update with
        | Imap.Response.Untagged
            (Imap.Response.Fetch fetched | Imap.Response.Uidfetch fetched) ->
            let* row = row_of_fetch fetched in
            (match row with
             | Some row when Imap.Uid.to_int64 row.uid <= upper ->
                 add map row
             | _ -> Ok map)
        | _ -> Ok map) (Ok old) updates in
      let rec fetch_new first map =
        if first > upper then Ok map else
        let last = Int64.min upper (Int64.add first 999L) in
        let* fetched = network (Imap_eio.Selected.fetch_metadata_range
          selected ~first ~last ~modseq:true) in
        let* map = List.fold_left (fun accumulated fetched ->
          let* map = accumulated in
          let* row = row_of_fetch fetched in
          match row with Some row -> add map row | None -> Ok map)
          (Ok map) fetched in
        fetch_new (Int64.succ last) map in
      let* by_uid = fetch_new (Int64.succ frontier) by_uid in
      let found_count = ref 0 in
      let rec inventory first found =
        if first > upper then Ok found else
        let last = Int64.min upper (Int64.add first 999L) in
        let* uids = network (Imap_eio.Selected.uid_search_range selected
          ~first ~last) in
        let* found = List.fold_left (fun accumulated uid ->
          let* found = accumulated in
          if uid < first || uid > last then
            Error (Incomplete "SEARCH returned UID outside requested range")
          else if not (Uids.mem uid by_uid) then
            Error (Incomplete "QRESYNC omitted a live UID")
          else (
            if not (Uid_set.mem uid found) then incr found_count;
            if !found_count > max_rows then
              Error (Limit "QRESYNC membership exceeds row budget")
            else Ok (Uid_set.add uid found))) (Ok found) uids in
        inventory (Int64.succ last) found in
      let* found = inventory 1L Uid_set.empty in
      Ok (Uid_set.elements found |> List.map (fun uid -> Uids.find uid by_uid))

let run_once ?(max_windows=1000) ?(max_rows=100_000) ~client ~store
    ~scope ~mailbox ~stage_id () =
  if max_windows < 1 || max_rows < 1 then
    Error (Limit "scan budgets must be positive")
  else
    let* () = validate_scope ~client ~scope ~mailbox in
    let* objectid_enabled=prepare_object_identity ~client ~store
      ~scope ~mailbox in
    let current = Imap_store.load store ~scope in
    let observed_identity=ref None in
    let qresync = match current.cursor.uidvalidity,
      current.cursor.anchor with
      | Some validity, Some anchor
        when Imap_eio.Client.is_enabled client Imap.Capability.Qresync ->
          Some (Imap.Uidvalidity.to_int64 validity,
                Imap.Modseq.to_int64 anchor)
      | _ -> None in
    let* scan_result = network
      (Imap_eio.Client.with_mailbox client ?qresync
        ~mode:`Read_only mailbox
        (fun selected ->
          let result =
            let* info = network (Imap_eio.Selected.info selected) in
            let* selected_info = selected_metadata info in
            let validity = selected_info.uidvalidity in
            let* identity=observe_identity ~objectid_enabled ~store ~scope
              ~cursor:current.cursor ~validity info in
            observed_identity:=identity;
            let highestmodseq = selected_info.highestmodseq in
            let use_modseq = not selected_info.nomodseq in
            let* action = mirror (Mirror.plan current.cursor
              ~stage_id selected_info) in
            let incremental = match qresync, current.snapshot,
                action.restart with
              | Some _, Some previous, None when use_modseq ->
                  Some previous
              | _ -> None in
            let* rows = match incremental with
              | None -> scan ~max_windows ~max_rows
                  ~modseq:use_modseq selected action
              | Some previous ->
                  (match scan_qresync ~max_windows ~max_rows selected
                    action previous ~frontier:current.cursor.frontier with
                   | Error (Incomplete _) ->
                       scan ~max_windows ~max_rows
                         ~modseq:use_modseq selected action
                   | result -> result) in
            let completed : Mirror.completed = {
              action_id=action.id; uidvalidity=validity;
              covered_upper=action.upper_uid;
              inventory_complete=true; commands_complete=true;
              rows; explicit_highestmodseq=highestmodseq;
              nomodseq=not use_modseq
            } in
            let* staged = mirror (Mirror.complete current.cursor
              action completed) in
            mirror (Mirror.publish current.cursor
              ~published:current.snapshot staged)
          in Ok result)) in
    let* transition = scan_result in
    let* ()=pin_observed_identity ~client ~mailbox !observed_identity in
    (match Imap_store.publish store transition with
     | `Committed -> Ok transition
     | `Stale_revision -> Error Stale_revision)

let run_once_staged ?(max_windows=100_000) ?expected_uidvalidity
    ~client ~store ~scope
    ~mailbox ~stage_id () =
  if max_windows<1 then Error (Limit "scan window budget must be positive")
  else
    let* () = validate_scope ~client ~scope ~mailbox in
    let* objectid_enabled=prepare_object_identity ~client ~store
      ~scope ~mailbox in
    let cursor=Imap_store.load_cursor store ~scope in
    let observed_identity=ref None in
    let stage_created=ref false in
    let published=ref false in
    Fun.protect ~finally:(fun () ->
      if !stage_created && not !published then
        Eio.Cancel.protect (fun () ->
          Imap_store.discard_stage store ~stage_id)) @@ fun () ->
    let selection=network (Imap_eio.Client.with_mailbox client
      ~mode:`Read_only mailbox (fun selected ->
        let result=
          let* info=network (Imap_eio.Selected.info selected) in
          let* selected_info = selected_metadata info in
          let validity = selected_info.uidvalidity in
          let* ()=match expected_uidvalidity with
            | Some expected when expected<>validity ->
                Error Uidvalidity_changed
            | _ -> Ok () in
          let* identity=observe_identity ~objectid_enabled ~store ~scope
            ~cursor ~validity info in
          observed_identity:=identity;
          let highestmodseq = selected_info.highestmodseq in
          let use_modseq = not selected_info.nomodseq in
          let* action=mirror (Mirror.plan cursor ~stage_id selected_info) in
          let upper=action.upper_uid in
          let windows=if upper=0L then 0L else
            Int64.div (Int64.add upper 999L) 1000L in
          if windows>Int64.of_int max_windows then
            Error (Limit "UID range exceeds configured window budget")
          else (
            Imap_store.begin_stage store ~cursor ~action;
            stage_created := true;
            let incremental=match cursor.phase,cursor.uidvalidity,
              cursor.anchor,cursor.inventory_ref,action.restart with
              | Mirror.Live,Some previous_epoch,Some anchor,Some _,None
                when previous_epoch=validity &&
                  cursor.frontier<=upper && use_modseq &&
                  (Imap_eio.Client.has client Imap.Capability.Condstore ||
                   Imap_eio.Client.has client Imap.Capability.Qresync) ->
                  Some anchor
              | _ -> None in
            let ceil_windows n=if n<=0L then 0L else
              Int64.div (Int64.add n 999L) 1000L in
            let incremental=match incremental with
              | Some _ when Int64.add
                  (ceil_windows cursor.frontier)
                  (ceil_windows (Int64.sub upper cursor.frontier)) >
                    Int64.of_int max_windows -> None
              | value -> value in
            let* ()=match incremental with
              | None -> Ok ()
              | Some _ ->
                  (match Imap_store.seed_stage_from_published store
                    ~cursor ~action with
                   | `Seeded -> Ok ()
                   | `Stale_revision -> Error Stale_revision) in
            let staged f =
              try f (); Ok ()
              with Invalid_argument message -> Error (Incomplete message) in
            let rec fetch first=
              if first>upper then Ok () else
              let last=Int64.min upper (Int64.add first 999L) in
              let last=match incremental with
                | Some _ when first<=cursor.frontier ->
                    Int64.min last cursor.frontier
                | _ -> last in
              let* parsed=match incremental with
                | Some anchor when last<=cursor.frontier ->
                    let* fetched=network
                      (Imap_eio.Selected.fetch_changes_range selected
                        ~first ~last ~since:anchor) in
                    List.fold_right (fun item acc ->
                      let* rest=acc in
                      let* row=row_of_fetch item in
                      match row with
                      | Some row when row.modseq<>None ->
                          Ok (row::rest)
                      | _ -> Error (Incomplete "CHANGEDSINCE omitted MODSEQ"))
                      fetched (Ok [])
                | _ ->
                    let* fetched=network
                      (Imap_eio.Selected.fetch_metadata_range selected
                        ~first ~last ~modseq:use_modseq) in
                    List.fold_right
                      (fun item acc ->
                        let* rest=acc in
                        let* row=row_of_fetch item in
                        match row with
                        | Some row when not use_modseq ||
                            row.modseq<>None -> Ok (row::rest)
                        | _ -> Error (Incomplete
                            "FETCH metadata omitted requested MODSEQ"))
                      fetched (Ok []) in
              let* ()=staged (fun () ->
                Imap_store.stage_rows store ~stage_id ~first ~last
                  ~preserve_newer:(Option.is_some incremental) parsed) in
              fetch (Int64.succ last) in
            let rec inventory first=
              if first>upper then Ok () else
              let last=Int64.min upper (Int64.add first 999L) in
              let* found=network
                (Imap_eio.Selected.uid_search_range selected
                  ~first ~last) in
              let* ()=staged (fun () ->
                Imap_store.stage_membership store ~stage_id
                  ~first ~last found) in
              inventory (Int64.succ last) in
            let* ()=fetch 1L in
            let* ()=inventory 1L in
            Ok (action,highestmodseq,not use_modseq))
        in Ok result)) in
    match selection with
    | Error _ as error -> error
    | Ok (Error _ as error) -> error
    | Ok (Ok (action,highestmodseq,nomodseq)) ->
        (match pin_observed_identity ~client ~mailbox
           !observed_identity with
         | Error _ as error -> error
         | Ok () ->
           match Imap_store.publish_stage store ~cursor ~action
             ~explicit_highestmodseq:highestmodseq ~nomodseq with
           | `Committed receipt -> published:=true; Ok receipt
           | `Stale_revision -> Error Stale_revision)

type append_outcome =
  | Identified of Imap_eio.Client.append_receipt
  | Needs_reconciliation

let append_journaled ~client ~store ~scope ~mailbox ~id ~message_id
    ~content_digest ~spool_ref ?flags ?internal_date ~length source =
  let* () = validate_scope ~client ~scope ~mailbox in
  let* ()=verify_mutation_destination ~client ~store ~scope ~mailbox in
  let* expected_flags = match flags with
    | None -> Ok (Some [])
    | Some flags ->
        let rec parse acc = function
          | [] -> Ok (Some (List.rev acc))
          | flag :: rest ->
              let* parsed = validation (fun s -> Incomplete s)
                (Mail_flag.Imap_flag.of_wire flag) in
              parse (parsed :: acc) rest
        in parse [] flags in
  let current = Imap_store.load_cursor store ~scope in
  let intent : Imap_store.intent = {
    id; scope; state=Imap_store.Prepared;
    kind=Imap_store.Append {message_id; content_digest; spool_ref;
      pre_send_uid_frontier=Some current.frontier;
      expected_length=Some length; expected_flags;
      expected_internal_date=Option.map
        Imap.Internal_date.to_string internal_date};
    uidvalidity=current.uidvalidity; uid=None
  } in
  Imap_store.prepare_intent store intent;
  (* Sent is durable before the first network write. A crash between this
     transition and the write is conservatively ambiguous on restart. *)
  Imap_store.set_intent_state store ~id Imap_store.Sent;
  match Imap_eio.Client.append_flow_receipt client ~mailbox ?flags
    ?internal_date ~length source with
  | Ok (Some receipt) ->
      Imap_store.confirm_intent store ~id
        ~uidvalidity:(Some receipt.uidvalidity) ~uid:(Some receipt.uid);
      Ok (Identified receipt)
  | Ok None ->
      Imap_store.set_intent_state store ~id Imap_store.Ambiguous;
      Ok Needs_reconciliation
  | Error (Imap_eio.Error.Uncertain _ as error) ->
      Imap_store.set_intent_state store ~id Imap_store.Ambiguous;
      Error (Client error)
  | Error (Imap_eio.Error.Rejected _ as error) ->
      Imap_store.set_intent_state store ~id Imap_store.Rejected;
      Error (Client error)
  | Error error ->
      Imap_store.set_intent_state store ~id Imap_store.Ambiguous;
      Error (Client error)

let append_blob_journaled ~client ~store ~scope ~mailbox ~id ~message_id
    ?flags ?internal_date blob =
  if not (Imap_store.Blob.verify store blob) then
    Error (Incomplete "APPEND source blob failed integrity verification")
  else
    Eio.Switch.run @@ fun sw ->
    let source = Imap_store.Blob.open_in store ~sw blob in
    append_journaled ~client ~store ~scope ~mailbox ~id ~message_id
      ~content_digest:blob.sha256 ~spool_ref:blob.sha256 ?flags
      ?internal_date
      ~length:blob.length source

type uid_digest = { sha256:string; length:int64 }

let with_fetched_uid ~max_bytes ~client ~store ~scope ~mailbox ~uid ~epoch
    ~spool ~on_spool =
  let* () = validate_scope ~client ~scope ~mailbox in
  let* () = guard_bound_mailbox ~client ~store ~scope ~mailbox in
  let* epoch = epoch () in
  Spool.with_spool spool (fun output ->
    let* fetch_result = network
      (Imap_eio.Client.with_mailbox client ~mode:`Read_only mailbox
        (fun selected ->
          let result =
            let* info = network (Imap_eio.Selected.info selected) in
            if info.uidvalidity <> Imap.Uidvalidity.to_int64 epoch then
              Error Uidvalidity_changed
            else network (Imap_eio.Selected.fetch_to selected
              ~max_bytes ~uid:(Imap.Uid.to_int64 uid) output)
          in Ok result)) in
    let* () = fetch_result in
    on_spool epoch spool)

let archive_uid ?(max_bytes=1_073_741_824L) ~client ~store ~scope
    ~mailbox ~uid ~spool () =
  let epoch () = match (Imap_store.load_cursor store ~scope).uidvalidity with
    | None -> Error (Incomplete "mailbox has no published UIDVALIDITY")
    | Some epoch -> Ok epoch in
  with_fetched_uid ~max_bytes ~client ~store ~scope ~mailbox ~uid ~epoch
    ~spool ~on_spool:(fun epoch spool ->
      let blob=Eio.Path.with_open_in spool (fun input ->
        let length=Optint.Int63.to_int64 (Eio.File.size input) in
        Imap_store.Blob.put store ~source:input ~length ()) in
      Imap_store.Blob.attach ~verify:false store ~scope ~uidvalidity:epoch
        ~uid blob;
      Ok blob)

type hydration_receipt = {
  cursor : Mirror.cursor;
  hydrated : int;
  bytes : int64;
  last_uid : Imap.Uid.t option;
  skipped : Imap.Uid.t list;
  more : bool;
}

type cache_audit_receipt = {
  cursor : Mirror.cursor;
  checked : int;
  invalidated : int;
  bytes : int64;
  last_uid : Imap.Uid.t option;
  skipped : Imap.Uid.t list;
  more : bool;
}

let audit_cache_once ?after_uid ?expected_revision ?(max_messages=100)
    ?(max_total_bytes=1_073_741_824L) ~store ~scope () =
  if max_messages<1 || max_messages>10_000 || max_total_bytes<1L then
    Error (Limit "invalid cache audit count or byte budget")
  else
    let cursor=Imap_store.load_cursor store ~scope in
    if (match expected_revision with
        | Some revision -> revision<>cursor.revision
        | None -> false) then Error Stale_revision else
    match cursor.phase,cursor.uidvalidity,cursor.inventory_ref with
    | Mirror.Live,Some _,Some _ ->
        let receipt ~after ~checked ~invalidated ~bytes ~skipped ~more =
          Ok ({cursor;checked;invalidated;bytes;last_uid=after;
               skipped=List.rev skipped;more} : cache_audit_receipt) in
        let stale ~after ~checked ~invalidated ~bytes ~skipped =
          if checked=0 then Error Stale_revision
          else receipt ~after ~checked ~invalidated ~bytes ~skipped
            ~more:true in
        let rec pages after considered checked invalidated bytes skipped =
          if considered>=max_messages || bytes>=max_total_bytes then
            match Imap_store.Blob.referenced_page store ~scope ~cursor
              ?after_uid:after ~limit:1 () with
            | `Stale_revision ->
                stale ~after ~checked ~invalidated ~bytes ~skipped
            | `Refs refs ->
                receipt ~after ~checked ~invalidated ~bytes ~skipped
                  ~more:(refs<>[])
          else
            match Imap_store.Blob.referenced_page store ~scope ~cursor
              ?after_uid:after ~limit:(min 100 (max_messages-considered))
              () with
            | `Stale_revision ->
                stale ~after ~checked ~invalidated ~bytes ~skipped
            | `Refs [] -> receipt ~after ~checked ~invalidated ~bytes
                ~skipped ~more:false
            | `Refs refs ->
                process after considered checked invalidated bytes skipped
                  refs
        and process after considered checked invalidated bytes skipped =
          function
          | [] -> pages after considered checked invalidated bytes skipped
          | (uid,(blob:Imap_store.Blob.blob))::rest ->
              if blob.length>max_total_bytes then
                process (Some uid) (considered+1) checked invalidated bytes
                  (uid::skipped) rest
              else if blob.length>Int64.sub max_total_bytes bytes then
                receipt ~after ~checked ~invalidated ~bytes ~skipped
                  ~more:true
              else if Imap_store.Blob.verify store blob then
                process (Some uid) (considered+1) (checked+1) invalidated
                  (Int64.add bytes blob.length) skipped rest
              else
                match Imap_store.Blob.detach_if_matches store ~scope
                  ~cursor ~uid blob with
                | `Stale_revision ->
                    stale ~after ~checked ~invalidated ~bytes ~skipped
                | (`Detached | `Unchanged) as outcome ->
                    let invalidated=if outcome=`Detached then invalidated+1
                      else invalidated in
                    process (Some uid) (considered+1) (checked+1)
                      invalidated (Int64.add bytes blob.length) skipped
                      rest in
        pages after_uid 0 0 0 0L []
    | _ -> Error (Incomplete
        "cache audit requires a complete published mailbox inventory")

let valid_spool_id id =
  id<>"" && String.length id<=128 &&
  String.for_all (function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' -> true
    | _ -> false) id

let remote_size selected uid =
  let raw_uid=Imap.Uid.to_int64 uid in
  let* metadata=network (Imap_eio.Selected.fetch_metadata_range selected
    ~first:raw_uid ~last:raw_uid ~modseq:false ~size:true) in
  match List.find_opt (fun (row:Imap.Response.fetch) ->
      row.uid=Some raw_uid) metadata with
  | None -> Error (Incomplete "published UID vanished before hydration")
  | Some {size=Some size;_} -> Ok size
  | Some _ -> Error (Incomplete "hydration FETCH omitted RFC822.SIZE")

let hydrate_once ?after_uid ?(max_messages=100)
    ?(max_body_bytes=1_073_741_824L) ?(max_total_bytes=1_073_741_824L)
    ~client ~store ~scope ~mailbox ~spool_dir ~next_spool_id () =
  if max_messages<1 || max_messages>10_000 || max_body_bytes<1L ||
     max_total_bytes<1L || not (Eio.Path.is_directory spool_dir) then
    Error (Limit "invalid hydration count, byte budget or spool directory")
  else
    let* () = validate_scope ~client ~scope ~mailbox in
    let* ()=guard_bound_mailbox ~client ~store ~scope ~mailbox in
    let cursor=Imap_store.load_cursor store ~scope in
    match cursor.phase,cursor.uidvalidity,cursor.inventory_ref with
    | Mirror.Live,Some epoch,Some _ ->
      let body_limit=Int64.min max_body_bytes max_total_bytes in
      let receipt ~after ~hydrated ~bytes ~skipped ~more =
        Ok ({cursor;hydrated;bytes;last_uid=after;skipped=List.rev skipped;
             more=more || skipped<>[]} : hydration_receipt) in
      let stale ~after ~hydrated ~bytes ~skipped =
        if hydrated=0 then Error Stale_revision
        else receipt ~after ~hydrated ~bytes ~skipped ~more:true in
      let hydrate_uid selected uid ~size =
        let id=next_spool_id () in
        if not (valid_spool_id id) then
          Error (Limit "invalid hydration spool identifier")
        else
          let spool=Eio.Path.(spool_dir / ("imap-hydrate-" ^ id)) in
          Spool.with_spool spool (fun output ->
            let* ()=network (Imap_eio.Selected.fetch_to selected
              ~max_bytes:size ~uid:(Imap.Uid.to_int64 uid) output) in
            Eio.Path.with_open_in spool (fun input ->
              let length=Optint.Int63.to_int64 (Eio.File.size input) in
              if length<>size then Error (Incomplete
                "hydrated body length differs from RFC822.SIZE")
              else
                let blob=Imap_store.Blob.put store ~source:input ~length () in
                match Imap_store.Blob.attach ~verify:false store ~scope
                    ~uidvalidity:epoch ~uid blob with
                | () -> Ok (Some length)
                | exception Invalid_argument _ -> Ok None)) in
      let hydrate selected =
        let* info=network (Imap_eio.Selected.info selected) in
        if info.uidvalidity<>
           Imap.Uidvalidity.to_int64 epoch then
          Error Uidvalidity_changed
        else
          let rec pages after considered hydrated bytes skipped =
            if considered>=max_messages || bytes>=max_total_bytes then
              match Imap_store.Blob.missing_page store ~scope ~cursor
                ?after_uid:after ~limit:1 () with
              | `Stale_revision -> stale ~after ~hydrated ~bytes ~skipped
              | `Uids uids ->
                  receipt ~after ~hydrated ~bytes ~skipped ~more:(uids<>[])
            else
              match Imap_store.Blob.missing_page store ~scope ~cursor
                ?after_uid:after ~limit:(min 100 (max_messages-considered))
                () with
              | `Stale_revision -> stale ~after ~hydrated ~bytes ~skipped
              | `Uids [] ->
                  receipt ~after ~hydrated ~bytes ~skipped ~more:false
              | `Uids uids ->
                  process after considered hydrated bytes skipped uids
          and process after considered hydrated bytes skipped = function
            | [] -> pages after considered hydrated bytes skipped
            | uid::rest ->
                let* size=remote_size selected uid in
                if size>body_limit then
                  process (Some uid) (considered+1) hydrated bytes
                    (uid::skipped) rest
                else if size>Int64.sub max_total_bytes bytes then
                  receipt ~after ~hydrated ~bytes ~skipped ~more:true
                else
                  let* attached=hydrate_uid selected uid ~size in
                  match attached with
                  | None -> stale ~after ~hydrated ~bytes ~skipped
                  | Some length ->
                      process (Some uid) (considered+1) (hydrated+1)
                        (Int64.add bytes length) skipped rest in
          pages after_uid 0 0 0L [] in
      let* nested=network (Imap_eio.Client.with_mailbox client
        ~mode:`Read_only mailbox (fun selected -> Ok (hydrate selected))) in
      nested
    | _ -> Error (Incomplete
        "hydration requires a complete published mailbox inventory")

let fetch_uid_digest ?(max_bytes=1_073_741_824L) ~client ~store ~scope
    ~mailbox ~uidvalidity ~uid ~spool () =
  with_fetched_uid ~max_bytes ~client ~store ~scope ~mailbox ~uid
    ~epoch:(fun () -> Ok uidvalidity) ~spool
    ~on_spool:(fun _epoch spool ->
      let length,sha256 = Spool.hash_file spool in
      Ok {sha256;length})
