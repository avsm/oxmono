module J = Imap_store.Sync
module P = Imap.Proto
module F = Mail_flag.Imap_flag

type error =
  | Client of Imap_eio.Error.t
  | Missing_pair
  | Stale_pair
  | Missing_occurrence
  | Uidvalidity_changed
  | Deleted_flag_held
  | Conditional_store_unavailable
  | Permanent_flag_unavailable of F.t
  | Modified
  | Pending_operation of string
  | Content_mismatch of string
  | Diverged of string

let pp_error ppf = function
  | Client error -> Imap_eio.Client.pp_error ppf error
  | Missing_pair -> Format.pp_print_string ppf "sync pair is missing"
  | Stale_pair -> Format.pp_print_string ppf "sync pair revision changed"
  | Missing_occurrence -> Format.pp_print_string ppf "paired occurrence is absent"
  | Uidvalidity_changed -> Format.pp_print_string ppf "UIDVALIDITY changed"
  | Deleted_flag_held -> Format.pp_print_string ppf "\\Deleted change needs policy"
  | Conditional_store_unavailable ->
      Format.pp_print_string ppf "conditional UID STORE is unavailable"
  | Permanent_flag_unavailable flag ->
      Format.fprintf ppf "remote flag %a is not permanently writable"
        F.pp flag
  | Modified -> Format.pp_print_string ppf "conditional UID STORE conflicted"
  | Pending_operation id -> Format.fprintf ppf "flag operation %s is pending" id
  | Content_mismatch id ->
      Format.fprintf ppf "paired local content differs for %s" id
  | Diverged text -> Format.pp_print_string ppf text

type outcome = Unchanged | Updated of J.pair
type plan = No_change | Apply of F.t list

let ( let* ) result f = match result with Ok x -> f x | Error _ as e -> e
let network = function Ok x -> Ok x | Error e -> Error (Client e)
let flags = F.durable
let same = F.equal_durable

let plan_flags ?(propagate_deleted=false) ~base ~remote ~local
    ~condstore ~remote_modseq () =
  match Imap.Sync_policy.reconcile_flags ~propagate_deleted ~base ~remote
    ~local () with
  | policy when policy.deleted_held -> Error Deleted_flag_held
  | policy ->
      let merged=flags policy.merged in
      if same remote merged && same local merged && same base merged then
        Ok No_change
      else if not (same remote merged) &&
        (not condstore ||
         (match remote_modseq with Some n when n>0L -> false | _ -> true)) then
        Error Conditional_store_unavailable
      else Ok (Apply merged)
let decode raw =
  let rec loop acc = function
    | [] -> Ok (flags acc)
    | wire :: rest ->
        (match F.of_wire wire with
         | Ok flag -> loop (flag :: acc) rest
         | Error message -> Error (Diverged message)) in
  loop [] raw

let validate_permanent_flags ~available ~remote ~merged =
  let* permanent,wildcard=match available with
    | None -> Error (Diverged "SELECT omitted PERMANENTFLAGS")
    | Some raw ->
        let rec collect known wildcard = function
          | [] -> Ok (known,wildcard)
          | "\\*" :: rest -> collect known true rest
          | wire :: rest ->
              (match F.of_wire wire with
               | Ok flag -> collect (flag::known) wildcard rest
               | Error _ -> Error (Diverged
                   "SELECT returned malformed PERMANENTFLAGS")) in
        collect [] false raw in
  let permitted ~adding flag =
    List.exists (F.equal flag) permanent ||
    (adding && wildcard && match flag with F.Keyword _ -> true | _ -> false) in
  let find_bad ~adding from other =
    List.find_opt (fun flag ->
      not (List.exists (F.equal flag) other) &&
      not (permitted ~adding flag)) from in
  match find_bad ~adding:true merged remote with
  | Some flag -> Error (Permanent_flag_unavailable flag)
  | None ->
      (match find_bad ~adding:false remote merged with
       | Some flag -> Error (Permanent_flag_unavailable flag)
       | None -> Ok ())

let check_permanent_flags selected ~remote ~merged =
  let* info=network (Imap_eio.Selected.info selected) in
  validate_permanent_flags ~available:info.permanentflags ~remote ~merged

let bound (pair:J.pair) =
  match pair.remote_uidvalidity,pair.remote_uid,pair.local_id,
        pair.remote_tombstone,pair.local_tombstone with
  | Some epoch,Some uid,Some local_id,None,None ->
      Ok (epoch,uid,local_id)
  | _ -> Error Missing_occurrence

let current_pair store (pair:J.pair) =
  match J.find_pair store ~id:pair.id with
  | None -> Error Missing_pair
  | Some current when current.revision<>pair.revision || current<>pair ->
      Error Stale_pair
  | Some current -> Ok current

let local maildir id =
  match Imap_maildir.find maildir ~id with
  | None -> Error Missing_occurrence
  | Some occurrence -> Ok occurrence

let local_content_matches maildir (pair:J.pair)
    (occurrence:Imap_maildir.occurrence) =
  match pair.content_sha256,pair.content_length with
  | Some sha256,Some length ->
      occurrence.length=length &&
      Imap_maildir.sha256 maildir occurrence=sha256
  | _ -> false

let remote selected ~epoch ~uid ~modseq =
  let* info = network (Imap_eio.Selected.info selected) in
  if info.uidvalidity<>P.Uidvalidity.to_int64 epoch then
    Error Uidvalidity_changed
  else
    let raw=P.Uid.to_int64 uid in
    let* rows=network (Imap_eio.Selected.fetch_metadata_range selected
      ~first:raw ~last:raw ~modseq) in
    match rows with
    | [row] when row.uid=Some raw ->
        (match row.flags with
         | Some wire ->
             let* flags=decode wire in
             Ok (flags,row.modseq)
         | None -> Error (Diverged "UID FETCH omitted FLAGS"))
    | _ -> Error Missing_occurrence

let pending_for_pair store (pair:J.pair) =
  J.active_operation_for_pair store ~pair_id:pair.id

let intent (pair:J.pair) ~id ~epoch ~uid ~local_id ~merged : J.operation = {
  id;pair_id=Some pair.id;local_id=Some local_id;scope=pair.scope;
  kind=J.Flags;state=J.Prepared;
  source_uidvalidity=Some epoch;source_uid=Some uid;
  destination=None;destination_uidvalidity=None;
  blob_sha256=None;blob_length=None;desired_flags=Some (flags merged);
  receipt=None;receipt_uidvalidity=None;receipt_uid=None}

let commit store (pair:J.pair) ~id ~merged =
  let next={pair with common_flags=flags merged} in
  match J.commit_operation_with_pair store ~id
    ~expected_pair_revision:(Some pair.revision) next with
  | `Committed pair -> Ok (Updated pair)
  | `Stale_revision -> Error Stale_pair

let pending store (pair:J.pair) ~id evidence =
  match J.ensure_open_conflict store ~pair ~kind:J.Flag_conflict
    ~id:(id ^ "-flag-conflict") ~evidence with
  | `Open _ -> Error (Pending_operation id)
  | `Stale_revision -> Error Stale_pair

let hold_content store (pair:J.pair) ~id =
  match J.ensure_open_conflict store ~pair ~kind:J.Content_conflict
    ~id ~evidence:"local message content differs from the paired digest" with
  | `Open _ -> Error (Content_mismatch pair.id)
  | `Stale_revision -> Error Stale_pair

let clear_content_hold store (pair:J.pair) =
  match J.resolve_open_conflicts store ~pair ~kind:J.Content_conflict with
  | `Resolved _ -> Ok ()
  | `Stale_revision -> Error Stale_pair

let recover_operation ~client ~store ~maildir ~mailbox
    ~(operation:J.operation) =
  if operation.kind<>J.Flags then
    Error (Diverged "operation is not FLAGS")
  else if operation.state=J.Prepared then (
    J.reject_prepared_operation store ~id:operation.id
      ~receipt:"prepared FLAGS was not dispatched";
    Ok Unchanged)
  else (match operation.pair_id,operation.desired_flags with
  | Some id,Some merged ->
      let* pair=match J.find_pair store ~id with
        | Some pair when pair.scope=operation.scope -> Ok pair
        | Some _ -> Error Stale_pair
        | None -> Error Missing_pair in
      let* epoch,uid,local_id=bound pair in
      if operation.source_uidvalidity<>Some epoch ||
         operation.source_uid<>Some uid ||
         operation.local_id<>Some local_id then Error Stale_pair
      else if J.operation_pair_revision store ~id:operation.id<>
              Some pair.revision then Error Stale_pair
      else (match operation.state with
      | J.Prepared -> assert false
      | J.Sent | J.Ambiguous | J.Observed ->
          let* local_before=local maildir local_id in
          let* ()=if local_content_matches maildir pair local_before
            then Ok ()
            else pending store pair ~id:operation.id
              "uncertain FLAGS write: local message content differs from the paired digest" in
          let* remote_flags=match Imap_eio.Client.with_mailbox client
            ~mode:`Read_only mailbox (fun selected ->
              Ok (remote selected ~epoch ~uid ~modseq:false)) with
            | Error e -> Error (Client e)
            | Ok (Ok (flags,_)) -> Ok flags
            | Ok (Error e) -> Error e in
          if not (same remote_flags merged) then
            pending store pair ~id:operation.id
              "uncertain FLAGS write: remote flags differ from the journaled target"
          else
            let* () = if same local_before.flags merged then Ok ()
              else match J.local_flags_preimage store ~id:operation.id with
              | Some before when same local_before.flags before ->
                  (* A sent/ambiguous STORE may have reached the server.  Its
                     verified target and the unchanged Maildir preimage are
                     enough to finish locally, without replaying STORE. *)
                  let* _=current_pair store pair in
                  ignore (Imap_maildir.set_flags maildir local_before merged);
                  Ok ()
              | _ -> pending store pair ~id:operation.id
                  "uncertain FLAGS write: local flags differ from the saved preimage" in
            let* local_after=local maildir local_id in
            if not (local_content_matches maildir pair local_after) then
              pending store pair ~id:operation.id
                "uncertain FLAGS write: local message content changed during recovery"
            else if not (same local_after.flags merged) then
              pending store pair ~id:operation.id
                "uncertain FLAGS write: local flags did not reach the target"
            else
              let* remote_after=match Imap_eio.Client.with_mailbox client
                ~mode:`Read_only mailbox (fun selected ->
                  Ok (remote selected ~epoch ~uid ~modseq:false)) with
                | Error e -> Error (Client e)
                | Ok (Ok (flags,_)) -> Ok flags
                | Ok (Error e) -> Error e in
              if not (same remote_after merged) then
                pending store pair ~id:operation.id
                  "uncertain FLAGS write: remote flags changed during verification"
              else (
                let* _=current_pair store pair in
                if operation.state<>J.Observed then
                  J.observe_operation store ~id:operation.id
                    ~receipt:"verified FLAGS on both endpoints"
                    ~destination_uidvalidity:None ~destination_uid:None;
                commit store pair ~id:operation.id ~merged)
      | J.Committed | J.Rejected -> Error (Diverged "operation is terminal"))
  | _ -> Error (Diverged "FLAGS operation has no pair or target flags"))

let settle_operation ~client ~store ~maildir ~scope ~mailbox ~id ~evidence () =
  if String.trim evidence="" || String.length evidence>1024 ||
     not (String.for_all (fun c -> let n=Char.code c in
       n>=32 && n<>127) evidence) then
    Error (Diverged "operator evidence must be 1..1024 printable bytes")
  else Imap_maildir.with_writer_lock maildir (fun () ->
    let* operation=match J.find_operation store ~id with
      | Some op when op.scope=scope && op.kind=J.Flags &&
          List.mem op.state [J.Sent;J.Ambiguous;J.Observed] -> Ok op
      | _ -> Error (Diverged "no pending FLAGS operation in this scope") in
    let* pair=match operation.pair_id with
      | Some pair_id ->
          (match J.find_pair store ~id:pair_id with
           | Some pair when pair.scope=scope -> Ok pair
           | _ -> Error Missing_pair)
      | None -> Error Missing_pair in
    let* epoch,uid,local_id=bound pair in
    if operation.source_uidvalidity<>Some epoch ||
       operation.source_uid<>Some uid ||
       operation.local_id<>Some local_id ||
       J.operation_pair_revision store ~id<>Some pair.revision then
      Error Stale_pair
    else
      let* ()=match Engine.guard_bound_mailbox ~client ~store ~scope
          ~mailbox with
        | Ok () -> Ok ()
        | Error (Engine.Client error) -> Error (Client error)
        | Error error -> Error (Diverged
            (Format.asprintf "%a" Engine.pp_error error)) in
      let local_read () =
        let* occurrence=local maildir local_id in
        if not (local_content_matches maildir pair occurrence) then
          Error (Diverged "local message differs from paired content")
        else match pair.internal_date with
          | None -> Ok occurrence
          | Some date ->
              (match Imap_maildir.upload_internal_date occurrence with
               | Ok observed when Imap.Internal_date.equal_instant observed date -> Ok occurrence
               | _ -> Error (Diverged
                   "local message date differs from paired date")) in
      let remote_read () =
        match Imap_eio.Client.with_mailbox client ~mode:`Read_only mailbox
          (fun selected -> Ok (remote selected ~epoch ~uid ~modseq:true)) with
        | Error e -> Error (Client e)
        | Ok result -> result in
      let* local_before=local_read () in
      let* remote_flags,first_modseq=remote_read () in
      let* modseq=match first_modseq with
        | Some value when value>0L -> Ok value
        | _ -> Error Conditional_store_unavailable in
      if not (same local_before.flags remote_flags) then
        Error (Diverged "remote and local flags still differ")
      else
        let* local_after=local_read () in
        if not (same local_after.flags local_before.flags) then
          Error (Diverged "local flags changed during repair")
        else
          let* remote_after,second_modseq=remote_read () in
          if not (same remote_after remote_flags) ||
             second_modseq<>Some modseq then
            Error (Diverged "remote flags changed during repair")
          else
            let* _=current_pair store pair in
            match J.settle_flag_operation store ~id pair
                ~flags:remote_flags ~evidence with
            | `Settled pair -> Ok (Updated pair)
            | `Stale_revision -> Error Stale_pair
            | `Invalid_operation -> Error (Diverged
                "FLAGS operation changed during repair"))

let reconcile_pair ?(propagate_deleted=false) ~client ~store ~maildir
    ~mailbox ~(pair:J.pair) ~next_id () =
  let* pair=current_pair store pair in
  let* epoch,uid,local_id=bound pair in
  match pending_for_pair store pair with
  | Some op -> Error (Pending_operation op.id)
  | None ->
      let* local_before=local maildir local_id in
      let* ()=if local_content_matches maildir pair local_before then
          clear_content_hold store pair
        else hold_content store pair ~id:(next_id ()) in
      let condstore=List.mem "CONDSTORE" (Imap_eio.Client.capabilities client) ||
        List.mem "QRESYNC" (Imap_eio.Client.capabilities client) in
      let reconcile_selected selected =
        let* remote_before,remote_modseq =
          remote selected ~epoch ~uid ~modseq:condstore in
        let* plan=plan_flags ~propagate_deleted ~base:pair.common_flags
          ~remote:remote_before ~local:local_before.flags ~condstore
          ~remote_modseq () in
        match plan with
        | No_change -> Ok Unchanged
        | Apply merged ->
        let remote_needed=not (same remote_before merged) in
        let local_needed=not (same local_before.flags merged) in
        let* ()=if remote_needed then
            check_permanent_flags selected ~remote:remote_before ~merged
          else Ok () in
        (
          let id=next_id () in
          J.prepare_operation ~local_flags:local_before.flags store
            (intent pair ~id ~epoch ~uid ~local_id ~merged);
          let* ()=match current_pair store pair with
            | Ok _ -> Ok ()
            | Error error ->
                J.reject_prepared_operation store ~id
                  ~receipt:"pair changed before FLAGS dispatch";
                Error error in
          J.mark_sent store ~id;
          let set=P.Uid_set.singleton uid in
          let write_result =
            if not remote_needed then Ok () else
            let result=Imap_eio.Selected.uid_store_flags selected ~set
              ~operation:`Replace ~flags:merged
              ?unchangedsince:remote_modseq () in
            match result with
            | Error e -> Error (Client e)
            | Ok receipt when P.Uid_set.mem uid receipt.modified ->
                Error Modified
            | Ok receipt when P.Uid_set.to_wire receipt.modified<>"" ->
                Error (Diverged "MODIFIED named an unrelated UID")
            | Ok _ -> Ok () in
          match write_result with
          | Error Modified ->
              J.reject_operation store ~id ~receipt:"MODIFIED";
              Error Modified
          | Error e ->
              J.mark_ambiguous store ~id;
              Error e
          | Ok () ->
              let* observed,_=remote selected ~epoch ~uid ~modseq:false in
              if not (same observed merged) then
                pending store pair ~id
                  "FLAGS write returned but remote flags differ from the target"
              else (
                let* local_now=local maildir local_id in
                if not (local_content_matches maildir pair local_now) then
                  pending store pair ~id
                    "local message content changed while the FLAGS write was in flight"
                else if not (same local_now.flags local_before.flags) then
                  pending store pair ~id
                    "local flags changed while the FLAGS write was in flight"
                else (
                  if local_needed then
                    ignore (Imap_maildir.set_flags maildir local_now merged);
                  let* local_after=local maildir local_id in
                  if not (local_content_matches maildir pair local_after) then
                    pending store pair ~id
                      "local message content changed during the FLAGS update"
                  else if not (same local_after.flags merged) then
                    pending store pair ~id
                      "local flags did not reach the journaled target"
                  else
                    let* observed_again,_=remote selected ~epoch ~uid
                      ~modseq:false in
                    if not (same observed_again merged) then
                      pending store pair ~id
                        "remote flags changed after the local FLAGS update"
                    else (
                      J.observe_operation store ~id
                        ~receipt:"verified FLAGS on both endpoints"
                        ~destination_uidvalidity:None ~destination_uid:None;
                      commit store pair ~id ~merged)))) in
      (* Preserve the typed policy/journal errors across [with_mailbox]'s
         client-error-only callback by nesting a result. *)
      match Imap_eio.Client.with_mailbox client ~mode:`Read_write mailbox
        (fun selected -> Ok (reconcile_selected selected)) with
      | Error e -> Error (Client e)
      | Ok result -> result
