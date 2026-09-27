module J = Imap_store.Journal
module P = Imap.Proto
module F = Mail_flag.Imap_flag

type error =
  | Client of Imap_eio.Error.t
  | Missing_pair
  | Stale_pair
  | Missing_occurrence
  | Uidvalidity_changed
  | Conditional_store_unavailable
  | Permanent_flag_unavailable of F.t
  | Modified
  | Pending_operation of string
  | No_pending_operation
  | Content_mismatch of string
  | Diverged of string

let pp_error ppf = function
  | Client error -> Imap_eio.Client.pp_error ppf error
  | Missing_pair -> Format.pp_print_string ppf "sync pair is missing"
  | Stale_pair -> Format.pp_print_string ppf "sync pair revision changed"
  | Missing_occurrence ->
      Format.pp_print_string ppf "paired occurrence is absent"
  | Uidvalidity_changed -> Format.pp_print_string ppf "UIDVALIDITY changed"
  | Conditional_store_unavailable ->
      Format.pp_print_string ppf "conditional UID STORE is unavailable"
  | Permanent_flag_unavailable flag ->
      Format.fprintf ppf "remote flag %a is not permanently writable"
        F.pp flag
  | Modified -> Format.pp_print_string ppf "flags changed concurrently"
  | Pending_operation id -> Format.fprintf ppf "flag operation %s is pending" id
  | No_pending_operation ->
      Format.pp_print_string ppf "no pending FLAGS operation in this scope"
  | Content_mismatch id ->
      Format.fprintf ppf "paired local content differs for %s" id
  | Diverged text -> Format.pp_print_string ppf text

type outcome = Unchanged | Updated of J.pair
type plan = No_change | Apply of F.t list
type decision = { plan : plan; deleted_held : bool }
type reconciled = { outcome : outcome; deleted_held : bool }

let ( let* ) result f = match result with Ok x -> f x | Error _ as e -> e
let network = function Ok x -> Ok x | Error e -> Error (Client e)
let flags = F.durable
let same = F.equal_durable

let deleted = F.system F.Deleted
let has_deleted flags = List.exists (F.equal deleted) flags

(* An operation whose target agrees with the base on \Deleted never writes
   it: each endpoint keeps its own \Deleted state, which is how a held
   \Deleted change survives while the other flags merge. *)
let target ~base ~desired observed =
  if has_deleted desired<>has_deleted base ||
     has_deleted observed=has_deleted desired then desired
  else if has_deleted observed then flags (deleted :: desired)
  else List.filter (fun flag -> not (F.equal deleted flag)) desired

let plan_flags ?(propagate_deleted=false) ~base ~remote ~local
    ~condstore ~remote_modseq () =
  let policy=Imap.Sync_policy.reconcile_flags ~propagate_deleted ~base
    ~remote ~local () in
  let merged=flags policy.merged in
  let remote_target=target ~base ~desired:merged remote in
  let local_target=target ~base ~desired:merged local in
  let deleted_held=policy.deleted_held in
  if same remote remote_target && same local local_target &&
     same base merged then
    Ok {plan=No_change;deleted_held}
  else if not (same remote remote_target) &&
    (not condstore ||
     (match remote_modseq with Some n when n>0L -> false | _ -> true)) then
    Error Conditional_store_unavailable
  else Ok {plan=Apply merged;deleted_held}

let decode raw =
  let rec loop acc = function
    | [] -> Ok (flags acc)
    | wire :: rest ->
        (match F.of_wire wire with
         | Ok flag -> loop (flag :: acc) rest
         | Error message -> Error (Diverged message)) in
  loop [] raw

let validate_permanent_flags ~available ~defined ~remote ~merged =
  match available with
  | None -> Ok ()
  | Some raw ->
      let rec collect known wildcard = function
        | [] -> Ok (known,wildcard)
        | "\\*" :: rest -> collect known true rest
        | wire :: rest ->
            (match F.of_wire wire with
             | Ok flag -> collect (flag::known) wildcard rest
             | Error _ -> Error (Diverged
                 "SELECT returned malformed PERMANENTFLAGS")) in
      let* permanent,wildcard=collect [] false raw in
      let defined=List.filter_map (fun wire ->
        Result.to_option (F.of_wire wire))
        (Option.value ~default:[] defined) in
      let new_keyword = function
        | F.Keyword _ as flag -> not (List.exists (F.equal flag) defined)
        | _ -> false in
      let permitted ~adding flag =
        List.exists (F.equal flag) permanent ||
        (adding && wildcard && new_keyword flag) in
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

let bound (pair:J.pair) =
  match pair.remote_uidvalidity,pair.remote_uid,pair.local_id,
        pair.remote_tombstone,pair.local_tombstone with
  | Some epoch,Some uid,Some local_id,None,None ->
      Ok (epoch,uid,local_id)
  | _ -> Error Missing_occurrence

let current_pair store (pair:J.pair) =
  match J.find_pair store ~id:pair.id with
  | None -> Error Missing_pair
  | Some current when current<>pair -> Error Stale_pair
  | Some current -> Ok current

let local ?inventory maildir id =
  let found=match inventory with
    | Some inventory -> Local_inventory.find inventory ~id
    | None -> Maildir.find maildir ~id in
  match found with
  | None -> Error Missing_occurrence
  | Some occurrence -> Ok occurrence

let unchanged ?inventory maildir occurrence =
  match Local_inventory.with_unchanged_occurrence ?inventory maildir occurrence
      ignore with
  | Ok () -> true
  | Error `Changed -> false

(* [`Changed] means the observation is stale, not that the bytes differ. *)
let content ?inventory maildir (pair:J.pair)
    (occurrence:Maildir.occurrence) =
  match pair.content_sha256,pair.content_length with
  | Some sha256,Some length when occurrence.length=length ->
      (match Local_inventory.with_unchanged_occurrence ?inventory maildir
          occurrence (fun () ->
            Local_inventory.sha256 ?inventory maildir occurrence) with
       | Ok digest when digest=sha256 -> `Matches
       | Ok _ -> `Differs
       | Error `Changed -> `Changed)
  | Some _,Some _ ->
      if unchanged ?inventory maildir occurrence then `Differs else `Changed
  | _ -> `Differs

let check_epoch (info:Imap.Response.select_metadata) epoch =
  if info.uidvalidity<>P.Uidvalidity.to_int64 epoch then
    Error Uidvalidity_changed
  else Ok ()

let remote selected ~uid ~modseq =
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

let with_selected client ~mode mailbox ~epoch f =
  match Imap_eio.Client.with_mailbox client ~mode mailbox (fun selected ->
    Ok (let* info=network (Imap_eio.Selected.info selected) in
        let* ()=check_epoch info epoch in
        f selected info)) with
  | Error e -> Error (Client e)
  | Ok result -> result

let remote_now client ~mailbox ~epoch ~uid ~modseq =
  with_selected client ~mode:`Read_only mailbox ~epoch (fun selected _ ->
    remote selected ~uid ~modseq)

let engine_error = function
  | Engine.Client error -> Client error
  | Engine.Stale_revision -> Stale_pair
  | Engine.Uidvalidity_changed -> Uidvalidity_changed
  | error -> Diverged (Format.asprintf "%a" Engine.pp_error error)

let describe error =
  let text=Format.asprintf "%a" Imap_eio.Client.pp_error error in
  if String.length text<=512 then text else String.sub text 0 512

let kind_name = function
  | J.Append -> "APPEND" | J.Local_append -> "local APPEND"
  | J.Copy -> "COPY" | J.Move -> "MOVE" | J.Flags -> "FLAGS"
  | J.Delete -> "DELETE" | J.Local_delete -> "local DELETE"

let intent (pair:J.pair) ~id ~epoch ~uid ~local_id ~merged : J.operation = {
  id;pair_id=Some pair.id;local_id=Some local_id;scope=pair.scope;
  kind=J.Flags;state=J.Prepared;
  source_uidvalidity=Some epoch;source_uid=Some uid;
  destination=None;destination_uidvalidity=None;
  blob_sha256=None;blob_length=None;desired_flags=Some merged;
  receipt=None;receipt_uidvalidity=None;receipt_uid=None}

let commit store (pair:J.pair) ~id ~merged =
  let next={pair with common_flags=flags merged} in
  match J.commit_operation_with_pair store ~id
    ~expected_pair_revision:(Some pair.revision) next with
  | `Committed pair -> Ok (Updated pair)
  | `Stale_revision -> Error Stale_pair

let flag_conflict store (pair:J.pair) ~id evidence =
  match J.ensure_open_conflict store ~pair ~kind:J.Flag_conflict
    ~id:(id ^ "-flag-conflict") ~evidence with
  | `Open _ -> Ok ()
  | `Stale_revision -> Error Stale_pair

let pending store pair ~id evidence =
  let* ()=flag_conflict store pair ~id evidence in
  Error (Pending_operation id)

let hold_content store (pair:J.pair) ~id =
  match J.ensure_open_conflict store ~pair ~kind:J.Content_conflict
    ~id ~evidence:"local message content differs from the paired digest" with
  | `Open _ -> Error (Content_mismatch pair.id)
  | `Stale_revision -> Error Stale_pair

let clear_content_hold store (pair:J.pair) =
  match J.resolve_open_conflicts store ~pair ~kind:J.Content_conflict with
  | `Resolved _ -> Ok ()
  | `Stale_revision -> Error Stale_pair

let recover_sent ~inventory ~client ~store ~maildir ~mailbox
    ~(operation:J.operation) ~pair_id ~merged =
  let* pair=match J.find_pair store ~id:pair_id with
    | Some pair when pair.scope=operation.scope -> Ok pair
    | Some _ -> Error Stale_pair
    | None -> Error Missing_pair in
  let* epoch,uid,local_id=bound pair in
  if operation.source_uidvalidity<>Some epoch ||
     operation.source_uid<>Some uid ||
     operation.local_id<>Some local_id ||
     J.operation_pair_revision store ~id:operation.id<>Some pair.revision then
    Error Stale_pair
  else
    let base=pair.common_flags in
    let pending=pending store pair ~id:operation.id in
    let* staged=local ?inventory maildir local_id in
    let* local_before,state=match content ?inventory maildir pair staged with
      | `Changed ->
          let* fresh=local maildir local_id in
          Ok (fresh,content maildir pair fresh)
      | state -> Ok (staged,state) in
    let* ()=if state=`Matches then Ok ()
      else pending
        "uncertain FLAGS write: local message content differs from the paired \
         digest" in
    let* remote_flags,_=remote_now client ~mailbox ~epoch ~uid ~modseq:false in
    if not (same remote_flags (target ~base ~desired:merged remote_flags)) then
      pending
        "uncertain FLAGS write: remote flags differ from the journaled target"
    else
      let local_target=target ~base ~desired:merged local_before.flags in
      let* local_after=
        if same local_before.flags local_target then
          if unchanged maildir local_before then Ok local_before
          else pending
            "uncertain FLAGS write: local message changed during recovery"
        else match J.local_flags_preimage store ~id:operation.id with
          | Some before when same local_before.flags before ->
              (* A sent/ambiguous STORE may have reached the server.  Its
                 verified target and the unchanged Maildir preimage are
                 enough to finish locally, without replaying STORE. *)
              let* _=current_pair store pair in
              (match Maildir.set_flags maildir local_before local_target
               with
               | exception Maildir.Stale_occurrence ->
                   pending
                     "uncertain FLAGS write: local message changed during \
                      recovery"
               | written when content maildir pair written=`Matches ->
                   Ok written
               | _ -> pending
                   "uncertain FLAGS write: local message content changed \
                    during recovery")
          | _ -> pending
              "uncertain FLAGS write: local flags differ from the saved \
               preimage" in
      if not (same local_after.flags local_target) then
        pending "uncertain FLAGS write: local flags did not reach the target"
      else
        let* remote_after,_=remote_now client ~mailbox ~epoch ~uid
          ~modseq:false in
        if not (same remote_after (target ~base ~desired:merged remote_after))
        then pending
          "uncertain FLAGS write: remote flags changed during verification"
        else (
          if operation.state<>J.Observed then
            J.observe_operation store ~id:operation.id
              ~receipt:"verified FLAGS on both endpoints"
              ~destination_uidvalidity:None ~destination_uid:None;
          commit store pair ~id:operation.id ~merged)

let recover_operation ?inventory ~client ~store ~maildir ~mailbox
    ~(operation:J.operation) () =
  match operation.kind,operation.state with
  | kind,_ when kind<>J.Flags -> Error (Diverged "operation is not FLAGS")
  | _,J.Prepared ->
      J.reject_prepared_operation store ~id:operation.id
        ~receipt:"prepared FLAGS was not dispatched";
      Ok Unchanged
  | _,(J.Committed | J.Rejected) -> Error (Diverged "operation is terminal")
  | _,(J.Sent | J.Ambiguous | J.Observed) ->
      match operation.pair_id,operation.desired_flags with
      | Some pair_id,Some merged ->
          recover_sent ~inventory ~client ~store ~maildir ~mailbox ~operation
            ~pair_id ~merged
      | _ -> Error (Diverged "FLAGS operation has no pair or target flags")

let settle_operation ~client ~store ~maildir ~scope ~mailbox ~id ~evidence () =
  if String.trim evidence="" || String.length evidence>1024 ||
     not (String.for_all (fun c -> let n=Char.code c in
       n>=32 && n<>127) evidence) then
    Error (Diverged "operator evidence must be 1..1024 printable bytes")
  else Maildir.with_writer_lock maildir (fun () ->
    let* operation=match J.find_operation store ~id with
      | Some op when op.scope=scope && op.kind=J.Flags &&
          List.mem op.state [J.Sent;J.Ambiguous;J.Observed] -> Ok op
      | _ -> Error No_pending_operation in
    let* pair=match operation.pair_id with
      | Some pair_id ->
          (match J.find_pair store ~id:pair_id with
           | Some pair when pair.scope=scope -> Ok pair
           | Some _ -> Error Stale_pair
           | None -> Error Missing_pair)
      | None -> Error Missing_pair in
    let* epoch,uid,local_id=bound pair in
    if operation.source_uidvalidity<>Some epoch ||
       operation.source_uid<>Some uid ||
       operation.local_id<>Some local_id ||
       J.operation_pair_revision store ~id<>Some pair.revision then
      Error Stale_pair
    else
      let* ()=Result.map_error engine_error
        (Engine.guard_bound_mailbox ~client ~store ~scope ~mailbox) in
      let local_read () =
        let* occurrence=local maildir local_id in
        if content maildir pair occurrence<>`Matches then
          Error (Content_mismatch pair.id)
        else match pair.internal_date with
          | None -> Ok occurrence
          | Some date ->
              (match Local_date.of_occurrence occurrence with
               | Ok observed when
                   Imap.Internal_date.equal_instant observed date ->
                   Ok occurrence
               | _ -> Error (Diverged
                   "local message date differs from paired date")) in
      let remote_read () =
        remote_now client ~mailbox ~epoch ~uid ~modseq:true in
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
            match J.settle_flag_operation store ~id pair
                ~flags:remote_flags ~evidence with
            | `Settled pair -> Ok (Updated pair)
            | `Stale_revision -> Error Stale_pair
            | `Invalid_operation -> Error (Diverged
                "FLAGS operation changed during repair"))

let advance_baseline store (pair:J.pair) ~merged =
  match J.put_pair store ~expected_revision:(Some pair.revision)
    {pair with common_flags=merged} with
  | `Committed pair -> Ok (Updated pair)
  | `Stale_revision -> Error Stale_pair

let reconcile_pair ?(propagate_deleted=false) ?inventory ~client ~store
    ~maildir ~mailbox ~(pair:J.pair) ~next_id () =
  let* pair=current_pair store pair in
  let* epoch,uid,local_id=bound pair in
  match J.active_operation_for_pair store ~pair_id:pair.id with
  | Some op when op.kind=J.Flags -> Error (Pending_operation op.id)
  | Some op -> Error (Diverged (Printf.sprintf
      "pair has a pending %s operation %s" (kind_name op.kind) op.id))
  | None ->
      let* local_before=local ?inventory maildir local_id in
      let* ()=match content ?inventory maildir pair local_before with
        | `Matches -> clear_content_hold store pair
        | `Differs -> hold_content store pair ~id:(next_id ())
        | `Changed -> Error Modified in
      let has=Imap_eio.Client.has client in
      let condstore=has Imap.Capability.Condstore ||
        has Imap.Capability.Qresync in
      let base=pair.common_flags in
      let apply selected (info:Imap.Response.select_metadata)
          ~remote_before ~remote_modseq ~merged =
        let remote_target=target ~base ~desired:merged remote_before in
        let local_target=target ~base ~desired:merged local_before.flags in
        let remote_needed=not (same remote_before remote_target) in
        let local_needed=not (same local_before.flags local_target) in
        if not remote_needed && not local_needed then
          advance_baseline store pair ~merged
        else
        let* ()=if remote_needed then
            validate_permanent_flags ~available:info.permanentflags
              ~defined:info.flags ~remote:remote_before ~merged:remote_target
          else Ok () in
        let id=next_id () in
        let pending=pending store pair ~id in
        let unsent receipt error =
          J.reject_prepared_operation store ~id ~receipt;
          Error error in
        J.prepare_operation ~local_flags:local_before.flags store
          (intent pair ~id ~epoch ~uid ~local_id ~merged);
        let write_local ~stale =
          if not local_needed then
            if unchanged ?inventory maildir local_before then Ok local_before
            else stale ()
          else match Maildir.set_flags maildir local_before local_target
          with
          | exception Maildir.Stale_occurrence -> stale ()
          | written when content maildir pair written=`Matches -> Ok written
          | _ -> pending
              "local message content changed during the FLAGS update" in
        let finish ~stale ~verify_remote =
          let* local_after=write_local ~stale in
          if not (same local_after.flags local_target) then
            pending "local flags did not reach the journaled target"
          else
            let* ()=if not verify_remote then Ok () else
              let* observed,_=remote selected ~uid ~modseq:false in
              if same observed remote_target then Ok ()
              else pending
                "remote flags changed after the local FLAGS update" in
            J.observe_operation store ~id
              ~receipt:"verified FLAGS on both endpoints"
              ~destination_uidvalidity:None ~destination_uid:None;
            commit store pair ~id ~merged in
        match current_pair store pair with
        | Error error -> unsent "pair changed before FLAGS dispatch" error
        | Ok _ when not remote_needed ->
            (match remote selected ~uid ~modseq:false with
             | Error error ->
                 unsent "remote read failed before FLAGS dispatch" error
             | Ok (observed,_) when not (same observed remote_before) ->
                 unsent "remote flags changed before the local FLAGS write"
                   Modified
             | Ok _ when not (unchanged ?inventory maildir local_before) ->
                 unsent "local message changed before the FLAGS write"
                   Modified
             | Ok _ ->
                 J.mark_sent store ~id;
                 finish ~verify_remote:false ~stale:(fun () ->
                   J.reject_operation store ~id
                     ~receipt:"local message changed before the FLAGS write";
                   Error Modified))
        | Ok _ ->
            J.mark_sent store ~id;
            let set=P.Uid_set.singleton uid in
            match Imap_eio.Selected.uid_store_flags selected ~set
                ~operation:`Replace ~flags:remote_target
                ?unchangedsince:remote_modseq () with
            | Error (Imap_eio.Error.Rejected _ | Imap_eio.Error.State _
                     | Imap_eio.Error.Unsupported _
                     | Imap_eio.Error.Not_enabled _ as error) ->
                J.reject_operation store ~id
                  ~receipt:("UID STORE not applied: " ^ describe error);
                Error (Client error)
            | Error error ->
                let reason="UID STORE outcome unknown: " ^ describe error in
                J.mark_ambiguous ~reason store ~id;
                let* ()=flag_conflict store pair ~id reason in
                Error (Client error)
            | Ok receipt when P.Uid_set.mem uid receipt.modified ->
                J.reject_operation store ~id ~receipt:"MODIFIED";
                Error Modified
            | Ok receipt when not (P.Uid_set.is_empty receipt.modified) ->
                let reason="MODIFIED named an unrelated UID" in
                J.mark_ambiguous ~reason store ~id;
                pending reason
            | Ok _ ->
                match remote selected ~uid ~modseq:false with
                | Error error ->
                    let* ()=flag_conflict store pair ~id
                      "FLAGS write returned but its verification read failed" in
                    Error error
                | Ok (observed,_) when not (same observed remote_target) ->
                    pending
                      "FLAGS write returned but remote flags differ from the \
                       target"
                | Ok _ ->
                    finish ~verify_remote:true ~stale:(fun () ->
                      pending
                        "local message changed while the FLAGS write was in \
                         flight") in
      with_selected client ~mode:`Read_write mailbox ~epoch
        (fun selected info ->
          let* remote_before,remote_modseq=
            remote selected ~uid ~modseq:condstore in
          let* decision=plan_flags ~propagate_deleted ~base
            ~remote:remote_before ~local:local_before.flags ~condstore
            ~remote_modseq () in
          let* outcome=match decision.plan with
            | No_change -> Ok Unchanged
            | Apply merged ->
                apply selected info ~remote_before ~remote_modseq ~merged in
          Ok {outcome;deleted_held=decision.deleted_held})
