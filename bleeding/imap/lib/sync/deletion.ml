module J = Imap_store.Sync
module P = Imap.Proto
module F = Mail_flag.Imap_flag

type error =
  | Client of Imap_eio.Error.t
  | Missing_pair
  | Stale_pair
  | Stale_inventory
  | Identity_changed
  | Unsupported of string
  | Pending_operation of string
  | Diverged of string

let pp_error ppf = function
  | Client e -> Imap_eio.Client.pp_error ppf e
  | Missing_pair -> Format.pp_print_string ppf "sync pair is missing"
  | Stale_pair -> Format.pp_print_string ppf "sync pair revision changed"
  | Stale_inventory -> Format.pp_print_string ppf "complete inventory changed"
  | Identity_changed -> Format.pp_print_string ppf "paired occurrence identity changed"
  | Unsupported s -> Format.fprintf ppf "deletion unavailable: %s" s
  | Pending_operation id -> Format.fprintf ppf "deletion %s remains pending" id
  | Diverged s -> Format.pp_print_string ppf s

type outcome = Unchanged | Held of Imap.Sync_policy.deletion_hold | Deleted of J.pair

let ( let* ) result f = match result with Ok x -> f x | Error _ as e -> e
let network = function Ok x -> Ok x | Error e -> Error (Client e)

let flags = F.durable
let same_flags = F.equal_durable

let expunge_preflight ~before_flags ~before_modseq after =
  let deleted=F.system F.Deleted in
  let expected=flags (deleted :: before_flags) in
  match after with
  | Some (after_flags,Some after_modseq) ->
      same_flags after_flags expected &&
      (if List.exists (F.equal deleted) before_flags then
         after_modseq>=before_modseq
       else after_modseq>before_modseq)
  | _ -> false

let decode_flags raw =
  let rec loop acc = function
    | [] -> Ok (flags acc)
    | wire :: tail ->
        (match F.of_wire wire with
         | Ok flag -> loop (flag :: acc) tail
         | Error text -> Error (Diverged text)) in
  loop [] raw

let current_pair store (pair:J.pair) =
  match J.find_pair store ~id:pair.id with
  | None -> Error Missing_pair
  | Some current when current<>pair -> Error Stale_pair
  | Some current -> Ok current

let identity (pair:J.pair) =
  match pair.remote_uidvalidity,pair.remote_uid,pair.local_id,
        pair.content_sha256,pair.content_length with
  | Some epoch,Some uid,Some local_id,Some digest,Some length ->
      Ok (epoch,uid,local_id,digest,length)
  | _ -> Error Identity_changed

let bound (pair:J.pair) =
  match pair.remote_uidvalidity,pair.remote_uid,pair.local_id with
  | Some epoch,Some uid,Some local_id -> Ok (epoch,uid,local_id)
  | _ -> Error Identity_changed

let published_presence store ~(cursor:Imap.Mirror.cursor)
    (pair:J.pair) epoch uid =
  if cursor.scope<>pair.scope || cursor.phase<>Imap.Mirror.Live ||
     cursor.uidvalidity<>Some epoch || cursor.inventory_ref=None then
    Error Stale_inventory
  else match Imap_store.snapshot_contains_uid store ~scope:pair.scope
      ~cursor ~uid with
    | `Stale_revision -> Error Stale_inventory
    | `Present present -> Ok present

let pending_for_pair store (pair:J.pair) =
  J.active_operation_for_pair store ~pair_id:pair.id

let intent (pair:J.pair) ~id ~kind ~epoch ~uid ~local_id
    ~digest ~length : J.operation = {
  id;pair_id=Some pair.id;local_id=Some local_id;scope=pair.scope;
  kind;state=J.Prepared;source_uidvalidity=Some epoch;source_uid=Some uid;
  destination=None;destination_uidvalidity=None;
  blob_sha256=Some digest;blob_length=Some length;
  desired_flags=Some (flags pair.common_flags);
  receipt=None;receipt_uidvalidity=None;receipt_uid=None;
}

let commit store (pair:J.pair) ~id ~remote_tombstone ~local_tombstone =
  let next={pair with remote_tombstone;local_tombstone} in
  match J.commit_operation_with_pair store ~id
      ~expected_pair_revision:(Some pair.revision) next with
  | `Committed pair -> Ok (Deleted pair)
  | `Stale_revision -> Error Stale_pair

let check_local maildir ~id ~digest ~length ~common_flags =
  match Imap_maildir.find maildir ~id with
  | None -> None
  | Some occurrence ->
      if occurrence.length<>length ||
         not (same_flags occurrence.flags common_flags) ||
         Imap_maildir.sha256 maildir occurrence<>digest then
        None
      else Some occurrence

let check_remote_absence_tombstone (pair:J.pair) =
  match pair.remote_tombstone with
  | Some {reason=J.Inventory_absence;generation=Some _;_} -> true
  | _ -> false

let check_local_absence_tombstone (pair:J.pair) =
  match pair.local_tombstone with
  | Some {reason=J.Local_absence;_} -> true
  | _ -> false

let remote_metadata selected ~epoch ~uid ~modseq =
  let* info=network (Imap_eio.Selected.info selected) in
  if info.uidvalidity<>P.Uidvalidity.to_int64 epoch then
    Error Stale_inventory
  else
    let raw=P.Uid.to_int64 uid in
    let* rows=network (Imap_eio.Selected.fetch_metadata_range selected
      ~first:raw ~last:raw ~modseq) in
    match rows with
    | [row] when row.uid=Some raw ->
        (match row.flags with
         | None -> Error (Diverged "UID FETCH omitted FLAGS")
         | Some raw_flags ->
             let* parsed=decode_flags raw_flags in
             Ok (Some (parsed,row.modseq)))
    | [] -> Ok None
    | _ -> Error (Diverged "UID FETCH returned ambiguous target rows")

let verify_remote_body selected ~uid ~digest ~length ~spool =
  Spool.with_spool spool (fun output ->
      let* ()=network (Imap_eio.Selected.fetch_to selected
          ~max_bytes:length ~uid:(P.Uid.to_int64 uid) output) in
      let found_length,found_digest=Spool.hash_file spool in
      Ok (found_length=length && found_digest=digest))

let with_selected client ~mailbox ~mode f =
  match Imap_eio.Client.with_mailbox client ~mode mailbox
      (fun selected -> Ok (f selected)) with
  | Error e -> Error (Client e)
  | Ok result -> result

let delete_local ~client ~store ~maildir ~mailbox ~cursor
    (pair:J.pair) ~epoch ~uid ~local_id ~digest ~length ~next_id =
  if not (check_remote_absence_tombstone pair) then
    Error Stale_inventory
  else
    let* absent=with_selected client ~mailbox ~mode:`Read_only
      (fun selected ->
        let* found=remote_metadata selected ~epoch ~uid ~modseq:false in
        Ok (found=None)) in
    if not absent then Error Stale_inventory else
    let local=check_local maildir ~id:local_id ~digest ~length
      ~common_flags:pair.common_flags in
    match local with
    | None -> Ok (Held Imap.Sync_policy.Survivor_changed)
    | Some occurrence ->
        let id=next_id () in
        J.prepare_operation store (intent pair ~id ~kind:J.Local_delete
          ~epoch ~uid ~local_id ~digest ~length);
        (match current_pair store pair with
         | Error error ->
             J.reject_prepared_operation store ~id
               ~receipt:"pair changed before local DELETE dispatch";
             Error error
         | Ok _ ->
             J.mark_sent store ~id;
             Imap_maildir.remove maildir occurrence;
             if Imap_maildir.find maildir ~id:local_id<>None then
               Error (Pending_operation id)
             else (
               J.observe_operation store ~id
                 ~receipt:"Maildir ID absent after unlink"
                 ~destination_uidvalidity:None ~destination_uid:None;
               let tombstone={J.reason=J.Explicit_delete;evidence=id;
                 generation=None} in
               commit store pair ~id ~remote_tombstone:pair.remote_tombstone
                 ~local_tombstone:(Some tombstone)))

let has_capability client cap =
  List.mem cap (Imap_eio.Client.capabilities client)

let delete_remote ~client ~store ~maildir ~mailbox ~cursor
    (pair:J.pair) ~epoch ~uid ~local_id ~digest ~length ~next_id
    ~spool_dir =
  if not (has_capability client "UIDPLUS") then
    Error (Unsupported "UIDPLUS required for targeted UID EXPUNGE")
  else if not (has_capability client "CONDSTORE" ||
               has_capability client "QRESYNC") then
    Error (Unsupported "CONDSTORE required for conditional UID STORE")
  else if not (Eio.Path.is_directory spool_dir) then
    Error (Unsupported "spool_dir is not a directory")
  else if Imap_maildir.find maildir ~id:local_id<>None then
    Error Stale_inventory
  else
    let id=next_id () in
    let spool=Eio.Path.(spool_dir / ("imap-delete-" ^ Imap_maildir.reserve_id ())) in
    with_selected client ~mailbox ~mode:`Read_write (fun selected ->
      let* remote=remote_metadata selected ~epoch ~uid ~modseq:true in
      match remote with
      | None -> Error Stale_inventory
      | Some (remote_flags,modseq) ->
          let* modseq=match modseq with
            | Some n when n>0L -> Ok n
            | _ -> Error (Unsupported "target UID has no MODSEQ") in
          let* info=network (Imap_eio.Selected.info selected) in
          let deleted=F.system F.Deleted in
          let writable=match info.permanentflags with
            | None -> false
            | Some wires -> List.exists (fun wire ->
                match F.of_wire wire with
                | Ok flag -> F.equal flag deleted
                | Error _ -> false) wires in
          if not writable then
            Error (Unsupported "\\Deleted is not a PERMANENTFLAG")
          else if not (same_flags remote_flags pair.common_flags) then
            Ok (Held Imap.Sync_policy.Survivor_changed)
          else
            let* body_match=verify_remote_body selected ~uid ~digest
              ~length ~spool in
            if not body_match then Ok (Held Imap.Sync_policy.Survivor_changed)
            else
              let* after_body=remote_metadata selected ~epoch ~uid
                ~modseq:true in
              match after_body with
              | Some (after_flags,Some after_modseq) when
                  after_modseq=modseq &&
                  same_flags after_flags pair.common_flags ->
                  J.prepare_operation store
                    (intent pair ~id ~kind:J.Delete ~epoch ~uid ~local_id
                      ~digest ~length);
                  (match current_pair store pair with
                   | Error error ->
                       J.reject_prepared_operation store ~id
                         ~receipt:"pair changed before DELETE dispatch";
                       Error error
                   | Ok _ ->
                       J.mark_sent store ~id;
                       let set=P.Uid_set.singleton uid in
                       let receipt=network
                         (Imap_eio.Selected.uid_store_flags selected ~set
                           ~operation:`Add ~flags:[deleted]
                           ~unchangedsince:modseq ()) in
                       (match receipt with
                        | Ok receipt when P.Uid_set.mem uid receipt.modified ->
                            J.reject_operation store ~id ~receipt:"MODIFIED";
                            Error (Diverged "conditional UID STORE conflicted")
                        | Ok receipt when
                            P.Uid_set.to_wire receipt.modified<>"" ->
                            J.mark_ambiguous store ~id;
                            Error (Diverged "MODIFIED named another UID")
                        | Error error ->
                            J.mark_ambiguous store ~id;
                            Error error
                        | Ok _ ->
                            let* after_store=remote_metadata selected
                              ~epoch ~uid ~modseq:true in
                            if not (expunge_preflight
                              ~before_flags:remote_flags
                              ~before_modseq:modseq after_store) then
                              Error (Pending_operation id)
                            else (match network
                              (Imap_eio.Selected.uid_expunge selected ~set) with
                             | Error error ->
                                 J.mark_ambiguous store ~id;
                                 Error error
                             | Ok () ->
                                 let* absent=remote_metadata selected ~epoch
                                   ~uid ~modseq:false in
                                 match absent with
                                 | Some _ -> Error (Pending_operation id)
                                 | None ->
                                     J.observe_operation store ~id
                                       ~receipt:"targeted UID EXPUNGE and UID FETCH absent"
                                       ~destination_uidvalidity:None
                                       ~destination_uid:None;
                                     let tombstone={J.reason=J.Expunge_receipt;
                                       evidence=id;generation=None} in
                                     commit store pair ~id
                                       ~remote_tombstone:(Some tombstone)
                                       ~local_tombstone:pair.local_tombstone)))
              | _ -> Ok (Held Imap.Sync_policy.Survivor_changed))

let reconcile_pair ?(min_absence_scans=0) ~client ~store ~maildir ~mailbox ~cursor
    ~local_inventory ~(pair:J.pair) ~policy ~next_id ~spool_dir () =
  if min_absence_scans<0 then
    invalid_arg "Deletion.reconcile_pair: negative absence grace";
  let* pair=current_pair store pair in
  let* epoch,uid,local_id=bound pair in
  match pending_for_pair store pair with
  | Some op -> Error (Pending_operation op.id)
  | None when J.has_open_conflict store ~pair
      ~kind:J.Content_conflict ||
      J.has_open_conflict store ~pair ~kind:J.Identity_conflict ->
      Ok (Held Imap.Sync_policy.Survivor_changed)
  | None ->
      let* remote_present=published_presence store ~cursor pair epoch uid in
      let local_present=Imap_maildir.inventory_find local_inventory
        ~id:local_id<>None in
      let first_generation=if remote_present then
        Option.bind pair.local_tombstone (fun x -> x.generation)
        else Option.bind pair.remote_tombstone (fun x -> x.generation) in
      let last_present_generation=J.last_presence_generation store
        ~pair_id:pair.id ~side:(if remote_present then `Local else `Remote) in
      let absence_mature=Imap.Sync_policy.absence_mature
        ~last_present_generation ~current_generation:cursor.generation
        ~first_generation
        ~min_scans:min_absence_scans in
      let plan=Imap.Sync_policy.plan_disappearance_with_grace ~policy ~paired:true
        ~absence_mature
        ~remote_present ~remote_complete:true ~local_present
        ~local_complete:true
        ~local_retained:(match pair.local_tombstone with
          | Some {reason=J.Retention;_} -> true | _ -> false)
        ~survivor_unchanged:(pair.content_sha256<>None &&
          pair.content_length<>None) in
      match plan with
      | Imap.Sync_policy.No_deletion -> Ok Unchanged
      | Imap.Sync_policy.Hold_deletion why -> Ok (Held why)
      | Imap.Sync_policy.Delete_local ->
          if not (check_remote_absence_tombstone pair) then
            Ok (Held Imap.Sync_policy.Unverified_absence)
          else
            let* _,_,_,digest,length=identity pair in
            delete_local ~client ~store ~maildir ~mailbox ~cursor pair
              ~epoch ~uid ~local_id ~digest ~length ~next_id
      | Imap.Sync_policy.Delete_remote ->
          if not (check_local_absence_tombstone pair) then
            Ok (Held Imap.Sync_policy.Unverified_absence)
          else
            let* _,_,_,digest,length=identity pair in
            delete_remote ~client ~store ~maildir ~mailbox ~cursor
              pair ~epoch ~uid ~local_id ~digest ~length ~next_id ~spool_dir

let recover_operation ~store ~maildir ~cursor ~local_inventory
    ~(operation:J.operation) () =
  let* operation=match J.find_operation store ~id:operation.id with
    | Some current when current.id=operation.id &&
        current.scope=operation.scope && current.kind=operation.kind ->
        Ok current
    | _ -> Error (Diverged "deletion operation changed or disappeared") in
  if operation.kind<>J.Delete && operation.kind<>J.Local_delete then
    Error (Diverged "operation is not a deletion")
  else if operation.state=J.Prepared then (
    J.reject_prepared_operation store ~id:operation.id
      ~receipt:"prepared DELETE was not dispatched";
    Ok Unchanged)
  else
    let* pair=match operation.pair_id with
      | None -> Error Missing_pair
      | Some id ->
          (match J.find_pair store ~id with
           | None -> Error Missing_pair
           | Some pair when pair.scope<>operation.scope -> Error Stale_pair
           | Some pair -> Ok pair) in
    let* epoch,uid,local_id,digest,length=identity pair in
    if operation.source_uidvalidity<>Some epoch ||
       operation.source_uid<>Some uid ||
       operation.local_id<>Some local_id ||
       operation.blob_sha256<>Some digest ||
       operation.blob_length<>Some length ||
       not (Option.fold ~none:false
         ~some:(same_flags pair.common_flags) operation.desired_flags) then
      Error Stale_pair
    else match operation.state with
      | J.Prepared -> assert false
      | J.Sent | J.Ambiguous | J.Observed ->
          let* remote_present=published_presence store ~cursor pair
            epoch uid in
          let local_present=Imap_maildir.inventory_find local_inventory
            ~id:local_id<>None in
          let target_absent=match operation.kind with
            | J.Delete -> not remote_present && not local_present &&
                check_local_absence_tombstone pair
            | J.Local_delete -> not local_present && not remote_present &&
                check_remote_absence_tombstone pair
            | _ -> false in
          if not target_absent ||
             Imap_maildir.find maildir ~id:local_id<>None then
            Error (Pending_operation operation.id)
          else (
            if operation.state<>J.Observed then (
              let receipt=match operation.receipt with
                | Some previous when String.length previous<=4000 ->
                    previous ^ "; complete inventory proves deletion target absent"
                | _ -> "complete inventory proves deletion target absent" in
              J.observe_operation store ~id:operation.id
                ~receipt
                ~destination_uidvalidity:None ~destination_uid:None);
            let remote_tombstone,local_tombstone=match operation.kind with
              | J.Delete ->
                  let tombstone=match cursor.inventory_ref with
                    | Some evidence -> {J.reason=J.Inventory_absence;
                        evidence;generation=Some cursor.generation}
                    | None -> assert false in
                  Some tombstone,pair.local_tombstone
              | J.Local_delete ->
                  pair.remote_tombstone,
                  Some {J.reason=J.Explicit_delete;
                    evidence=operation.id;generation=None}
              | _ -> assert false in
            commit store pair ~id:operation.id
              ~remote_tombstone ~local_tombstone)
      | J.Committed | J.Rejected ->
          Error (Diverged "deletion operation is terminal")

let repair_local_delete ~client ~store ~maildir ~scope ~mailbox ~id
    ~evidence () =
  if String.trim evidence="" || String.length evidence>1024 ||
     not (String.for_all (fun c -> let n=Char.code c in
       n>=32 && n<>127) evidence) then
    Error (Diverged "operator evidence must be 1..1024 printable bytes")
  else Imap_maildir.with_writer_lock maildir (fun () ->
    let* operation=match J.find_operation store ~id with
      | Some op when op.scope=scope && op.kind=J.Local_delete &&
          (op.state=J.Sent || op.state=J.Ambiguous) -> Ok op
      | _ -> Error (Diverged "no pending local deletion in this scope") in
    let* pair=match operation.pair_id with
      | None -> Error Missing_pair
      | Some pair_id ->
          (match J.find_pair store ~id:pair_id with
           | Some pair when pair.scope=scope -> Ok pair
           | _ -> Error Missing_pair) in
    let* epoch,uid,local_id,digest,length=identity pair in
    if J.operation_pair_revision store ~id<>Some pair.revision ||
       operation.source_uidvalidity<>Some epoch ||
       operation.source_uid<>Some uid ||
       operation.local_id<>Some local_id ||
       operation.blob_sha256<>Some digest ||
       operation.blob_length<>Some length ||
       not (Option.fold ~none:false
         ~some:(same_flags pair.common_flags) operation.desired_flags) ||
       pair.local_tombstone<>None then Error Stale_pair
    else
      let* ()=match Engine.guard_bound_mailbox ~client ~store ~scope
          ~mailbox with
        | Ok () -> Ok ()
        | Error (Engine.Client error) -> Error (Client error)
        | Error error -> Error (Diverged
            (Format.asprintf "%a" Engine.pp_error error)) in
      if not (check_remote_absence_tombstone pair) then
        Error Stale_inventory
      else
      let cursor=Imap_store.load_cursor store ~scope in
      let* present=published_presence store ~cursor pair epoch uid in
      if present then Error Stale_inventory
      else
        let* absent=with_selected client ~mailbox ~mode:`Read_only
          (fun selected ->
            let* found=remote_metadata selected ~epoch ~uid
              ~modseq:false in
            Ok (found=None)) in
        if not absent then Error Stale_inventory
        else
          let* occurrence=match check_local maildir ~id:local_id
              ~digest ~length ~common_flags:pair.common_flags with
            | Some occurrence -> Ok occurrence
            | _ -> Error Identity_changed in
          (* The writer lease spans the final identity check, unlink and
             journal transition. A crash after remove is handled by normal
             complete-inventory recovery; the uncertain unlink is never
             automatically replayed. *)
          Imap_maildir.remove maildir occurrence;
          if Imap_maildir.find maildir ~id:local_id<>None then
            Error (Pending_operation id)
          else (
            J.observe_operation store ~id
              ~receipt:("operator repair: " ^ evidence ^
                "; read-only UID absent; exact local occurrence unlinked")
              ~destination_uidvalidity:None ~destination_uid:None;
            let tombstone={J.reason=J.Explicit_delete;evidence=id;
              generation=None} in
            commit store pair ~id ~remote_tombstone:pair.remote_tombstone
              ~local_tombstone:(Some tombstone)))

let reject_unchanged_remote_delete ~client ~store ~maildir ~scope
    ~mailbox ~id ~evidence ~spool_dir () =
  if String.trim evidence="" || String.length evidence>1024 ||
     not (String.for_all (fun c -> let n=Char.code c in
       n>=32 && n<>127) evidence) then
    Error (Diverged "operator evidence must be 1..1024 printable bytes")
  else if not (Eio.Path.is_directory spool_dir) then
    Error (Unsupported "spool_dir is not a directory")
  else Imap_maildir.with_writer_lock maildir (fun () ->
    Imap_maildir.with_inventory_pages maildir (fun inventory ->
      let* operation=match J.find_operation store ~id with
        | Some op when op.scope=scope && op.kind=J.Delete &&
            (op.state=J.Sent || op.state=J.Ambiguous) -> Ok op
        | _ -> Error (Diverged
            "no pending remote deletion in this scope") in
      let* pair=match operation.pair_id with
        | None -> Error Missing_pair
        | Some pair_id ->
            (match J.find_pair store ~id:pair_id with
             | Some pair when pair.scope=scope -> Ok pair
             | _ -> Error Missing_pair) in
      let* epoch,uid,local_id,digest,length=identity pair in
      if J.operation_pair_revision store ~id<>Some pair.revision ||
         operation.source_uidvalidity<>Some epoch ||
         operation.source_uid<>Some uid ||
         operation.local_id<>Some local_id ||
         operation.blob_sha256<>Some digest ||
         operation.blob_length<>Some length ||
         not (Option.fold ~none:false
           ~some:(same_flags pair.common_flags) operation.desired_flags) ||
         pair.remote_tombstone<>None ||
         not (check_local_absence_tombstone pair) then Error Stale_pair
      else if Imap_maildir.inventory_find inventory ~id:local_id<>None ||
              Imap_maildir.find maildir ~id:local_id<>None then
        Error Stale_inventory
      else
        let* ()=match Engine.guard_bound_mailbox ~client ~store ~scope
            ~mailbox with
          | Ok () -> Ok ()
          | Error (Engine.Client error) -> Error (Client error)
          | Error error -> Error (Diverged
              (Format.asprintf "%a" Engine.pp_error error)) in
        let cursor=Imap_store.load_cursor store ~scope in
        let* present=published_presence store ~cursor pair epoch uid in
        if not present then Error Stale_inventory
        else if not (has_capability client "CONDSTORE" ||
                     has_capability client "QRESYNC") then
          Error (Unsupported "CONDSTORE required for stable remote verification")
        else
          let spool=Eio.Path.(spool_dir /
            ("imap-delete-reject-" ^ Imap_maildir.reserve_id ())) in
          let* verified=with_selected client ~mailbox ~mode:`Read_only
            (fun selected ->
              let* before=remote_metadata selected ~epoch ~uid
                ~modseq:true in
              match before with
              | Some (before_flags,Some before_modseq) when
                  before_modseq>0L &&
                  same_flags before_flags pair.common_flags ->
                  let* body_match=verify_remote_body selected ~uid
                    ~digest ~length ~spool in
                  if not body_match then Ok false
                  else
                    let* after=remote_metadata selected ~epoch ~uid
                      ~modseq:true in
                    Ok (match after with
                      | Some (after_flags,Some after_modseq) ->
                          after_modseq=before_modseq &&
                          same_flags after_flags pair.common_flags
                      | _ -> false)
              | _ -> Ok false) in
          if not verified then Error Identity_changed
          else if Imap_maildir.find maildir ~id:local_id<>None ||
                  Imap_store.load_cursor store ~scope<>cursor then
            Error Stale_inventory
          else match J.reject_unchanged_delete_operation store
              ~id pair ~evidence with
            | `Rejected -> Ok ()
            | `Stale_revision -> Error Stale_pair
            | `Invalid_operation -> Error (Diverged
                "remote deletion changed before operator rejection")))

let finish_marked_remote_delete ~client ~store ~maildir ~scope
    ~mailbox ~id ~evidence ~spool_dir () =
  if String.trim evidence="" || String.length evidence>1024 ||
     not (String.for_all (fun c -> let n=Char.code c in
       n>=32 && n<>127) evidence) then
    Error (Diverged "operator evidence must be 1..1024 printable bytes")
  else if not (Eio.Path.is_directory spool_dir) then
    Error (Unsupported "spool_dir is not a directory")
  else if not (has_capability client "UIDPLUS") then
    Error (Unsupported "UIDPLUS required for targeted UID EXPUNGE")
  else if not (has_capability client "CONDSTORE" ||
               has_capability client "QRESYNC") then
    Error (Unsupported "CONDSTORE required for stable remote verification")
  else Imap_maildir.with_writer_lock maildir (fun () ->
    Imap_maildir.with_inventory_pages maildir (fun inventory ->
      let* operation=match J.find_operation store ~id with
        | Some op when op.scope=scope && op.kind=J.Delete &&
            (op.state=J.Sent || op.state=J.Ambiguous) -> Ok op
        | _ -> Error (Diverged
            "no pending remote deletion in this scope") in
      let* pair=match operation.pair_id with
        | None -> Error Missing_pair
        | Some pair_id ->
            (match J.find_pair store ~id:pair_id with
             | Some pair when pair.scope=scope -> Ok pair
             | _ -> Error Missing_pair) in
      let* epoch,uid,local_id,digest,length=identity pair in
      if J.operation_pair_revision store ~id<>Some pair.revision ||
         operation.source_uidvalidity<>Some epoch ||
         operation.source_uid<>Some uid ||
         operation.local_id<>Some local_id ||
         operation.blob_sha256<>Some digest ||
         operation.blob_length<>Some length ||
         not (Option.fold ~none:false
           ~some:(same_flags pair.common_flags) operation.desired_flags) ||
         pair.remote_tombstone<>None ||
         not (check_local_absence_tombstone pair) then Error Stale_pair
      else if Imap_maildir.inventory_find inventory ~id:local_id<>None ||
              Imap_maildir.find maildir ~id:local_id<>None then
        Error Stale_inventory
      else
        let* ()=match Engine.guard_bound_mailbox ~client ~store ~scope
            ~mailbox with
          | Ok () -> Ok ()
          | Error (Engine.Client error) -> Error (Client error)
          | Error error -> Error (Diverged
              (Format.asprintf "%a" Engine.pp_error error)) in
        let cursor=Imap_store.load_cursor store ~scope in
        let* present=published_presence store ~cursor pair epoch uid in
        if not present then Error Stale_inventory else
        let spool=Eio.Path.(spool_dir /
          ("imap-delete-finish-" ^ Imap_maildir.reserve_id ())) in
        with_selected client ~mailbox ~mode:`Read_write (fun selected ->
          let* before=remote_metadata selected ~epoch ~uid ~modseq:true in
          let expected=flags (F.system F.Deleted :: pair.common_flags) in
          match before with
          | Some (before_flags,Some before_modseq) when
              before_modseq>0L && same_flags before_flags expected ->
              let* body_match=verify_remote_body selected ~uid
                ~digest ~length ~spool in
              if not body_match then Error Identity_changed else
              let* after=remote_metadata selected ~epoch ~uid
                ~modseq:true in
              (match after with
               | Some (after_flags,Some after_modseq) when
                   after_modseq=before_modseq &&
                   same_flags after_flags expected ->
                   if Imap_maildir.find maildir ~id:local_id<>None ||
                      Imap_store.load_cursor store ~scope<>cursor then
                     Error Stale_inventory
                   else (match J.attest_targeted_expunge store ~id pair
                       ~evidence with
                     | `Stale_revision -> Error Stale_pair
                     | `Invalid_operation -> Error (Diverged
                         "remote deletion changed before operator EXPUNGE")
                     | `Attested ->
                         let set=P.Uid_set.singleton uid in
                         let* ()=network
                           (Imap_eio.Selected.uid_expunge selected ~set) in
                         let* target=remote_metadata selected ~epoch ~uid
                           ~modseq:false in
                         (match target with
                          | Some _ -> Error (Pending_operation id)
                          | None ->
                              J.observe_operation store ~id
                                ~receipt:("operator targeted UID EXPUNGE: " ^
                                  evidence ^ "; UID FETCH absent")
                                ~destination_uidvalidity:None
                                ~destination_uid:None;
                              let tombstone={J.reason=J.Expunge_receipt;
                                evidence=id;generation=None} in
                              commit store pair ~id
                                ~remote_tombstone:(Some tombstone)
                                ~local_tombstone:pair.local_tombstone))
               | _ -> Error Identity_changed)
          | _ -> Error Identity_changed)))
