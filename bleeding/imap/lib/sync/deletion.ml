module J = Imap_store.Journal
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
  | Maildir of Maildir.error

let pp_error ppf = function
  | Client e -> Imap_eio.Client.pp_error ppf e
  | Missing_pair -> Format.pp_print_string ppf "sync pair is missing"
  | Stale_pair -> Format.pp_print_string ppf "sync pair revision changed"
  | Stale_inventory -> Format.pp_print_string ppf "complete inventory changed"
  | Identity_changed ->
      Format.pp_print_string ppf "paired occurrence identity changed"
  | Unsupported s -> Format.fprintf ppf "deletion unavailable: %s" s
  | Pending_operation id -> Format.fprintf ppf "deletion %s remains pending" id
  | Diverged s -> Format.pp_print_string ppf s
  | Maildir e -> Maildir.pp_error ppf e

type outcome =
  | Unchanged
  | Held of Imap.Sync_policy.deletion_hold
  | Deleted of J.pair

let ( let* ) result f = match result with Ok x -> f x | Error _ as e -> e
let network = function Ok x -> Ok x | Error e -> Error (Client e)

let in_maildir maildir id =
  match Maildir.find maildir ~id with
  | Ok found -> Ok (Option.is_some found)
  | Error e -> Error (Maildir e)

let flags = F.durable
let same_flags = F.equal_durable
let deleted = F.system F.Deleted
let survivor_changed = Held Imap.Sync_policy.Survivor_changed

let expunge_preflight ~before_flags ~before_modseq after =
  let expected=flags (deleted :: before_flags) in
  match after with
  | Some (after_flags,Some after_modseq) ->
      same_flags after_flags expected &&
      (if List.exists (F.equal deleted) before_flags then
         after_modseq>=before_modseq
       else after_modseq>before_modseq)
  | _ -> false

let describe error =
  let text=Format.asprintf "%a" Imap_eio.Client.pp_error error in
  if String.length text<=512 then text else String.sub text 0 512

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

let operation_pair store (operation:J.operation) =
  match operation.pair_id with
  | None -> Error Missing_pair
  | Some id ->
      (match J.find_pair store ~id with
       | None -> Error Missing_pair
       | Some pair when pair.scope<>operation.scope -> Error Stale_pair
       | Some pair -> Ok pair)

let same_identity (operation:J.operation) (pair:J.pair)
    ~epoch ~uid ~local_id ~digest ~length =
  operation.source_uidvalidity=Some epoch &&
  operation.source_uid=Some uid &&
  operation.local_id=Some local_id &&
  operation.blob_sha256=Some digest &&
  operation.blob_length=Some length &&
  Option.fold ~none:false ~some:(same_flags pair.common_flags)
    operation.desired_flags

let published_presence store ~(cursor:Imap.Mirror.cursor)
    (pair:J.pair) epoch uid =
  if cursor.scope<>pair.scope || cursor.phase<>Imap.Mirror.Live ||
     cursor.uidvalidity<>Some epoch || cursor.inventory_ref=None then
    Error Stale_inventory
  else match Imap_store.snapshot_contains_uid store ~scope:pair.scope
      ~cursor ~uid with
    | `Stale_revision -> Error Stale_inventory
    | `Present present -> Ok present

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

let local_unchanged ?inventory maildir (occurrence:Maildir.occurrence)
    ~digest ~length ~common_flags =
  occurrence.length=length &&
  same_flags occurrence.flags common_flags &&
  match Local_inventory.with_unchanged_occurrence ?inventory maildir occurrence
      (fun () -> Local_inventory.sha256 ?inventory maildir occurrence) with
  | Ok found -> found=digest
  | Error `Changed -> false

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
  if info.uidvalidity<>Imap.Uidvalidity.to_int64 epoch then
    Error Stale_inventory
  else
    let* rows=network (Imap_eio.Selected.fetch selected ~uids:[uid]
      ~items:(if modseq then [Imap.Fetch_item.Modseq] else [])) in
    match rows with
    | {flags=Some remote;modseq;_} :: _ ->
        Ok (Some (flags remote,Option.map Imap.Modseq.to_int64 modseq))
    | _ -> Ok None

let with_selected client ~mailbox ~mode f =
  match Imap_eio.Client.with_mailbox client ~mode mailbox
      (fun selected -> Ok (f selected)) with
  | Error e -> Error (Client e)
  | Ok result -> result

let remote_absent client ~mailbox ~epoch ~uid =
  with_selected client ~mailbox ~mode:`Read_only (fun selected ->
    let* found=remote_metadata selected ~epoch ~uid ~modseq:false in
    Ok (found=None))

(* The body is streamed into [spool] under the selection and hashed after
   the selection ends, so spool I/O never runs inside the mailbox lease. *)
let remote_evidence ?(precheck=fun _ -> Ok ()) client ~mailbox ~mode ~spool
    ~epoch ~uid ~digest ~length ~expected =
  Spool.with_spool spool (fun sink ->
    let* seen=with_selected client ~mailbox ~mode (fun selected ->
      let* info=network (Imap_eio.Selected.info selected) in
      let* ()=precheck info in
      let* before=remote_metadata selected ~epoch ~uid ~modseq:true in
      match before with
      | None -> Ok `Absent
      | Some (flags,_) when not (same_flags flags expected) -> Ok `Changed
      | Some (_,modseq) ->
          match Imap_eio.Selected.fetch_to selected ~max_bytes:length
              ~uid sink with
          | Error (Imap_eio.Error.Missing_uid _) -> Ok `Absent
          | Error (Imap_eio.Error.Limit _) -> Ok `Changed
          | Error error -> Error (Client error)
          | Ok () ->
              let* after=remote_metadata selected ~epoch ~uid ~modseq:true in
              match after with
              | None -> Ok `Absent
              | Some (flags,after_modseq) when after_modseq=modseq &&
                  same_flags flags expected -> Ok (`Fetched modseq)
              | Some _ -> Ok `Changed) in
    match seen with
    | `Fetched modseq ->
        let found_length,found_digest=Spool.hash_file spool in
        if found_length=length && found_digest=digest then
          Ok (`Unchanged modseq)
        else Ok `Changed
    | (`Absent | `Changed) as seen -> Ok seen)

let delete_local ~client ~store ~writer ~local_inventory ~mailbox
    (pair:J.pair) ~epoch ~uid ~(local:Maildir.occurrence) ~digest
    ~length ~next_id =
  let maildir=Maildir.of_writer writer in
  let* absent=remote_absent client ~mailbox ~epoch ~uid in
  if not absent then Error Stale_inventory
  else if not (local_unchanged ~inventory:local_inventory maildir local
      ~digest ~length ~common_flags:pair.common_flags) then
    Ok survivor_changed
  else
    let id=next_id () in
    J.prepare_operation store (intent pair ~id ~kind:J.Local_delete
      ~epoch ~uid ~local_id:local.id ~digest ~length);
    match current_pair store pair with
    | Error error ->
        J.reject_prepared_operation store ~id
          ~receipt:"pair changed before local DELETE dispatch";
        Error error
    | Ok _ ->
        J.mark_sent store ~id;
        match Maildir.remove writer local with
        | exception Maildir.Stale_occurrence ->
            J.reject_operation store ~id
              ~receipt:"local occurrence changed before unlink";
            Ok survivor_changed
        | () ->
            let* still=in_maildir maildir local.id in
            if still then (
              J.mark_ambiguous store ~id
                ~reason:"Maildir ID still present after unlink";
              Error (Pending_operation id))
            else (
              J.observe_operation store ~id
                ~receipt:"Maildir ID absent after unlink"
                ~destination_uidvalidity:None ~destination_uid:None;
              let tombstone={J.reason=J.Explicit_delete;evidence=id;
                generation=None} in
              commit store pair ~id ~remote_tombstone:pair.remote_tombstone
                ~local_tombstone:(Some tombstone))

let has_capability = Imap_eio.Client.has

let delete_remote ~client ~store ~maildir ~mailbox (pair:J.pair) ~epoch
    ~uid ~local_id ~digest ~length ~next_id ~spool_dir =
  if not (has_capability client Imap.Capability.Uidplus) then
    Error (Unsupported "UIDPLUS required for targeted UID EXPUNGE")
  else if not (has_capability client Imap.Capability.Condstore ||
               has_capability client Imap.Capability.Qresync) then
    Error (Unsupported "CONDSTORE required for conditional UID STORE")
  else if not (Eio.Path.is_directory spool_dir) then
    Error (Unsupported "spool_dir is not a directory")
  else
    let* local_present=in_maildir maildir local_id in
    if local_present then Error Stale_inventory else
    let spool=Eio.Path.(spool_dir /
      ("imap-delete-" ^ Maildir.reserve_id ())) in
    let precheck (info:Imap.Response.select_metadata) =
      match Flags.validate_permanent_flags
          ~available:info.permanentflags ~defined:info.flags
          ~remote:[] ~merged:[deleted] with
      | Ok () -> Ok ()
      | Error (Flags.Permanent_flag_unavailable _) ->
          Error (Unsupported "\\Deleted is not a PERMANENTFLAG")
      | Error error -> Error (Diverged
          (Format.asprintf "%a" Flags.pp_error error)) in
    let* evidence=remote_evidence ~precheck client ~mailbox ~mode:`Read_write
      ~spool ~epoch ~uid ~digest ~length ~expected:pair.common_flags in
    match evidence with
    | `Absent -> Error Stale_inventory
    | `Changed -> Ok survivor_changed
    | `Unchanged None | `Unchanged (Some 0L) ->
        Error (Unsupported "target UID has no MODSEQ")
    | `Unchanged (Some modseq) ->
        let id=next_id () in
        J.prepare_operation store (intent pair ~id ~kind:J.Delete ~epoch ~uid
          ~local_id ~digest ~length);
        match current_pair store pair with
        | Error error ->
            J.reject_prepared_operation store ~id
              ~receipt:"pair changed before DELETE dispatch";
            Error error
        | Ok _ ->
            J.mark_sent store ~id;
            let set=Imap.Uid_set.singleton uid in
            let dispatched=with_selected client ~mailbox ~mode:`Read_write
              (fun selected ->
                let* before=remote_metadata selected ~epoch ~uid ~modseq:true in
                let* remote_flags=match before with
                  | Some (flags,Some current) when current=modseq -> Ok flags
                  | _ -> Error Identity_changed in
                let* condstore=network
                  (Imap_eio.Selected.Condstore.require selected) in
                let* uidplus=network
                  (Imap_eio.Selected.Uidplus.require selected) in
                match Imap_eio.Selected.Condstore.uid_store_flags condstore
                    ~set ~operation:`Add ~flags:[deleted] ~unchangedsince:modseq
                with
                | Error error -> Ok (`Store_failed error)
                | Ok receipt when Imap.Uid_set.mem uid receipt.modified ->
                    Ok `Modified
                | Ok receipt
                  when not (Imap.Uid_set.is_empty receipt.modified) ->
                    Ok (`Uncertain "MODIFIED named another UID")
                | Ok _ ->
                    match remote_metadata selected ~epoch ~uid ~modseq:true with
                    | Error _ -> Ok (`Uncertain
                        "target unreadable after conditional STORE")
                    | Ok after_store when not (expunge_preflight
                        ~before_flags:remote_flags ~before_modseq:modseq
                        after_store) ->
                        Ok (`Uncertain
                          "target changed after conditional STORE; \
                           EXPUNGE not sent")
                    | Ok _ ->
                        match Imap_eio.Selected.Uidplus.uid_expunge uidplus
                            ~set with
                        | Error error -> Ok (`Expunge_failed error)
                        | Ok () ->
                            match remote_metadata selected ~epoch ~uid
                                ~modseq:false with
                            | Ok None -> Ok `Absent
                            | Ok (Some _) -> Ok (`Uncertain
                                "UID remains after targeted UID EXPUNGE")
                            | Error _ -> Ok (`Uncertain
                                "target unreadable after UID EXPUNGE")) in
            match dispatched with
            | Error error ->
                J.reject_operation store ~id
                  ~receipt:"DELETE not dispatched: conditional STORE not sent";
                (match error with
                 | Identity_changed -> Ok survivor_changed
                 | error -> Error error)
            | Ok `Modified ->
                J.reject_operation store ~id ~receipt:"MODIFIED";
                Ok survivor_changed
            | Ok (`Store_failed (Imap_eio.Error.Rejected _
                                 | Imap_eio.Error.State _
                                 | Imap_eio.Error.Unsupported _
                                 | Imap_eio.Error.Not_enabled _ as error)) ->
                J.reject_operation store ~id
                  ~receipt:("conditional STORE not applied: " ^
                    describe error);
                Error (Client error)
            | Ok (`Store_failed error) ->
                J.mark_ambiguous store ~id
                  ~reason:("conditional STORE outcome unknown: " ^
                    describe error);
                Error (Client error)
            | Ok (`Expunge_failed error) ->
                J.mark_ambiguous store ~id
                  ~reason:("UID EXPUNGE outcome unknown: " ^ describe error);
                Error (Client error)
            | Ok (`Uncertain reason) ->
                J.mark_ambiguous store ~id ~reason;
                Error (Pending_operation id)
            | Ok `Absent ->
                J.observe_operation store ~id
                  ~receipt:"targeted UID EXPUNGE and UID FETCH absent"
                  ~destination_uidvalidity:None ~destination_uid:None;
                let tombstone={J.reason=J.Expunge_receipt;
                  evidence=id;generation=None} in
                commit store pair ~id ~remote_tombstone:(Some tombstone)
                  ~local_tombstone:pair.local_tombstone

let reconcile_pair ?(min_absence_scans=0) ~client ~store ~writer ~mailbox
    ~cursor ~local_inventory ~(pair:J.pair) ~policy ~next_id ~spool_dir () =
  let maildir=Maildir.of_writer writer in
  if min_absence_scans<0 then
    invalid_arg "Deletion.reconcile_pair: negative absence grace";
  let* pair=current_pair store pair in
  let* epoch,uid,local_id=bound pair in
  match J.active_operation_for_pair store ~pair_id:pair.id with
  | Some op -> Error (Pending_operation op.id)
  | None when J.has_open_conflict store ~pair
      ~kind:J.Content_conflict ||
      J.has_open_conflict store ~pair ~kind:J.Identity_conflict ->
      Ok survivor_changed
  | None ->
      let* remote_present=published_presence store ~cursor pair epoch uid in
      let local=Local_inventory.find local_inventory ~id:local_id in
      let first_generation=if remote_present then
        Option.bind pair.local_tombstone (fun x -> x.generation)
        else Option.bind pair.remote_tombstone (fun x -> x.generation) in
      let last_present_generation=J.last_presence_generation store
        ~pair_id:pair.id ~side:(if remote_present then `Local else `Remote) in
      let absence_mature=Imap.Sync_policy.absence_mature
        ~last_present_generation ~current_generation:cursor.generation
        ~first_generation
        ~min_scans:min_absence_scans in
      let plan=Imap.Sync_policy.plan_disappearance_with_grace ~policy
        ~paired:true ~absence_mature
        ~remote_present ~remote_complete:true ~local_present:(local<>None)
        ~local_complete:true
        ~local_retained:(match pair.local_tombstone with
          | Some {reason=J.Retention;_} -> true | _ -> false)
        ~survivor_unchanged:true in
      let evidence=match pair.content_sha256,pair.content_length with
        | Some digest,Some length -> Some (digest,length)
        | _ -> None in
      match plan,evidence,local with
      | Imap.Sync_policy.No_deletion,_,_ -> Ok Unchanged
      | Imap.Sync_policy.Hold_deletion why,_,_ -> Ok (Held why)
      | (Imap.Sync_policy.Delete_local | Imap.Sync_policy.Delete_remote),
        None,_ ->
          Ok (Held Imap.Sync_policy.Missing_content_evidence)
      | Imap.Sync_policy.Delete_local,_,_
        when not (check_remote_absence_tombstone pair) ->
          Ok (Held Imap.Sync_policy.Unverified_absence)
      | Imap.Sync_policy.Delete_local,Some (digest,length),Some local ->
          delete_local ~client ~store ~writer ~local_inventory ~mailbox pair
            ~epoch ~uid ~local ~digest ~length ~next_id
      | Imap.Sync_policy.Delete_local,Some _,None -> Ok Unchanged
      | Imap.Sync_policy.Delete_remote,_,_
        when not (check_local_absence_tombstone pair) ->
          Ok (Held Imap.Sync_policy.Unverified_absence)
      | Imap.Sync_policy.Delete_remote,Some (digest,length),_ ->
          delete_remote ~client ~store ~maildir ~mailbox pair ~epoch ~uid
            ~local_id ~digest ~length ~next_id ~spool_dir

let recover_operation ~store ~writer ~cursor ~local_inventory
    ~(operation:J.operation) () =
  let maildir=Maildir.of_writer writer in
  let* operation=match J.find_operation store ~id:operation.id with
    | Some current when current.scope=operation.scope &&
        current.kind=operation.kind -> Ok current
    | _ -> Error (Diverged "deletion operation changed or disappeared") in
  match operation.kind,operation.state with
  | (J.Append | J.Local_append | J.Copy | J.Move | J.Flags),_ ->
      Error (Diverged "operation is not a deletion")
  | _,J.Prepared ->
      J.reject_prepared_operation store ~id:operation.id
        ~receipt:"prepared DELETE was not dispatched";
      Ok Unchanged
  | _,(J.Committed | J.Rejected) ->
      Error (Diverged "deletion operation is terminal")
  | (J.Delete | J.Local_delete as kind),(J.Sent | J.Ambiguous | J.Observed) ->
      let* pair=operation_pair store operation in
      let* epoch,uid,local_id,digest,length=identity pair in
      if not (same_identity operation pair ~epoch ~uid ~local_id ~digest
          ~length) then
        Error Stale_pair
      else
        let* remote_present=published_presence store ~cursor pair
          epoch uid in
        let local_present=Local_inventory.find local_inventory
          ~id:local_id<>None in
        let tombstoned=if kind=J.Delete then check_local_absence_tombstone pair
          else check_remote_absence_tombstone pair in
        let* still_present=if remote_present || local_present ||
            not tombstoned then Ok true
          else in_maildir maildir local_id in
        match cursor.inventory_ref with
        | _ when still_present -> Error (Pending_operation operation.id)
        | None -> Error Stale_inventory
        | Some inventory_ref ->
            if operation.state<>J.Observed then (
              let receipt=match operation.receipt with
                | Some previous when String.length previous<=4000 ->
                    previous ^
                      "; complete inventory proves deletion target absent"
                | _ -> "complete inventory proves deletion target absent" in
              J.observe_operation store ~id:operation.id
                ~receipt
                ~destination_uidvalidity:None ~destination_uid:None);
            let remote_tombstone,local_tombstone=if kind=J.Delete then
                Some {J.reason=J.Inventory_absence;evidence=inventory_ref;
                  generation=Some cursor.generation},pair.local_tombstone
              else
                pair.remote_tombstone,
                Some {J.reason=J.Explicit_delete;evidence=operation.id;
                  generation=None} in
            commit store pair ~id:operation.id
              ~remote_tombstone ~local_tombstone

let printable evidence =
  String.trim evidence<>"" && String.length evidence<=1024 &&
  String.for_all (fun c -> let n=Char.code c in n>=32 && n<>127) evidence

let guard ~client ~store ~scope ~mailbox =
  match Engine.guard_bound_mailbox ~client ~store ~scope ~mailbox with
  | Ok () -> Ok ()
  | Error (Engine.Client error) -> Error (Client error)
  | Error Engine.Stale_revision -> Error Stale_pair
  | Error error -> Error (Diverged
      (Format.asprintf "%a" Engine.pp_error error))

let pending_delete store ~scope ~id ~kind =
  match J.find_operation store ~id with
  | Some op when op.scope=scope && op.kind=kind &&
      (op.state=J.Sent || op.state=J.Ambiguous) -> Ok op
  | _ -> Error (Diverged (if kind=J.Delete then
      "no pending remote deletion in this scope"
      else "no pending local deletion in this scope"))

let repair_local_delete ~client ~store ~maildir ~scope ~mailbox ~id
    ~evidence () =
  if not (printable evidence) then
    Error (Diverged "operator evidence must be 1..1024 printable bytes")
  else Maildir.with_writer maildir (fun writer ->
    let* operation=pending_delete store ~scope ~id ~kind:J.Local_delete in
    let* pair=operation_pair store operation in
    let* epoch,uid,local_id,digest,length=identity pair in
    if J.operation_pair_revision store ~id<>Some pair.revision ||
       not (same_identity operation pair ~epoch ~uid ~local_id ~digest
         ~length) ||
       pair.local_tombstone<>None then Error Stale_pair
    else
      let* ()=guard ~client ~store ~scope ~mailbox in
      if not (check_remote_absence_tombstone pair) then
        Error Stale_inventory
      else
      let cursor=Imap_store.load_cursor store ~scope in
      let* present=published_presence store ~cursor pair epoch uid in
      if present then Error Stale_inventory
      else
        let* absent=remote_absent client ~mailbox ~epoch ~uid in
        if not absent then Error Stale_inventory
        else
          let* occurrence=match Maildir.find maildir ~id:local_id with
            | Error e -> Error (Maildir e)
            | Ok (Some occurrence) when local_unchanged maildir occurrence
                ~digest ~length ~common_flags:pair.common_flags ->
                Ok occurrence
            | Ok _ -> Error Identity_changed in
          (* A crash after the unlink is recovered from the next complete
             inventory, and the uncertain unlink is never replayed. *)
          match Maildir.remove writer occurrence with
          | exception Maildir.Stale_occurrence -> Error Identity_changed
          | () ->
              let* still=in_maildir maildir local_id in
              if still then Error (Pending_operation id)
              else (
                J.observe_operation store ~id
                  ~receipt:("operator repair: " ^ evidence ^
                    "; read-only UID absent; exact local occurrence \
                     unlinked")
                  ~destination_uidvalidity:None ~destination_uid:None;
                let tombstone={J.reason=J.Explicit_delete;evidence=id;
                  generation=None} in
                commit store pair ~id
                  ~remote_tombstone:pair.remote_tombstone
                  ~local_tombstone:(Some tombstone)))

let pending_remote_delete store ~scope ~maildir ~id =
  let* operation=pending_delete store ~scope ~id ~kind:J.Delete in
  let* pair=operation_pair store operation in
  let* epoch,uid,local_id,digest,length=identity pair in
  if J.operation_pair_revision store ~id<>Some pair.revision ||
     not (same_identity operation pair ~epoch ~uid ~local_id ~digest
       ~length) ||
     pair.remote_tombstone<>None ||
     not (check_local_absence_tombstone pair) then Error Stale_pair
  else
    let* local_present=in_maildir maildir local_id in
    if local_present then Error Stale_inventory
    else Ok (pair,epoch,uid,local_id,digest,length)

let reject_unchanged_remote_delete ~client ~store ~maildir ~scope
    ~mailbox ~id ~evidence ~spool_dir () =
  if not (printable evidence) then
    Error (Diverged "operator evidence must be 1..1024 printable bytes")
  else if not (Eio.Path.is_directory spool_dir) then
    Error (Unsupported "spool_dir is not a directory")
  else Maildir.with_writer maildir (fun _ ->
    let* pair,epoch,uid,local_id,digest,length=
      pending_remote_delete store ~scope ~maildir ~id in
    let* ()=guard ~client ~store ~scope ~mailbox in
    let cursor=Imap_store.load_cursor store ~scope in
    let* present=published_presence store ~cursor pair epoch uid in
    if not present then Error Stale_inventory
    else if not (has_capability client Imap.Capability.Condstore ||
                 has_capability client Imap.Capability.Qresync) then
      Error (Unsupported "CONDSTORE required for stable remote verification")
    else
      let spool=Eio.Path.(spool_dir /
        ("imap-delete-reject-" ^ Maildir.reserve_id ())) in
      let* seen=remote_evidence client ~mailbox ~mode:`Read_only ~spool
        ~epoch ~uid ~digest ~length ~expected:pair.common_flags in
      match seen with
      | `Unchanged (Some modseq) when modseq>0L ->
          let* local_present=in_maildir maildir local_id in
          if local_present ||
             Imap_store.load_cursor store ~scope<>cursor then
            Error Stale_inventory
          else (match J.reject_unchanged_delete_operation store
              ~id pair ~evidence with
            | `Rejected -> Ok ()
            | `Stale_revision -> Error Stale_pair
            | `Invalid_operation -> Error (Diverged
                "remote deletion changed before operator rejection"))
      | `Absent | `Changed | `Unchanged _ -> Error Identity_changed)

let finish_marked_remote_delete ~client ~store ~maildir ~scope
    ~mailbox ~id ~evidence ~spool_dir () =
  if not (printable evidence) then
    Error (Diverged "operator evidence must be 1..1024 printable bytes")
  else if not (Eio.Path.is_directory spool_dir) then
    Error (Unsupported "spool_dir is not a directory")
  else if not (has_capability client Imap.Capability.Uidplus) then
    Error (Unsupported "UIDPLUS required for targeted UID EXPUNGE")
  else if not (has_capability client Imap.Capability.Condstore ||
               has_capability client Imap.Capability.Qresync) then
    Error (Unsupported "CONDSTORE required for stable remote verification")
  else Maildir.with_writer maildir (fun _ ->
    let* pair,epoch,uid,local_id,digest,length=
      pending_remote_delete store ~scope ~maildir ~id in
    let* ()=guard ~client ~store ~scope ~mailbox in
    let cursor=Imap_store.load_cursor store ~scope in
    let* present=published_presence store ~cursor pair epoch uid in
    if not present then Error Stale_inventory else
    let spool=Eio.Path.(spool_dir /
      ("imap-delete-finish-" ^ Maildir.reserve_id ())) in
    let expected=flags (deleted :: pair.common_flags) in
    let* seen=remote_evidence client ~mailbox ~mode:`Read_only ~spool
      ~epoch ~uid ~digest ~length ~expected in
    match seen with
    | `Unchanged (Some modseq) when modseq>0L ->
        let* local_present=in_maildir maildir local_id in
        if local_present ||
           Imap_store.load_cursor store ~scope<>cursor then
          Error Stale_inventory
        else (match J.attest_targeted_expunge store ~id pair ~evidence with
          | `Stale_revision -> Error Stale_pair
          | `Invalid_operation -> Error (Diverged
              "remote deletion changed before operator EXPUNGE")
          | `Attested ->
              let set=Imap.Uid_set.singleton uid in
              let* expunged=with_selected client ~mailbox ~mode:`Read_write
                (fun selected ->
                  let* before=remote_metadata selected ~epoch ~uid
                    ~modseq:true in
                  match before with
                  | Some (flags,Some current) when current=modseq &&
                      same_flags flags expected ->
                      let* uidplus=network
                        (Imap_eio.Selected.Uidplus.require selected) in
                      let* ()=network
                        (Imap_eio.Selected.Uidplus.uid_expunge uidplus ~set) in
                      let* target=remote_metadata selected ~epoch ~uid
                        ~modseq:false in
                      Ok (target=None)
                  | _ -> Error Identity_changed) in
              if not expunged then Error (Pending_operation id)
              else (
                J.observe_operation store ~id
                  ~receipt:("operator targeted UID EXPUNGE: " ^
                    evidence ^ "; UID FETCH absent")
                  ~destination_uidvalidity:None ~destination_uid:None;
                let tombstone={J.reason=J.Expunge_receipt;
                  evidence=id;generation=None} in
                commit store pair ~id ~remote_tombstone:(Some tombstone)
                  ~local_tombstone:pair.local_tombstone))
    | `Absent | `Changed | `Unchanged _ -> Error Identity_changed)
