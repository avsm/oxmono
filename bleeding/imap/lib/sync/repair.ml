module J = Imap_store.Journal
module F = Mail_flag.Imap_flag
module E = Pair_evidence

open Error

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as e -> e
let network = function Ok value -> Ok value | Error e -> Error (Client e)
let same_flags = F.equal_durable
let durable_flags = F.durable
let deleted = F.system F.Deleted

let settle_flags ~(ctx:Ctx.t) ~maildir ~id ~evidence () =
  let {Ctx.store;scope;_}=ctx in
  let* ()=E.check_evidence evidence in
  E.with_lease maildir (fun _ ->
    let* operation=match J.find_operation store ~id with
      | Some op when op.scope=scope && op.kind=J.Flags &&
          List.mem op.state [J.Sent;J.Ambiguous;J.Observed] -> Ok op
      | _ -> Error No_pending_operation in
    let* pair=E.operation_pair store operation in
    let* epoch,uid,local_id=E.live_target pair in
    if not (E.same_target operation ~epoch ~uid ~local_id &&
            E.journaled_at store operation pair) then
      Error Stale_pair
    else
      let* ()=E.guard_bound_mailbox ~ctx in
      let local_read () =
        let* occurrence=E.occurrence maildir local_id in
        if E.local_content maildir pair occurrence<>`Matches then
          Error (Content_mismatch pair.id)
        else if not (E.local_date_matches pair occurrence) then
          Error (Diverged "local message date differs from paired date")
        else Ok occurrence in
      let remote_read () =
        E.remote_flags_now ctx ~epoch ~uid ~modseq:true in
      let* local_before=local_read () in
      let* remote_flags,first_modseq=remote_read () in
      let* modseq=match first_modseq with
        | Some value when value>0L -> Ok value
        | _ -> Error Conditional_store_unavailable in
      if not (same_flags local_before.flags remote_flags) then
        Error (Diverged "remote and local flags still differ")
      else
        let* local_after=local_read () in
        if not (same_flags local_after.flags local_before.flags) then
          Error (Diverged "local flags changed during repair")
        else
          let* remote_after,second_modseq=remote_read () in
          if not (same_flags remote_after remote_flags) ||
             second_modseq<>Some modseq then
            Error (Diverged "remote flags changed during repair")
          else
            match J.settle_flag_operation store ~id pair
                ~flags:remote_flags ~evidence with
            | `Settled pair -> Ok (Flags.Updated pair)
            | `Stale_revision -> Error Stale_pair
            | `Invalid_operation -> Error (Diverged
                "FLAGS operation changed during repair"))

let pending_delete store ~scope ~id ~kind =
  let* operation=match J.find_operation store ~id with
    | Some op when op.scope=scope && op.kind=kind &&
        (op.state=J.Sent || op.state=J.Ambiguous) -> Ok op
    | _ -> Error (Diverged (if kind=J.Delete then
        "no pending remote deletion in this scope"
        else "no pending local deletion in this scope")) in
  let* pair=E.operation_pair store operation in
  let* epoch,uid,local_id,digest,length=E.content_identity pair in
  if not (E.journaled_at store operation pair &&
          E.same_identity operation pair ~epoch ~uid ~local_id ~digest
            ~length) then Error Stale_pair
  else Ok (pair,epoch,uid,local_id,digest,length)

let unchanged_local maildir (pair:J.pair) (occurrence:Maildir.occurrence) =
  same_flags occurrence.flags pair.common_flags &&
  E.local_content maildir pair occurrence=`Matches

let deleted_pair = function
  | Ok pair -> Ok (Deletion.Deleted pair)
  | Error _ as e -> e

let local_delete ~(ctx:Ctx.t) ~maildir ~id ~evidence () =
  let {Ctx.store;scope;_}=ctx in
  let* ()=E.check_evidence evidence in
  E.with_lease maildir (fun writer ->
    let* pair,epoch,uid,local_id,_,_=pending_delete store ~scope ~id
      ~kind:J.Local_delete in
    if pair.local_tombstone<>None then Error Stale_pair
    else
      let* ()=E.guard_bound_mailbox ~ctx in
      if not (E.remote_absence_proven pair) then Error Stale_inventory
      else
      let cursor=Imap_store.load_cursor store ~scope in
      let* present=E.published_presence store ~cursor pair epoch uid in
      if present then Error Stale_inventory
      else
        let* absent=E.remote_absent ctx ~epoch ~uid in
        if not absent then Error Stale_inventory
        else
          let* occurrence=match Maildir.find maildir ~id:local_id with
            | Error e -> Error (Maildir e)
            | Ok (Some occurrence) when unchanged_local maildir pair
                occurrence -> Ok occurrence
            | Ok _ -> Error Identity_changed in
          (* A crash after the unlink is recovered from the next complete
             inventory, and the uncertain unlink is never replayed. *)
          match Maildir.remove writer occurrence with
          | exception Maildir.Stale_occurrence -> Error Identity_changed
          | () ->
              let* still=E.present maildir ~id:local_id in
              if still then Error (Pending_operations [id])
              else deleted_pair (E.finish_unlink store pair ~id
                ~receipt:("operator repair: " ^ evidence ^
                  "; read-only UID absent; exact local occurrence \
                   unlinked")))

let pending_remote_delete store ~scope ~maildir ~id =
  let* (pair,_,_,local_id,_,_ as pending)=pending_delete store ~scope ~id
    ~kind:J.Delete in
  if pair.remote_tombstone<>None || not (E.local_absence_recorded pair) then
    Error Stale_pair
  else
    let* local_present=E.present maildir ~id:local_id in
    if local_present then Error Stale_inventory
    else Ok pending

let condstore client =
  let has=Imap_eio.Client.has client in
  has Imap.Capability.Condstore || has Imap.Capability.Qresync

(* [verified_remote] is the stable two-read remote check shared by the
   remote repairs, followed by the final local and inventory re-check. *)
let verified_remote (ctx:Ctx.t) ~maildir ~epoch ~uid ~local_id ~digest
    ~length ~cursor ~spool_name ~expected =
  let spool=Eio.Path.(ctx.spool_dir /
    (spool_name ^ Maildir.reserve_id ())) in
  let* seen=E.stable_remote_body ctx ~mode:`Read_only ~spool ~epoch ~uid
    ~digest ~length ~expected in
  match seen with
  | `Unchanged (Some modseq) when modseq>0L ->
      let* local_present=E.present maildir ~id:local_id in
      if local_present ||
         Imap_store.load_cursor ctx.store ~scope:ctx.scope<>cursor then
        Error Stale_inventory
      else Ok modseq
  | `Absent | `Changed | `Unchanged _ -> Error Identity_changed

let reject_remote_delete ~(ctx:Ctx.t) ~maildir ~id ~evidence () =
  let {Ctx.client;store;scope;spool_dir;_}=ctx in
  let* ()=E.check_evidence evidence in
  if not (Eio.Path.is_directory spool_dir) then
    Error (Unsupported "spool_dir is not a directory")
  else E.with_lease maildir (fun _ ->
    let* pair,epoch,uid,local_id,digest,length=
      pending_remote_delete store ~scope ~maildir ~id in
    let* ()=E.guard_bound_mailbox ~ctx in
    let cursor=Imap_store.load_cursor store ~scope in
    let* present=E.published_presence store ~cursor pair epoch uid in
    if not present then Error Stale_inventory
    else if not (condstore client) then
      Error (Unsupported "CONDSTORE required for stable remote verification")
    else
      let* _=verified_remote ctx ~maildir ~epoch ~uid ~local_id ~digest
        ~length ~cursor ~spool_name:"imap-delete-reject-"
        ~expected:pair.common_flags in
      match J.reject_unchanged_delete_operation store ~id pair ~evidence with
      | `Rejected -> Ok ()
      | `Stale_revision -> Error Stale_pair
      | `Invalid_operation -> Error (Diverged
          "remote deletion changed before operator rejection"))

let finish_remote_delete ~(ctx:Ctx.t) ~maildir ~id ~evidence () =
  let {Ctx.client;store;scope;spool_dir;_}=ctx in
  let* ()=E.check_evidence evidence in
  if not (Eio.Path.is_directory spool_dir) then
    Error (Unsupported "spool_dir is not a directory")
  else if not (Imap_eio.Client.has client Imap.Capability.Uidplus) then
    Error (Unsupported "UIDPLUS required for targeted UID EXPUNGE")
  else if not (condstore client) then
    Error (Unsupported "CONDSTORE required for stable remote verification")
  else E.with_lease maildir (fun _ ->
    let* pair,epoch,uid,local_id,digest,length=
      pending_remote_delete store ~scope ~maildir ~id in
    let* ()=E.guard_bound_mailbox ~ctx in
    let cursor=Imap_store.load_cursor store ~scope in
    let* present=E.published_presence store ~cursor pair epoch uid in
    if not present then Error Stale_inventory else
    let expected=durable_flags (deleted :: pair.common_flags) in
    let* modseq=verified_remote ctx ~maildir ~epoch ~uid ~local_id ~digest
      ~length ~cursor ~spool_name:"imap-delete-finish-" ~expected in
    match J.attest_targeted_expunge store ~id pair ~evidence with
    | `Stale_revision -> Error Stale_pair
    | `Invalid_operation -> Error (Diverged
        "remote deletion changed before operator EXPUNGE")
    | `Attested ->
        let set=Imap.Uid_set.singleton uid in
        let* expunged=E.with_selected ctx ~mode:`Read_write (fun selected ->
          let* before=E.epoch_flags selected ~epoch ~uid ~modseq:true in
          match before with
          | Some (flags,Some current) when current=modseq &&
              same_flags flags expected ->
              let* uidplus=network
                (Imap_eio.Selected.Uidplus.require selected) in
              let* ()=network
                (Imap_eio.Selected.Uidplus.uid_expunge uidplus ~set) in
              let* target=E.epoch_flags selected ~epoch ~uid ~modseq:false in
              Ok (target=None)
          | _ -> Error Identity_changed) in
        if not expunged then Error (Pending_operations [id])
        else deleted_pair (E.finish_expunge store pair ~id
          ~receipt:("operator targeted UID EXPUNGE: " ^ evidence ^
            "; UID FETCH absent")))

let local_append ~(ctx:Ctx.t) ~maildir ~id ~evidence () =
  let {Ctx.store;scope;spool_dir;_}=ctx in
  let* ()=E.check_evidence evidence in
  E.with_lease maildir (fun writer ->
    let* op=match J.find_operation store ~id with
      | Some op -> Ok op
      | None -> Error (Invalid_operation "unknown sync operation ID") in
    let* local_id,uidvalidity,uid,sha256,length,flags=
      match op.kind,op.state,op.local_id,op.source_uidvalidity,
            op.source_uid,op.blob_sha256,op.blob_length,op.desired_flags with
      | J.Local_append,(J.Sent|J.Ambiguous),Some local_id,
        Some uidvalidity,Some uid,Some sha256,Some length,Some flags
        when op.scope=scope && op.pair_id=None && op.destination=None ->
          Ok (local_id,uidvalidity,uid,sha256,length,flags)
      | _ -> Error (Invalid_operation
          "operation is not a pending local append in this scope") in
    let* found=E.find maildir ~id:local_id in
    let* ()=if Option.is_none found then Ok ()
      else Error (Invalid_operation
        "reserved Maildir occurrence already exists; run sync to \
         reconcile it") in
    let* ()=if J.find_remote store ~scope ~uidvalidity ~uid=None &&
        J.find_local store ~scope ~local_id=None then Ok ()
      else Error (Invalid_operation
        "source UID or reserved Maildir ID is already paired") in
    let* ()=E.guard_bound_mailbox ~ctx in
    let cursor=Imap_store.load_cursor store ~scope in
    let* ()=if cursor.uidvalidity=Some uidvalidity then Ok ()
      else Error Uidvalidity_changed in
    let* present=E.snapshot_has_uid store ~scope ~cursor uid in
    let* ()=if present then Ok () else Error (Source_vanished uid) in
    let* actual_flags,internal_date=E.remote_flags_and_date ctx ~uid
      ~uidvalidity in
    let* ()=if same_flags flags actual_flags then Ok ()
      else Error (Flags_diverged id) in
    let* internal_date=match J.operation_source_date store ~id with
      | None -> Ok internal_date
      | Some saved when Imap.Internal_date.equal_instant
          saved internal_date -> Ok saved
      | Some _ -> Error (Date_diverged id) in
    let spool=Eio.Path.(spool_dir /
      ("imap-repair-" ^ Maildir.reserve_id ())) in
    let* archived=match Engine.archive_uid ~ctx ~uid ~spool () with
      | Error (Client (Imap_eio.Error.Missing_uid missing))
        when Imap.Uid.equal missing uid -> Error (Source_vanished uid)
      | result -> result in
    let blob=archived.blob in
    let* ()=if blob.length=length && blob.sha256=sha256 then Ok ()
      else Error (Content_diverged id) in
    let* ()=if same_flags flags archived.flags then Ok ()
      else Error (Flags_diverged id) in
    let* ()=if Imap.Internal_date.equal_instant internal_date
        archived.internal_date then Ok ()
      else Error (Date_diverged id) in
    let* ()=if Imap_store.load_cursor store ~scope=cursor then Ok ()
      else Error Store_stale_revision in
    let* mtime=match E.storable writer ~flags internal_date with
      | Ok mtime -> Ok mtime
      | Error reason -> Error (Invalid_operation
          ("Maildir cannot store the message: " ^ reason)) in
    let* local=E.maildir_result @@ Eio.Switch.run @@ fun sw ->
      let source=Imap_store.Blob.open_in store ~sw blob in
      Maildir.append writer ~id:local_id ~source ~length
        ~flags ~mtime () in
    if local.length<>length || Maildir.sha256 maildir local<>sha256
       then Error (Content_diverged id)
    else if not (same_flags flags local.flags) then Error (Flags_diverged id)
    else (
      J.observe_operation store ~id
        ~receipt:("operator local append repair: " ^ evidence)
        ~destination_uidvalidity:None ~destination_uid:None;
      E.commit_new_pair store ~id
        (E.new_pair ~id ~scope ~uidvalidity ~uid ~local_id ~sha256 ~length
          ~internal_date ~flags ())))

let record_appenduid ~store ~scope ~maildir ~id ~uidvalidity ~uid ~evidence
    () =
  let* ()=E.check_evidence evidence in
  E.with_lease maildir (fun _ ->
    let* operation=match J.find_operation store ~id with
      | Some operation -> Ok operation
      | None -> Error (Invalid_operation "unknown sync operation ID") in
    let* ()=if operation.kind=J.Append && operation.scope=scope &&
        operation.pair_id=None && operation.destination=Some scope &&
        operation.destination_uidvalidity=Some uidvalidity &&
        operation.blob_sha256<>None && operation.blob_length<>None &&
        operation.desired_flags<>None then Ok ()
      else Error (Invalid_operation
        "operation is not a matching journaled APPEND") in
    let* observed=match operation.state with
      | J.Sent | J.Ambiguous -> Ok false
      | J.Observed when operation.receipt_uidvalidity=Some uidvalidity &&
          operation.receipt_uid=Some uid -> Ok true
      | _ -> Error (Invalid_operation
          "APPEND is not pending or already records another UID") in
    let* ()=match Imap_store.find_intent store ~id with
      | None -> Ok ()
      | Some intent ->
          let matches=intent.uidvalidity=Some uidvalidity &&
            E.append_intent_matches ~scope operation intent in
          if not matches then Error (Invalid_operation
            "legacy APPEND intent disagrees with sync operation")
          else match intent.state with
            | Imap_store.Sent | Imap_store.Ambiguous ->
                Imap_store.confirm_intent store ~id
                  ~uidvalidity:(Some uidvalidity) ~uid:(Some uid);
                Ok ()
            | Imap_store.Confirmed when
                intent.uidvalidity=Some uidvalidity &&
                intent.uid=Some uid -> Ok ()
            | _ -> Error (Invalid_operation
                "legacy APPEND intent is not pending or has another UID") in
    if not observed then
      J.observe_operation store ~id
        ~receipt:("operator APPENDUID: " ^ evidence)
        ~destination_uidvalidity:(Some uidvalidity)
        ~destination_uid:(Some uid);
    Ok ())

let mark_local_retention ~store ~scope ~spool_dir ~maildir ~pair_id
    ~evidence () =
  let* ()=E.check_evidence evidence in
  let* ()=E.require_spool_dir spool_dir in
  E.with_lease maildir (fun _ ->
    E.with_inventory ~spool_dir maildir (fun inventory ->
      match J.find_pair store ~id:pair_id with
      | None -> Error (Invalid_operation "retention pair does not exist")
      | Some pair when pair.scope<>scope ->
          Error (Invalid_operation "retention pair belongs to another mailbox")
      | Some pair ->
          match pair.remote_uidvalidity,pair.remote_uid,pair.local_id with
          | Some _,Some _,Some local_id when
              pair.remote_tombstone=None ->
              if J.active_operation_for_pair store ~pair_id<>None then
                Error (Invalid_operation
                  "retention pair has a pending operation")
              else if Local_inventory.find inventory ~id:local_id<>None then
                Error (Invalid_operation
                  "retention local occurrence is present")
              else (match pair.local_tombstone with
                | Some {reason=J.Explicit_delete;_} ->
                    Error (Invalid_operation
                      "local occurrence was explicitly deleted")
                | None | Some {reason=J.Local_absence;_} |
                  Some {reason=J.Retention;_} ->
                    let local_tombstone=Some {J.reason=J.Retention;
                      evidence;generation=None} in
                    (match J.put_pair store
                      ~expected_revision:(Some pair.revision)
                      {pair with local_tombstone} with
                     | `Committed _ -> Ok ()
                     | `Stale_revision -> Error Store_stale_revision)
                | Some _ ->
                    Error (Invalid_operation "invalid local tombstone"))
          | _ -> Error (Invalid_operation
              "retention requires an active paired remote binding")))

type append_candidates = {
  uidvalidity : Imap.Uidvalidity.t;
  inspected_uids : int;
  matching_uids : Imap.Uid.t list;
}

let inspect_append_candidates ?(max_uids=1000)
    ?(max_body_bytes=1_073_741_824L) ~(ctx:Ctx.t) ~id () =
  let {Ctx.store;scope;spool_dir;_}=ctx in
  let checked_uid raw=match Imap.Uid.of_int64 raw with
    | Ok uid -> Ok uid
    | Error message -> Error (Invalid_operation message) in
  if max_uids<1 || max_uids>10_000 || max_body_bytes<1L ||
     not (Eio.Path.is_directory spool_dir) then
    Error (Invalid_configuration
      "candidate UID budget must be 1..10000 and spool_dir must exist")
  else
    let* operation=match J.find_operation store ~id with
      | Some op when op.scope=scope && op.kind=J.Append &&
          op.pair_id=None && op.destination=Some scope &&
          (op.state=J.Sent || op.state=J.Ambiguous) -> Ok op
      | _ -> Error (Invalid_operation
          "operation is not a pending APPEND in this scope") in
    let* epoch,digest,length,expected_flags,expected_date,frontier=
      match operation.destination_uidvalidity,operation.blob_sha256,
        operation.blob_length,operation.desired_flags,
        Imap_store.find_intent store ~id with
      | Some epoch,Some digest,Some length,Some expected_flags,
        Some ({state=(Imap_store.Sent | Imap_store.Ambiguous);
            kind=Imap_store.Append metadata;_} as intent)
        when intent.uidvalidity=Some epoch &&
          E.append_intent_matches ~scope operation intent ->
          (match metadata.pre_send_uid_frontier with
           | Some frontier ->
               let* expected_date=match metadata.expected_internal_date with
                 | None -> Ok None
                 | Some raw ->
                     (match Imap.Internal_date.of_string raw with
                      | Ok date -> Ok (Some date)
                      | Error _ -> Error (Invalid_operation
                          "APPEND journal contains an invalid INTERNALDATE")) in
               Ok (epoch,digest,length,expected_flags,expected_date,frontier)
           | None -> Error (Invalid_operation
               "APPEND has no saved pre-send UID frontier"))
      | _ -> Error (Invalid_operation
          "APPEND journal and legacy intent disagree") in
    let* ()=E.guard_bound_mailbox ~ctx in
    let inspect selected =
      let* info=network (Imap_eio.Selected.info selected) in
      if info.uidvalidity<>Imap.Uidvalidity.to_int64 epoch then
        Error Uidvalidity_changed
      else
        let body_bytes=ref 0L in
        let upper=Int64.pred info.uidnext in
        if upper<frontier then Error (Invalid_operation
          "server UIDNEXT regressed below APPEND frontier")
        else
          let span=Int64.sub upper frontier in
          if span>Int64.of_int max_uids then
            Error (Invalid_configuration
              "APPEND candidate UID range exceeds configured budget")
          else
            let rec scan first matches =
              if first>upper then Ok (List.rev matches)
              else
                let last=Int64.min upper (Int64.add first 999L) in
                let* first_uid=checked_uid first in
                let* last_uid=checked_uid last in
                let* rows=network
                  (Imap_eio.Selected.fetch_range selected
                    ~first:first_uid ~last:last_uid
                    ~items:(Imap.Fetch_item.Rfc822_size ::
                      (if Option.is_some expected_date then [Internal_date]
                       else []))) in
                let rec check matches = function
                  | [] -> scan (Int64.succ last) matches
                  | (row:Imap_eio.Selected.row)::rest ->
                      (match row.flags,row.size with
                       | None,_ -> check matches rest
                       | Some row_flags,Some row_size ->
                           let uid=row.uid in
                           let row_flags=durable_flags row_flags in
                           let* date_matches=match expected_date,
                               row.internal_date with
                             | None,_ -> Ok true
                             | Some expected,Some found -> Ok
                                 (Imap.Internal_date.equal_instant
                                   expected found)
                             | Some _,None -> Error (Client
                                 (Imap_eio.Error.Protocol
                                   "candidate FETCH omitted INTERNALDATE")) in
                           if row_size<>length ||
                              not date_matches ||
                              not (same_flags row_flags expected_flags) then
                             check matches rest
                           else
                             let* ()=if
                               length>Int64.sub max_body_bytes !body_bytes then
                               Error (Invalid_configuration
                                 "APPEND candidate body byte budget exceeded")
                               else Ok () in
                             body_bytes:=Int64.add !body_bytes length;
                             let spool=Eio.Path.(spool_dir /
                               ("imap-candidate-" ^ Maildir.reserve_id ())) in
                             let fetched=Spool.with_spool spool
                               (fun sink ->
                                 let fetched=Imap_eio.Selected.fetch_to selected
                                   ~uid ~max_bytes:length sink in
                                 match fetched with
                                 | Error error -> Error (Client error)
                                 | Ok () ->
                                     let found_length,found_digest=
                                       Spool.hash_file spool in
                                     Ok (found_length=length &&
                                       found_digest=digest)) in
                             let* matches_body=fetched in
                             let matches=if matches_body then uid::matches
                               else matches in
                             check matches rest
                       | _ -> Error (Client (Imap_eio.Error.Protocol
                           "candidate FETCH omitted UID, FLAGS or \
                            RFC822.SIZE"))) in
                check matches rows in
            let* matching_uids=scan (Int64.succ frontier) [] in
            Ok {uidvalidity=epoch;inspected_uids=Int64.to_int span;
              matching_uids} in
    E.with_selected ctx ~mode:`Read_only inspect
