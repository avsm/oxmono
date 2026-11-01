module J = Imap_store.Journal
module F = Mail_flag.Imap_flag
module E = Pair_evidence

open Error

type receipt = {
  cursor : Imap.Mirror.cursor;
  remote_to_local : int;
  local_to_remote : int;
  flags_updated : int;
  deletions : int;
  flags_held : int;
  deletions_held : int;
  held_pair_ids : string list;
  more : bool;
}

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as e -> e
let with_lease = E.with_lease
let maildir_result = E.maildir_result
let find = E.find
let with_inventory = E.with_inventory
let durable_flags = F.durable
let same_flags = F.equal_durable

let appended_uid_missing uid =
  Invalid_operation (Printf.sprintf "APPENDUID target UID %Ld is missing"
    (Imap.Uid.to_int64 uid))

(* A UID named by APPENDUID that is gone is reported as such, not as a
   transport failure. *)
let appended ~uid = function
  | Error (Client (Imap_eio.Error.Missing_uid missing))
    when Imap.Uid.equal missing uid -> Error (appended_uid_missing uid)
  | result -> result

let remote_date ctx ~uid ~uidvalidity =
  let* _,date=E.remote_flags_and_date ctx ~uid ~uidvalidity in
  Ok date

let operation ?append ~kind ~id ~scope ~local_id ~source_uidvalidity
    ~source_uid ~destination ~destination_uidvalidity ~blob ~flags
    ~internal_date () : J.operation = {
  id;pair_id=None;local_id=Some local_id;scope;kind;
  state=J.Prepared;source_uidvalidity;source_uid;destination;
  destination_uidvalidity;blob_sha256=Some blob.Imap_store.Blob.sha256;
  blob_length=Some blob.length;desired_flags=Some (durable_flags flags);
  internal_date=Some internal_date;append;
  receipt=None;receipt_uidvalidity=None;receipt_uid=None}

let pair = E.new_pair
let commit_pair = E.commit_new_pair

let copy_remote_to_local ~(ctx:Ctx.t) ~writer ~local_inventory ~uidvalidity
    (row:Imap.Mirror.row) =
  let {Ctx.store;scope;spool_dir;next_id;_}=ctx in
  let uid=row.uid in
  let spool=Eio.Path.(spool_dir / ("imap-" ^ Maildir.reserve_id ())) in
  let* archived=match Engine.archive_uid ~ctx ~uid ~spool () with
    | Error (Client (Imap_eio.Error.Missing_uid missing)) when
        Imap.Uid.equal missing uid -> Error (Source_vanished uid)
    | result -> result in
  let blob=archived.blob and internal_date=archived.internal_date in
  let id=next_id () and local_id=Maildir.reserve_id () in
  let flags=durable_flags row.flags in
  J.prepare_operation store (operation ~kind:J.Local_append ~id ~scope
    ~local_id ~source_uidvalidity:(Some uidvalidity) ~source_uid:(Some uid)
    ~destination:None ~destination_uidvalidity:None ~blob ~flags
    ~internal_date ());
  match E.storable writer ~flags internal_date with
  | Error reason ->
      J.reject_prepared_operation store ~id
        ~receipt:("Maildir cannot store the message: " ^ reason);
      Error (Invalid_operation (Printf.sprintf
        "remote UID %Ld cannot be stored in Maildir: %s"
        (Imap.Uid.to_int64 uid) reason))
  | Ok mtime ->
  J.mark_sent store ~id;
  let* local=maildir_result @@ Eio.Switch.run @@ fun sw ->
    let source=Imap_store.Blob.open_in store ~sw blob in
    Local_inventory.append ~inventory:local_inventory writer
      ~id:local_id ~source
      ~length:blob.length ~flags ~mtime () in
  let maildir=Maildir.of_writer writer in
  if local.length<>blob.length ||
     Local_inventory.sha256 ~inventory:local_inventory maildir local<>
       blob.sha256 then
    Error (Content_diverged id)
  else (
    J.observe_operation store ~id ~receipt:("maildir:" ^ local_id)
      ~destination_uidvalidity:None ~destination_uid:None;
    if not (same_flags flags local.flags) then Error (Flags_diverged id)
    else commit_pair store ~id
      (pair ~id ~scope ~uidvalidity ~uid ~local_id
        ~sha256:blob.sha256 ~length:blob.length ~internal_date ~flags ()))

let copy_local_to_remote ~(ctx:Ctx.t) ~maildir ~local_inventory
    ~uidvalidity (local:Maildir.occurrence) =
  let {Ctx.store;scope;spool_dir;next_id;_}=ctx in
  let* internal_date=E.local_date local in
  let* blob = match Local_inventory.with_unchanged_occurrence
      ~inventory:local_inventory maildir local
      (fun () ->
        let digest=Local_inventory.sha256 ~inventory:local_inventory
          maildir local in
        Eio.Switch.run @@ fun sw ->
          let source=Local_inventory.open_message
            ~inventory:local_inventory maildir ~sw local in
          Imap_store.Blob.put store ~source ~length:local.length
            ~expected_sha256:digest ()) with
    | Ok blob -> Ok blob
    | Error `Changed -> Error (Local_source_changed local.id)
    | exception Imap_store.Blob.Digest_mismatch ->
        Error (Local_source_changed local.id) in
  let id=next_id () in
  let flags=durable_flags local.flags in
  let append : J.append = {message_id=id;spool_ref=blob.sha256;
    pre_send_frontier=(Imap_store.load_cursor store ~scope).frontier} in
  J.prepare_operation ~local_source_mtime:local.mtime store
    (operation ~append ~kind:J.Append ~id ~scope ~local_id:local.id
      ~source_uidvalidity:None ~source_uid:None
      ~destination:(Some scope) ~destination_uidvalidity:(Some uidvalidity)
      ~blob ~flags ~internal_date ());
  match Engine.append_blob_journaled ~ctx ~id blob with
  | Ok (Engine.Identified receipt) ->
      let spool=Eio.Path.(spool_dir /
        ("imap-upload-verify-" ^ Maildir.reserve_id ())) in
      let* remote=appended ~uid:receipt.uid (Engine.fetch_uid_digest ~ctx
        ~uidvalidity:receipt.uidvalidity ~uid:receipt.uid ~spool ()) in
      let* ()=if remote.length=blob.length &&
          remote.sha256=blob.sha256 then Ok ()
        else Error (Content_diverged id) in
      let observed=remote.flags in
      if not (same_flags flags observed) then Error (Flags_diverged id)
      else let* ()=
        if Imap.Internal_date.equal_instant internal_date
            remote.internal_date then Ok ()
        else Error (Date_diverged id) in
      let* ()=match Local_inventory.with_unchanged_occurrence
          ~inventory:local_inventory maildir local (fun () ->
            Local_inventory.sha256 ~inventory:local_inventory maildir
              local) with
        | Ok digest when digest=blob.sha256 -> Ok ()
        | Ok _ -> Error (Content_diverged id)
        | Error `Changed -> Error (Local_source_changed local.id) in
      commit_pair store ~id
        (pair ~id ~scope ~uidvalidity:receipt.uidvalidity
          ~uid:receipt.uid ~local_id:local.id
          ~sha256:blob.sha256 ~length:blob.length ~internal_date
          ~flags:observed ())
  | Ok Engine.Needs_reconciliation -> Error (Pending_operations [id])
  | Error _ as error ->
      (match J.find_operation store ~id with
       | Some {state=J.Prepared;_} ->
           J.reject_prepared_operation store ~id
             ~receipt:"APPEND was not dispatched"
       | _ -> ());
      error

let snapshot_has_uid = E.snapshot_has_uid
let snapshot_row_for_uid = E.snapshot_row

let reconcile_local_append ~(ctx:Ctx.t) ~maildir ~(cursor:Imap.Mirror.cursor)
    (operation:J.operation) =
  let {Ctx.store;scope;_}=ctx in
  match operation.kind,operation.state,operation.local_id,
        operation.source_uidvalidity,operation.source_uid,
        operation.blob_sha256,operation.blob_length,
        operation.desired_flags with
  | J.Local_append,(J.Sent|J.Ambiguous|J.Observed),
    Some local_id,Some uidvalidity,Some uid,Some sha256,
    Some length,Some flags when cursor.uidvalidity=Some uidvalidity ->
      let* found=find maildir ~id:local_id in
      (match found with
       | None -> Ok ()
       | Some local ->
           let* present=snapshot_has_uid store ~scope ~cursor uid in
           if not present then Ok ()
           else if local.length<>length ||
              Maildir.sha256 maildir local<>sha256 then
             Error (Content_diverged operation.id)
           else if not (same_flags local.flags flags) then
             Error (Flags_diverged operation.id)
           else let* expected_date=match operation.internal_date with
             | Some expected ->
                 (match Local_date.of_occurrence local with
                  | Ok actual when Imap.Internal_date.equal_instant
                      expected actual -> Ok (Some expected)
                  | _ -> Error (Date_diverged operation.id))
             | None -> Result.map Option.some (E.local_date local) in
           let* ()=match expected_date with
             | None -> Ok ()
             | Some expected ->
                 let* actual=remote_date ctx ~uid ~uidvalidity in
                 if Imap.Internal_date.equal_instant expected actual then Ok ()
                 else Error (Date_diverged operation.id) in
           (
             if operation.state<>J.Observed then
               J.observe_operation store ~id:operation.id
                 ~receipt:("maildir:" ^ local_id)
                 ~destination_uidvalidity:None ~destination_uid:None;
             commit_pair store ~id:operation.id
               (pair ~id:operation.id ~scope ~uidvalidity ~uid
                 ~local_id ~sha256 ~length
                 ?internal_date:expected_date ~flags ())))
  | _ -> Ok ()

let reconcile_remote_append ~(ctx:Ctx.t) ~maildir
    ~(cursor:Imap.Mirror.cursor) (operation:J.operation) =
  let {Ctx.store;scope;spool_dir;_}=ctx in
  match operation.kind,operation.state,operation.local_id,
        operation.blob_sha256,operation.blob_length,
        operation.desired_flags,operation.receipt_uidvalidity,
        operation.receipt_uid with
  | J.Append,J.Observed,Some local_id,Some sha256,Some length,Some flags,
    Some uidvalidity,Some uid when cursor.uidvalidity=Some uidvalidity ->
      let* present=snapshot_has_uid store ~scope ~cursor uid in
      if not present then Ok ()
      else
        let* found=find maildir ~id:local_id in
        (match found with
         | None -> Ok ()
         | Some local ->
             let* ()=match J.operation_source_mtime store
                 ~id:operation.id with
               | Some mtime when mtime=local.mtime -> Ok ()
               | Some _ -> Error (Local_source_changed local_id)
               | None -> Error (Pending_operations [operation.id]) in
             let* expected_date=match operation.internal_date with
               | Some intended ->
                   let* saved=E.local_date local in
                   if Imap.Internal_date.equal_instant saved intended
                   then Ok (Some intended)
                   else Error (Date_diverged operation.id)
               | None -> Result.map Option.some (E.local_date local) in
             if local.length<>length ||
                Maildir.sha256 maildir local<>sha256 then
               Error (Content_diverged operation.id)
             else if not (same_flags flags local.flags) then
               Error (Flags_diverged operation.id)
             else
               let spool=Eio.Path.(spool_dir /
                 ("imap-recover-" ^ Maildir.reserve_id ())) in
               let* remote=appended ~uid (Engine.fetch_uid_digest ~ctx
                 ~uidvalidity ~uid ~spool ()) in
               if remote.length<>length || remote.sha256<>sha256 then
                 Error (Content_diverged operation.id)
               else if not (same_flags flags remote.flags) then
                 Error (Flags_diverged operation.id)
               else let* ()=match expected_date with
                 | Some expected when not
                     (Imap.Internal_date.equal_instant expected
                       remote.internal_date) ->
                     Error (Date_diverged operation.id)
                 | _ -> Ok () in
                 commit_pair store ~id:operation.id
                   (pair ~id:operation.id ~scope ~uidvalidity ~uid
                     ~local_id ~sha256 ~length
                     ?internal_date:expected_date ~flags ()))
  | _ -> Ok ()

(* A prepared copy was never dispatched, since APPEND and the Maildir
   write each move their operation to Sent first. *)
let reject_unsent_copy ~store ~maildir (operation:J.operation) =
  match operation.kind,operation.state with
  | (J.Append | J.Local_append),J.Prepared ->
      let* local_exists=match operation.kind,operation.local_id with
        | J.Local_append,Some id ->
            let* found=find maildir ~id in
            Ok (Option.is_some found)
        | _ -> Ok false in
      if local_exists then Error (Pending_operations [operation.id])
      else (
        J.reject_prepared_operation store ~id:operation.id
          ~receipt:"prepared copy was not dispatched";
        Ok ())
  | _ -> Ok ()

let record_absences ~store ~maildir ~scope ~(cursor:Imap.Mirror.cursor)
    ~stage_id ~next_id local_inventory =
  let rec pages after =
    let rows=J.pairs_page store ~scope ?after ~limit:1000 () in
    let rec process = function
      | [] ->
          if List.length rows<1000 then Ok ()
          else pages (Some (List.hd (List.rev rows)).id)
      | (pair:J.pair)::rest ->
          let* remote_present=match pair.remote_uidvalidity,
            pair.remote_uid with
            | Some epoch,Some uid when cursor.uidvalidity=Some epoch ->
                let* present=snapshot_has_uid store ~scope ~cursor uid in
                Ok (Some present)
            | _ -> Ok None in
          let local_occurrence=Option.bind pair.local_id
            (fun id -> Local_inventory.find local_inventory ~id) in
          let local_present=local_occurrence<>None in
          let* ()=match remote_present,pair.remote_tombstone with
            | Some true,Some {reason=J.Inventory_absence;_} ->
                (match J.note_presence store ~pair ~side:`Remote
                  ~generation:cursor.generation with
                 | `Recorded -> Ok ()
                 | `Stale_revision -> Error Store_stale_revision)
            | _ -> Ok () in
          let* ()=match local_present,pair.local_tombstone with
            | true,Some {reason=J.Local_absence;_} ->
                (match J.note_presence store ~pair ~side:`Local
                  ~generation:cursor.generation with
                 | `Recorded -> Ok ()
                 | `Stale_revision -> Error Store_stale_revision)
            | _ -> Ok () in
          let* pair=match pair.local_tombstone,local_occurrence,
              pair.content_sha256,pair.content_length with
            | Some {reason=J.Local_absence;_},Some local,Some _,Some _ ->
                if E.local_content ~inventory:local_inventory maildir pair
                    local=`Matches then
                  if not (E.local_date_matches pair local) then Ok pair
                  else (match J.reactivate_local store ~pair
                    ~generation:cursor.generation with
                   | `Reactivated updated -> Ok updated
                   | `Stale_revision -> Error Store_stale_revision)
                else
                  (match J.ensure_open_conflict store ~pair
                    ~kind:J.Content_conflict ~id:(next_id ())
                    ~evidence:"reappeared local occurrence differs from paired content" with
                   | `Open _ -> Ok pair
                   | `Stale_revision -> Error Store_stale_revision)
            | _ -> Ok pair in
          let absence_reset side (tombstone:J.tombstone option) =
            match tombstone with
            | Some {generation=Some first;_} ->
                (match J.last_presence_generation store ~pair_id:pair.id ~side with
                 | Some last -> last>=first | None -> false)
            | Some {generation=None;_} ->
                J.last_presence_generation store ~pair_id:pair.id ~side<>None
            | None -> false in
          let remote_absent=remote_present=Some false &&
            (pair.remote_tombstone=None ||
             match pair.remote_tombstone with
             | Some {reason=J.Inventory_absence;_} ->
                 absence_reset `Remote pair.remote_tombstone
             | _ -> false) in
          let local_absent=not local_present && pair.local_id<>None &&
            (pair.local_tombstone=None ||
             match pair.local_tombstone with
             | Some {reason=J.Local_absence;_} ->
                 absence_reset `Local pair.local_tombstone
             | _ -> false) in
          if remote_absent || local_absent then (
            let remote_tombstone=if not remote_absent then
                pair.remote_tombstone
              else match cursor.inventory_ref with
                | Some evidence -> Some {J.reason=J.Inventory_absence;
                    evidence;generation=Some cursor.generation}
                | None -> None in
            let local_tombstone=if not local_absent then
                pair.local_tombstone
              else Some {J.reason=J.Local_absence;
                evidence=stage_id;generation=Some cursor.generation} in
            if remote_absent && remote_tombstone=None then
              Error (Invalid_configuration
                "complete remote inventory has no durable reference")
            else
              let updated={pair with remote_tombstone;local_tombstone} in
              (match J.put_pair store ~expected_revision:(Some pair.revision)
                updated with
               | `Stale_revision -> Error Store_stale_revision
               | `Committed _ -> process rest))
          else process rest in
    process rows in
  pages None

let copy_once_unlocked ~max_transfers ~min_absence_scans
    ~allow_bootstrap_duplicates ~propagate_deleted ~deletion_policy
    ~(ctx:Ctx.t) ~writer ~stage_id () =
  let {Ctx.store;scope;spool_dir;next_id;_}=ctx in
  let maildir=Maildir.of_writer writer in
    let prior_cursor=Imap_store.load_cursor store ~scope in
    let has_durable_identity=
      J.pairs_page store ~scope ~limit:1 ()<>[] ||
      J.active_operations_page store ~scope ~limit:1 ()<>[] in
    let expected_uidvalidity=if has_durable_identity then
      prior_cursor.uidvalidity else None in
    let* published=Engine.scan_once ~ctx ~stage_id ?expected_uidvalidity
        () in
      let cursor=published.cursor in
      let rec reconcile_pages after =
        let page=J.active_operations_page store ~scope ?after
          ~limit:256 () in
        let rec reconcile = function
        | [] -> Ok ()
        | operation::rest ->
            let* ()=reject_unsent_copy ~store ~maildir operation in
            let* ()=reconcile_local_append ~ctx ~maildir ~cursor
              operation in
            let* ()=reconcile_remote_append ~ctx ~maildir ~cursor
              operation in
            let* ()=if operation.kind<>J.Flags then Ok () else
              match Flags.recover_operation ~ctx ~writer ~operation () with
              | Ok _ | Error (Pending_operations _) -> Ok ()
              | Error _ as error -> error in
            reconcile rest in
        let* ()=reconcile page in
        if List.length page<256 then Ok ()
        else reconcile_pages (Some (List.hd (List.rev page)).id) in
      let* ()=reconcile_pages None in
      let* uidvalidity=match cursor.uidvalidity with
        | Some value -> Ok value
        | None -> Error (Invalid_configuration "published scan has no epoch") in
      with_inventory ~spool_dir maildir (fun local_inventory ->
        let rec recover_deletions = function
          | [] -> Ok ()
          | (operation:J.operation)::rest ->
              if operation.kind<>J.Delete &&
                 operation.kind<>J.Local_delete then
                recover_deletions rest
              else
                let* _ = match Deletion.recover_operation
                  ~store ~writer ~cursor ~local_inventory ~operation () with
                | Ok outcome -> Ok outcome
                | Error (Pending_operations _) -> Ok Deletion.Unchanged
                | Error _ as error -> error in
                recover_deletions rest in
        let rec recover_pages after =
          let page=J.active_operations_page store ~scope ?after
            ~limit:256 () in
          let* ()=recover_deletions page in
          if List.length page<256 then Ok ()
          else recover_pages (Some (List.hd (List.rev page)).id) in
        let* ()=recover_pages None in
        let pending=J.active_operations_page store ~scope
          ~limit:256 () in
        let* ()=if pending=[] then Ok () else Error
          (Pending_operations (List.map
            (fun (x:J.operation) -> x.id) pending)) in
        let rec check_epochs after had_pairs =
          let page=J.pairs_page store ~scope ?after ~limit:1000 () in
          let stale=List.exists (fun (x:J.pair) ->
            x.remote_tombstone=None &&
            match x.remote_uidvalidity with
            | Some value -> value<>uidvalidity
            | None -> false) page in
          if stale then Error Uidvalidity_changed
          else if List.length page<1000 then Ok (had_pairs || page<>[])
          else
            let last=(List.hd (List.rev page):J.pair).id in
            check_epochs (Some last) true in
        let* had_pairs=check_epochs None false in
        let* ()=record_absences ~store ~maildir ~scope ~cursor ~stage_id
          ~next_id
          local_inventory in
        if not had_pairs && published.row_count>0L &&
            Local_inventory.count local_inventory>0L &&
            not allow_bootstrap_duplicates then
          Error Bootstrap_requires_pairing
        else
          let copied_remote=ref 0 and copied_local=ref 0 in
          let flags_updated=ref 0 and deletions=ref 0 in
          let flags_held=ref 0 and deletions_held=ref 0 in
          let held_pair_ids=ref [] in
          let record_hold id =
            if List.length !held_pair_ids<100 &&
               not (List.mem id !held_pair_ids) then
              held_pair_ids:=id::!held_pair_ids in
          let budget_used ()=
            !copied_remote+ !copied_local+ !flags_updated+ !deletions in
          let resolve_policy_conflict pair=match
            J.resolve_open_conflicts store ~pair ~kind:J.Policy_conflict with
            | `Resolved _ -> Ok ()
            | `Stale_revision -> Error Store_stale_revision in
          let hold_deleted_flag pair=match
            J.ensure_open_conflict store ~pair ~kind:J.Policy_conflict
              ~id:(next_id ())
              ~evidence:"\\Deleted differs from the paired flag baseline; propagation requires an explicit policy decision" with
            | `Open _ -> Ok ()
            | `Stale_revision -> Error Store_stale_revision in
          let resolve_deletion_hold pair=match
            J.resolve_open_conflicts store ~pair ~kind:J.Deletion_hold with
            | `Resolved _ -> Ok ()
            | `Stale_revision -> Error Store_stale_revision in
          let hold_deletion_evidence pair evidence=
            match J.ensure_open_conflict store ~pair
              ~kind:J.Deletion_hold ~id:(next_id ()) ~evidence with
            | `Open _ -> Ok ()
            | `Stale_revision -> Error Store_stale_revision in
          let hold_deletion pair reason=
            let evidence=match reason with
              | Imap.Sync_policy.Preservation_policy ->
                  "one paired side is absent; preserve policy holds survivor deletion"
              | Imap.Sync_policy.Direction_policy ->
                  "one paired side is absent; deletion direction is disabled"
              | Imap.Sync_policy.Retention_policy ->
                  "local copy was retained or evicted; remote deletion is held"
              | Imap.Sync_policy.Unverified_absence ->
                  "one paired side is absent without the required inventory tombstone"
              | Imap.Sync_policy.Survivor_changed ->
                  "one paired side is absent; survivor lacks unchanged identity evidence"
              | Imap.Sync_policy.Incomplete_inventory ->
                  "one paired side is absent; complete inventory is required"
              | Imap.Sync_policy.Unpaired_identity ->
                  "one paired side is absent; pair identity is incomplete"
              | Imap.Sync_policy.Grace_period ->
                  "one paired side is absent; configured complete-scan grace period has not elapsed"
              | Imap.Sync_policy.Missing_content_evidence ->
                  "one paired side is absent; the pair has no content \
                   evidence to verify the survivor" in
            hold_deletion_evidence pair evidence in
          let hold_flags pair evidence=
            match J.ensure_open_conflict store ~pair ~kind:J.Policy_conflict
              ~id:(next_id ()) ~evidence with
            | `Open _ ->
                incr flags_held;
                record_hold pair.J.id;
                Ok ()
            | `Stale_revision -> Error Store_stale_revision in
          let hold_pair (pair:J.pair)=
            incr flags_held;
            record_hold pair.id in
          let verify_pair_date (pair:J.pair) =
            let resolve ()=match J.resolve_open_conflicts store ~pair
                ~kind:J.Identity_conflict with
              | `Resolved _ -> Ok true
              | `Stale_revision -> Error Store_stale_revision in
            match pair.internal_date,pair.local_id with
            | Some _,Some local_id ->
                (match Local_inventory.find local_inventory
                    ~id:local_id with
                 | None -> Ok true
                 | Some local ->
                     if E.local_date_matches pair local then resolve ()
                     else
                       let evidence=match Local_date.of_occurrence local with
                         | Ok _ ->
                             "local INTERNALDATE differs from paired baseline"
                         | Error reason ->
                             "local INTERNALDATE unavailable: " ^ reason in
                       match J.ensure_open_conflict store ~pair
                           ~kind:J.Identity_conflict ~id:(next_id ())
                           ~evidence with
                       | `Open _ -> Ok false
                       | `Stale_revision -> Error Store_stale_revision)
            | _ -> Ok true in
          let both_present (pair:J.pair)=
            match pair.remote_uidvalidity,pair.remote_uid,pair.local_id with
            | Some epoch,Some uid,Some local_id
              when cursor.uidvalidity=Some epoch ->
                let* remote=snapshot_has_uid store ~scope ~cursor uid in
                Ok (remote && Local_inventory.find local_inventory
                  ~id:local_id<>None)
            | _ -> Ok false in
          let rec remote_pages after_uid =
            if budget_used ()>=max_transfers then Ok ()
            else
              match Imap_store.snapshot_page store ~scope ~cursor
                ?after_uid ~limit:1000 () with
              | `Stale_revision -> Error Store_stale_revision
              | `Rows [] -> Ok ()
              | `Rows rows ->
                  let rec process = function
                    | [] ->
                        remote_pages (Some (List.hd (List.rev rows)).uid)
                    | _ when budget_used ()>=max_transfers -> Ok ()
                    | (row:Imap.Mirror.row)::rest ->
                        (match J.find_remote store ~scope ~uidvalidity
                            ~uid:row.uid with
                         | Some _ -> process rest
                         | None ->
                             let* ()=copy_remote_to_local ~ctx ~writer
                               ~local_inventory ~uidvalidity row in
                             incr copied_remote;
                             process rest) in
                  process rows in
          let* ()=remote_pages None in
          let rec local_pages after =
            if budget_used ()>=max_transfers then Ok ()
            else
              let page=Local_inventory.page local_inventory
                ?after ~limit:1000 () in
              let rec process = function
                | [] ->
                    (match page.next_after with
                     | None -> Ok ()
                     | Some after -> local_pages (Some after))
                | _ when budget_used ()>=max_transfers ->
                    Ok ()
                | (local:Maildir.occurrence)::rest ->
                    (match J.find_local store ~scope ~local_id:local.id with
                     | Some _ -> process rest
                     | None ->
                         let* ()=copy_local_to_remote ~ctx ~maildir
                           ~local_inventory ~uidvalidity local in
                         incr copied_local;
                         process rest) in
              process page.occurrences in
          let* ()=local_pages None in
          let rec flag_pages after =
            if budget_used ()>=max_transfers then Ok ()
            else
              let rows=J.pairs_page store ~scope ?after ~limit:1000 () in
              let rec process = function
                | [] ->
                    if List.length rows<1000 then Ok ()
                    else flag_pages (Some (List.hd (List.rev rows)).id)
                | _ when budget_used ()>=max_transfers -> Ok ()
                | (pair:J.pair)::rest when
                    pair.remote_tombstone<>None ||
                    pair.local_tombstone<>None ->
                    let* dated=verify_pair_date pair in
                    let* both=both_present pair in
                    let* ()=if dated && not both then
                        resolve_policy_conflict pair
                      else Ok (hold_pair pair) in
                    process rest
                | (pair:J.pair)::rest ->
                    let* dated=verify_pair_date pair in
                    if not dated then (hold_pair pair; process rest) else
                    let* needs_check=match pair.remote_uid,
                      pair.local_id with
                      | Some uid,Some local_id ->
                          let* remote=snapshot_row_for_uid store ~scope
                            ~cursor uid in
                          let local=Local_inventory.find
                            local_inventory ~id:local_id in
                          (match remote,local with
                           | Some remote,Some local ->
                               Ok (not (same_flags remote.flags
                                 pair.common_flags &&
                                 same_flags local.flags
                                   pair.common_flags))
                           | _ -> Ok false)
                      | _ -> Ok false in
                    if not needs_check then (
                      let* ()=resolve_policy_conflict pair in
                      process rest) else
                      (match Flags.reconcile_pair ~propagate_deleted
                        ~inventory:local_inventory ~ctx ~writer ~pair () with
                       | Ok {outcome;deleted_held} ->
                           let current=match outcome with
                             | Flags.Unchanged -> pair
                             | Flags.Updated updated ->
                                 incr flags_updated; updated in
                           let* ()=if deleted_held then (
                               let* ()=hold_deleted_flag current in
                               incr flags_held;
                               record_hold pair.id;
                               Ok ())
                             else resolve_policy_conflict current in
                           process rest
                       | Error Modified ->
                           hold_pair pair;
                           process rest
                       | Error (Content_mismatch _) -> process rest
                       | Error Conditional_store_unavailable ->
                           let* ()=hold_flags pair
                             "remote flag change needs CONDSTORE and a \
                              message MODSEQ" in
                           process rest
                       | Error (Permanent_flag_unavailable flag) ->
                           let* ()=hold_flags pair (Format.asprintf
                             "remote flag %a is not permanently writable"
                             F.pp flag) in
                           process rest
                       | Error _ as error -> error) in
              process rows in
          let* ()=flag_pages None in
          let rec content_conflict_pages after =
            let page=J.open_conflicts_page store ~scope ?after
              ~limit:1000 () in
            let rec process = function
              | [] ->
                  if List.length page<1000 then Ok ()
                  else content_conflict_pages
                    (Some (List.hd (List.rev page)).id)
              | (conflict:J.conflict)::rest when
                  conflict.kind<>J.Content_conflict -> process rest
              | (conflict:J.conflict)::rest ->
                  let* pair=match J.find_pair store ~id:conflict.pair_id with
                    | Some pair when pair.scope=scope -> Ok pair
                    | _ -> Error (Invalid_operation (Printf.sprintf
                        "content conflict %s names missing pair %s"
                        conflict.id conflict.pair_id)) in
                  let matches=match Option.bind pair.local_id (fun id ->
                      Local_inventory.find local_inventory ~id) with
                    | Some local ->
                        E.local_content ~inventory:local_inventory maildir
                          pair local=`Matches
                    | None -> false in
                  if matches then (
                    let* ()=match J.resolve_open_conflicts store ~pair
                        ~kind:J.Content_conflict with
                      | `Resolved _ -> Ok ()
                      | `Stale_revision -> Error Store_stale_revision in
                    process rest)
                  else (hold_pair pair; process rest) in
            process page in
          let* ()=content_conflict_pages None in
          let rec delete_pages after =
            if budget_used ()>=max_transfers then Ok ()
            else
              let rows=J.pairs_page store ~scope ?after ~limit:1000 () in
              let rec process = function
                | [] ->
                    if List.length rows<1000 then Ok ()
                    else delete_pages (Some (List.hd (List.rev rows)).id)
                | _ when budget_used ()>=max_transfers -> Ok ()
                | (pair:J.pair)::rest when
                    pair.remote_tombstone=None &&
                    pair.local_tombstone=None -> process rest
                | (pair:J.pair)::rest when
                    pair.remote_tombstone<>None &&
                    pair.local_tombstone<>None ->
                    let* ()=resolve_deletion_hold pair in
                    process rest
                | (pair:J.pair)::rest when
                    pair.remote_uidvalidity=None || pair.remote_uid=None ||
                    pair.local_id=None ->
                    let* ()=hold_deletion pair
                      Imap.Sync_policy.Unpaired_identity in
                    incr deletions_held;
                    record_hold pair.id;
                    process rest
                | (pair:J.pair)::rest ->
                    (match Deletion.reconcile_pair ~min_absence_scans ~ctx
                      ~writer ~cursor ~local_inventory ~pair
                      ~policy:deletion_policy () with
                     | Ok (Deletion.Deleted updated) ->
                         let* ()=resolve_deletion_hold updated in
                         incr deletions; process rest
                     | Ok Deletion.Unchanged ->
                         let* ()=resolve_deletion_hold pair in
                         process rest
                     | Ok (Deletion.Held reason) ->
                         let* ()=hold_deletion pair reason in
                         incr deletions_held;
                         record_hold pair.id;
                         process rest
                     | Error (Unsupported reason) ->
                         let* ()=hold_deletion_evidence pair
                           ("targeted deletion unavailable: " ^ reason) in
                         incr deletions_held;
                         record_hold pair.id;
                         process rest
                     | Error _ as error -> error) in
              process rows in
          let* ()=delete_pages None in
          Ok {cursor;remote_to_local= !copied_remote;
              local_to_remote= !copied_local;
              flags_updated= !flags_updated;
              deletions= !deletions;
              flags_held= !flags_held;
              deletions_held= !deletions_held;
              held_pair_ids=List.rev !held_pair_ids;
              more=budget_used ()>=max_transfers})

let copy_once ?(max_transfers=100) ?(min_absence_scans=0)
    ?(allow_bootstrap_duplicates=false) ?(propagate_deleted=false)
    ?(deletion_policy=Imap.Sync_policy.Preserve) ~(ctx:Ctx.t) ~maildir
    ~stage_id () =
  if max_transfers<1 || min_absence_scans<0 ||
      not (Eio.Path.is_directory ctx.spool_dir) then
    Error (Invalid_configuration
      "max_transfers must be positive, min_absence_scans nonnegative, and \
       spool_dir must exist")
  else
    with_lease maildir (fun writer ->
      copy_once_unlocked ~max_transfers ~min_absence_scans
        ~allow_bootstrap_duplicates ~propagate_deleted ~deletion_policy ~ctx
        ~writer ~stage_id ())

let recover_local ~maildir ~spool_dir () =
  with_lease maildir (fun writer ->
    ignore (Maildir.recover writer : Maildir.recovery);
    ignore (Local_inventory.recover spool_dir : string list);
    Ok ())

type local_verification = {
  checked : int64;
  mismatched : int64;
  restored : int64;
  missing : int64;
  unverified : int64;
}

let verify_local_content ~store ~maildir ~scope ~next_id ~spool_dir
    ~on_issue () =
  let* ()=E.require_spool_dir spool_dir in
  with_lease maildir (fun _ ->
    with_inventory ~spool_dir maildir (fun inventory ->
      let checked=ref 0L and mismatched=ref 0L and restored=ref 0L in
      let missing=ref 0L and unverified=ref 0L in
      let bump counter=counter:=Int64.succ !counter in
      let rec pages after =
        let rows=J.pairs_page store ~scope ?after ~limit:1000 () in
        let rec process = function
          | [] ->
              if List.length rows<1000 then Ok ()
              else pages (Some (List.hd (List.rev rows)).id)
          | (pair:J.pair)::rest ->
              let* ()=match pair.local_tombstone,pair.local_id,
                  pair.content_sha256,pair.content_length with
                | Some _,_,_,_ -> Ok ()
                | None,Some local_id,Some _,Some _ ->
                    (match Local_inventory.find inventory
                        ~id:local_id with
                     | None ->
                         bump missing;
                         on_issue pair.id "paired local occurrence is absent";
                         Ok ()
                     | Some local ->
                         bump checked;
                         let verified=E.local_content ~inventory maildir pair
                           local in
                         (match verified with
                          | `Matches ->
                              if J.has_open_conflict store ~pair
                                  ~kind:J.Content_conflict then
                                (match J.resolve_open_conflicts store ~pair
                                    ~kind:J.Content_conflict with
                                 | `Resolved _ -> bump restored; Ok ()
                                 | `Stale_revision ->
                                     Error Store_stale_revision)
                              else Ok ()
                          | `Differs | `Changed ->
                              let evidence=match verified with
                                | `Changed ->
                                    "local occurrence changed during content verification"
                                | _ ->
                                    "local message content differs from the paired digest" in
                              (match J.ensure_open_conflict store ~pair
                                  ~kind:J.Content_conflict ~id:(next_id ())
                                  ~evidence with
                               | `Open _ ->
                                   bump mismatched;
                                   on_issue pair.id evidence;
                                   Ok ()
                               | `Stale_revision ->
                                   Error Store_stale_revision)))
                | None,_,_,_ ->
                    bump unverified;
                    on_issue pair.id "paired local content evidence is incomplete";
                    Ok () in
              process rest in
        process rows in
      let* ()=pages None in
      Ok {checked= !checked;mismatched= !mismatched;
          restored= !restored;missing= !missing;
          unverified= !unverified}))

