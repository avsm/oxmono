module J = Imap_store.Journal
module F = Mail_flag.Imap_flag
module E = Pair_evidence

open Error

type outcome =
  | Unchanged
  | Held of Imap.Sync_policy.deletion_hold
  | Deleted of J.pair

let ( let* ) result f = match result with Ok x -> f x | Error _ as e -> e
let network = function Ok x -> Ok x | Error e -> Error (Client e)

let deleted = F.system F.Deleted
let survivor_changed = Held Imap.Sync_policy.Survivor_changed
let deleted_pair = function Ok pair -> Ok (Deleted pair) | Error _ as e -> e

let expunge_preflight ~before_flags ~before_modseq after =
  let expected=F.durable (deleted :: before_flags) in
  match after with
  | Some (after_flags,Some after_modseq) ->
      F.equal_durable after_flags expected &&
      (if List.exists (F.equal deleted) before_flags then
         after_modseq>=before_modseq
       else after_modseq>before_modseq)
  | _ -> false

let plan ~policy ~min_absence_scans ~current_generation ~last_presence
    ~remote_present ~local_present (pair:J.pair) =
  if min_absence_scans<0 then
    invalid_arg "Deletion.plan: negative absence grace";
  let missing=if remote_present then pair.local_tombstone
    else pair.remote_tombstone in
  let mature=Imap.Sync_policy.absence_mature
    ~last_present_generation:
      (last_presence (if remote_present then `Local else `Remote))
    ~current_generation
    ~first_generation:(Option.bind missing (fun (x:J.tombstone) ->
      x.generation))
    ~min_scans:min_absence_scans in
  let local_retained=match pair.local_tombstone with
    | Some {reason=J.Retention;_} -> true
    | _ -> false in
  let open Imap.Sync_policy in
  match plan_disappearance_with_grace ~absence_mature:mature ~policy
      ~paired:true ~local_retained ~survivor_unchanged:true
      ~remote:{present=remote_present;complete=true}
      ~local:{present=local_present;complete=true} with
  | Delete_local | Delete_remote
    when pair.content_sha256=None || pair.content_length=None ->
      Hold_deletion Missing_content_evidence
  | Delete_local when not (E.remote_absence_proven pair) ->
      Hold_deletion Unverified_absence
  | Delete_remote when not (E.local_absence_recorded pair) ->
      Hold_deletion Unverified_absence
  | plan -> plan

let bound (pair:J.pair) =
  match pair.remote_uidvalidity,pair.remote_uid,pair.local_id with
  | Some epoch,Some uid,Some local_id -> Ok (epoch,uid,local_id)
  | _ -> Error Identity_changed

let intent (pair:J.pair) ~id ~kind ~epoch ~uid ~local_id
    ~digest ~length : J.operation = {
  id;pair_id=Some pair.id;local_id=Some local_id;scope=pair.scope;
  kind;state=J.Prepared;source_uidvalidity=Some epoch;source_uid=Some uid;
  destination=None;destination_uidvalidity=None;
  blob_sha256=Some digest;blob_length=Some length;
  desired_flags=Some (F.durable pair.common_flags);
  receipt=None;receipt_uidvalidity=None;receipt_uid=None;
}

let local_unchanged ?inventory maildir (pair:J.pair)
    (occurrence:Maildir.occurrence) =
  F.equal_durable occurrence.flags pair.common_flags &&
  E.local_content ?inventory maildir pair occurrence=`Matches

let delete_local ~(ctx:Ctx.t) ~writer ~local_inventory (pair:J.pair) ~epoch
    ~uid ~(local:Maildir.occurrence) ~digest ~length =
  let store=ctx.store in
  let maildir=Maildir.of_writer writer in
  let* absent=E.remote_absent ctx ~epoch ~uid in
  if not absent then Error Stale_inventory
  else if not (local_unchanged ~inventory:local_inventory maildir pair local)
  then Ok survivor_changed
  else
    let id=ctx.next_id () in
    J.prepare_operation store (intent pair ~id ~kind:J.Local_delete
      ~epoch ~uid ~local_id:local.id ~digest ~length);
    match E.current_pair store pair with
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
            let* still=E.present maildir ~id:local.id in
            if still then (
              J.mark_ambiguous store ~id
                ~reason:"Maildir ID still present after unlink";
              Error (Pending_operations [id]))
            else deleted_pair (E.finish_unlink store pair ~id
              ~receipt:"Maildir ID absent after unlink")

let delete_remote ~(ctx:Ctx.t) ~maildir (pair:J.pair) ~epoch ~uid ~local_id
    ~digest ~length =
  let {Ctx.client;store;next_id;spool_dir;_}=ctx in
  let has=Imap_eio.Client.has client in
  if not (has Imap.Capability.Uidplus) then
    Error (Unsupported "UIDPLUS required for targeted UID EXPUNGE")
  else if not (has Imap.Capability.Condstore ||
               has Imap.Capability.Qresync) then
    Error (Unsupported "CONDSTORE required for conditional UID STORE")
  else if not (Eio.Path.is_directory spool_dir) then
    Error (Invalid_configuration "spool_dir must exist")
  else
    let* local_present=E.present maildir ~id:local_id in
    if local_present then Error Stale_inventory else
    let spool=Eio.Path.(spool_dir /
      ("imap-delete-" ^ Maildir.reserve_id ())) in
    let precheck (info:Imap.Response.select_metadata) =
      match Flags.validate_permanent_flags
          ~available:info.permanentflags ~defined:info.flags
          ~remote:[] ~merged:[deleted] with
      | Ok () -> Ok ()
      | Error (Permanent_flag_unavailable _) ->
          Error (Unsupported "\\Deleted is not a PERMANENTFLAG")
      | Error _ as error -> error in
    let* evidence=E.stable_remote_body ~precheck ctx ~mode:`Read_write
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
        match E.current_pair store pair with
        | Error error ->
            J.reject_prepared_operation store ~id
              ~receipt:"pair changed before DELETE dispatch";
            Error error
        | Ok _ ->
            J.mark_sent store ~id;
            let set=Imap.Uid_set.singleton uid in
            let dispatched=E.with_selected ctx ~mode:`Read_write
              (fun selected ->
                let* before=E.epoch_flags selected ~epoch ~uid ~modseq:true in
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
                    match E.epoch_flags selected ~epoch ~uid ~modseq:true with
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
                            match E.epoch_flags selected ~epoch ~uid
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
                    E.describe error);
                Error (Client error)
            | Ok (`Store_failed error) ->
                J.mark_ambiguous store ~id
                  ~reason:("conditional STORE outcome unknown: " ^
                    E.describe error);
                Error (Pending_operations [id])
            | Ok (`Expunge_failed error) ->
                J.mark_ambiguous store ~id
                  ~reason:("UID EXPUNGE outcome unknown: " ^
                    E.describe error);
                Error (Pending_operations [id])
            | Ok (`Uncertain reason) ->
                J.mark_ambiguous store ~id ~reason;
                Error (Pending_operations [id])
            | Ok `Absent ->
                deleted_pair (E.finish_expunge store pair ~id
                  ~receipt:"targeted UID EXPUNGE and UID FETCH absent")

let reconcile_pair ?(min_absence_scans=0) ~(ctx:Ctx.t) ~writer ~cursor
    ~local_inventory ~(pair:J.pair) ~policy () =
  let store=ctx.store in
  let maildir=Maildir.of_writer writer in
  if min_absence_scans<0 then
    invalid_arg "Deletion.reconcile_pair: negative absence grace";
  let* pair=E.current_pair store pair in
  let* ()=if pair.scope=ctx.scope then Ok () else Error Stale_pair in
  let* epoch,uid,local_id=bound pair in
  match J.active_operation_for_pair store ~pair_id:pair.id with
  | Some op -> Error (Pending_operations [op.id])
  | None when J.has_open_conflict store ~pair
      ~kind:J.Content_conflict ||
      J.has_open_conflict store ~pair ~kind:J.Identity_conflict ->
      Ok survivor_changed
  | None ->
      let* remote_present=E.published_presence store ~cursor pair epoch uid in
      let local=Local_inventory.find local_inventory ~id:local_id in
      let last_presence side=J.last_presence_generation store
        ~pair_id:pair.id ~side in
      match plan ~policy ~min_absence_scans
          ~current_generation:cursor.generation ~last_presence
          ~remote_present ~local_present:(local<>None) pair,
        pair.content_sha256,pair.content_length,local with
      | No_deletion,_,_,_ -> Ok Unchanged
      | Hold_deletion why,_,_,_ -> Ok (Held why)
      | Delete_local,Some digest,Some length,Some local ->
          delete_local ~ctx ~writer ~local_inventory pair ~epoch ~uid ~local
            ~digest ~length
      | Delete_remote,Some digest,Some length,_ ->
          delete_remote ~ctx ~maildir pair ~epoch ~uid ~local_id ~digest
            ~length
      | (Delete_local | Delete_remote),_,_,_ -> Ok Unchanged

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
      let* pair=E.operation_pair store operation in
      let* epoch,uid,local_id,digest,length=E.content_identity pair in
      if not (E.same_identity operation pair ~epoch ~uid ~local_id ~digest
          ~length) then
        Error Stale_pair
      else
        let* remote_present=E.published_presence store ~cursor pair
          epoch uid in
        let local_present=Local_inventory.find local_inventory
          ~id:local_id<>None in
        let tombstoned=if kind=J.Delete then E.local_absence_recorded pair
          else E.remote_absence_proven pair in
        let* still_present=if remote_present || local_present ||
            not tombstoned then Ok true
          else E.present maildir ~id:local_id in
        match cursor.inventory_ref with
        | _ when still_present -> Error (Pending_operations [operation.id])
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
            deleted_pair (E.commit_tombstones store pair ~id:operation.id
              ~remote_tombstone ~local_tombstone)
