module J = Imap_store.Journal
module E = Pair_evidence

open Error

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as e -> e

type deletion_preview = {
  pair_id : string;
  remote_uid : Imap.Uid.t;
  local_id : string;
  remote_present : bool option;
  local_present : bool;
  decision : [ `Pending of string | `Stale_epoch |
    `Plan of Imap.Sync_policy.deletion_plan ];
}

let deletion_preview_of ~store ~policy ~min_absence_scans
    ~cursor_generation (pair:J.pair) ~uid ~local_id
    ~remote_present ~local_present =
  let decision=match J.active_operation_for_pair store ~pair_id:pair.id with
    | Some operation -> `Pending operation.id
    | None when J.has_open_conflict store ~pair
        ~kind:J.Content_conflict ||
        J.has_open_conflict store ~pair ~kind:J.Identity_conflict ->
        `Plan (Imap.Sync_policy.Hold_deletion Imap.Sync_policy.Survivor_changed)
    | None ->
        match remote_present with
        | None -> `Stale_epoch
        | Some remote_present ->
            let last_presence side=J.last_presence_generation store
              ~pair_id:pair.id ~side in
            `Plan (Deletion.plan ~policy ~min_absence_scans
              ~current_generation:cursor_generation ~last_presence
              ~remote_present ~local_present pair) in
  {pair_id=pair.id;remote_uid=uid;local_id;
   remote_present;local_present;decision}

type sync_preview =
  | Preview_pending of string
  | Preview_bootstrap_hold
  | Preview_copy_remote of Imap.Uid.t
  | Preview_copy_local of string
  | Preview_flags of {
      pair_id : string;
      to_remote : Imap.Sync_policy.flag_delta;
      to_local : Imap.Sync_policy.flag_delta;
    }
  | Preview_pair_hold of string * string
  | Preview_deletion of deletion_preview

let preview_sync ?(allow_bootstrap_duplicates=false)
    ?(min_absence_scans=0) ~store ~maildir
    ~scope ~policy ~spool_dir ~on_preview () =
  if min_absence_scans<0 then
    Error (Invalid_configuration "min_absence_scans must be nonnegative")
  else
    let* ()=E.require_spool_dir spool_dir in
    E.with_lease maildir (fun _ ->
    let cursor=Imap_store.load_cursor store ~scope in
    match cursor.phase,cursor.uidvalidity,cursor.inventory_ref with
    | Imap.Mirror.Live,Some current_epoch,Some _ ->
      E.with_inventory ~spool_dir maildir (fun inventory ->
        let pending=J.active_operations_page store ~scope ~limit:1 () in
        if pending<>[] then (
          on_preview (Preview_pending (List.hd pending).id);
          Ok cursor)
        else
          let had_pairs=J.pairs_page store ~scope ~limit:1 ()<>[] in
          let* has_remote=match Imap_store.snapshot_page store ~scope
              ~cursor ~limit:1 () with
            | `Stale_revision -> Error Store_stale_revision
            | `Rows rows -> Ok (rows<>[]) in
          if not had_pairs && has_remote &&
             Local_inventory.count inventory>0L &&
             not allow_bootstrap_duplicates then (
            on_preview Preview_bootstrap_hold;
            Ok cursor)
          else
            let rec remote_pages after_uid =
              match Imap_store.snapshot_page store ~scope ~cursor
                  ?after_uid ~limit:1000 () with
              | `Stale_revision -> Error Store_stale_revision
              | `Rows [] -> Ok ()
              | `Rows rows ->
                  List.iter (fun (row:Imap.Mirror.row) ->
                    if J.find_remote store ~scope
                        ~uidvalidity:current_epoch ~uid:row.uid=None then
                      on_preview (Preview_copy_remote row.uid)) rows;
                  remote_pages (Some (List.hd (List.rev rows)).uid) in
            let* ()=remote_pages None in
            let rec local_pages after =
              let page=Local_inventory.page inventory ?after
                ~limit:1000 () in
              List.iter (fun (local:Maildir.occurrence) ->
                if J.find_local store ~scope ~local_id:local.id=None then
                  on_preview (Preview_copy_local local.id))
                page.occurrences;
              match page.next_after with
              | None -> Ok ()
              | Some after -> local_pages (Some after) in
            let* ()=local_pages None in
            let rec pair_pages after =
              let rows=J.pairs_page store ~scope ?after ~limit:1000 () in
              let rec process = function
                | [] ->
                    if List.length rows<1000 then Ok ()
                    else pair_pages (Some (List.hd (List.rev rows)).id)
                | (pair:J.pair)::rest ->
                    let* ()=match pair.remote_uidvalidity,pair.remote_uid,
                      pair.local_id with
                    | Some epoch,Some uid,Some local_id ->
                        let local=Local_inventory.find inventory
                          ~id:local_id in
                        let* remote=if epoch<>current_epoch then Ok None
                          else E.snapshot_row store ~scope ~cursor uid in
                        let remote_present=if epoch<>current_epoch then
                            None else Some (remote<>None) in
                        let local_present=local<>None in
                        (match remote,local with
                         | Some remote,Some local when
                             pair.remote_tombstone=None &&
                             pair.local_tombstone=None ->
                             if not (E.local_date_matches pair local) then
                               on_preview (Preview_pair_hold (pair.id,
                                 "local INTERNALDATE differs from paired baseline"))
                             else (
                               let flags=Imap.Sync_policy.reconcile_flags
                                 ~base:pair.common_flags
                                 ~remote:remote.flags ~local:local.flags () in
                               if flags.deleted_held then
                                 on_preview (Preview_pair_hold (pair.id,
                                   "\\Deleted differs from paired baseline"));
                               let nonempty
                                   (delta:Imap.Sync_policy.flag_delta) =
                                 delta.add<>[] || delta.remove<>[] in
                               if (nonempty flags.to_remote ||
                                   nonempty flags.to_local) &&
                                  not (J.has_open_conflict store ~pair
                                    ~kind:J.Content_conflict) then
                                 on_preview (Preview_flags {
                                   pair_id=pair.id;
                                   to_remote=flags.to_remote;
                                   to_local=flags.to_local}));
                             Ok ()
                         | Some _,Some _ ->
                             on_preview (Preview_pair_hold (pair.id,
                               "tombstoned pair has both endpoints present"));
                             Ok ()
                         | None,None -> Ok ()
                         | _ ->
                             on_preview (Preview_deletion
                               (deletion_preview_of ~store ~policy
                                 ~min_absence_scans
                                 ~cursor_generation:cursor.generation pair
                                 ~uid ~local_id ~remote_present
                                 ~local_present));
                             Ok ())
                    | _ ->
                        on_preview (Preview_pair_hold (pair.id,
                          "paired occurrence identity is incomplete"));
                        Ok () in
                    process rest in
              process rows in
            let* ()=pair_pages None in
            let rec content_conflict_pages after =
              let page=J.open_conflicts_page store ~scope ?after
                ~limit:1000 () in
              List.iter (fun (conflict:J.conflict) ->
                if conflict.kind=J.Content_conflict then
                  on_preview (Preview_pair_hold (conflict.pair_id,
                    "saved local content conflict; restore paired bytes")))
                page;
              if List.length page<1000 then Ok ()
              else content_conflict_pages
                (Some (List.hd (List.rev page)).id) in
            let* ()=content_conflict_pages None in
            if Imap_store.load_cursor store ~scope<>cursor then
              Error Store_stale_revision else Ok cursor)
    | _ -> Error (Invalid_configuration
        "sync preview requires a complete published remote inventory"))
