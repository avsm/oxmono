module J = Imap_store.Journal
module P = Imap.Proto
module F = Mail_flag.Imap_flag

type error =
  | Sync of Engine.error
  | Flag_sync of Flags.error
  | Delete_sync of Deletion.error
  | Writer_busy
  | Client of Imap_eio.Error.t
  | Pending_operations of string list
  | Bootstrap_requires_pairing
  | Uidvalidity_changed
  | Source_vanished of P.Uid.t
  | Local_source_changed of string
  | Stale_revision
  | Content_diverged of string
  | Flags_diverged of string
  | Date_diverged of string
  | Invalid_operation of string
  | Invalid_configuration of string

let pp_error ppf = function
  | Sync error -> Engine.pp_error ppf error
  | Flag_sync error -> Flags.pp_error ppf error
  | Delete_sync error -> Deletion.pp_error ppf error
  | Writer_busy -> Format.pp_print_string ppf "Maildir writer lease is busy"
  | Client error -> Imap_eio.Client.pp_error ppf error
  | Pending_operations ids ->
      Format.fprintf ppf "pending sync operations: %s" (String.concat ", " ids)
  | Bootstrap_requires_pairing ->
      Format.pp_print_string ppf "both endpoints contain unpaired messages"
  | Uidvalidity_changed ->
      Format.pp_print_string ppf "remote UIDVALIDITY changed since pairing"
  | Source_vanished uid ->
      Format.fprintf ppf "remote UID %Ld vanished before archival"
        (P.Uid.to_int64 uid)
  | Local_source_changed id ->
      Format.fprintf ppf "local occurrence %s changed before archival" id
  | Stale_revision -> Format.pp_print_string ppf "sync revision changed"
  | Content_diverged id -> Format.fprintf ppf "copied content diverged for %s" id
  | Flags_diverged id -> Format.fprintf ppf "copied flags diverged for %s" id
  | Date_diverged id -> Format.fprintf ppf "copied INTERNALDATE diverged for %s" id
  | Invalid_operation message -> Format.pp_print_string ppf message
  | Invalid_configuration message -> Format.pp_print_string ppf message

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

let ( let* ) result f = match result with Ok value -> f value | Error _ as e -> e
let network = function Ok value -> Ok value | Error error -> Error (Client error)
let sync = function
  | Ok value -> Ok value
  | Error Engine.Uidvalidity_changed -> Error Uidvalidity_changed
  | Error error -> Error (Sync error)

let with_lease maildir f =
  let entered=ref false in
  try Maildir.with_writer_lock maildir (fun () -> entered:=true; f ())
  with Maildir.Writer_lock_busy _ when not !entered -> Error Writer_busy

let local_date (local:Maildir.occurrence) =
  match Local_date.of_occurrence local with
  | Ok date -> Ok date
  | Error reason -> Error (Invalid_operation
      ("local occurrence " ^ local.id ^ ": " ^ reason))

let durable_flags = F.durable
let same_flags = F.equal_durable

let parse_flags raw =
  let rec parse acc = function
    | [] -> Ok (durable_flags (List.rev acc))
    | value :: rest ->
        (match F.of_wire value with
         | Error message -> Error (Client (Imap_eio.Error.Protocol message))
         | Ok flag -> parse (flag :: acc) rest) in
  parse [] raw

let appended_uid_missing uid =
  Invalid_operation (Printf.sprintf "APPENDUID target UID %Ld is missing"
    (P.Uid.to_int64 uid))

let remote_metadata ?(missing=fun uid -> Source_vanished uid) client ~mailbox
    ~uid ~uidvalidity ~internal_date =
  let raw_uid=P.Uid.to_int64 uid in
  let* row=match Imap_eio.Client.with_mailbox client ~mode:`Read_only mailbox
    (fun selected -> Ok (
      let* info=network (Imap_eio.Selected.info selected) in
      if info.uidvalidity<>P.Uidvalidity.to_int64 uidvalidity then
        Error Uidvalidity_changed
      else
        let* rows=network (Imap_eio.Selected.fetch_metadata_range selected
          ~first:raw_uid ~last:raw_uid ~modseq:false ~internal_date) in
        match rows with
        | row :: _ -> Ok row
        | [] -> Error (missing uid))) with
    | Error error -> Error (Client error)
    | Ok result -> result in
  let* flags=match row.flags with
    | Some flags -> parse_flags flags
    | None -> Error (Client (Imap_eio.Error.Protocol
        "message FETCH omitted FLAGS")) in
  Ok (flags,row.internal_date)

let remote_flags_and_date ?missing client ~mailbox ~uid ~uidvalidity =
  let* flags,date=remote_metadata ?missing client ~mailbox ~uid ~uidvalidity
    ~internal_date:true in
  match date with
  | Some date -> Ok (flags,date)
  | None -> Error (Client (Imap_eio.Error.Protocol
      "message FETCH omitted INTERNALDATE"))

let remote_date client ~mailbox ~uid ~uidvalidity =
  let* _,date=remote_flags_and_date client ~mailbox ~uid ~uidvalidity in
  Ok date

let pair ~id ~scope ~uidvalidity ~uid ~local_id ~sha256 ~length
    ?internal_date ~flags () : J.pair = {
  id;scope;remote_uidvalidity=Some uidvalidity;remote_uid=Some uid;
  local_id=Some local_id;content_sha256=Some sha256;
  content_length=Some length;internal_date;
  common_flags=durable_flags flags;
  remote_tombstone=None;local_tombstone=None;revision=0L}

let operation ~kind ~id ~scope ~local_id ~source_uidvalidity ~source_uid
    ~destination ~destination_uidvalidity ~blob ~flags : J.operation = {
  id;pair_id=None;local_id=Some local_id;scope;kind;
  state=J.Prepared;source_uidvalidity;source_uid;destination;
  destination_uidvalidity;blob_sha256=Some blob.Imap_store.Blob.sha256;
  blob_length=Some blob.length;desired_flags=Some (durable_flags flags);
  receipt=None;receipt_uidvalidity=None;receipt_uid=None}

let commit_pair store ~id pair =
  match J.commit_operation_with_pair store ~id
    ~expected_pair_revision:None pair with
  | `Committed _ -> Ok ()
  | `Stale_revision -> Error Stale_revision

let copy_remote_to_local ~client:remote_client ~store ~maildir
    ~local_inventory ~scope
    ~mailbox ~spool_dir ~next_id ~uidvalidity (row:Imap.Mirror.row) =
  let uid=row.uid in
  let* internal_date=remote_date remote_client ~mailbox ~uid ~uidvalidity in
  let spool=Eio.Path.(spool_dir / ("imap-" ^ Maildir.reserve_id ())) in
  let* blob=match Engine.archive_uid ~client:remote_client ~store ~scope
    ~mailbox ~uid ~spool () with
    | Error (Engine.Client (Imap_eio.Error.Missing_uid raw)) when
        raw=P.Uid.to_int64 uid -> Error (Source_vanished uid)
    | result -> sync result in
  let id=next_id () and local_id=Maildir.reserve_id () in
  let flags=durable_flags row.flags in
  let intent=operation ~kind:J.Local_append ~id ~scope ~local_id
    ~source_uidvalidity:(Some uidvalidity) ~source_uid:(Some uid)
    ~destination:None ~destination_uidvalidity:None ~blob ~flags in
  J.prepare_operation ~source_internal_date:internal_date store intent;
  let storable=match Maildir.check_append maildir ~flags () with
    | Ok () -> Local_date.to_mtime internal_date
    | Error _ as error -> error in
  match storable with
  | Error reason ->
      J.reject_prepared_operation store ~id
        ~receipt:("Maildir cannot store the message: " ^ reason);
      Error (Invalid_operation (Printf.sprintf
        "remote UID %Ld cannot be stored in Maildir: %s"
        (P.Uid.to_int64 uid) reason))
  | Ok mtime ->
  J.mark_sent store ~id;
  let local=Eio.Switch.run @@ fun sw ->
    let source=Imap_store.Blob.open_in store ~sw blob in
    Local_inventory.append ~inventory:local_inventory maildir
      ~id:local_id ~source
      ~length:blob.length ~flags ~mtime () in
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

let copy_local_to_remote ~client:remote_client ~store ~maildir
    ~local_inventory ~scope
    ~mailbox ~spool_dir ~next_id ~uidvalidity
    (local:Maildir.occurrence) =
  let* internal_date=local_date local in
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
  let intent=operation ~kind:J.Append ~id ~scope ~local_id:local.id
    ~source_uidvalidity:None ~source_uid:None
    ~destination:(Some scope) ~destination_uidvalidity:(Some uidvalidity)
    ~blob ~flags in
  J.prepare_operation ~local_source_mtime:local.mtime store intent;
  J.mark_sent store ~id;
  let flag_wires=List.map F.to_wire flags in
  match Engine.append_blob_journaled ~client:remote_client ~store
      ~scope ~mailbox ~id ~message_id:id ~flags:flag_wires
      ~internal_date blob with
  | Ok (Engine.Identified receipt) ->
      J.observe_operation store ~id ~receipt:"APPENDUID"
        ~destination_uidvalidity:(Some receipt.uidvalidity)
        ~destination_uid:(Some receipt.uid);
      let spool=Eio.Path.(spool_dir /
        ("imap-upload-verify-" ^ Maildir.reserve_id ())) in
      let* remote_blob=sync (Engine.fetch_uid_digest
        ~client:remote_client ~store ~scope ~mailbox
        ~uidvalidity:receipt.uidvalidity ~uid:receipt.uid ~spool ()) in
      let* ()=if remote_blob.length=blob.length &&
          remote_blob.sha256=blob.sha256 then Ok ()
        else Error (Content_diverged id) in
      let* observed,actual=remote_flags_and_date ~missing:appended_uid_missing
        remote_client ~mailbox ~uid:receipt.uid
        ~uidvalidity:receipt.uidvalidity in
      if not (same_flags flags observed) then Error (Flags_diverged id)
      else let* ()=
        if Imap.Internal_date.equal_instant internal_date actual then Ok ()
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
  | Ok Engine.Needs_reconciliation ->
      J.mark_ambiguous
        ~reason:"APPEND completed without an attributable APPENDUID"
        store ~id;
      Error (Pending_operations [id])
  | Error (Engine.Client (Imap_eio.Error.Rejected _) as error) ->
      J.reject_operation store ~id ~receipt:"APPEND rejected";
      Error (Sync error)
  | Error error ->
      (match Imap_store.find_intent store ~id with
       | None ->
           J.reject_operation store ~id
             ~receipt:"APPEND was not dispatched"
       | Some _ ->
           J.mark_ambiguous
             ~reason:"APPEND may have reached the server; verify before repair"
             store ~id);
      Error (Sync error)

let snapshot_has_uid store ~scope ~cursor target =
  match Imap_store.snapshot_contains_uid store ~scope ~cursor ~uid:target with
  | `Stale_revision -> Error Stale_revision
  | `Present present -> Ok present

let snapshot_row_for_uid store ~scope ~cursor target =
  let raw=P.Uid.to_int64 target in
  let after_uid=if raw=1L then None else
    match P.Uid.of_int64 (Int64.pred raw) with
    | Ok uid -> Some uid | Error _ -> assert false in
  match Imap_store.snapshot_page store ~scope ~cursor ?after_uid
    ~limit:1 () with
  | `Stale_revision -> Error Stale_revision
  | `Rows ((row:Imap.Mirror.row)::_) when
      P.Uid.to_int64 row.uid=raw -> Ok (Some row)
  | `Rows _ -> Ok None

let reconcile_local_append ~client ~mailbox ~store ~maildir ~scope
    ~(cursor:Imap.Mirror.cursor)
    (operation:J.operation) =
  match operation.kind,operation.state,operation.local_id,
        operation.source_uidvalidity,operation.source_uid,
        operation.blob_sha256,operation.blob_length,
        operation.desired_flags with
  | J.Local_append,(J.Sent|J.Ambiguous|J.Observed),
    Some local_id,Some uidvalidity,Some uid,Some sha256,
    Some length,Some flags when cursor.uidvalidity=Some uidvalidity ->
      (match Maildir.find maildir ~id:local_id with
       | None -> Ok ()
       | Some local ->
           let* present=snapshot_has_uid store ~scope ~cursor uid in
           if not present then Ok ()
           else if local.length<>length ||
              Maildir.sha256 maildir local<>sha256 then
             Error (Content_diverged operation.id)
           else if not (same_flags local.flags flags) then
             Error (Flags_diverged operation.id)
           else let* expected_date=match J.operation_source_date store
               ~id:operation.id with
             | Some expected ->
                 (match Local_date.of_occurrence local with
                  | Ok actual when Imap.Internal_date.equal_instant
                      expected actual -> Ok (Some expected)
                  | _ -> Error (Date_diverged operation.id))
             | None -> Result.map Option.some (local_date local) in
           let* ()=match expected_date with
             | None -> Ok ()
             | Some expected ->
                 let* actual=remote_date client ~mailbox ~uid ~uidvalidity in
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

let reconcile_remote_append ~client ~store ~maildir ~scope ~mailbox
    ~spool_dir ~(cursor:Imap.Mirror.cursor) (operation:J.operation) =
  match operation.kind,operation.state,operation.local_id,
        operation.blob_sha256,operation.blob_length,
        operation.desired_flags with
  | J.Append,(J.Sent|J.Ambiguous|J.Observed),Some local_id,
    Some sha256,Some length,Some flags ->
      let receipt=match operation.state,operation.receipt_uidvalidity,
        operation.receipt_uid with
        | J.Observed,Some epoch,Some uid -> Some (epoch,uid)
        | _ ->
            (match Imap_store.find_intent store ~id:operation.id with
             | Some {scope=legacy_scope;state=Imap_store.Confirmed;
                 kind=Imap_store.Append {content_digest;expected_length;
                   expected_flags=Some expected_flags;_};
                 uidvalidity=Some epoch;uid=Some uid;_}
               when legacy_scope=scope && content_digest=sha256 &&
                 expected_length=Some length &&
                 same_flags expected_flags flags -> Some (epoch,uid)
             | _ -> None) in
      (match receipt with
       | None -> Ok ()
       | Some (uidvalidity,_) when cursor.uidvalidity<>Some uidvalidity ->
           Ok ()
       | Some (uidvalidity,uid) ->
           let* present=snapshot_has_uid store ~scope ~cursor uid in
           if not present then Ok ()
           else
             (match Maildir.find maildir ~id:local_id with
              | None -> Ok ()
              | Some local ->
                  let* ()=match J.operation_source_mtime store
                      ~id:operation.id with
                    | Some mtime when mtime=local.mtime -> Ok ()
                    | Some _ -> Error (Local_source_changed local_id)
                    | None -> Error (Pending_operations [operation.id]) in
                  let* expected_date=match Imap_store.find_intent store
                      ~id:operation.id with
                    | Some {kind=Imap_store.Append
                        {expected_internal_date=Some raw;_};_} ->
                        (match Imap.Internal_date.of_string raw with
                         | Error _ -> Error (Invalid_operation
                             "APPEND journal contains an invalid INTERNALDATE")
                         | Ok intended ->
                             let* saved=local_date local in
                             if Imap.Internal_date.equal_instant saved
                                 intended then Ok (Some intended)
                             else Error (Date_diverged operation.id))
                    | _ -> Result.map Option.some (local_date local) in
                  if local.length<>length ||
                     Maildir.sha256 maildir local<>sha256 then
                    Error (Content_diverged operation.id)
                  else if not (same_flags flags local.flags) then
                    Error (Flags_diverged operation.id)
                  else
                    let spool=Eio.Path.(spool_dir /
                      ("imap-recover-" ^ Maildir.reserve_id ())) in
                    let* blob=sync (Engine.fetch_uid_digest ~client ~store
                      ~scope ~mailbox ~uidvalidity ~uid ~spool ()) in
                    if blob.length<>length || blob.sha256<>sha256 then
                      Error (Content_diverged operation.id)
                    else
                      let* observed,actual_date=remote_metadata
                        ~missing:appended_uid_missing client ~mailbox ~uid
                        ~uidvalidity ~internal_date:(Option.is_some expected_date) in
                      if not (same_flags flags observed) then
                        Error (Flags_diverged operation.id)
                      else let* ()=match expected_date,actual_date with
                        | Some expected,Some actual when not
                            (Imap.Internal_date.equal_instant expected
                              actual) ->
                            Error (Date_diverged operation.id)
                        | Some _,None -> Error (Client (Imap_eio.Error.Protocol
                            "message FETCH omitted INTERNALDATE"))
                        | _ -> Ok () in
                      (
                        if operation.state<>J.Observed then
                          J.observe_operation store ~id:operation.id
                            ~receipt:"APPENDUID"
                            ~destination_uidvalidity:(Some uidvalidity)
                            ~destination_uid:(Some uid);
                        commit_pair store ~id:operation.id
                          (pair ~id:operation.id ~scope ~uidvalidity ~uid
                            ~local_id ~sha256 ~length
                            ?internal_date:expected_date ~flags ()))))
  | _ -> Ok ()

let reject_unsent_copy ~store ~maildir (operation:J.operation) =
  match operation.kind,operation.state with
  | J.Append,(J.Sent | J.Ambiguous) ->
      let reject () =
        J.reject_operation store ~id:operation.id
          ~receipt:"no lower-layer APPEND dispatch occurred";
        Ok () in
      (match Imap_store.find_intent store ~id:operation.id with
       | None -> reject ()
       | Some {scope;kind=Imap_store.Append _;
               state=Imap_store.Prepared;_}
           when scope=operation.scope ->
           Imap_store.set_intent_state store ~id:operation.id
             Imap_store.Rejected;
           reject ()
       | Some {scope;kind=Imap_store.Append _;
               state=Imap_store.Rejected;_}
           when scope=operation.scope -> reject ()
       | _ -> Ok ())
  | (J.Append | J.Local_append),J.Prepared ->
      let local_exists=match operation.kind,operation.local_id with
        | J.Local_append,Some id ->
            Maildir.find maildir ~id<>None
        | _ -> false in
      if local_exists then Error (Pending_operations [operation.id])
      else
        (match Imap_store.find_intent store ~id:operation.id with
         | Some {state=(Imap_store.Sent | Imap_store.Ambiguous |
             Imap_store.Confirmed);_} ->
             Error (Pending_operations [operation.id])
         | Some {state=Imap_store.Prepared;_} ->
             Imap_store.set_intent_state store ~id:operation.id
               Imap_store.Rejected;
             J.reject_prepared_operation store ~id:operation.id
               ~receipt:"prepared copy was not dispatched";
             Ok ()
         | Some {state=Imap_store.Rejected;_} | None ->
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
                 | `Recorded -> Ok () | `Stale_revision -> Error Stale_revision)
            | _ -> Ok () in
          let* ()=match local_present,pair.local_tombstone with
            | true,Some {reason=J.Local_absence;_} ->
                (match J.note_presence store ~pair ~side:`Local
                  ~generation:cursor.generation with
                 | `Recorded -> Ok () | `Stale_revision -> Error Stale_revision)
            | _ -> Ok () in
          let* pair=match pair.local_tombstone,local_occurrence,
              pair.content_sha256,pair.content_length with
            | Some {reason=J.Local_absence;_},Some local,
              Some digest,Some length ->
                let verified=local.length=length &&
                  (match Local_inventory.with_unchanged_occurrence
                    ~inventory:local_inventory maildir local (fun () ->
                      Local_inventory.sha256 ~inventory:local_inventory
                        maildir local=digest) with
                   | Ok equal -> equal | Error `Changed -> false) in
                if verified then
                  let date_matches=match pair.internal_date with
                    | None -> true
                    | Some expected ->
                        (match Local_date.of_occurrence local with
                         | Ok actual -> Imap.Internal_date.equal_instant
                             expected actual
                         | Error _ -> false) in
                  if not date_matches then Ok pair
                  else (match J.reactivate_local store ~pair
                    ~generation:cursor.generation with
                   | `Reactivated updated -> Ok updated
                   | `Stale_revision -> Error Stale_revision)
                else
                  (match J.ensure_open_conflict store ~pair
                    ~kind:J.Content_conflict ~id:(next_id ())
                    ~evidence:"reappeared local occurrence differs from paired content" with
                   | `Open _ -> Ok pair
                   | `Stale_revision -> Error Stale_revision)
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
               | `Stale_revision -> Error Stale_revision
               | `Committed _ -> process rest))
          else process rest in
    process rows in
  pages None

let copy_once_unlocked ?(max_transfers=100) ?(min_absence_scans=0)
    ?(allow_bootstrap_duplicates=false)
    ?(deletion_policy=Imap.Sync_policy.Preserve)
    ~client:remote_client ~store ~maildir ~scope ~mailbox ~stage_id
    ~next_id ~spool_dir () =
  if max_transfers<1 || min_absence_scans<0 ||
      not (Eio.Path.is_directory spool_dir) then
    Error (Invalid_configuration
      "max_transfers must be positive, min_absence_scans nonnegative, and spool_dir must exist")
  else
    let prior_cursor=Imap_store.load_cursor store ~scope in
    let has_durable_identity=
      J.pairs_page store ~scope ~limit:1 ()<>[] ||
      J.active_operations_page store ~scope ~limit:1 ()<>[] in
    let expected_uidvalidity=if has_durable_identity then
      prior_cursor.uidvalidity else None in
    let* published=sync (Engine.run_once_staged ~client:remote_client
        ~store ~scope ~mailbox ~stage_id ?expected_uidvalidity ()) in
      let cursor=published.cursor in
      let rec reconcile_pages after =
        let page=J.active_operations_page store ~scope ?after
          ~limit:256 () in
        let rec reconcile = function
        | [] -> Ok ()
        | operation::rest ->
            let* ()=reject_unsent_copy ~store ~maildir operation in
            let* ()=reconcile_local_append ~client:remote_client ~mailbox
              ~store ~maildir
              ~scope ~cursor
              operation in
            let* ()=reconcile_remote_append ~client:remote_client ~store
              ~maildir ~scope ~mailbox ~spool_dir ~cursor operation in
            let* ()=if operation.kind<>J.Flags then Ok () else
              match Flags.recover_operation ~client:remote_client
                ~store ~maildir ~mailbox ~operation () with
              | Ok _ | Error (Flags.Pending_operation _) -> Ok ()
              | Error error -> Error (Flag_sync error) in
            reconcile rest in
        let* ()=reconcile page in
        if List.length page<256 then Ok ()
        else reconcile_pages (Some (List.hd (List.rev page)).id) in
      let* ()=reconcile_pages None in
      let* uidvalidity=match cursor.uidvalidity with
        | Some value -> Ok value
        | None -> Error (Invalid_configuration "published scan has no epoch") in
      Local_inventory.with_pages ~spool_dir maildir (fun local_inventory ->
        let rec recover_deletions = function
          | [] -> Ok ()
          | (operation:J.operation)::rest ->
              if operation.kind<>J.Delete &&
                 operation.kind<>J.Local_delete then
                recover_deletions rest
              else
                let* _ = match Deletion.recover_operation
                  ~store ~maildir ~cursor ~local_inventory ~operation () with
                | Ok outcome -> Ok outcome
                | Error (Deletion.Pending_operation _) ->
                    Ok Deletion.Unchanged
                | Error error -> Error (Delete_sync error) in
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
            | `Stale_revision -> Error Stale_revision in
          let hold_deleted_flag pair=match
            J.ensure_open_conflict store ~pair ~kind:J.Policy_conflict
              ~id:(next_id ())
              ~evidence:"\\Deleted differs from the paired flag baseline; propagation requires an explicit policy decision" with
            | `Open _ -> Ok ()
            | `Stale_revision -> Error Stale_revision in
          let resolve_deletion_hold pair=match
            J.resolve_open_conflicts store ~pair ~kind:J.Deletion_hold with
            | `Resolved _ -> Ok ()
            | `Stale_revision -> Error Stale_revision in
          let hold_deletion_evidence pair evidence=
            match J.ensure_open_conflict store ~pair
              ~kind:J.Deletion_hold ~id:(next_id ()) ~evidence with
            | `Open _ -> Ok ()
            | `Stale_revision -> Error Stale_revision in
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
            | `Stale_revision -> Error Stale_revision in
          let hold_pair (pair:J.pair)=
            incr flags_held;
            record_hold pair.id in
          let verify_pair_date (pair:J.pair) =
            let resolve ()=match J.resolve_open_conflicts store ~pair
                ~kind:J.Identity_conflict with
              | `Resolved _ -> Ok true
              | `Stale_revision -> Error Stale_revision in
            match pair.internal_date,pair.local_id with
            | Some expected,Some local_id ->
                (match Local_inventory.find local_inventory
                    ~id:local_id with
                 | None -> Ok true
                 | Some local ->
                     let observed=Local_date.of_occurrence local in
                     (match observed with
                      | Ok observed when
                          Imap.Internal_date.equal_instant expected observed ->
                          resolve ()
                      | _ ->
                          let evidence=match observed with
                            | Ok _ -> "local INTERNALDATE differs from paired baseline"
                            | Error reason -> "local INTERNALDATE unavailable: " ^ reason in
                          (match J.ensure_open_conflict store
                              ~pair ~kind:J.Identity_conflict
                              ~id:(next_id ()) ~evidence with
                            | `Open _ -> Ok false
                            | `Stale_revision -> Error Stale_revision)))
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
              | `Stale_revision -> Error Stale_revision
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
                             let* ()=copy_remote_to_local
                               ~client:remote_client ~store ~maildir
                               ~local_inventory ~scope
                               ~mailbox ~spool_dir ~next_id ~uidvalidity row in
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
                         let* ()=copy_local_to_remote
                           ~client:remote_client ~store ~maildir
                           ~local_inventory ~scope
                           ~mailbox ~spool_dir ~next_id ~uidvalidity local in
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
                      (match Flags.reconcile_pair
                        ~inventory:local_inventory
                        ~client:remote_client ~store ~maildir ~mailbox
                        ~pair ~next_id () with
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
                       | Error Flags.Modified ->
                           hold_pair pair;
                           process rest
                       | Error (Flags.Content_mismatch _) -> process rest
                       | Error Flags.Conditional_store_unavailable ->
                           let* ()=hold_flags pair
                             "remote flag change needs CONDSTORE and a \
                              message MODSEQ" in
                           process rest
                       | Error (Flags.Permanent_flag_unavailable flag) ->
                           let* ()=hold_flags pair (Format.asprintf
                             "remote flag %a is not permanently writable"
                             F.pp flag) in
                           process rest
                       | Error (Flags.Pending_operation id) ->
                           Error (Pending_operations [id])
                       | Error error -> Error (Flag_sync error)) in
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
                  let matches=match pair.local_id,
                      pair.content_sha256,pair.content_length with
                    | Some local_id,Some digest,Some length ->
                        (match Local_inventory.find local_inventory
                            ~id:local_id with
                         | Some local when local.length=length ->
                             (match Local_inventory.with_unchanged_occurrence
                               ~inventory:local_inventory maildir local
                               (fun () -> Local_inventory.sha256
                                 ~inventory:local_inventory maildir local=digest) with
                              | Ok equal -> equal | Error `Changed -> false)
                         | _ -> false)
                    | _ -> false in
                  if matches then (
                    let* ()=match J.resolve_open_conflicts store ~pair
                        ~kind:J.Content_conflict with
                      | `Resolved _ -> Ok ()
                      | `Stale_revision -> Error Stale_revision in
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
                    (match Deletion.reconcile_pair
                      ~min_absence_scans ~client:remote_client ~store
                      ~maildir ~mailbox
                      ~cursor ~local_inventory ~pair ~policy:deletion_policy
                      ~next_id ~spool_dir () with
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
                     | Error (Deletion.Unsupported reason) ->
                         let* ()=hold_deletion_evidence pair
                           ("targeted deletion unavailable: " ^ reason) in
                         incr deletions_held;
                         record_hold pair.id;
                         process rest
                     | Error error -> Error (Delete_sync error)) in
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

let copy_once ?max_transfers ?min_absence_scans
    ?allow_bootstrap_duplicates ?deletion_policy
    ~client ~store ~maildir ~scope ~mailbox ~stage_id ~next_id
    ~spool_dir () =
  with_lease maildir (fun () ->
    copy_once_unlocked ?max_transfers ?min_absence_scans
      ?allow_bootstrap_duplicates
      ?deletion_policy
      ~client ~store ~maildir ~scope ~mailbox ~stage_id ~next_id
      ~spool_dir ())

let recover_local ~maildir ~spool_dir () =
  with_lease maildir (fun () ->
    ignore (Maildir.recover maildir : Maildir.recovery);
    ignore (Local_inventory.recover spool_dir : string list);
    Ok ())

type local_verification = {
  checked : int64;
  mismatched : int64;
  restored : int64;
  missing : int64;
  unverified : int64;
}

let spool_missing spool_dir =
  if Eio.Path.is_directory spool_dir then None
  else Some (Error (Invalid_configuration "spool_dir must exist"))

let verify_local_content ~store ~maildir ~scope ~next_id ~spool_dir
    ~on_issue () =
  match spool_missing spool_dir with
  | Some error -> error
  | None ->
  with_lease maildir (fun () ->
    Local_inventory.with_pages ~spool_dir maildir (fun inventory ->
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
                | None,Some local_id,Some digest,Some length ->
                    (match Local_inventory.find inventory
                        ~id:local_id with
                     | None ->
                         bump missing;
                         on_issue pair.id "paired local occurrence is absent";
                         Ok ()
                     | Some local ->
                         bump checked;
                         let verified=if local.length<>length then
                           Ok false
                         else match Local_inventory.with_unchanged_occurrence
                             ~inventory maildir local (fun () ->
                               Local_inventory.sha256 ~inventory
                                 maildir local=digest) with
                           | Ok equal -> Ok equal
                           | Error `Changed -> Error `Changed in
                         (match verified with
                          | Ok true ->
                              if J.has_open_conflict store ~pair
                                  ~kind:J.Content_conflict then
                                (match J.resolve_open_conflicts store ~pair
                                    ~kind:J.Content_conflict with
                                 | `Resolved _ -> bump restored; Ok ()
                                 | `Stale_revision -> Error Stale_revision)
                              else Ok ()
                          | Ok false | Error `Changed ->
                              let evidence=match verified with
                                | Error `Changed ->
                                    "local occurrence changed during content verification"
                                | Ok _ ->
                                    "local message content differs from the paired digest" in
                              (match J.ensure_open_conflict store ~pair
                                  ~kind:J.Content_conflict ~id:(next_id ())
                                  ~evidence with
                               | `Open _ ->
                                   bump mismatched;
                                   on_issue pair.id evidence;
                                   Ok ()
                               | `Stale_revision -> Error Stale_revision)))
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

let mark_local_retention ~store ~maildir ~scope ~pair_id ~evidence
    ~spool_dir () =
  let printable=String.for_all (fun c ->
    let n=Char.code c in n>=32 && n<>127) evidence in
  if pair_id="" || String.trim evidence="" ||
     String.length evidence>1024 || not printable then
    Error (Invalid_configuration "retention requires a pair ID and 1..1024 printable evidence bytes")
  else match spool_missing spool_dir with
  | Some error -> error
  | None -> with_lease maildir (fun () ->
    Local_inventory.with_pages ~spool_dir maildir (fun inventory ->
      match J.find_pair store ~id:pair_id with
      | None -> Error (Invalid_operation "retention pair does not exist")
      | Some pair when pair.scope<>scope ->
          Error (Invalid_operation "retention pair belongs to another mailbox")
      | Some pair ->
          match pair.remote_uidvalidity,pair.remote_uid,pair.local_id with
          | Some _,Some _,Some local_id when
              pair.remote_tombstone=None ->
              if J.active_operation_for_pair store ~pair_id<>None then
                Error (Invalid_operation "retention pair has a pending operation")
              else if Local_inventory.find inventory ~id:local_id<>None then
                Error (Invalid_operation "retention local occurrence is present")
              else (match pair.local_tombstone with
                | Some {reason=J.Explicit_delete;_} ->
                    Error (Invalid_operation "local occurrence was explicitly deleted")
                | None | Some {reason=J.Local_absence;_} |
                  Some {reason=J.Retention;_} ->
                    let local_tombstone=Some {J.reason=J.Retention;
                      evidence;generation=None} in
                    (match J.put_pair store
                      ~expected_revision:(Some pair.revision)
                      {pair with local_tombstone} with
                     | `Committed _ -> Ok ()
                     | `Stale_revision -> Error Stale_revision)
                | Some _ ->
                    Error (Invalid_operation "invalid local tombstone"))
          | _ -> Error (Invalid_operation
              "retention requires an active paired remote binding")))

type deletion_preview = {
  pair_id : string;
  remote_uid : P.Uid.t;
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
        (match remote_present with
         | None -> `Stale_epoch
         | Some remote_present ->
             let first_generation=if remote_present then
               Option.bind pair.local_tombstone (fun x -> x.generation)
               else Option.bind pair.remote_tombstone
                 (fun x -> x.generation) in
             let last_present_generation=J.last_presence_generation store
               ~pair_id:pair.id
               ~side:(if remote_present then `Local else `Remote) in
             let absence_mature=Imap.Sync_policy.absence_mature
               ~last_present_generation ~current_generation:cursor_generation
               ~first_generation
               ~min_scans:min_absence_scans in
             let plan=Imap.Sync_policy.plan_disappearance_with_grace
               ~absence_mature
               ~policy ~paired:true ~remote_present ~remote_complete:true
               ~local_present ~local_complete:true
               ~local_retained:(match pair.local_tombstone with
                 | Some {reason=J.Retention;_} -> true | _ -> false)
               ~survivor_unchanged:true in
             let plan=match plan with
               | (Imap.Sync_policy.Delete_local |
                  Imap.Sync_policy.Delete_remote) when
                   pair.content_sha256=None || pair.content_length=None ->
                   Imap.Sync_policy.Hold_deletion
                     Imap.Sync_policy.Missing_content_evidence
               | Imap.Sync_policy.Delete_local ->
                   (match pair.remote_tombstone with
                    | Some {reason=J.Inventory_absence;
                        generation=Some _;_} -> plan
                    | _ -> Imap.Sync_policy.Hold_deletion
                        Imap.Sync_policy.Unverified_absence)
               | Imap.Sync_policy.Delete_remote ->
                   (match pair.local_tombstone with
                    | Some {reason=J.Local_absence;_} -> plan
                    | _ -> Imap.Sync_policy.Hold_deletion
                        Imap.Sync_policy.Unverified_absence)
               | _ -> plan in
             `Plan plan) in
  {pair_id=pair.id;remote_uid=uid;local_id;
   remote_present;local_present;decision}

let preview_deletions ?(min_absence_scans=0) ~store ~maildir ~scope
    ~policy ~spool_dir ~on_preview () =
  if min_absence_scans<0 then
    Error (Invalid_configuration "min_absence_scans must be nonnegative")
  else match spool_missing spool_dir with
  | Some error -> error
  | None -> with_lease maildir (fun () ->
    let cursor=Imap_store.load_cursor store ~scope in
    match cursor.phase,cursor.uidvalidity,cursor.inventory_ref with
    | Imap.Mirror.Live,Some current_epoch,Some _ ->
      Local_inventory.with_pages ~spool_dir maildir (fun inventory ->
        let rec pages after =
          let rows=J.pairs_page store ~scope ?after ~limit:1000 () in
          let rec process = function
            | [] ->
                if List.length rows<1000 then Ok cursor
                else pages (Some (List.hd (List.rev rows)).id)
            | (pair:J.pair)::rest ->
                (match pair.remote_uidvalidity,pair.remote_uid,pair.local_id with
                 | Some epoch,Some uid,Some local_id ->
                     let local_present=Local_inventory.find inventory
                       ~id:local_id<>None in
                     let remote_presence=if epoch<>current_epoch then
                       Ok None
                       else match Imap_store.snapshot_contains_uid store
                         ~scope ~cursor ~uid with
                         | `Stale_revision -> Error Stale_revision
                         | `Present present -> Ok (Some present) in
                     let* remote_present=remote_presence in
                     let one_sided=match remote_present with
                       | Some present -> present<>local_present
                       | None -> local_present in
                     if one_sided then on_preview
                       (deletion_preview_of ~store ~policy
                         ~min_absence_scans
                         ~cursor_generation:cursor.generation pair ~uid
                         ~local_id ~remote_present ~local_present);
                     process rest
                 | _ -> process rest) in
          process rows in
        pages None)
    | _ -> Error (Invalid_configuration
        "deletion preview requires a complete published remote inventory"))

type sync_preview =
  | Preview_pending of string
  | Preview_bootstrap_hold
  | Preview_copy_remote of P.Uid.t
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
  else match spool_missing spool_dir with
  | Some error -> error
  | None -> with_lease maildir (fun () ->
    let cursor=Imap_store.load_cursor store ~scope in
    match cursor.phase,cursor.uidvalidity,cursor.inventory_ref with
    | Imap.Mirror.Live,Some current_epoch,Some _ ->
      Local_inventory.with_pages ~spool_dir maildir (fun inventory ->
        let pending=J.active_operations_page store ~scope ~limit:1 () in
        if pending<>[] then (
          on_preview (Preview_pending (List.hd pending).id);
          Ok cursor)
        else
          let had_pairs=J.pairs_page store ~scope ~limit:1 ()<>[] in
          let* has_remote=match Imap_store.snapshot_page store ~scope
              ~cursor ~limit:1 () with
            | `Stale_revision -> Error Stale_revision
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
              | `Stale_revision -> Error Stale_revision
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
                          else snapshot_row_for_uid store ~scope
                            ~cursor uid in
                        let remote_present=if epoch<>current_epoch then
                            None else Some (remote<>None) in
                        let local_present=local<>None in
                        (match remote,local with
                         | Some remote,Some local when
                             pair.remote_tombstone=None &&
                             pair.local_tombstone=None ->
                             let date_matches=match pair.internal_date with
                               | None -> true
                               | Some expected ->
                                   (match Local_date.of_occurrence
                                     local with
                                    | Ok actual ->
                                        Imap.Internal_date.equal_instant
                                          expected actual
                                    | Error _ -> false) in
                             if not date_matches then
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
                         | None,None when remote_present=Some false -> Ok ()
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
              Error Stale_revision else Ok cursor)
    | _ -> Error (Invalid_configuration
        "sync preview requires a complete published remote inventory"))

let repair_local_append ~client ~store ~maildir ~scope ~mailbox ~id
    ~evidence ~spool_dir () =
  let safe_evidence=String.for_all (fun c ->
    let n=Char.code c in n>=32 && n<>127) evidence in
  if id="" || String.trim evidence="" || String.length evidence>1024 ||
     not safe_evidence then
    Error (Invalid_operation
      "repair evidence must be 1..1024 printable bytes")
  else
    with_lease maildir (fun () ->
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
      let* ()=if Maildir.find maildir ~id:local_id=None then Ok ()
        else Error (Invalid_operation
          "reserved Maildir occurrence already exists; run sync to reconcile it") in
      let* ()=if J.find_remote store ~scope ~uidvalidity ~uid=None &&
          J.find_local store ~scope ~local_id=None then Ok ()
        else Error (Invalid_operation
          "source UID or reserved Maildir ID is already paired") in
      let* ()=sync (Engine.guard_bound_mailbox ~client ~store ~scope
        ~mailbox) in
      let cursor=Imap_store.load_cursor store ~scope in
      let* ()=if cursor.uidvalidity=Some uidvalidity then Ok ()
        else Error Uidvalidity_changed in
      let* present=snapshot_has_uid store ~scope ~cursor uid in
      let* ()=if present then Ok () else Error (Source_vanished uid) in
      let* actual_flags,internal_date=remote_flags_and_date client ~mailbox ~uid ~uidvalidity in
      let* ()=if same_flags flags actual_flags then Ok ()
        else Error (Flags_diverged id) in
      let* internal_date=match J.operation_source_date store ~id with
        | None -> Ok internal_date
        | Some saved when Imap.Internal_date.equal_instant
            saved internal_date -> Ok saved
        | Some _ -> Error (Date_diverged id) in
      let spool=Eio.Path.(spool_dir /
        ("imap-repair-" ^ Maildir.reserve_id ())) in
      let* blob=match Engine.archive_uid ~client ~store ~scope
        ~mailbox ~uid ~spool () with
        | Error (Engine.Client (Imap_eio.Error.Missing_uid raw))
          when raw=P.Uid.to_int64 uid -> Error (Source_vanished uid)
        | result -> sync result in
      let* ()=if blob.length=length && blob.sha256=sha256 then Ok ()
        else Error (Content_diverged id) in
      let* current_flags,current_date=remote_flags_and_date client ~mailbox ~uid ~uidvalidity in
      let* ()=if same_flags flags current_flags then Ok ()
        else Error (Flags_diverged id) in
      let* ()=if Imap.Internal_date.equal_instant internal_date current_date
        then Ok () else Error (Date_diverged id) in
      let* ()=if Imap_store.load_cursor store ~scope=cursor then Ok ()
        else Error Stale_revision in
      let storable=match Maildir.check_append maildir ~flags () with
        | Ok () -> Local_date.to_mtime internal_date
        | Error _ as error -> error in
      let* mtime=match storable with
        | Ok mtime -> Ok mtime
        | Error reason -> Error (Invalid_operation
            ("Maildir cannot store the message: " ^ reason)) in
      let local=Eio.Switch.run @@ fun sw ->
        let source=Imap_store.Blob.open_in store ~sw blob in
        Maildir.append maildir ~id:local_id ~source ~length
          ~flags ~mtime () in
      if local.length<>length || Maildir.sha256 maildir local<>sha256
         then Error (Content_diverged id)
      else if not (same_flags flags local.flags) then Error (Flags_diverged id)
      else (
        J.observe_operation store ~id
          ~receipt:("operator local append repair: " ^ evidence)
          ~destination_uidvalidity:None ~destination_uid:None;
        commit_pair store ~id
          (pair ~id ~scope ~uidvalidity ~uid ~local_id ~sha256 ~length
            ~internal_date ~flags ())))

let record_appenduid_evidence ~store ~maildir ~scope ~id
    ~uidvalidity ~uid ~evidence () =
  let safe_evidence=String.for_all (fun c ->
    let n=Char.code c in n>=32 && n<>127) evidence in
  if id="" || String.trim evidence="" || String.length evidence>1024 ||
     not safe_evidence then
    Error (Invalid_operation
      "APPENDUID evidence must be 1..1024 printable bytes")
  else
    with_lease maildir (fun () ->
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
            let matches=match intent.kind with
              | Imap_store.Append metadata ->
                  intent.scope=scope &&
                  intent.uidvalidity=Some uidvalidity &&
                  Some metadata.content_digest=operation.blob_sha256 &&
                  metadata.expected_length=operation.blob_length &&
                  (match metadata.expected_flags,operation.desired_flags with
                   | Some expected,Some desired -> same_flags expected desired
                   | _ -> false)
              | Imap_store.Other _ -> false in
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

type append_candidates = {
  uidvalidity : P.Uidvalidity.t;
  inspected_uids : int;
  matching_uids : P.Uid.t list;
}

let inspect_append_candidates ?(max_uids=1000)
    ?(max_body_bytes=1_073_741_824L) ~client ~store ~scope
    ~mailbox ~id ~spool_dir () =
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
        Some {scope=intent_scope;state=(Imap_store.Sent |
            Imap_store.Ambiguous);uidvalidity=Some intent_epoch;
            kind=Imap_store.Append metadata;_}
        when intent_scope=scope && epoch=intent_epoch &&
          metadata.content_digest=digest &&
          metadata.expected_length=Some length &&
          Option.fold ~none:false
            ~some:(same_flags expected_flags)
            metadata.expected_flags ->
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
    let* ()=sync (Engine.validate_scope ~client ~scope ~mailbox) in
    let* ()=sync (Engine.guard_bound_mailbox ~client ~store ~scope
      ~mailbox) in
    let inspect selected =
      let* info=network (Imap_eio.Selected.info selected) in
      if info.uidvalidity<>P.Uidvalidity.to_int64 epoch then
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
                let* rows=network
                  (Imap_eio.Selected.fetch_metadata_range selected
                    ~first ~last ~modseq:false ~size:true
                    ~internal_date:(Option.is_some expected_date)) in
                let rec check matches = function
                  | [] -> scan (Int64.succ last) matches
                  | (row:Imap.Response.fetch)::rest ->
                      (match row.uid,row.flags,row.size with
                       | Some raw_uid,Some raw_flags,Some row_size when
                           raw_uid>=first && raw_uid<=last ->
                           let* row_flags=parse_flags raw_flags in
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
                                   ~uid:raw_uid ~max_bytes:length sink in
                                 match fetched with
                                 | Error error -> Error (Client error)
                                 | Ok () ->
                                     let found_length,found_digest=
                                       Spool.hash_file spool in
                                     Ok (found_length=length &&
                                       found_digest=digest)) in
                             let* matches_body=fetched in
                             let* matches=if not matches_body then Ok matches
                               else match P.Uid.of_int64 raw_uid with
                                 | Ok uid -> Ok (uid::matches)
                                 | Error message -> Error
                                     (Invalid_operation message) in
                             check matches rest
                       | _ -> Error (Client (Imap_eio.Error.Protocol
                           "candidate FETCH omitted UID, FLAGS or RFC822.SIZE"))) in
                check matches rows in
            let* matching_uids=scan (Int64.succ frontier) [] in
            Ok {uidvalidity=epoch;inspected_uids=Int64.to_int span;
              matching_uids} in
    match Imap_eio.Client.with_mailbox client ~mode:`Read_only mailbox
      (fun selected -> Ok (inspect selected)) with
    | Error error -> Error (Client error)
    | Ok result -> result
