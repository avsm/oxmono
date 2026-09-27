module J = Imap_store.Journal
module F = Mail_flag.Imap_flag

open Error

let ( let* ) result f = match result with Ok x -> f x | Error _ as e -> e
let network = function Ok x -> Ok x | Error e -> Error (Client e)

let check_evidence evidence =
  if String.trim evidence<>"" && String.length evidence<=1024 &&
     String.for_all (fun c -> let n=Char.code c in n>=32 && n<>127) evidence
  then Ok ()
  else Error (Invalid_configuration
    "operator evidence must be 1..1024 printable bytes")

let require_spool_dir spool_dir =
  if Eio.Path.is_directory spool_dir then Ok ()
  else Error (Invalid_configuration "spool_dir must exist")

let with_lease maildir f =
  let entered=ref false in
  try Maildir.with_writer maildir (fun writer -> entered:=true; f writer)
  with Maildir.Writer_lock_busy _ when not !entered -> Error Writer_busy

let maildir_result = function Ok v -> Ok v | Error e -> Error (Maildir e)
let find maildir ~id = maildir_result (Maildir.find maildir ~id)
let present maildir ~id =
  let* found=find maildir ~id in
  Ok (Option.is_some found)

let with_inventory ~spool_dir maildir f =
  match Local_inventory.with_pages ~spool_dir maildir f with
  | Ok result -> result
  | Error e -> Error (Maildir e)

let storable writer ~flags internal_date =
  match Maildir.check_append writer ~flags () with
  | Ok () -> Local_date.to_mtime internal_date
  | Error e -> Error (Format.asprintf "%a" Maildir.pp_error e)

let local_date (local:Maildir.occurrence) =
  match Local_date.of_occurrence local with
  | Ok date -> Ok date
  | Error reason -> Error (Invalid_operation
      ("local occurrence " ^ local.id ^ ": " ^ reason))

let unchanged ?inventory maildir occurrence =
  match Local_inventory.with_unchanged_occurrence ?inventory maildir
      occurrence ignore with
  | Ok () -> true
  | Error `Changed -> false

(* [`Changed] means the observation is stale, not that the bytes differ. *)
let local_content ?inventory maildir (pair:J.pair)
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

let local_date_matches (pair:J.pair) occurrence =
  match pair.internal_date with
  | None -> true
  | Some expected ->
      (match Local_date.of_occurrence occurrence with
       | Ok actual -> Imap.Internal_date.equal_instant expected actual
       | Error _ -> false)

let current_pair store (pair:J.pair) =
  match J.find_pair store ~id:pair.id with
  | None -> Error Missing_pair
  | Some current when current<>pair -> Error Stale_pair
  | Some current -> Ok current

let remote_absence_proven (pair:J.pair) =
  match pair.remote_tombstone with
  | Some {reason=J.Inventory_absence;generation=Some _;_} -> true
  | _ -> false

let local_absence_recorded (pair:J.pair) =
  match pair.local_tombstone with
  | Some {reason=J.Local_absence;_} -> true
  | _ -> false

let commit_tombstones store (pair:J.pair) ~id ~remote_tombstone
    ~local_tombstone =
  let next={pair with remote_tombstone;local_tombstone} in
  match J.commit_operation_with_pair store ~id
      ~expected_pair_revision:(Some pair.revision) next with
  | `Committed pair -> Ok pair
  | `Stale_revision -> Error Stale_pair

let observe store ~id receipt =
  J.observe_operation store ~id ~receipt ~destination_uidvalidity:None
    ~destination_uid:None

let finish_unlink store (pair:J.pair) ~id ~receipt =
  observe store ~id receipt;
  commit_tombstones store pair ~id ~remote_tombstone:pair.remote_tombstone
    ~local_tombstone:(Some {J.reason=J.Explicit_delete;evidence=id;
      generation=None})

let finish_expunge store (pair:J.pair) ~id ~receipt =
  observe store ~id receipt;
  commit_tombstones store pair ~id
    ~remote_tombstone:(Some {J.reason=J.Expunge_receipt;evidence=id;
      generation=None})
    ~local_tombstone:pair.local_tombstone

let truncate text =
  if String.length text<=512 then text else String.sub text 0 512

let describe error =
  truncate (Format.asprintf "%a" Imap_eio.Client.pp_error error)

let describe_error error = truncate (Error.to_string error)

let live_target (pair:J.pair) =
  match pair.remote_uidvalidity,pair.remote_uid,pair.local_id,
        pair.remote_tombstone,pair.local_tombstone with
  | Some epoch,Some uid,Some local_id,None,None -> Ok (epoch,uid,local_id)
  | _ -> Error Missing_occurrence

let occurrence ?inventory maildir id =
  let found=match inventory with
    | Some inventory -> Ok (Local_inventory.find inventory ~id)
    | None -> Maildir.find maildir ~id in
  match found with
  | Error error -> Error (Maildir error)
  | Ok None -> Error Missing_occurrence
  | Ok (Some occurrence) -> Ok occurrence

let journaled_at store (operation:J.operation) (pair:J.pair) =
  J.operation_pair_revision store ~id:operation.id=Some pair.revision

let content_identity (pair:J.pair) =
  match pair.remote_uidvalidity,pair.remote_uid,pair.local_id,
        pair.content_sha256,pair.content_length with
  | Some epoch,Some uid,Some local_id,Some digest,Some length ->
      Ok (epoch,uid,local_id,digest,length)
  | _ -> Error Identity_changed

let same_target (operation:J.operation) ~epoch ~uid ~local_id =
  operation.source_uidvalidity=Some epoch &&
  operation.source_uid=Some uid &&
  operation.local_id=Some local_id

let same_identity (operation:J.operation) (pair:J.pair) ~epoch ~uid
    ~local_id ~digest ~length =
  same_target operation ~epoch ~uid ~local_id &&
  operation.blob_sha256=Some digest &&
  operation.blob_length=Some length &&
  Option.fold ~none:false ~some:(F.equal_durable pair.common_flags)
    operation.desired_flags

let operation_pair store (operation:J.operation) =
  match operation.pair_id with
  | None -> Error Missing_pair
  | Some id ->
      (match J.find_pair store ~id with
       | None -> Error Missing_pair
       | Some pair when pair.scope<>operation.scope -> Error Stale_pair
       | Some pair -> Ok pair)

let append_intent_matches ~scope (operation:J.operation)
    (intent:Imap_store.intent) =
  intent.scope=scope &&
  match intent.kind with
  | Imap_store.Append metadata ->
      Some metadata.content_digest=operation.blob_sha256 &&
      metadata.expected_length=operation.blob_length &&
      (match metadata.expected_flags,operation.desired_flags with
       | Some expected,Some desired -> F.equal_durable expected desired
       | _ -> false)
  | Imap_store.Other _ -> false

let conflicting_identity =
  Invalid_scope "saved OBJECTID+ binding names another mailbox"

let enable_object_identity ~(ctx:Ctx.t) =
  let {Ctx.client;store;scope;mailbox;_}=ctx in
  let has=Imap_eio.Client.has client in
  let offered=has Imap.Capability.Objectid_plus &&
    has Imap.Capability.Enable in
  match Imap_store.object_identity store ~scope with
  | `Conflict -> Error conflicting_identity
  | `Bound _ when not offered ->
    Error (Invalid_scope "saved OBJECTID+ identity cannot be verified")
  | `Unbound when not offered -> Ok None
  | `Unbound | `Bound _ as bound ->
    let* objectid=network (Imap_eio.Client.Objectid_plus.enable client) in
    let* ()=match bound with
      | `Unbound -> Ok ()
      | `Bound (identity:Imap_store.object_identity) ->
          let* status=network (Imap_eio.Client.Objectid_plus.status objectid
            ~mailbox ~items:[Imap.Status_item.Objectid]) in
          (match status.objectid with
           | Some ids when ids.account_id=Some identity.account_id &&
               ids.mailbox_id=Some identity.mailbox_id ->
               network (Imap_eio.Client.Objectid_plus.pin_mailbox objectid
                 ~mailbox ~account_id:identity.account_id
                 ~mailbox_id:identity.mailbox_id)
           | _ -> Error (Invalid_scope
               "configured mailbox name no longer matches saved OBJECTID+")) in
    Ok (Some objectid)

let guard_bound_mailbox ~(ctx:Ctx.t) =
  match Imap_store.object_identity ctx.store ~scope:ctx.scope with
  | `Unbound -> Ok ()
  | `Conflict -> Error conflicting_identity
  | `Bound _ ->
      let* _=enable_object_identity ~ctx in
      Ok ()

let verify_mutation_destination ~(ctx:Ctx.t) =
  match Imap_store.object_identity ctx.store ~scope:ctx.scope with
  | `Unbound -> Ok ()
  | `Conflict -> Error conflicting_identity
  | `Bound (identity:Imap_store.object_identity) ->
      if not (Imap_eio.Client.is_enabled ctx.client
                Imap.Capability.Objectid_plus) then
        Error (Invalid_scope "saved OBJECTID+ identity is not enabled")
      else
        let* status=network (Imap_eio.Client.status ctx.client
          ~mailbox:ctx.mailbox ~items:[Imap.Status_item.Objectid]) in
        (match status.objectid with
         | Some ids when ids.account_id=Some identity.account_id &&
             ids.mailbox_id=Some identity.mailbox_id -> Ok ()
         | _ -> Error (Invalid_scope
             "APPEND destination name no longer matches saved OBJECTID+"))

let with_selected (ctx:Ctx.t) ~mode f =
  match Imap_eio.Client.with_mailbox ctx.client ~mode ctx.mailbox
      (fun selected -> Ok (f selected)) with
  | Error e -> Error (Client e)
  | Ok result -> result

let remote_flags selected ~uid ~modseq =
  let* rows=network (Imap_eio.Selected.fetch selected ~uids:[uid]
    ~items:(if modseq then [Imap.Fetch_item.Modseq] else [])) in
  match rows with
  | {flags=Some flags;modseq;_} :: _ ->
      Ok (Some (F.durable flags,Option.map Imap.Modseq.to_int64 modseq))
  | _ -> Ok None

let epoch_flags selected ~epoch ~uid ~modseq =
  let* info=network (Imap_eio.Selected.info selected) in
  if info.uidvalidity<>Imap.Uidvalidity.to_int64 epoch then
    Error Stale_inventory
  else remote_flags selected ~uid ~modseq

let remote_flags_now ctx ~epoch ~uid ~modseq =
  with_selected ctx ~mode:`Read_only (fun selected ->
    let* info=network (Imap_eio.Selected.info selected) in
    if info.uidvalidity<>Imap.Uidvalidity.to_int64 epoch then
      Error Uidvalidity_changed
    else
      let* found=remote_flags selected ~uid ~modseq in
      match found with
      | Some flags -> Ok flags
      | None -> Error Missing_occurrence)

let remote_absent ctx ~epoch ~uid =
  with_selected ctx ~mode:`Read_only (fun selected ->
    let* found=epoch_flags selected ~epoch ~uid ~modseq:false in
    Ok (found=None))

let remote_flags_and_date (ctx:Ctx.t) ~uid ~uidvalidity =
  with_selected ctx ~mode:`Read_only (fun selected ->
    let* info=network (Imap_eio.Selected.info selected) in
    if info.uidvalidity<>Imap.Uidvalidity.to_int64 uidvalidity then
      Error Uidvalidity_changed
    else
      let* rows=network (Imap_eio.Selected.fetch selected ~uids:[uid]
        ~items:[Imap.Fetch_item.Internal_date]) in
      match rows with
      | {flags=Some flags;internal_date=Some date;_} :: _ ->
          Ok (F.durable flags,date)
      | {flags=Some _;internal_date=None;_} :: _ ->
          Error (Client (Imap_eio.Error.Protocol
            "message FETCH omitted INTERNALDATE"))
      | _ -> Error (Source_vanished uid))

(* The body is streamed into [spool] under the selection and hashed after
   the selection ends, so spool I/O never runs inside the mailbox lease. *)
let stable_remote_body ?(precheck=fun _ -> Ok ()) ctx ~mode ~spool ~epoch
    ~uid ~digest ~length ~expected =
  Spool.with_spool spool (fun sink ->
    let* seen=with_selected ctx ~mode (fun selected ->
      let* info=network (Imap_eio.Selected.info selected) in
      let* ()=precheck info in
      let* before=epoch_flags selected ~epoch ~uid ~modseq:true in
      match before with
      | None -> Ok `Absent
      | Some (flags,_) when not (F.equal_durable flags expected) ->
          Ok `Changed
      | Some (_,modseq) ->
          match Imap_eio.Selected.fetch_to selected ~max_bytes:length
              ~uid sink with
          | Error (Imap_eio.Error.Missing_uid _) -> Ok `Absent
          | Error (Imap_eio.Error.Limit _) -> Ok `Changed
          | Error error -> Error (Client error)
          | Ok () ->
              let* after=epoch_flags selected ~epoch ~uid ~modseq:true in
              match after with
              | None -> Ok `Absent
              | Some (flags,after_modseq) when after_modseq=modseq &&
                  F.equal_durable flags expected -> Ok (`Fetched modseq)
              | Some _ -> Ok `Changed) in
    match seen with
    | `Fetched modseq ->
        let found_length,found_digest=Spool.hash_file spool in
        if found_length=length && found_digest=digest then
          Ok (`Unchanged modseq)
        else Ok `Changed
    | (`Absent | `Changed) as seen -> Ok seen)

let published_presence store ~(cursor:Imap.Mirror.cursor) (pair:J.pair)
    epoch uid =
  if cursor.scope<>pair.scope || cursor.phase<>Imap.Mirror.Live ||
     cursor.uidvalidity<>Some epoch || cursor.inventory_ref=None then
    Error Stale_inventory
  else match Imap_store.snapshot_contains_uid store ~scope:pair.scope
      ~cursor ~uid with
    | `Stale_revision -> Error Stale_inventory
    | `Present present -> Ok present

let snapshot_has_uid store ~scope ~cursor uid =
  match Imap_store.snapshot_contains_uid store ~scope ~cursor ~uid with
  | `Stale_revision -> Error Store_stale_revision
  | `Present present -> Ok present

let snapshot_row store ~scope ~cursor uid =
  match Imap_store.snapshot_page store ~scope ~cursor
    ?after_uid:(Imap.Uid.pred uid) ~limit:1 () with
  | `Stale_revision -> Error Store_stale_revision
  | `Rows ((row:Imap.Mirror.row)::_) when Imap.Uid.equal row.uid uid ->
      Ok (Some row)
  | `Rows _ -> Ok None

let new_pair ~id ~scope ~uidvalidity ~uid ~local_id ~sha256 ~length
    ?internal_date ~flags () : J.pair = {
  id;scope;remote_uidvalidity=Some uidvalidity;remote_uid=Some uid;
  local_id=Some local_id;content_sha256=Some sha256;
  content_length=Some length;internal_date;
  common_flags=F.durable flags;
  remote_tombstone=None;local_tombstone=None;revision=0L}

let commit_new_pair store ~id pair =
  match J.commit_operation_with_pair store ~id
    ~expected_pair_revision:None pair with
  | `Committed _ -> Ok ()
  | `Stale_revision -> Error Store_stale_revision
