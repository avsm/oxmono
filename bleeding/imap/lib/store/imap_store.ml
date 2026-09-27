open Record_codec
open Database
module M = Imap.Mirror

type t = Database.t
include Operation_intent

let open_readonly = Schema.open_readonly
let open_path = Schema.open_path

exception Scope_mismatch = Record_codec.Scope_mismatch

let snapshot_rows rows =
  group_flags "snapshot flag" ~flag:2 rows
  |> List.map (fun (r,flags) ->
    {M.uid=uid (int r.(0));modseq=Option.map modseq (nullable_int r.(1));
     flags})

type object_identity = { account_id:string; mailbox_id:string }

let valid_object_id value =
  let n=String.length value in
  n>0 && n<=255 && String.for_all (function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' -> true
    | _ -> false) value

let stored_identity t (scope:M.scope) =
  match rows t "SELECT raw_name,encoding,account_id,mailbox_id \
    FROM mailbox_object_ids WHERE endpoint=? AND account=? AND mailbox_key=?"
    (scope_key scope) with
  | [] -> `Unbound
  | r :: _ ->
    if text r.(0)<>scope.raw_name || dec_enc (text r.(1))<>scope.encoding
    then `Conflict
    else `Bound {account_id=text r.(2);mailbox_id=text r.(3)}

let object_identity t ~scope =
  if t.schema_version<12L then `Unbound
  else transaction ~begin_sql:"BEGIN" t (fun () -> stored_identity t scope)

let observe_object_identity t ~(scope:M.scope) identity =
  if not (valid_object_id identity.account_id &&
          valid_object_id identity.mailbox_id) then
    invalid_arg "Imap_store.observe_object_identity: invalid OBJECTID+";
  transaction t (fun () ->
    match stored_identity t scope with
    | `Bound stored when stored=identity -> `Matched
    | `Bound _ | `Conflict -> `Conflict
    | `Unbound ->
        (match rows t "SELECT mailbox_key FROM mailbox_object_ids \
          WHERE endpoint=? AND account=? AND account_id=? AND mailbox_id=?"
          [s scope.endpoint;s scope.account;s identity.account_id;
           s identity.mailbox_id] with
         | [] ->
             run t "INSERT INTO mailbox_object_ids \
               (endpoint,account,mailbox_key,raw_name,encoding,account_id,\
               mailbox_id) VALUES (?,?,?,?,?,?,?)"
               (scope_key scope @ [s scope.raw_name;
                 s (enc scope.encoding);s identity.account_id;
                 s identity.mailbox_id]);
             `Bound
         | _ :: _ -> `Conflict))

let load_cursor t ~scope =
  transaction ~begin_sql:"BEGIN" t (fun () -> cursor_exn t scope)

let snapshot_page t ~(scope:M.scope) ~(cursor:M.cursor) ?after_uid ~limit () =
  check_page_args "Imap_store.snapshot_page" scope cursor limit;
  transaction ~begin_sql:"BEGIN" t (fun () ->
    if stale t cursor then `Stale_revision else
    match cursor.uidvalidity with
    | None -> `Rows []
    | Some epoch ->
      let found=rows t "SELECT m.uid,m.modseq,f.flag FROM ( \
        SELECT * FROM snapshots WHERE endpoint=? AND account=? \
        AND mailbox_key=? AND uidvalidity=? AND uid>? ORDER BY uid LIMIT ?) \
        AS m LEFT JOIN snapshot_flags AS f ON f.endpoint=m.endpoint \
        AND f.account=m.account AND f.mailbox_key=m.mailbox_key \
        AND f.uidvalidity=m.uidvalidity AND f.uid=m.uid \
        ORDER BY m.uid,f.ord"
        (scope_key scope@[i (Imap.Uidvalidity.to_int64 epoch);
          i (match after_uid with None -> 0L | Some u -> Imap.Uid.to_int64 u);
          i (Int64.of_int limit)]) in
      `Rows (snapshot_rows found))

let snapshot_contains_uid t ~(scope:M.scope) ~(cursor:M.cursor) ~uid:target =
  if cursor.scope<>scope then
    invalid_arg "Imap_store.snapshot_contains_uid: scope/cursor mismatch";
  transaction ~begin_sql:"BEGIN" t (fun () ->
    if stale t cursor then `Stale_revision
    else match cursor.uidvalidity with
      | None -> `Present false
      | Some epoch ->
          let found=rows t "SELECT 1 FROM snapshots WHERE endpoint=? \
            AND account=? AND mailbox_key=? AND uidvalidity=? AND uid=? \
            LIMIT 1"
            (scope_key scope@[i (Imap.Uidvalidity.to_int64 epoch);
              i (Imap.Uid.to_int64 target)]) in
          `Present (found<>[]))

type staged_receipt = { cursor : M.cursor; row_count : int64 }

let check_action who (cursor:M.cursor) (action:M.action) =
  if action.scope<>cursor.scope ||
     action.expected_revision<>cursor.revision ||
     action.expected_generation<>cursor.generation then
    invalid_arg (who ^ ": action/cursor mismatch")

let stage_header t id =
  match rows t "SELECT endpoint,account,mailbox_key,raw_name,encoding,\
    mailbox_id,uidvalidity,upper_uid,expected_revision,fetch_upper,\
    search_upper FROM scan_stages WHERE id=?" [s id] with
  | r :: _ -> r
  | [] -> invalid_arg "Imap_store: unknown scan stage"

(* [stage_for who t cursor action] is the header of [action]'s stage after
   checking that the stage was begun for [cursor] and [action]. *)
let stage_for who t (cursor:M.cursor) (action:M.action) =
  let h=stage_header t action.id in
  let scope=cursor.scope in
  if [text h.(0);text h.(1);text h.(2)]<>
     [scope.endpoint;scope.account;scope.mailbox_key] ||
     text h.(3)<>scope.raw_name || dec_enc (text h.(4))<>scope.encoding ||
     nullable_text h.(5)<>scope.mailbox_id ||
     int h.(6)<>Imap.Uidvalidity.to_int64 action.uidvalidity ||
     int h.(7)<>action.upper_uid || int h.(8)<>cursor.revision then
    invalid_arg (who ^ ": stage metadata mismatch");
  h

let begin_stage t ~(cursor:M.cursor) ~(action:M.action) =
  check_action "Imap_store.begin_stage" cursor action;
  transaction t (fun () ->
    run t "INSERT INTO scan_stages (id,endpoint,account,mailbox_key,\
      raw_name,encoding,mailbox_id,uidvalidity,upper_uid,expected_revision) \
      VALUES (?,?,?,?,?,?,?,?,?,?)"
      ([s action.id] @ scope_key cursor.scope @
       [s cursor.scope.raw_name;s (enc cursor.scope.encoding);
        ns cursor.scope.mailbox_id;
        i (Imap.Uidvalidity.to_int64 action.uidvalidity);
        i action.upper_uid;i action.expected_revision]))

let seed_stage_from_published t ~(cursor:M.cursor) ~(action:M.action) =
  let who="Imap_store.seed_stage_from_published" in
  check_action who cursor action;
  if cursor.uidvalidity<>Some action.uidvalidity then
    invalid_arg (who ^ ": action/cursor mismatch");
  transaction t (fun () ->
    let h=stage_for who t cursor action in
    if int h.(9)<>0L || int h.(10)<>0L then
      invalid_arg (who ^ ": stage already has coverage");
    if stale t cursor then `Stale_revision else (
      let values=[s action.id] @ scope_key cursor.scope @
        [i (Imap.Uidvalidity.to_int64 action.uidvalidity);i action.upper_uid] in
      run t "INSERT INTO scan_rows(stage_id,uid,modseq) SELECT ?,uid,modseq \
        FROM snapshots WHERE endpoint=? AND account=? AND mailbox_key=? \
        AND uidvalidity=? AND uid<=?" values;
      run t "INSERT INTO scan_flags(stage_id,uid,ord,flag) \
        SELECT ?,uid,ord,flag FROM snapshot_flags WHERE endpoint=? \
        AND account=? AND mailbox_key=? AND uidvalidity=? AND uid<=?" values;
      `Seeded))

let stage_rows ?(preserve_newer=false) t ~stage_id ~first ~last batch =
  let who="Imap_store.stage_rows" in
  if first < 1L || last < first then invalid_arg (who ^ ": range");
  transaction t (fun () ->
    let h=stage_header t stage_id in
    let upper=int h.(7) and prior=int h.(9) in
    if first<>Int64.succ prior || last>upper then
      invalid_arg (who ^ ": noncontiguous FETCH coverage");
    with_stmt t "SELECT modseq FROM scan_rows WHERE stage_id=? AND uid=?"
    @@ fun seeded_stmt ->
    with_stmt t "INSERT INTO scan_rows (stage_id,uid,modseq) VALUES (?,?,?) \
      ON CONFLICT(stage_id,uid) DO UPDATE SET modseq=excluded.modseq"
    @@ fun row_stmt ->
    with_stmt t "DELETE FROM scan_flags WHERE stage_id=? AND uid=?"
    @@ fun clear_stmt ->
    with_stmt t "INSERT INTO scan_flags VALUES (?,?,?,?)" @@ fun flag_stmt ->
    List.iter (fun (row:M.row) ->
      let uid=Imap.Uid.to_int64 row.uid in
      if uid<first || uid>last then
        invalid_arg (who ^ ": UID outside FETCH range");
      let newer=if not preserve_newer then true else
        match rows_prepared t seeded_stmt [s stage_id;i uid] with
        | [] -> true
        | seeded :: _ ->
            (match nullable_int seeded.(0),row.modseq with
             | Some old,Some now -> Imap.Modseq.to_int64 now>=old
             | None,Some _ -> true
             | Some _,None ->
                 invalid_arg (who ^ ": incremental row lacks MODSEQ")
             | None,None ->
                 invalid_arg
                   (who ^ ": seeded and incremental rows lack MODSEQ")) in
      if newer then (
        run_prepared t row_stmt
          [s stage_id;i uid;ni (Option.map Imap.Modseq.to_int64 row.modseq)];
        run_prepared t clear_stmt [s stage_id;i uid];
        List.iteri (fun ord flag -> run_prepared t flag_stmt
          [s stage_id;i uid;i (Int64.of_int ord);
           s (Mail_flag.Imap_flag.to_wire flag)]) row.flags)) batch;
    run t "UPDATE scan_stages SET fetch_upper=? WHERE id=?"
      [i last;s stage_id])

let stage_membership t ~stage_id ~first ~last uids =
  let who="Imap_store.stage_membership" in
  if first<1L || last<first then invalid_arg (who ^ ": range");
  transaction t (fun () ->
    let h=stage_header t stage_id in
    let upper=int h.(7) and prior=int h.(10) in
    if first<>Int64.succ prior || last>upper || int h.(9)<last then
      invalid_arg (who ^ ": incomplete FETCH coverage");
    let unique=Hashtbl.create (List.length uids) in
    with_stmt t "SELECT 1 FROM scan_rows WHERE stage_id=? AND uid=?"
    @@ fun check_stmt ->
    with_stmt t "UPDATE scan_rows SET seen=1 WHERE stage_id=? AND uid=?"
    @@ fun mark_stmt ->
    List.iter (fun uid ->
      let uid=Imap.Uid.to_int64 uid in
      if uid<first || uid>last then
        invalid_arg (who ^ ": UID outside SEARCH range");
      if Hashtbl.mem unique uid then invalid_arg (who ^ ": duplicate UID");
      Hashtbl.add unique uid ();
      if rows_prepared t check_stmt [s stage_id;i uid]=[] then
        invalid_arg (who ^ ": live UID absent from FETCH");
      run_prepared t mark_stmt [s stage_id;i uid]) uids;
    run t "UPDATE scan_stages SET search_upper=? WHERE id=?"
      [i last;s stage_id])

let discard_stage t ~stage_id =
  transaction t (fun () ->
    run t "DELETE FROM scan_stages WHERE id=?" [s stage_id])

let abandoned_stages t =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    rows t "SELECT id FROM scan_stages ORDER BY id" []
    |> List.map (fun r -> text r.(0)))

let write_cursor t (c:M.cursor) =
  let scope=c.scope in
  run t "INSERT INTO mailboxes VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?) \
    ON CONFLICT(endpoint,account,mailbox_key) DO UPDATE SET \
    raw_name=excluded.raw_name,encoding=excluded.encoding, \
    mailbox_id=excluded.mailbox_id,phase=excluded.phase, \
    uidvalidity=excluded.uidvalidity,generation=excluded.generation, \
    revision=excluded.revision,anchor=excluded.anchor, \
    frontier=excluded.frontier,inventory_ref=excluded.inventory_ref, \
    mode=excluded.mode"
    (scope_key scope @ [s scope.raw_name;s (enc scope.encoding);
      ns scope.mailbox_id;i (phase c.phase);
      ni (Option.map Imap.Uidvalidity.to_int64 c.uidvalidity);
      i c.generation;i c.revision;
      ni (Option.map Imap.Modseq.to_int64 c.anchor);i c.frontier;
      ns c.inventory_ref;i (mode c.mode)])

(* [replace_epoch t scope epoch fill] empties [epoch]'s snapshot, lets
   [fill] insert its rows under the bound key and drops blob references
   to UIDs no longer present. *)
let replace_epoch t scope epoch fill =
  let key=scope_key scope @ [i (Imap.Uidvalidity.to_int64 epoch)] in
  run t "DELETE FROM snapshots WHERE endpoint=? AND account=? \
    AND mailbox_key=? AND uidvalidity=?" key;
  let result=fill key in
  run t "DELETE FROM blob_refs WHERE endpoint=? AND account=? \
    AND mailbox_key=? AND uidvalidity=? AND NOT EXISTS ( \
    SELECT 1 FROM snapshots AS m WHERE m.endpoint=blob_refs.endpoint \
    AND m.account=blob_refs.account AND m.mailbox_key=blob_refs.mailbox_key \
    AND m.uidvalidity=blob_refs.uidvalidity AND m.uid=blob_refs.uid)" key;
  result

let forget_epochs t ~(scope:M.scope) ~(cursor:M.cursor) =
  if cursor.scope<>scope then
    invalid_arg "Imap_store.forget_epochs: scope/cursor mismatch";
  transaction t (fun () ->
    if stale t cursor then `Stale_revision else (
      let key=scope_key scope @
        [ni (Option.map Imap.Uidvalidity.to_int64 cursor.uidvalidity)] in
      let others="endpoint=? AND account=? AND mailbox_key=? \
        AND uidvalidity IS NOT ?" in
      let dropped=match rows t ("SELECT count(*) FROM (\
          SELECT uidvalidity FROM snapshots WHERE " ^ others ^ " UNION \
          SELECT uidvalidity FROM blob_refs WHERE " ^ others ^ ")")
          (key @ key) with
        | r :: _ -> Int64.to_int (int r.(0))
        | [] -> 0 in
      run t ("DELETE FROM snapshots WHERE " ^ others) key;
      run t ("DELETE FROM blob_refs WHERE " ^ others) key;
      `Dropped dropped))

let publish_stage t ~(cursor:M.cursor) ~(action:M.action)
    ~explicit_highestmodseq ~nomodseq =
  let who="Imap_store.publish_stage" in
  check_action who cursor action;
  transaction t (fun () ->
    let h=stage_for who t cursor action in
    let scope=cursor.scope in
    if stale_revision t scope ~revision:cursor.revision then `Stale_revision
    else if int h.(9)<>action.upper_uid || int h.(10)<>action.upper_uid then
      invalid_arg (who ^ ": incomplete range coverage")
    else (
      let resolved_mode=if nomodseq then M.Baseline else action.mode in
      let anchor=
        if resolved_mode=M.Baseline then None else explicit_highestmodseq in
      (match action.previous_anchor,anchor with
       | Some old,Some now when Imap.Modseq.compare now old<0 ->
           invalid_arg (who ^ ": MODSEQ regression")
       | _ -> ());
      let next : M.cursor =
        match M.restore ~schema_version:cursor.schema_version ~scope
          ~phase:M.Live ~uidvalidity:(Some action.uidvalidity)
          ~generation:(Int64.succ cursor.generation)
          ~revision:(Int64.succ cursor.revision) ~anchor
          ~frontier:action.upper_uid ~inventory_ref:(Some action.id)
          ~mode:resolved_mode with
        | Ok cursor -> cursor
        | Error e -> invalid_arg (who ^ ": " ^ mirror_error e) in
      write_cursor t next;
      let row_count=replace_epoch t scope action.uidvalidity (fun key ->
        run t "INSERT INTO snapshots SELECT ?,?,?,?,uid,modseq FROM scan_rows \
          WHERE stage_id=? AND seen=1" (key@[s action.id]);
        let inserted=Int64.of_int (changes t) in
        run t "INSERT INTO snapshot_flags SELECT ?,?,?,?,f.uid,f.ord,f.flag \
          FROM scan_flags AS f JOIN scan_rows AS r ON r.stage_id=f.stage_id \
          AND r.uid=f.uid WHERE f.stage_id=? AND r.seen=1"
          (key@[s action.id]);
        inserted) in
      run t "DELETE FROM scan_stages WHERE id=?" [s action.id];
      `Committed {cursor=next;row_count}))

module Journal = Sync_journal
module Blob = Blob_store
