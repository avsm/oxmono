open Record_codec
open Database
module M = Imap.Mirror
module P = Imap.Proto
module S = Sqlite3
module SE = Sqlite3_eio

type t = Database.t
type mailbox = { cursor : M.cursor; snapshot : M.snapshot option }
type intent_kind = Operation_intent.intent_kind =
  | Append of {
      message_id : string; content_digest : string; spool_ref : string;
      pre_send_uid_frontier : int64 option;
      expected_length : int64 option;
      expected_flags : Mail_flag.Imap_flag.t list option;
      expected_internal_date : string option
    }
  | Other of string
type intent_state = Operation_intent.intent_state = Prepared | Sent | Ambiguous | Confirmed | Rejected
type intent = Operation_intent.intent = {
  id : string; scope : M.scope; kind : intent_kind; state : intent_state;
  uidvalidity : P.Uidvalidity.t option; uid : P.Uid.t option
}

let open_readonly = Schema.open_readonly
let open_path = Schema.open_path

exception Scope_mismatch = Record_codec.Scope_mismatch

let load_unlocked t ~scope =
    let cursor = cursor_exn t scope in
    let snapshot = match cursor.uidvalidity with
      | None -> None
      | Some epoch ->
        let key = scope_key scope @ [i (P.Uidvalidity.to_int64 epoch)] in
        let joined = rows t "SELECT m.uid,m.modseq,f.flag FROM snapshots AS m \
          LEFT JOIN snapshot_flags AS f ON f.endpoint=m.endpoint \
          AND f.account=m.account AND f.mailbox_key=m.mailbox_key \
          AND f.uidvalidity=m.uidvalidity AND f.uid=m.uid WHERE \
          m.endpoint=? AND m.account=? AND m.mailbox_key=? AND m.uidvalidity=? \
          ORDER BY m.uid,f.ord" key in
        let current = ref None and snapshot_rows = ref [] in
        let flush () = match !current with
          | None -> ()
          | Some (u,seq,flags) ->
            snapshot_rows := { M.uid=uid u; flags=List.rev flags;
              modseq=Option.map modseq seq } :: !snapshot_rows in
        List.iter (fun r ->
          let u=int r.(0) in
          let f=match r.(2) with
            | S.Data.NULL -> None
            | x -> Some (of_checked "flag" Mail_flag.Imap_flag.of_wire (text x)) in
          (match !current with
           | Some (previous,seq,flags) when previous=u ->
             current := Some (previous,seq,
               match f with None -> flags | Some flag -> flag::flags)
           | _ ->
             flush ();
             current := Some (u,nullable_int r.(1),
               match f with None -> [] | Some flag -> [flag]))) joined;
        flush ();
        let snapshot_rows = List.rev !snapshot_rows in
        (match M.snapshot ~uidvalidity:epoch snapshot_rows with
         | Ok snapshot -> Some snapshot
         | Error e -> fail ("persisted snapshot: " ^ mirror_error e)) in
    {cursor; snapshot}

let load t ~scope =
  transaction ~begin_sql:"BEGIN" t (fun () -> load_unlocked t ~scope)

type object_identity = { account_id:string; mailbox_id:string }

let valid_object_id value =
  let n=String.length value in
  n>0 && n<=255 && String.for_all (function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' -> true
    | _ -> false) value

let object_identity t ~(scope:M.scope) =
  if t.schema_version<12L then None else
  transaction ~begin_sql:"BEGIN" t (fun () ->
    match rows t "SELECT raw_name,encoding,account_id,mailbox_id \
      FROM mailbox_object_ids WHERE endpoint=? AND account=? AND mailbox_key=?"
      (scope_key scope) with
    | [] -> None
    | [r] ->
        if text r.(0)<>scope.raw_name ||
           dec_enc (text r.(1))<>scope.encoding then
          fail "stored OBJECTID+ binding scope differs";
        Some {account_id=text r.(2);mailbox_id=text r.(3)}
    | _ -> fail "duplicate OBJECTID+ binding")

let observe_object_identity t ~(scope:M.scope) identity =
  if not (valid_object_id identity.account_id &&
          valid_object_id identity.mailbox_id) then
    invalid_arg "Imap_store.observe_object_identity: invalid OBJECTID+";
  transaction t (fun () ->
    let existing=rows t "SELECT raw_name,encoding,account_id,mailbox_id \
      FROM mailbox_object_ids WHERE endpoint=? AND account=? AND mailbox_key=?"
      (scope_key scope) in
    match existing with
    | [r] when text r.(0)=scope.raw_name &&
               dec_enc (text r.(1))=scope.encoding &&
               text r.(2)=identity.account_id &&
               text r.(3)=identity.mailbox_id -> `Matched
    | [_] -> `Conflict
    | [] ->
        (match rows t "SELECT mailbox_key FROM mailbox_object_ids \
          WHERE endpoint=? AND account=? AND account_id=? AND mailbox_id=?"
          [s scope.endpoint;s scope.account;s identity.account_id;
           s identity.mailbox_id] with
         | [] ->
             run t "INSERT INTO mailbox_object_ids \
               (endpoint,account,mailbox_key,raw_name,encoding,account_id,mailbox_id) \
               VALUES (?,?,?,?,?,?,?)"
               (scope_key scope @ [s scope.raw_name;
                 s (enc scope.encoding);s identity.account_id;
                 s identity.mailbox_id]);
             `Bound
         | _ -> `Conflict)
    | _ -> fail "duplicate OBJECTID+ binding")

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
        SELECT * FROM snapshots WHERE endpoint=? AND account=? AND mailbox_key=? \
        AND uidvalidity=? AND uid>? ORDER BY uid LIMIT ?) AS m \
        LEFT JOIN snapshot_flags AS f ON f.endpoint=m.endpoint \
        AND f.account=m.account AND f.mailbox_key=m.mailbox_key \
        AND f.uidvalidity=m.uidvalidity AND f.uid=m.uid \
        ORDER BY m.uid,f.ord"
        (scope_key scope@[i (P.Uidvalidity.to_int64 epoch);
          i (match after_uid with None -> 0L | Some u -> P.Uid.to_int64 u);
          i (Int64.of_int limit)]) in
      let current_row=ref None and result=ref [] in
      let flush ()=match !current_row with
        | None -> ()
        | Some (u,seq,flags) ->
          result:={M.uid=uid u;modseq=Option.map modseq seq;
            flags=List.rev flags}::!result in
      List.iter (fun r ->
        let u=int r.(0) in
        let f=match r.(2) with
          | S.Data.NULL -> None
          | x -> Some (of_checked "snapshot flag"
              Mail_flag.Imap_flag.of_wire (text x)) in
        match !current_row with
        | Some (previous,seq,flags) when previous=u ->
          current_row:=Some (u,seq,
            match f with None -> flags | Some flag -> flag::flags)
        | _ ->
          flush ();
          current_row:=Some (u,nullable_int r.(1),
            match f with None -> [] | Some flag -> [flag])) found;
      flush ();
      `Rows (List.rev !result))

let snapshot_contains_uid t ~(scope:M.scope) ~(cursor:M.cursor) ~uid:target =
  if cursor.scope<>scope then
    invalid_arg "Imap_store.snapshot_contains_uid: scope/cursor mismatch";
  transaction ~begin_sql:"BEGIN" t (fun () ->
    if stale t cursor then `Stale_revision
    else match cursor.uidvalidity with
      | None -> `Present false
      | Some epoch ->
          let found=rows t "SELECT 1 FROM snapshots WHERE endpoint=? AND account=? AND mailbox_key=? AND uidvalidity=? AND uid=? LIMIT 1"
            (scope_key scope@[i (P.Uidvalidity.to_int64 epoch);
              i (P.Uid.to_int64 target)]) in
          `Present (found<>[]))

type staged_receipt = { cursor : M.cursor; row_count : int64 }

let stage_header t id =
  match rows t "SELECT endpoint,account,mailbox_key,raw_name,encoding,mailbox_id,uidvalidity,upper_uid,expected_revision,fetch_upper,search_upper FROM scan_stages WHERE id=?" [s id] with
  | [r] -> r
  | [] -> invalid_arg "Imap_store: unknown scan stage"
  | _ -> fail "duplicate scan stage"

let begin_stage t ~(cursor:M.cursor) ~(action:M.action) =
  if action.id="" || action.scope<>cursor.scope ||
     action.expected_revision<>cursor.revision ||
     action.expected_generation<>cursor.generation then
    invalid_arg "Imap_store.begin_stage: action/cursor mismatch";
  transaction t (fun () ->
    run t "INSERT INTO scan_stages (id,endpoint,account,mailbox_key,raw_name,encoding,mailbox_id,uidvalidity,upper_uid,expected_revision) VALUES (?,?,?,?,?,?,?,?,?,?)"
      ([s action.id] @ scope_key cursor.scope @
       [s cursor.scope.raw_name;s (enc cursor.scope.encoding);
        ns cursor.scope.mailbox_id;i (P.Uidvalidity.to_int64 action.uidvalidity);
        i action.upper_uid;i action.expected_revision]))

let seed_stage_from_published t ~(cursor:M.cursor) ~(action:M.action) =
  if action.scope<>cursor.scope ||
     action.expected_revision<>cursor.revision ||
     action.expected_generation<>cursor.generation ||
     cursor.uidvalidity<>Some action.uidvalidity then
    invalid_arg "Imap_store.seed_stage_from_published: action/cursor mismatch";
  transaction t (fun () ->
    let h=stage_header t action.id in
    let scope=cursor.scope in
    if [text h.(0);text h.(1);text h.(2)]<>
       [scope.endpoint;scope.account;scope.mailbox_key] ||
       text h.(3)<>scope.raw_name ||
       dec_enc (text h.(4))<>scope.encoding ||
       nullable_text h.(5)<>scope.mailbox_id ||
       int h.(6)<>P.Uidvalidity.to_int64 action.uidvalidity ||
       int h.(7)<>action.upper_uid || int h.(8)<>cursor.revision ||
       int h.(9)<>0L || int h.(10)<>0L then
      invalid_arg "Imap_store.seed_stage_from_published: stage mismatch";
    if stale t cursor then `Stale_revision else (
      let key=scope_key cursor.scope @
        [i (P.Uidvalidity.to_int64 action.uidvalidity)] in
      run t "INSERT INTO scan_rows(stage_id,uid,modseq) SELECT ?,uid,modseq FROM snapshots WHERE endpoint=? AND account=? AND mailbox_key=? AND uidvalidity=? AND uid<=?"
        ([s action.id] @ key @ [i action.upper_uid]);
      run t "INSERT INTO scan_flags(stage_id,uid,ord,flag) SELECT ?,uid,ord,flag FROM snapshot_flags WHERE endpoint=? AND account=? AND mailbox_key=? AND uidvalidity=? AND uid<=?"
        ([s action.id] @ key @ [i action.upper_uid]);
      `Seeded))

let stage_rows ?(preserve_newer=false) t ~stage_id ~first ~last batch =
  if first < 1L || last < first then invalid_arg "Imap_store.stage_rows: range";
  transaction t (fun () ->
    let h=stage_header t stage_id in
    let upper=int h.(7) and prior=int h.(9) in
    if first<>Int64.succ prior || last>upper then
      invalid_arg "Imap_store.stage_rows: noncontiguous FETCH coverage";
    with_stmt t "INSERT INTO scan_rows (stage_id,uid,modseq) VALUES (?,?,?) ON CONFLICT(stage_id,uid) DO UPDATE SET modseq=excluded.modseq"
      (fun row_stmt ->
        with_stmt t "INSERT INTO scan_flags VALUES (?,?,?,?)" (fun flag_stmt ->
          List.iter (fun (row:M.row) ->
            let uid=P.Uid.to_int64 row.uid in
            if uid<first || uid>last then
              invalid_arg "Imap_store.stage_rows: UID outside FETCH range";
            let newer=if not preserve_newer then true else
              match rows t "SELECT modseq FROM scan_rows WHERE stage_id=? AND uid=?"
                [s stage_id;i uid] with
              | [] -> true
              | [existing] ->
                  (match nullable_int existing.(0),row.modseq with
                   | Some old,Some now ->
                       P.Modseq.to_int64 now>=old
                   | None,Some _ -> true
                   | _ -> invalid_arg
                       "Imap_store.stage_rows: incremental row lacks MODSEQ")
              | _ -> fail "duplicate staged UID" in
            if newer then (
            run_prepared t row_stmt
              [s stage_id;i uid;ni (Option.map P.Modseq.to_int64 row.modseq)];
            run t "DELETE FROM scan_flags WHERE stage_id=? AND uid=?"
              [s stage_id;i uid];
            List.iteri (fun ord flag -> run_prepared t flag_stmt
              [s stage_id;i uid;i (Int64.of_int ord);
               s (Mail_flag.Imap_flag.to_wire flag)]) row.flags)) batch));
    run t "UPDATE scan_stages SET fetch_upper=? WHERE id=?"
      [i last;s stage_id])

let stage_membership t ~stage_id ~first ~last uids =
  if first<1L || last<first then invalid_arg "Imap_store.stage_membership: range";
  transaction t (fun () ->
    let h=stage_header t stage_id in
    let upper=int h.(7) and prior=int h.(10) in
    if first<>Int64.succ prior || last>upper || int h.(9)<last then
      invalid_arg "Imap_store.stage_membership: incomplete FETCH coverage";
    let unique=Hashtbl.create (List.length uids) in
    with_stmt t "SELECT 1 FROM scan_rows WHERE stage_id=? AND uid=?"
      (fun check_stmt ->
        with_stmt t "UPDATE scan_rows SET seen=1 WHERE stage_id=? AND uid=?"
          (fun mark_stmt ->
            List.iter (fun uid ->
              if uid<first || uid>last then
                invalid_arg "Imap_store.stage_membership: UID outside SEARCH range";
              if Hashtbl.mem unique uid then
                invalid_arg "Imap_store.stage_membership: duplicate UID";
              Hashtbl.add unique uid ();
              if rows_prepared t check_stmt [s stage_id;i uid]=[] then
                invalid_arg
                  "Imap_store.stage_membership: live UID absent from FETCH";
              run_prepared t mark_stmt [s stage_id;i uid]) uids));
    run t "UPDATE scan_stages SET search_upper=? WHERE id=?"
      [i last;s stage_id])

let discard_stage t ~stage_id =
  transaction t (fun () -> run t "DELETE FROM scan_stages WHERE id=?" [s stage_id])

let abandoned_stages t =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    rows t "SELECT id FROM scan_stages ORDER BY id" []
    |> List.map (fun r -> text r.(0)))

let publish t (change:M.transition) =
  transaction t (fun () ->
    let c = change.cursor and scope = change.cursor.scope in
    if stale_revision t scope ~revision:(Int64.pred c.revision) then
      `Stale_revision else (
      if M.snapshot_uidvalidity change.snapshot <> Option.get c.uidvalidity then
        invalid_arg "Imap_store.publish: cursor/snapshot epoch mismatch";
      let key = scope_key scope in
      run t "INSERT INTO mailboxes VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?) \
        ON CONFLICT(endpoint,account,mailbox_key) DO UPDATE SET \
        raw_name=excluded.raw_name,encoding=excluded.encoding, \
        mailbox_id=excluded.mailbox_id,phase=excluded.phase, \
        uidvalidity=excluded.uidvalidity,generation=excluded.generation, \
        revision=excluded.revision,anchor=excluded.anchor, \
        frontier=excluded.frontier,inventory_ref=excluded.inventory_ref, \
        mode=excluded.mode"
        (key @ [s scope.raw_name; s (enc scope.encoding); ns scope.mailbox_id;
          i (phase c.phase); ni (Option.map P.Uidvalidity.to_int64 c.uidvalidity);
          i c.generation; i c.revision;
          ni (Option.map P.Modseq.to_int64 c.anchor); i c.frontier;
          ns c.inventory_ref; i (mode c.mode)]);
      let epoch = P.Uidvalidity.to_int64 (Option.get c.uidvalidity) in
      let snap_key = key @ [i epoch] in
      run t "DELETE FROM snapshots WHERE endpoint=? AND account=? \
        AND mailbox_key=? AND uidvalidity=?" snap_key;
      with_stmt t "INSERT INTO snapshots VALUES (?,?,?,?,?,?)" (fun snap_stmt ->
        with_stmt t "INSERT INTO snapshot_flags VALUES (?,?,?,?,?,?,?)"
          (fun flag_stmt ->
            List.iter (fun (row:M.row) ->
              let row_key = snap_key @ [i (P.Uid.to_int64 row.uid)] in
              run_prepared t snap_stmt
                (row_key @ [ni (Option.map P.Modseq.to_int64 row.modseq)]);
              List.iteri (fun ord flag ->
                run_prepared t flag_stmt
                  (row_key @ [i (Int64.of_int ord);
                    s (Mail_flag.Imap_flag.to_wire flag)])) row.flags)
              (M.rows change.snapshot)));
      run t "DELETE FROM blob_refs WHERE endpoint=? AND account=? \
        AND mailbox_key=? AND uidvalidity=? AND NOT EXISTS ( \
        SELECT 1 FROM snapshots AS m WHERE m.endpoint=blob_refs.endpoint \
        AND m.account=blob_refs.account AND m.mailbox_key=blob_refs.mailbox_key \
        AND m.uidvalidity=blob_refs.uidvalidity AND m.uid=blob_refs.uid)" snap_key;
      `Committed))

let publish_stage t ~(cursor:M.cursor) ~(action:M.action)
    ~explicit_highestmodseq ~nomodseq =
  if action.scope<>cursor.scope ||
     action.expected_revision<>cursor.revision ||
     action.expected_generation<>cursor.generation then
    invalid_arg "Imap_store.publish_stage: action/cursor mismatch";
  transaction t (fun () ->
    let h=stage_header t action.id in
    let scope=cursor.scope in
    if [text h.(0);text h.(1);text h.(2)]<>
       [scope.endpoint;scope.account;scope.mailbox_key] ||
       text h.(3)<>scope.raw_name || dec_enc (text h.(4))<>scope.encoding ||
       nullable_text h.(5)<>scope.mailbox_id ||
       int h.(6)<>P.Uidvalidity.to_int64 action.uidvalidity ||
       int h.(7)<>action.upper_uid || int h.(8)<>cursor.revision then
      invalid_arg "Imap_store.publish_stage: stage metadata mismatch";
    if int h.(9)<>action.upper_uid || int h.(10)<>action.upper_uid then
      invalid_arg "Imap_store.publish_stage: incomplete range coverage";
    if stale_revision t scope ~revision:cursor.revision then `Stale_revision
    else (
      let resolved_mode=if nomodseq then M.Baseline else action.mode in
      let anchor=if resolved_mode=M.Baseline then None else explicit_highestmodseq in
      (match action.previous_anchor,anchor with
       | Some old,Some now when P.Modseq.to_int64 now<P.Modseq.to_int64 old ->
           invalid_arg "Imap_store.publish_stage: MODSEQ regression"
       | _ -> ());
      let next : M.cursor =
        match M.restore ~schema_version:cursor.schema_version ~scope
          ~phase:M.Live ~uidvalidity:(Some action.uidvalidity)
          ~generation:(Int64.succ cursor.generation)
          ~revision:(Int64.succ cursor.revision) ~anchor
          ~frontier:action.upper_uid ~inventory_ref:(Some action.id)
          ~mode:resolved_mode with
        | Ok cursor -> cursor
        | Error e -> invalid_arg
            ("Imap_store.publish_stage: " ^ mirror_error e) in
      let key=scope_key scope in
      run t "INSERT INTO mailboxes VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?) ON CONFLICT(endpoint,account,mailbox_key) DO UPDATE SET raw_name=excluded.raw_name,encoding=excluded.encoding,mailbox_id=excluded.mailbox_id,phase=excluded.phase,uidvalidity=excluded.uidvalidity,generation=excluded.generation,revision=excluded.revision,anchor=excluded.anchor,frontier=excluded.frontier,inventory_ref=excluded.inventory_ref,mode=excluded.mode"
        (key @ [s scope.raw_name;s (enc scope.encoding);ns scope.mailbox_id;
          i (phase next.phase);
          ni (Option.map P.Uidvalidity.to_int64 next.uidvalidity);
          i next.generation;i next.revision;
          ni (Option.map P.Modseq.to_int64 next.anchor);i next.frontier;
          ns next.inventory_ref;i (mode next.mode)]);
      let epoch=P.Uidvalidity.to_int64 action.uidvalidity in
      let snap_key=key@[i epoch] in
      run t "DELETE FROM snapshots WHERE endpoint=? AND account=? AND mailbox_key=? AND uidvalidity=?" snap_key;
      run t "INSERT INTO snapshots SELECT ?,?,?,?,uid,modseq FROM scan_rows WHERE stage_id=? AND seen=1"
        (snap_key@[s action.id]);
      run t "INSERT INTO snapshot_flags SELECT ?,?,?,?,f.uid,f.ord,f.flag FROM scan_flags AS f JOIN scan_rows AS r ON r.stage_id=f.stage_id AND r.uid=f.uid WHERE f.stage_id=? AND r.seen=1"
        (snap_key@[s action.id]);
      run t "DELETE FROM blob_refs WHERE endpoint=? AND account=? AND mailbox_key=? AND uidvalidity=? AND NOT EXISTS (SELECT 1 FROM snapshots AS m WHERE m.endpoint=blob_refs.endpoint AND m.account=blob_refs.account AND m.mailbox_key=blob_refs.mailbox_key AND m.uidvalidity=blob_refs.uidvalidity AND m.uid=blob_refs.uid)"
        snap_key;
      let row_count=match rows t "SELECT count(*) FROM scan_rows WHERE stage_id=? AND seen=1" [s action.id] with
        | [r] -> int r.(0) | _ -> fail "invalid staged row count" in
      run t "DELETE FROM scan_stages WHERE id=?" [s action.id];
      `Committed {cursor=next;row_count}))

let prepare_intent = Operation_intent.prepare_intent
let set_intent_state = Operation_intent.set_intent_state
let confirm_intent = Operation_intent.confirm_intent
let pending_intents = Operation_intent.pending_intents
let find_intent = Operation_intent.find_intent

module Sync = Sync_journal
module Blob = Blob_store
