(* SPDX-License-Identifier: ISC *)

type t = {
  db : Sqlite3_eio.t;
  lock : Eio.Mutex.t;
  mutable inbox_limit : int;
  mutable inbox_bytes : int;
}

exception Inbox_full

let check = Sqlite3.Rc.check

let statement t sql args f =
  let stmt = Sqlite3_eio.prepare t.db sql in
  Fun.protect
    ~finally:(fun () -> check (Sqlite3_eio.finalize t.db stmt))
    (fun () ->
      check (Sqlite3.bind_values stmt args);
      f stmt)

let execute t sql args =
  statement t sql args (fun stmt -> check (Sqlite3_eio.step t.db stmt))

let rows t sql args =
  statement t sql args (fun stmt ->
      let rec loop acc =
        match Sqlite3_eio.step t.db stmt with
        | Sqlite3.Rc.ROW -> loop (Sqlite3.row_data stmt :: acc)
        | Sqlite3.Rc.DONE -> List.rev acc
        | rc ->
            check rc;
            assert false
      in
      loop [])

let text s = Sqlite3.Data.TEXT s
let string = Sqlite3.Data.to_string_exn
let locked t f = Lock.protect t.lock f

let open_ ~sw directory =
  let db =
    Sqlite3_eio.open_path ~sw ~busy_timeout:5000
      Eio.Path.(directory / "spindle.db")
  in
  let t =
    {
      db;
      lock = Eio.Mutex.create ();
      inbox_limit = 10000;
      inbox_bytes = 64 * 1024 * 1024;
    }
  in
  check
    (Sqlite3_eio.exec db
       {|
    PRAGMA journal_mode=WAL;
    PRAGMA synchronous=FULL;
    CREATE TABLE IF NOT EXISTS kv (
      namespace TEXT NOT NULL, key TEXT NOT NULL, value TEXT NOT NULL,
      PRIMARY KEY(namespace, key));
    CREATE TABLE IF NOT EXISTS replay (
      issuer TEXT NOT NULL, jti TEXT NOT NULL, expires REAL NOT NULL,
      PRIMARY KEY(issuer, jti));
    CREATE INDEX IF NOT EXISTS replay_expiry ON replay(expires);
    CREATE TABLE IF NOT EXISTS retry (
      namespace TEXT NOT NULL, key TEXT NOT NULL, attempts INTEGER NOT NULL,
      due REAL NOT NULL, PRIMARY KEY(namespace, key));
    CREATE TABLE IF NOT EXISTS age (
      namespace TEXT NOT NULL, key TEXT NOT NULL, updated REAL NOT NULL,
      PRIMARY KEY(namespace,key));
    CREATE INDEX IF NOT EXISTS age_updated ON age(namespace,updated,key);
    CREATE TRIGGER IF NOT EXISTS kv_age_insert AFTER INSERT ON kv BEGIN
      INSERT INTO age VALUES(NEW.namespace,NEW.key,unixepoch('subsec'))
        ON CONFLICT(namespace,key) DO UPDATE SET updated=excluded.updated;
    END;
    CREATE TRIGGER IF NOT EXISTS kv_age_update AFTER UPDATE OF value ON kv BEGIN
      INSERT INTO age VALUES(NEW.namespace,NEW.key,unixepoch('subsec'))
        ON CONFLICT(namespace,key) DO UPDATE SET updated=excluded.updated;
    END;
    CREATE TRIGGER IF NOT EXISTS kv_age_delete AFTER DELETE ON kv BEGIN
      DELETE FROM age WHERE namespace=OLD.namespace AND key=OLD.key;
    END;
    INSERT OR IGNORE INTO age SELECT namespace,key,unixepoch('subsec') FROM kv;
    CREATE TABLE IF NOT EXISTS event_position (
      key TEXT PRIMARY KEY, source TEXT NOT NULL, position INTEGER NOT NULL);
    CREATE INDEX IF NOT EXISTS event_source ON event_position(source,position);
    CREATE TABLE IF NOT EXISTS ref_state (
      repo TEXT NOT NULL, ref TEXT NOT NULL, sha TEXT NOT NULL,
      position INTEGER NOT NULL, updated REAL NOT NULL,
      PRIMARY KEY(repo,ref));
  |});
  t

let get t namespace key =
  locked t (fun () ->
      match
        rows t "SELECT value FROM kv WHERE namespace=? AND key=?"
          [ text namespace; text key ]
      with
      | [ row ] -> Some (string row.(0))
      | [] -> None
      | _ -> assert false)

let list t namespace =
  locked t (fun () ->
      rows t "SELECT key,value FROM kv WHERE namespace=? ORDER BY key"
        [ text namespace ]
      |> List.map (fun row -> (string row.(0), string row.(1))))

let fold t namespace ?(descending = false) ~init ~f () =
  let rec pages after acc =
    let page =
      locked t (fun () ->
          let comparison, order =
            if descending then ("<", "DESC") else (">", "ASC")
          in
          rows t
            ("SELECT key,value FROM kv WHERE namespace=?"
            ^ (if after = None then "" else " AND key" ^ comparison ^ "?")
            ^ " ORDER BY key " ^ order ^ " LIMIT 128")
            (text namespace :: Option.to_list (Option.map text after))
          |> List.map (fun row -> (string row.(0), string row.(1))))
    in
    match page with
    | [] -> acc
    | _ ->
        let acc = List.fold_left f acc page in
        pages (Some (fst (List.hd (List.rev page)))) acc
  in
  pages None init

let ready t namespace ~now ~limit =
  if limit < 1 || limit > 128 then invalid_arg "Store.ready limit";
  locked t (fun () ->
      rows t
        {|SELECT kv.key,kv.value FROM kv LEFT JOIN retry
          ON retry.namespace=kv.namespace AND retry.key=kv.key
          WHERE kv.namespace=? AND (retry.due IS NULL OR retry.due<=?)
          ORDER BY COALESCE(retry.due,?),kv.key LIMIT ?|}
        [
          text namespace;
          Sqlite3.Data.FLOAT now;
          Sqlite3.Data.FLOAT now;
          Sqlite3.Data.INT (Int64.of_int limit);
        ]
      |> List.map (fun row -> (string row.(0), string row.(1))))

let defer t namespace key ~now =
  locked t (fun () ->
      execute t
        {|INSERT INTO retry(namespace,key,attempts,due) VALUES(?,?,1,?+1)
          ON CONFLICT(namespace,key) DO UPDATE SET
          attempts=MIN(retry.attempts+1,6),
          due=?+MIN(1 << retry.attempts,60)|}
        [
          text namespace;
          text key;
          Sqlite3.Data.FLOAT now;
          Sqlite3.Data.FLOAT now;
        ])

let batch_unlocked t ~puts ~deletes =
  check (Sqlite3_eio.exec t.db "BEGIN IMMEDIATE");
  match
    List.iter
      (fun (ns, key) ->
        execute t "DELETE FROM kv WHERE namespace=? AND key=?"
          [ text ns; text key ];
        execute t "DELETE FROM retry WHERE namespace=? AND key=?"
          [ text ns; text key ])
      deletes;
    List.iter
      (fun (ns, key, value) ->
        execute t "INSERT OR REPLACE INTO kv(namespace,key,value) VALUES(?,?,?)"
          [ text ns; text key; text value ])
      puts;
    check (Sqlite3_eio.exec t.db "COMMIT")
  with
  | () -> ()
  | exception ex ->
      ignore (Sqlite3_eio.exec t.db "ROLLBACK");
      raise ex

let batch t ~puts ~deletes =
  locked t (fun () -> batch_unlocked t ~puts ~deletes)

let schedule t namespace key =
  locked t (fun () ->
      execute t
        "INSERT OR REPLACE INTO kv(namespace,key,value) \
         VALUES(?,?,hex(randomblob(16)))"
        [ text namespace; text key ])

let complete t namespace key ~value ~puts ~deletes =
  locked t (fun () ->
      match
        rows t "SELECT value FROM kv WHERE namespace=? AND key=?"
          [ text namespace; text key ]
      with
      | [ row ] when string row.(0) = value ->
          batch_unlocked t ~puts ~deletes:((namespace, key) :: deletes);
          true
      | _ -> false)

let put t ns key value = batch t ~puts:[ (ns, key, value) ] ~deletes:[]
let delete t ns key = batch t ~puts:[] ~deletes:[ (ns, key) ]

let set_limits t (policy : Operations.t) =
  t.inbox_limit <- policy.inbox_limit;
  t.inbox_bytes <- policy.inbox_megabytes * 1024 * 1024

let usage_unlocked t namespace =
  match
    rows t
      {|SELECT count(*),COALESCE(sum(length(CAST(kv.value AS BLOB))),0),
         COALESCE(min(age.updated),0.0) FROM kv JOIN age USING(namespace,key)
         WHERE kv.namespace=?|}
      [ text namespace ]
  with
  | [ row ] ->
      ( Sqlite3.Data.to_int_exn row.(0),
        Sqlite3.Data.to_int_exn row.(1),
        Sqlite3.Data.to_float_exn row.(2) )
  | _ -> assert false

let usage t namespace = locked t (fun () -> usage_unlocked t namespace)

let pending_source t source =
  locked t (fun () ->
      match
        rows t
          {|SELECT count(*) FROM kv JOIN event_position USING(key)
      WHERE namespace='inbox' AND source=?|}
          [ text source ]
      with
      | [ row ] -> Sqlite3.Data.to_int_exn row.(0)
      | _ -> assert false)

let ref_state t ~repo ~ref_ =
  locked t (fun () ->
      match
        rows t "SELECT sha,position FROM ref_state WHERE repo=? AND ref=?"
          [ text repo; text ref_ ]
      with
      | [ row ] -> Some (string row.(0), Sqlite3.Data.to_int64_exn row.(1))
      | [] -> None
      | _ -> assert false)

let checkpoint_ref t ~repo ~ref_ ~sha ~position =
  locked t (fun () ->
      execute t
        {|INSERT INTO ref_state VALUES(?,?,?,?,unixepoch('subsec'))
      ON CONFLICT(repo,ref) DO UPDATE SET sha=excluded.sha,
        position=excluded.position,updated=excluded.updated
      WHERE excluded.position>=ref_state.position|}
        [ text repo; text ref_; text sha; Sqlite3.Data.INT position ])

let touch_ref t ~repo ~ref_ =
  locked t (fun () ->
      execute t
        "UPDATE ref_state SET updated=unixepoch('subsec') WHERE repo=? AND \
         ref=?"
        [ text repo; text ref_ ])

(* Checking and consuming are one transaction. Failed verification never
   reaches this function, and an accepted nonce survives process restart. *)
let consume t ~now ~issuer ~jti ~expires =
  locked t (fun () ->
      check (Sqlite3_eio.exec t.db "BEGIN IMMEDIATE");
      match
        execute t "DELETE FROM replay WHERE expires<=?"
          [ Sqlite3.Data.FLOAT now ];
        let count =
          match rows t "SELECT count(*) FROM replay" [] with
          | [ row ] -> Sqlite3.Data.to_int_exn row.(0)
          | _ -> assert false
        in
        let accepted =
          if count >= 65536 then false
          else (
            execute t "INSERT OR IGNORE INTO replay VALUES(?,?,?)"
              [ text issuer; text jti; Sqlite3.Data.FLOAT expires ];
            Sqlite3.changes (Sqlite3_eio.db t.db) = 1)
        in
        check (Sqlite3_eio.exec t.db "COMMIT");
        accepted
      with
      | accepted -> accepted
      | exception ex ->
          ignore (Sqlite3_eio.exec t.db "ROLLBACK");
          raise ex)

let enqueue t ~source ~cursor ~key ~value =
  locked t (fun () ->
      ignore (Int64.of_string cursor);
      check (Sqlite3_eio.exec t.db "BEGIN IMMEDIATE");
      match
        let known =
          rows t
            "SELECT 1 FROM kv WHERE namespace IN ('done','inbox') AND key=? \
             LIMIT 1"
            [ text key ]
          <> []
        in
        let below_floor =
          rows t
            {|SELECT 1 FROM kv WHERE namespace='replay-floor' AND key=?
            AND CAST(value AS INTEGER)>=CAST(? AS INTEGER)|}
            [ text source; text cursor ]
          <> []
        in
        (if (not known) && not below_floor then
           let count, bytes, _ = usage_unlocked t "inbox" in
           if
             count >= t.inbox_limit
             || bytes + String.length value > t.inbox_bytes
           then raise Inbox_full);
        execute t
          {|
      INSERT OR IGNORE INTO kv(namespace,key,value)
      SELECT 'inbox',?,? WHERE NOT EXISTS
        (SELECT 1 FROM kv WHERE namespace='done' AND key=?)
        AND NOT EXISTS (SELECT 1 FROM kv WHERE namespace='replay-floor'
          AND key=? AND CAST(value AS INTEGER)>=CAST(? AS INTEGER))
    |}
          [ text key; text value; text key; text source; text cursor ];
        if not below_floor then
          execute t
            "INSERT OR IGNORE INTO event_position VALUES(?,?,CAST(? AS \
             INTEGER))"
            [ text key; text source; text cursor ];
        execute t
          {|
      INSERT INTO kv(namespace,key,value) VALUES('cursor',?,?)
      ON CONFLICT(namespace,key) DO UPDATE SET value=excluded.value
      WHERE CAST(excluded.value AS INTEGER)>CAST(kv.value AS INTEGER)
    |}
          [ text source; text cursor ];
        check (Sqlite3_eio.exec t.db "COMMIT")
      with
      | () -> ()
      | exception ex ->
          ignore (Sqlite3_eio.exec t.db "ROLLBACK");
          raise ex)

let transaction t f =
  check (Sqlite3_eio.exec t.db "BEGIN IMMEDIATE");
  match
    let result = f () in
    check (Sqlite3_eio.exec t.db "COMMIT");
    result
  with
  | result -> result
  | exception exn ->
      ignore (Sqlite3_eio.exec t.db "ROLLBACK");
      raise exn

let remove_unlocked t namespace key =
  execute t "DELETE FROM kv WHERE namespace=? AND key=?"
    [ text namespace; text key ];
  execute t "DELETE FROM retry WHERE namespace=? AND key=?"
    [ text namespace; text key ]

let prune t ~now (policy : Operations.t) =
  locked t (fun () ->
      let history =
        {| FROM kv v JOIN age a ON a.namespace=v.namespace AND a.key=v.key
         LEFT JOIN kv p ON p.namespace='pipeline' AND p.key=v.key
         WHERE v.namespace='pipeline-view' AND json_valid(v.value)
         AND NOT EXISTS (SELECT 1 FROM json_each(v.value,'$.workflows') w
           WHERE json_extract(w.value,'$.status') IN ('pending','running')) |}
      in
      let size =
        "length(CAST(v.value AS BLOB))+COALESCE(length(CAST(p.value AS \
         BLOB)),0)"
      in
      let count, bytes =
        match
          rows t ("SELECT count(*),COALESCE(sum(" ^ size ^ "),0)" ^ history) []
        with
        | [ r ] -> (Sqlite3.Data.to_int_exn r.(0), Sqlite3.Data.to_int_exn r.(1))
        | _ -> assert false
      in
      let rec pipelines count bytes removed =
        let candidates =
          rows t
            ("SELECT v.key,a.updated," ^ size ^ history
           ^ " ORDER BY a.updated,v.key LIMIT 128")
            []
        in
        let count, bytes, removed, progress =
          transaction t (fun () ->
              List.fold_left
                (fun (count, bytes, removed, progress) row ->
                  let expired =
                    Sqlite3.Data.to_float_exn row.(1)
                    < now -. float_of_int (policy.history_days * 86400)
                  in
                  if
                    expired
                    || count > policy.history_limit
                    || bytes > policy.history_megabytes * 1024 * 1024
                  then (
                    let key = string row.(0) in
                    remove_unlocked t "pipeline" key;
                    remove_unlocked t "pipeline-view" key;
                    ( count - 1,
                      bytes - Sqlite3.Data.to_int_exn row.(2),
                      removed + 1,
                      true ))
                  else (count, bytes, removed, progress))
                (count, bytes, removed, false)
                candidates)
        in
        if progress then pipelines count bytes removed else removed
      in
      let pipelines = pipelines count bytes 0 in
      let rec receipts removed =
        let count, _, _ = usage_unlocked t "done" in
        let candidates =
          rows t
            {|SELECT kv.key,age.updated FROM kv JOIN age USING(namespace,key)
          WHERE namespace='done' ORDER BY age.updated,kv.key LIMIT 128|}
            []
        in
        let _, removed, progress =
          transaction t (fun () ->
              List.fold_left
                (fun (count, removed, progress) row ->
                  if
                    count > policy.receipt_limit
                    || Sqlite3.Data.to_float_exn row.(1)
                       < now -. float_of_int (policy.receipt_days * 86400)
                  then (
                    let key = string row.(0) in
                    (* A durable floor replaces expired receipts. Reconnects
                       below it require current-state recovery. *)
                    execute t
                      {|INSERT INTO kv(namespace,key,value)
                SELECT 'replay-floor',source,CAST(position AS TEXT) FROM event_position WHERE key=?
                ON CONFLICT(namespace,key) DO UPDATE SET value=excluded.value
                WHERE CAST(excluded.value AS INTEGER)>CAST(kv.value AS INTEGER)|}
                      [ text key ];
                    (* Legacy receipts have no position metadata. Protect open
                       streams using their durable cursors, and record a
                       wall-clock floor for disconnected sources. *)
                    if
                      rows t "SELECT 1 FROM event_position WHERE key=?"
                        [ text key ]
                      = []
                    then (
                      execute t
                        {|INSERT INTO kv(namespace,key,value)
                          SELECT 'replay-floor',key,value FROM kv
                          WHERE namespace='cursor'
                          ON CONFLICT(namespace,key) DO UPDATE
                          SET value=excluded.value
                          WHERE CAST(excluded.value AS INTEGER)>
                            CAST(kv.value AS INTEGER)|}
                        [];
                      execute t
                        "INSERT OR REPLACE INTO kv \
                         VALUES('replay-floor','legacy-seconds',?)"
                        [ text (Printf.sprintf "%.6f" now) ]);
                    remove_unlocked t "done" key;
                    remove_unlocked t "rejected" key;
                    execute t "DELETE FROM event_position WHERE key=?"
                      [ text key ];
                    (count - 1, removed + 1, true))
                  else (count, removed, progress))
                (count, removed, false) candidates)
        in
        if progress then receipts removed else removed
      in
      let receipts = receipts 0 in
      transaction t (fun () ->
          execute t "DELETE FROM replay WHERE expires<=?"
            [ Sqlite3.Data.FLOAT now ];
          execute t
            {|DELETE FROM kv WHERE namespace='dispatch' AND key IN
          (SELECT key FROM age WHERE namespace='dispatch' AND updated<?)
          AND NOT EXISTS (SELECT 1 FROM kv p WHERE p.namespace='pipeline-view'
            AND p.key=kv.value AND json_valid(p.value)
            AND EXISTS (SELECT 1 FROM json_each(p.value,'$.workflows') w
              WHERE json_extract(w.value,'$.status') IN ('pending','running')))|}
            [
              Sqlite3.Data.FLOAT
                (now -. float_of_int (policy.receipt_days * 86400));
            ];
          execute t
            {|DELETE FROM kv WHERE namespace='dispatch' AND key IN (
          SELECT d.key FROM kv d JOIN age a ON a.namespace=d.namespace AND a.key=d.key
          WHERE d.namespace='dispatch' AND NOT EXISTS (
            SELECT 1 FROM kv p WHERE p.namespace='pipeline-view' AND p.key=d.value
            AND json_valid(p.value) AND EXISTS (
              SELECT 1 FROM json_each(p.value,'$.workflows') w
              WHERE json_extract(w.value,'$.status') IN ('pending','running')))
          ORDER BY a.updated DESC,d.key DESC LIMIT -1 OFFSET ?)|}
            [ Sqlite3.Data.INT (Int64.of_int policy.receipt_limit) ];
          execute t "DELETE FROM ref_state WHERE updated<?"
            [
              Sqlite3.Data.FLOAT
                (now -. float_of_int (policy.receipt_days * 86400));
            ];
          execute t
            {|DELETE FROM kv WHERE namespace IN
           ('pull-seen','observer','sh.tangled.repo','sh.tangled.spindle.member')
          AND (namespace,key) IN (SELECT namespace,key FROM age WHERE updated<?)|}
            [
              Sqlite3.Data.FLOAT
                (now -. float_of_int (policy.receipt_days * 86400));
            ];
          execute t
            {|DELETE FROM kv WHERE namespace='gap' AND json_valid(value)
          AND json_extract(value,'$.status')='reconciled' AND key IN
            (SELECT key FROM age WHERE namespace='gap' AND updated<?)|}
            [
              Sqlite3.Data.FLOAT
                (now -. float_of_int (policy.history_days * 86400));
            ]);
      (* Free pages remain reusable. Checkpointing keeps the WAL bounded without
       a blocking full VACUUM or deleting pending work. *)
      ignore (Sqlite3_eio.exec t.db "PRAGMA wal_checkpoint(PASSIVE)");
      (pipelines, receipts))
