(* SPDX-License-Identifier: ISC *)

type t = { db : Sqlite3_eio.t; lock : Eio.Mutex.t }

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
let locked t f = Eio.Mutex.use_rw ~protect:true t.lock f

let open_ ~sw directory =
  let db =
    Sqlite3_eio.open_path ~sw ~busy_timeout:5000
      Eio.Path.(directory / "spindle.db")
  in
  let t = { db; lock = Eio.Mutex.create () } in
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

let batch t ~puts ~deletes =
  locked t (fun () ->
      check (Sqlite3_eio.exec t.db "BEGIN IMMEDIATE");
      match
        List.iter
          (fun (ns, key) ->
            execute t "DELETE FROM kv WHERE namespace=? AND key=?"
              [ text ns; text key ])
          deletes;
        List.iter
          (fun (ns, key, value) ->
            execute t
              "INSERT OR REPLACE INTO kv(namespace,key,value) VALUES(?,?,?)"
              [ text ns; text key; text value ])
          puts;
        check (Sqlite3_eio.exec t.db "COMMIT")
      with
      | () -> ()
      | exception ex ->
          ignore (Sqlite3_eio.exec t.db "ROLLBACK");
          raise ex)

let put t ns key value = batch t ~puts:[ (ns, key, value) ] ~deletes:[]
let delete t ns key = batch t ~puts:[] ~deletes:[ (ns, key) ]

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
        execute t
          {|
      INSERT OR REPLACE INTO kv(namespace,key,value)
      SELECT 'inbox',?,? WHERE NOT EXISTS
        (SELECT 1 FROM kv WHERE namespace='done' AND key=?)
    |}
          [ text key; text value; text key ];
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
