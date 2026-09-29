let check rc = Sqlite3.Rc.check rc
let invalid f = match f () with
  | exception Failure _ -> ()
  | _ -> failwith "damaged schema accepted"
let with_database env f =
  let path=Filename.temp_file "imap-schema-guards-" ".db" in
  Fun.protect ~finally:(fun () -> List.iter (fun path ->
    try Sys.remove path with Sys_error _ -> ()) [path;path^"-wal";path^"-shm"])
    (fun () ->
      let location=Eio.Path.(Eio.Stdenv.fs env / path) in
      Eio.Switch.run (fun sw -> ignore (Imap_store.open_path ~sw location));
      f path location)
let mutate path sql =
  let db=Sqlite3.db_open path in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () -> check (Sqlite3.exec db sql))
let descriptors path =
  Sys.readdir "/proc/self/fd" |> Array.fold_left (fun total entry ->
    try if Unix.readlink ("/proc/self/fd/" ^ entry)=path then total+1 else total
    with Unix.Unix_error _ -> total) 0
let object_ids constraints =
  "DROP TABLE mailbox_object_ids; CREATE TABLE mailbox_object_ids(\
   endpoint,account,mailbox_key,raw_name,encoding,account_id,mailbox_id" ^
  constraints ^ ")"
let run env =
  List.iter (fun sql -> with_database env (fun path location ->
    mutate path sql;
    Eio.Switch.run (fun sw ->
      for _=1 to 8 do
        invalid (fun () -> Imap_store.open_path ~sw location);
        invalid (fun () -> Imap_store.open_readonly ~sw location)
      done;
      if Sys.file_exists "/proc/self/fd" && descriptors path<>0 then
        failwith "failed schema initialization retained SQLite descriptors")))
    ["DROP INDEX sync_pairs_remote";
     "DROP INDEX sync_pairs_local";
     "DROP INDEX sync_pairs_remote; CREATE INDEX sync_pairs_remote ON sync_pairs(endpoint,account,mailbox_key,remote_epoch,remote_uid) WHERE remote_uid IS NOT NULL";
     "DROP INDEX sync_pairs_local; CREATE UNIQUE INDEX sync_pairs_local ON sync_pairs(endpoint,account,mailbox_key,local_id) WHERE local_id = 'one-name-only'";
     (* Foreign tables with the expected column names but other keys. *)
     object_ids "";
     object_ids ",PRIMARY KEY(endpoint,account,mailbox_key)";
     object_ids ",PRIMARY KEY(endpoint,account,mailbox_key),\
       UNIQUE(account_id,mailbox_id)";
     "DROP TABLE sync_pair_presence; CREATE TABLE sync_pair_presence(\
       pair_id,side,generation,PRIMARY KEY(pair_id))";
     "DROP TABLE mailboxes; CREATE TABLE mailboxes(endpoint,account,\
       mailbox_key,raw_name,encoding,mailbox_id,phase,uidvalidity,generation,\
       revision,anchor,frontier,inventory_ref,mode)";
     (* Another user_version, with and without the current tables. *)
     "PRAGMA user_version=2";
     "PRAGMA user_version=0";
     (* The same columns under another primary key. *)
     "DROP TABLE snapshots; CREATE TABLE snapshots ( \
       endpoint TEXT NOT NULL, account TEXT NOT NULL, \
       mailbox_key TEXT NOT NULL, uidvalidity INTEGER NOT NULL, \
       uid INTEGER NOT NULL, modseq INTEGER, flags TEXT NOT NULL, \
       PRIMARY KEY(endpoint,account,mailbox_key,uid))";
     "DROP INDEX sync_operations_scope_id";
     "DROP INDEX sync_conflicts_open_id";
     "CREATE INDEX sync_pairs_scope ON \
       sync_pairs(endpoint,account,mailbox_key)";
     "CREATE TABLE extra(a)"];
  (* The failure names the violated rule. *)
  let reason f = match f () with
    | exception Failure message -> message
    | _ -> failwith "damaged schema accepted" in
  List.iter (fun (sql,read_write,read_only) -> with_database env
    (fun path location ->
      mutate path sql;
      Eio.Switch.run (fun sw ->
        let expect what expected actual =
          if actual<>"Imap_store: " ^ expected then
            failwith (Printf.sprintf "%s after %S: %S" what sql actual) in
        expect "open_path" read_write
          (reason (fun () -> Imap_store.open_path ~sw location));
        expect "open_readonly" read_only
          (reason (fun () -> Imap_store.open_readonly ~sw location)))))
    ["PRAGMA user_version=2","unsupported schema version",
       "unsupported schema version";
     "PRAGMA user_version=0","unversioned database already contains tables",
       "unsupported schema version";
     "DROP TABLE blob_refs; CREATE TABLE blob_refs ( \
       endpoint TEXT NOT NULL, account TEXT NOT NULL, \
       mailbox_key TEXT NOT NULL, uidvalidity INTEGER NOT NULL, \
       uid INTEGER NOT NULL, sha256 TEXT NOT NULL, length INTEGER NOT NULL, \
       PRIMARY KEY(endpoint,account,mailbox_key,uid)); \
       CREATE INDEX blob_refs_hash ON blob_refs(sha256)",
       "incompatible table: blob_refs","incompatible table: blob_refs";
     "DROP INDEX sync_operations_pair_id",
       "missing index: sync_operations_pair_id",
       "missing index: sync_operations_pair_id";
     "CREATE INDEX sync_pairs_scope ON \
       sync_pairs(endpoint,account,mailbox_key)",
       "unexpected index: sync_pairs_scope",
       "unexpected index: sync_pairs_scope"];
  (* A table whose name only resembles SQLite's reserved prefix is foreign. *)
  let path=Filename.temp_file "imap-schema-foreign-" ".db" in
  Fun.protect ~finally:(fun () -> List.iter (fun path ->
    try Sys.remove path with Sys_error _ -> ()) [path;path^"-wal";path^"-shm"])
    (fun () ->
      mutate path "CREATE TABLE sqliteX(a)";
      Eio.Switch.run (fun sw ->
        invalid (fun () ->
          Imap_store.open_path ~sw Eio.Path.(Eio.Stdenv.fs env / path))));
  with_database env (fun path location ->
    Eio.Switch.run (fun sw ->
      let invalid_dir=Eio.Path.(Eio.Stdenv.fs env / (path ^ "-missing")) in
      (match Imap_store.open_path ~sw ~blob_dir:invalid_dir location with
       | exception Invalid_argument _ -> ()
       | _ -> failwith "missing blob directory accepted");
      if Sys.file_exists "/proc/self/fd" && descriptors path<>0 then
        failwith "invalid blob directory retained descriptor"))
let () = Eio_main.run run
