open Database
module SE = Sqlite3_eio

let version = 1L

(* Each object is created from, and validated against, its exact
   statement, which covers column order, keys, constraints and index
   predicates in one comparison. *)
let objects = [
  "table","mailboxes",
  "CREATE TABLE mailboxes ( \
   endpoint TEXT NOT NULL, account TEXT NOT NULL, mailbox_key TEXT NOT NULL, \
   raw_name TEXT NOT NULL, encoding TEXT NOT NULL, mailbox_id TEXT, \
   phase INTEGER NOT NULL, uidvalidity INTEGER, generation INTEGER NOT NULL, \
   revision INTEGER NOT NULL, anchor INTEGER, frontier INTEGER NOT NULL, \
   inventory_ref TEXT, mode INTEGER NOT NULL, \
   PRIMARY KEY(endpoint,account,mailbox_key))";
  "table","snapshots",
  "CREATE TABLE snapshots ( \
   endpoint TEXT NOT NULL, account TEXT NOT NULL, mailbox_key TEXT NOT NULL, \
   uidvalidity INTEGER NOT NULL, uid INTEGER NOT NULL, modseq INTEGER, \
   PRIMARY KEY(endpoint,account,mailbox_key,uidvalidity,uid))";
  "table","snapshot_flags",
  "CREATE TABLE snapshot_flags ( \
   endpoint TEXT NOT NULL, account TEXT NOT NULL, mailbox_key TEXT NOT NULL, \
   uidvalidity INTEGER NOT NULL, uid INTEGER NOT NULL, ord INTEGER NOT NULL, \
   flag TEXT NOT NULL, \
   PRIMARY KEY(endpoint,account,mailbox_key,uidvalidity,uid,ord), \
   FOREIGN KEY(endpoint,account,mailbox_key,uidvalidity,uid) \
     REFERENCES snapshots(endpoint,account,mailbox_key,uidvalidity,uid) \
     ON DELETE CASCADE)";
  "table","blob_refs",
  "CREATE TABLE blob_refs ( \
   endpoint TEXT NOT NULL, account TEXT NOT NULL, mailbox_key TEXT NOT NULL, \
   uidvalidity INTEGER NOT NULL, uid INTEGER NOT NULL, \
   sha256 TEXT NOT NULL, length INTEGER NOT NULL, \
   PRIMARY KEY(endpoint,account,mailbox_key,uidvalidity,uid))";
  "index","blob_refs_hash",
  "CREATE INDEX blob_refs_hash ON blob_refs(sha256)";
  "table","scan_stages",
  "CREATE TABLE scan_stages ( \
   id TEXT PRIMARY KEY, endpoint TEXT NOT NULL, account TEXT NOT NULL, \
   mailbox_key TEXT NOT NULL, raw_name TEXT NOT NULL, encoding TEXT NOT NULL, \
   mailbox_id TEXT, uidvalidity INTEGER NOT NULL, upper_uid INTEGER NOT NULL, \
   expected_revision INTEGER NOT NULL, \
   fetch_upper INTEGER NOT NULL DEFAULT 0, \
   search_upper INTEGER NOT NULL DEFAULT 0)";
  "table","scan_rows",
  "CREATE TABLE scan_rows ( \
   stage_id TEXT NOT NULL, uid INTEGER NOT NULL, modseq INTEGER, \
   seen INTEGER NOT NULL DEFAULT 0, \
   PRIMARY KEY(stage_id,uid), \
   FOREIGN KEY(stage_id) REFERENCES scan_stages(id) ON DELETE CASCADE)";
  "table","scan_flags",
  "CREATE TABLE scan_flags ( \
   stage_id TEXT NOT NULL, uid INTEGER NOT NULL, ord INTEGER NOT NULL, \
   flag TEXT NOT NULL, \
   PRIMARY KEY(stage_id,uid,ord), \
   FOREIGN KEY(stage_id,uid) REFERENCES scan_rows(stage_id,uid) \
     ON DELETE CASCADE)";
  "table","sync_pairs",
  "CREATE TABLE sync_pairs ( \
   id TEXT PRIMARY KEY, endpoint TEXT NOT NULL, account TEXT NOT NULL, \
   mailbox_key TEXT NOT NULL, raw_name TEXT NOT NULL, encoding TEXT NOT NULL, \
   mailbox_id TEXT, remote_epoch INTEGER, remote_uid INTEGER, local_id TEXT, \
   revision INTEGER NOT NULL, remote_tombstone_kind TEXT, \
   remote_tombstone_evidence TEXT, remote_tombstone_generation INTEGER, \
   local_tombstone_kind TEXT, local_tombstone_evidence TEXT, \
   local_tombstone_generation INTEGER, content_sha256 TEXT, \
   content_length INTEGER, internal_date TEXT, \
   CHECK ((remote_epoch IS NULL) = (remote_uid IS NULL)), \
   CHECK (remote_uid IS NOT NULL OR local_id IS NOT NULL))";
  "index","sync_pairs_remote",
  "CREATE UNIQUE INDEX sync_pairs_remote ON sync_pairs \
   (endpoint,account,mailbox_key,remote_epoch,remote_uid) \
   WHERE remote_uid IS NOT NULL";
  "index","sync_pairs_local",
  "CREATE UNIQUE INDEX sync_pairs_local ON sync_pairs \
   (endpoint,account,mailbox_key,local_id) WHERE local_id IS NOT NULL";
  "index","sync_pairs_scope_id",
  "CREATE INDEX sync_pairs_scope_id ON \
   sync_pairs(endpoint,account,mailbox_key,id)";
  "table","sync_pair_flags",
  "CREATE TABLE sync_pair_flags ( \
   pair_id TEXT NOT NULL, ord INTEGER NOT NULL, flag TEXT NOT NULL, \
   PRIMARY KEY(pair_id,ord), \
   FOREIGN KEY(pair_id) REFERENCES sync_pairs(id) ON DELETE CASCADE)";
  "table","sync_pair_presence",
  "CREATE TABLE sync_pair_presence ( \
   pair_id TEXT NOT NULL, side TEXT NOT NULL, generation INTEGER NOT NULL, \
   PRIMARY KEY(pair_id,side), \
   CHECK (side IN ('remote','local')), CHECK (generation >= 0), \
   FOREIGN KEY(pair_id) REFERENCES sync_pairs(id) ON DELETE CASCADE)";
  "table","sync_conflicts",
  "CREATE TABLE sync_conflicts ( \
   id TEXT PRIMARY KEY, pair_id TEXT NOT NULL, kind TEXT NOT NULL, \
   evidence TEXT NOT NULL, pair_revision INTEGER NOT NULL, \
   resolved INTEGER NOT NULL DEFAULT 0, \
   FOREIGN KEY(pair_id) REFERENCES sync_pairs(id) ON DELETE RESTRICT)";
  "index","sync_conflicts_open",
  "CREATE INDEX sync_conflicts_open ON sync_conflicts(pair_id,resolved)";
  "index","sync_conflicts_open_id",
  "CREATE INDEX sync_conflicts_open_id ON sync_conflicts(id) \
   WHERE resolved=0";
  "table","sync_operations",
  "CREATE TABLE sync_operations ( \
   id TEXT PRIMARY KEY, pair_id TEXT, local_id TEXT, endpoint TEXT NOT NULL, \
   account TEXT NOT NULL, mailbox_key TEXT NOT NULL, raw_name TEXT NOT NULL, \
   encoding TEXT NOT NULL, mailbox_id TEXT, kind TEXT NOT NULL, \
   state TEXT NOT NULL, source_epoch INTEGER, source_uid INTEGER, \
   dest_endpoint TEXT, dest_account TEXT, dest_mailbox_key TEXT, \
   dest_raw_name TEXT, dest_encoding TEXT, dest_mailbox_id TEXT, \
   dest_epoch INTEGER, receipt_epoch INTEGER, receipt_uid INTEGER, \
   blob_sha256 TEXT, blob_length INTEGER, desired_flags_known INTEGER, \
   receipt TEXT, message_id TEXT, spool_ref TEXT, pre_send_frontier INTEGER, \
   FOREIGN KEY(pair_id) REFERENCES sync_pairs(id) ON DELETE RESTRICT)";
  "index","sync_operations_pending",
  "CREATE INDEX sync_operations_pending ON sync_operations \
   (endpoint,account,mailbox_key,state)";
  "index","sync_operations_scope_id",
  "CREATE INDEX sync_operations_scope_id ON \
   sync_operations(endpoint,account,mailbox_key,id)";
  "index","sync_operations_pair_id",
  "CREATE INDEX sync_operations_pair_id ON sync_operations(pair_id,id)";
  "index","sync_operations_blob_pending",
  "CREATE INDEX sync_operations_blob_pending ON sync_operations(blob_sha256) \
   WHERE state NOT IN ('committed','rejected')";
  "table","sync_operation_flags",
  "CREATE TABLE sync_operation_flags ( \
   operation_id TEXT NOT NULL, ord INTEGER NOT NULL, flag TEXT NOT NULL, \
   PRIMARY KEY(operation_id,ord), \
   FOREIGN KEY(operation_id) REFERENCES sync_operations(id) \
     ON DELETE CASCADE)";
  "table","sync_operation_preconditions",
  "CREATE TABLE sync_operation_preconditions ( \
   operation_id TEXT PRIMARY KEY, pair_revision INTEGER NOT NULL, \
   FOREIGN KEY(operation_id) REFERENCES sync_operations(id) \
     ON DELETE CASCADE)";
  "table","sync_operation_local_preimages",
  "CREATE TABLE sync_operation_local_preimages ( \
   operation_id TEXT PRIMARY KEY, \
   FOREIGN KEY(operation_id) REFERENCES sync_operations(id) \
     ON DELETE CASCADE)";
  "table","sync_operation_local_preimage_flags",
  "CREATE TABLE sync_operation_local_preimage_flags ( \
   operation_id TEXT NOT NULL, ord INTEGER NOT NULL, flag TEXT NOT NULL, \
   PRIMARY KEY(operation_id,ord), \
   FOREIGN KEY(operation_id) \
     REFERENCES sync_operation_local_preimages(operation_id) \
     ON DELETE CASCADE)";
  "table","sync_operation_local_sources",
  "CREATE TABLE sync_operation_local_sources ( \
   operation_id TEXT PRIMARY KEY, mtime REAL NOT NULL, \
   FOREIGN KEY(operation_id) REFERENCES sync_operations(id) \
     ON DELETE CASCADE)";
  "table","sync_operation_source_dates",
  "CREATE TABLE sync_operation_source_dates ( \
   operation_id TEXT PRIMARY KEY, internal_date TEXT NOT NULL, \
   FOREIGN KEY(operation_id) REFERENCES sync_operations(id) \
     ON DELETE CASCADE)";
  "table","mailbox_object_ids",
  "CREATE TABLE mailbox_object_ids ( \
   endpoint TEXT NOT NULL, account TEXT NOT NULL, mailbox_key TEXT NOT NULL, \
   raw_name TEXT NOT NULL, encoding TEXT NOT NULL, \
   account_id TEXT NOT NULL, mailbox_id TEXT NOT NULL, \
   PRIMARY KEY(endpoint,account,mailbox_key), \
   UNIQUE(endpoint,account,account_id,mailbox_id))";
]

let user_version t =
  match rows t "PRAGMA user_version" [] with
  | [r] -> int r.(0)
  | _ -> fail "invalid schema version"

let stored_objects t =
  rows t "SELECT type,name,sql FROM sqlite_master WHERE sql IS NOT NULL \
    AND name NOT LIKE 'sqlite\\_%' ESCAPE '\\'" []
  |> List.map (fun r -> text r.(1),(text r.(0),text r.(2)))

(* SQLite keeps a statement's text with its whitespace, so only the
   tokens are compared. *)
let compact sql =
  String.to_seq sql |> Seq.filter (function
    | ' ' | '\t' | '\r' | '\n' | ';' -> false | _ -> true)
  |> String.of_seq |> String.uppercase_ascii

let validate_schema t =
  if user_version t<>version then fail "unsupported schema version";
  let stored=stored_objects t in
  List.iter (fun (kind,name,sql) ->
    match List.assoc_opt name stored with
    | Some (k,s) when k=kind && compact s=compact sql -> ()
    | Some _ -> fail (Printf.sprintf "incompatible %s: %s" kind name)
    | None -> fail (Printf.sprintf "missing %s: %s" kind name)) objects;
  List.iter (fun (name,(kind,_)) ->
    if not (List.exists (fun (_,n,_) -> n=name) objects) then
      fail (Printf.sprintf "unexpected %s: %s" kind name)) stored

let initialize db f =
  match f () with
  | result -> result
  | exception ex ->
      let backtrace=Printexc.get_raw_backtrace () in
      (try Eio.Cancel.protect (fun () -> Eio.Resource.close db) with _ -> ());
      Printexc.raise_with_backtrace ex backtrace

let open_readonly ~sw path =
  let db = SE.open_path ~sw ~busy_timeout:5000 ~mode:`READONLY path in
  initialize db (fun () ->
  let t = { db; mutex = Eio.Mutex.create (); blob_dir = None } in
  transaction ~begin_sql:"BEGIN" t (fun () -> validate_schema t);
  t)

let open_path ~sw ?blob_dir path =
  let blob_dir = Option.map (fun dir ->
    if not (Eio.Path.is_directory dir) then
      invalid_arg "Imap_store.open_path: blob directory must exist";
    ignore (Eio.Path.native_exn dir : string);
    Dir dir) blob_dir in
  let db = SE.open_path ~sw ~busy_timeout:5000 path in
  initialize db (fun () ->
  let t = { db; mutex = Eio.Mutex.create (); blob_dir } in
  sql t "PRAGMA journal_mode=WAL";
  sql t "PRAGMA synchronous=FULL";
  sql t "PRAGMA foreign_keys=ON";
  (match rows t "PRAGMA journal_mode" [] with
   | [r] when String.lowercase_ascii (text r.(0)) = "wal" -> ()
   | _ -> fail "WAL mode unavailable");
  (match rows t "PRAGMA synchronous" [] with
   | [r] when int r.(0) = 2L -> ()
   | _ -> fail "synchronous=FULL unavailable");
  (match rows t "PRAGMA foreign_keys" [] with
   | [r] when int r.(0) = 1L -> ()
   | _ -> fail "foreign keys unavailable");
  transaction t (fun () ->
    if user_version t=0L then (
      if stored_objects t<>[] then
        fail "unversioned database already contains tables";
      List.iter (fun (_,_,statement) -> sql t statement) objects;
      sql t (Printf.sprintf "PRAGMA user_version=%Ld" version));
    validate_schema t);
  t)
