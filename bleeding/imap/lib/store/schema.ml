open Database
module SE = Sqlite3_eio

let current_version = 13L

let user_version t =
  match rows t "PRAGMA user_version" [] with
  | [r] -> int r.(0)
  | _ -> fail "invalid schema version"

let validate_schema t =
  let version = user_version t in
  if version < 8L || version > current_version then
    fail "unsupported schema version";
  let expect_table ?(unique=[]) table ~key expected =
    let info = rows t ("PRAGMA table_info(" ^ table ^ ")") [] in
    let key_rank r = int r.(5) in
    let actual_key = List.filter (fun r -> key_rank r > 0L) info
      |> List.sort (fun a b -> compare (key_rank a) (key_rank b))
      |> List.map (fun r -> text r.(1)) in
    let index_columns name =
      rows t ("PRAGMA index_info(" ^ name ^ ")") []
      |> List.sort (fun a b -> compare (int a.(0)) (int b.(0)))
      |> List.map (fun r -> text r.(2)) in
    let actual_unique = rows t ("PRAGMA index_list(" ^ table ^ ")") []
      |> List.filter (fun r -> text r.(3) = "u")
      |> List.map (fun r -> index_columns (text r.(1)))
      |> List.sort compare in
    if List.map (fun r -> text r.(1)) info <> expected ||
       actual_key <> key || actual_unique <> List.sort compare unique then
      fail ("incompatible table: " ^ table) in
  let expect_index name =
    match rows t "SELECT 1 FROM sqlite_master WHERE type='index' AND name=?"
      [s name] with
    | [_] -> ()
    | _ -> fail ("missing index: " ^ name) in
  let scope = ["endpoint";"account";"mailbox_key"] in
  let epoch_uid = scope @ ["uidvalidity";"uid"] in
  expect_table "mailboxes" ~key:scope (scope @ [
    "raw_name";"encoding";"mailbox_id";"phase";"uidvalidity";
    "generation";"revision";"anchor";"frontier";"inventory_ref";"mode"]);
  expect_table "snapshots" ~key:epoch_uid (epoch_uid @ ["modseq"]);
  expect_table "snapshot_flags" ~key:(epoch_uid @ ["ord"])
    (epoch_uid @ ["ord";"flag"]);
  expect_table "intents" ~key:["id"] ["id";"endpoint";"account";"mailbox_key";
    "raw_name";"encoding";"mailbox_id";"kind";"message_id";
    "digest";"spool_ref";"state";"uidvalidity";"uid";
    "pre_send_frontier";"expected_length";"expected_flags_known";
    "expected_internal_date"];
  expect_table "intent_flags" ~key:["intent_id";"ord"]
    ["intent_id";"ord";"flag"];
  expect_table "blob_refs" ~key:epoch_uid (epoch_uid @ ["sha256";"length"]);
  expect_table "scan_stages" ~key:["id"] ["id";"endpoint";"account";
    "mailbox_key";"raw_name";"encoding";"mailbox_id";"uidvalidity";
    "upper_uid";"expected_revision";"fetch_upper";"search_upper"];
  expect_table "scan_rows" ~key:["stage_id";"uid"]
    ["stage_id";"uid";"modseq";"seen"];
  expect_table "scan_flags" ~key:["stage_id";"uid";"ord"]
    ["stage_id";"uid";"ord";"flag"];
  expect_table "sync_pairs" ~key:["id"] (["id";"endpoint";"account";
    "mailbox_key";"raw_name";"encoding";"mailbox_id";"remote_epoch";
    "remote_uid";"local_id";"revision";"remote_tombstone_kind";
    "remote_tombstone_evidence";"remote_tombstone_generation";
    "local_tombstone_kind";"local_tombstone_evidence";
    "local_tombstone_generation";"content_sha256";"content_length"] @
    (if version>=10L then ["internal_date"] else []));
  expect_table "sync_pair_flags" ~key:["pair_id";"ord"]
    ["pair_id";"ord";"flag"];
  expect_table "sync_conflicts" ~key:["id"] ["id";"pair_id";"kind";
    "evidence";"pair_revision";"resolved"];
  expect_table "sync_operations" ~key:["id"] ["id";"pair_id";"local_id";
    "endpoint";"account";"mailbox_key";"raw_name";"encoding";"mailbox_id";
    "kind";"state";"source_epoch";"source_uid";"dest_endpoint";
    "dest_account";"dest_mailbox_key";"dest_raw_name";"dest_encoding";
    "dest_mailbox_id";"dest_epoch";"receipt_epoch";"receipt_uid";
    "blob_sha256";"blob_length";"desired_flags_known";"receipt"];
  expect_table "sync_operation_flags" ~key:["operation_id";"ord"]
    ["operation_id";"ord";"flag"];
  expect_table "sync_operation_preconditions" ~key:["operation_id"]
    ["operation_id";"pair_revision"];
  expect_table "sync_operation_local_preimages" ~key:["operation_id"]
    ["operation_id"];
  expect_table "sync_operation_local_preimage_flags"
    ~key:["operation_id";"ord"] ["operation_id";"ord";"flag"];
  if version>=9L then expect_table "sync_operation_local_sources"
    ~key:["operation_id"] ["operation_id";"mtime"];
  if version>=11L then expect_table "sync_operation_source_dates"
    ~key:["operation_id"] ["operation_id";"internal_date"];
  if version>=12L then expect_table "mailbox_object_ids" ~key:scope
    ~unique:[["endpoint";"account";"account_id";"mailbox_id"]]
    (scope @ ["raw_name";"encoding";"account_id";"mailbox_id"]);
  if version>=13L then expect_table "sync_pair_presence"
    ~key:["pair_id";"side"] ["pair_id";"side";"generation"];
  (* The scope/ID and pair/ID operation indexes were added to existing v7
     databases without a schema bump. A read-only opener cannot create them. *)
  List.iter expect_index ["sync_pairs_scope";"sync_conflicts_open"];
  let compact sql =
    String.to_seq sql |> Seq.filter (function
      | ' ' | '\t' | '\r' | '\n' | ';' -> false | _ -> true)
    |> String.of_seq |> String.uppercase_ascii in
  List.iter (fun (name,columns,predicate) ->
    let expected="CREATE UNIQUE INDEX " ^ name ^ " ON sync_pairs (" ^
      columns ^ ") WHERE " ^ predicate in
    match rows t "SELECT sql FROM sqlite_master WHERE type='index' AND name=?" [s name] with
    | [r] when compact (text r.(0))=compact expected -> ()
    | _ -> fail ("incompatible unique occurrence index: " ^ name))
    ["sync_pairs_remote","endpoint,account,mailbox_key,remote_epoch,remote_uid",
       "remote_uid IS NOT NULL";
     "sync_pairs_local","endpoint,account,mailbox_key,local_id","local_id IS NOT NULL"];
  version

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
  let t = { db; mutex = Eio.Mutex.create (); blob_dir = None;
            schema_version=0L } in
  transaction ~begin_sql:"BEGIN" t (fun () ->
  {t with schema_version=validate_schema t}))

let open_path ~sw ?blob_dir path =
  let blob_dir = Option.map (fun dir ->
    if not (Eio.Path.is_directory dir) then
      invalid_arg "Imap_store.open_path: blob directory must exist";
    ignore (Eio.Path.native_exn dir : string);
    Dir dir) blob_dir in
  let db = SE.open_path ~sw ~busy_timeout:5000 path in
  initialize db (fun () ->
  let t = { db; mutex = Eio.Mutex.create (); blob_dir;
            schema_version=current_version } in
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
  let version = user_version t in
  if version > current_version || version < 0L then
    fail "unsupported schema version";
  if version = 0L && rows t "SELECT name FROM sqlite_master WHERE \
    type='table' AND name NOT LIKE 'sqlite\\_%' ESCAPE '\\'" [] <> [] then
    fail "unversioned database already contains tables";
  if version = 0L then (
    sql t "CREATE TABLE mailboxes ( \
      endpoint TEXT NOT NULL, account TEXT NOT NULL, mailbox_key TEXT NOT NULL, \
      raw_name TEXT NOT NULL, encoding TEXT NOT NULL, mailbox_id TEXT, \
      phase INTEGER NOT NULL, uidvalidity INTEGER, generation INTEGER NOT NULL, \
      revision INTEGER NOT NULL, anchor INTEGER, frontier INTEGER NOT NULL, \
      inventory_ref TEXT, mode INTEGER NOT NULL, \
      PRIMARY KEY(endpoint,account,mailbox_key))";
    sql t "CREATE TABLE snapshots ( \
      endpoint TEXT NOT NULL, account TEXT NOT NULL, mailbox_key TEXT NOT NULL, \
      uidvalidity INTEGER NOT NULL, uid INTEGER NOT NULL, modseq INTEGER, \
      PRIMARY KEY(endpoint,account,mailbox_key,uidvalidity,uid))";
    sql t "CREATE TABLE snapshot_flags ( \
      endpoint TEXT NOT NULL, account TEXT NOT NULL, mailbox_key TEXT NOT NULL, \
      uidvalidity INTEGER NOT NULL, uid INTEGER NOT NULL, ord INTEGER NOT NULL, \
      flag TEXT NOT NULL, \
      PRIMARY KEY(endpoint,account,mailbox_key,uidvalidity,uid,ord), \
      FOREIGN KEY(endpoint,account,mailbox_key,uidvalidity,uid) \
        REFERENCES snapshots(endpoint,account,mailbox_key,uidvalidity,uid) \
        ON DELETE CASCADE)";
    sql t "CREATE TABLE intents ( \
      id TEXT PRIMARY KEY, endpoint TEXT NOT NULL, account TEXT NOT NULL, \
      mailbox_key TEXT NOT NULL, raw_name TEXT NOT NULL, encoding TEXT NOT NULL, \
      mailbox_id TEXT, kind TEXT NOT NULL, message_id TEXT, digest TEXT, \
      spool_ref TEXT, state TEXT NOT NULL, uidvalidity INTEGER, uid INTEGER)";
    sql t "CREATE INDEX intents_pending ON intents \
      (endpoint,account,mailbox_key,state)";
    sql t "PRAGMA user_version=1");
  if version <= 1L then (
    sql t "CREATE TABLE blob_refs ( \
      endpoint TEXT NOT NULL, account TEXT NOT NULL, mailbox_key TEXT NOT NULL, \
      uidvalidity INTEGER NOT NULL, uid INTEGER NOT NULL, \
      sha256 TEXT NOT NULL, length INTEGER NOT NULL, \
      PRIMARY KEY(endpoint,account,mailbox_key,uidvalidity,uid))";
    sql t "CREATE INDEX blob_refs_hash ON blob_refs(sha256)";
    sql t "PRAGMA user_version=2");
  if version <= 2L then (
    sql t "ALTER TABLE intents ADD COLUMN pre_send_frontier INTEGER";
    sql t "ALTER TABLE intents ADD COLUMN expected_length INTEGER";
    sql t "ALTER TABLE intents ADD COLUMN expected_flags_known INTEGER";
    sql t "ALTER TABLE intents ADD COLUMN expected_internal_date TEXT";
    sql t "CREATE TABLE intent_flags ( \
      intent_id TEXT NOT NULL, ord INTEGER NOT NULL, flag TEXT NOT NULL, \
      PRIMARY KEY(intent_id,ord), \
      FOREIGN KEY(intent_id) REFERENCES intents(id) ON DELETE CASCADE)";
    sql t "PRAGMA user_version=3");
  if version <= 3L then (
    sql t "CREATE TABLE scan_stages ( \
      id TEXT PRIMARY KEY, endpoint TEXT NOT NULL, account TEXT NOT NULL, \
      mailbox_key TEXT NOT NULL, raw_name TEXT NOT NULL, encoding TEXT NOT NULL, \
      mailbox_id TEXT, uidvalidity INTEGER NOT NULL, upper_uid INTEGER NOT NULL, \
      expected_revision INTEGER NOT NULL, fetch_upper INTEGER NOT NULL DEFAULT 0, \
      search_upper INTEGER NOT NULL DEFAULT 0)";
    sql t "CREATE TABLE scan_rows ( \
      stage_id TEXT NOT NULL, uid INTEGER NOT NULL, modseq INTEGER, seen INTEGER NOT NULL DEFAULT 0, \
      PRIMARY KEY(stage_id,uid), \
      FOREIGN KEY(stage_id) REFERENCES scan_stages(id) ON DELETE CASCADE)";
    sql t "CREATE TABLE scan_flags ( \
      stage_id TEXT NOT NULL, uid INTEGER NOT NULL, ord INTEGER NOT NULL, flag TEXT NOT NULL, \
      PRIMARY KEY(stage_id,uid,ord), \
      FOREIGN KEY(stage_id,uid) REFERENCES scan_rows(stage_id,uid) ON DELETE CASCADE)";
    sql t "PRAGMA user_version=4");
  if version <= 4L then (
    sql t "CREATE TABLE sync_pairs ( \
      id TEXT PRIMARY KEY, endpoint TEXT NOT NULL, account TEXT NOT NULL, \
      mailbox_key TEXT NOT NULL, raw_name TEXT NOT NULL, encoding TEXT NOT NULL, \
      mailbox_id TEXT, remote_epoch INTEGER, remote_uid INTEGER, local_id TEXT, \
      revision INTEGER NOT NULL, remote_tombstone_kind TEXT, \
      remote_tombstone_evidence TEXT, remote_tombstone_generation INTEGER, \
      local_tombstone_kind TEXT, local_tombstone_evidence TEXT, \
      local_tombstone_generation INTEGER, \
      CHECK ((remote_epoch IS NULL) = (remote_uid IS NULL)), \
      CHECK (remote_uid IS NOT NULL OR local_id IS NOT NULL))";
    sql t "CREATE UNIQUE INDEX sync_pairs_remote ON sync_pairs \
      (endpoint,account,mailbox_key,remote_epoch,remote_uid) \
      WHERE remote_uid IS NOT NULL";
    sql t "CREATE UNIQUE INDEX sync_pairs_local ON sync_pairs \
      (endpoint,account,mailbox_key,local_id) WHERE local_id IS NOT NULL";
    sql t "CREATE INDEX sync_pairs_scope ON sync_pairs \
      (endpoint,account,mailbox_key)";
    sql t "CREATE TABLE sync_pair_flags ( \
      pair_id TEXT NOT NULL, ord INTEGER NOT NULL, flag TEXT NOT NULL, \
      PRIMARY KEY(pair_id,ord), \
      FOREIGN KEY(pair_id) REFERENCES sync_pairs(id) ON DELETE CASCADE)";
    sql t "CREATE TABLE sync_conflicts ( \
      id TEXT PRIMARY KEY, pair_id TEXT NOT NULL, kind TEXT NOT NULL, \
      evidence TEXT NOT NULL, pair_revision INTEGER NOT NULL, \
      resolved INTEGER NOT NULL DEFAULT 0, \
      FOREIGN KEY(pair_id) REFERENCES sync_pairs(id) ON DELETE RESTRICT)";
    sql t "CREATE INDEX sync_conflicts_open ON sync_conflicts(pair_id,resolved)";
    sql t "CREATE TABLE sync_operations ( \
      id TEXT PRIMARY KEY, pair_id TEXT, local_id TEXT, endpoint TEXT NOT NULL, \
      account TEXT NOT NULL, mailbox_key TEXT NOT NULL, raw_name TEXT NOT NULL, \
      encoding TEXT NOT NULL, mailbox_id TEXT, kind TEXT NOT NULL, state TEXT NOT NULL, \
      source_epoch INTEGER, source_uid INTEGER, \
      dest_endpoint TEXT, dest_account TEXT, dest_mailbox_key TEXT, \
      dest_raw_name TEXT, dest_encoding TEXT, dest_mailbox_id TEXT, \
      dest_epoch INTEGER, receipt_epoch INTEGER, receipt_uid INTEGER, \
      blob_sha256 TEXT, blob_length INTEGER, desired_flags_known INTEGER, \
      receipt TEXT, \
      FOREIGN KEY(pair_id) REFERENCES sync_pairs(id) ON DELETE RESTRICT)";
    sql t "CREATE INDEX sync_operations_pending ON sync_operations \
      (endpoint,account,mailbox_key,state)";
    sql t "CREATE TABLE sync_operation_flags ( \
      operation_id TEXT NOT NULL, ord INTEGER NOT NULL, flag TEXT NOT NULL, \
      PRIMARY KEY(operation_id,ord), \
      FOREIGN KEY(operation_id) REFERENCES sync_operations(id) ON DELETE CASCADE)";
    sql t "PRAGMA user_version=5");
  if version <= 5L then (
    sql t "CREATE TABLE sync_operation_preconditions ( \
      operation_id TEXT PRIMARY KEY, pair_revision INTEGER NOT NULL, \
      FOREIGN KEY(operation_id) REFERENCES sync_operations(id) ON DELETE CASCADE)";
    (* A v5 pending operation has no saved pair revision. It must remain
       unreconciled until an operator can establish its original baseline. *)
    sql t "PRAGMA user_version=6");
  if version <= 6L then (
    sql t "ALTER TABLE sync_pairs ADD COLUMN content_sha256 TEXT";
    sql t "ALTER TABLE sync_pairs ADD COLUMN content_length INTEGER";
    sql t "PRAGMA user_version=7");
  if version <= 7L then (
    sql t "CREATE TABLE sync_operation_local_preimages ( \
      operation_id TEXT PRIMARY KEY, \
      FOREIGN KEY(operation_id) REFERENCES sync_operations(id) ON DELETE CASCADE)";
    sql t "CREATE TABLE sync_operation_local_preimage_flags ( \
      operation_id TEXT NOT NULL, ord INTEGER NOT NULL, flag TEXT NOT NULL, \
      PRIMARY KEY(operation_id,ord), \
      FOREIGN KEY(operation_id) REFERENCES sync_operation_local_preimages(operation_id) ON DELETE CASCADE)";
    sql t "PRAGMA user_version=8");
  if version <= 8L then (
    sql t "CREATE TABLE sync_operation_local_sources ( \
      operation_id TEXT PRIMARY KEY, mtime REAL NOT NULL, \
      FOREIGN KEY(operation_id) REFERENCES sync_operations(id) ON DELETE CASCADE)";
    sql t "PRAGMA user_version=9");
  if version <= 9L then (
    sql t "ALTER TABLE sync_pairs ADD COLUMN internal_date TEXT";
    sql t "PRAGMA user_version=10");
  if version <= 10L then (
    sql t "CREATE TABLE sync_operation_source_dates ( \
      operation_id TEXT PRIMARY KEY, internal_date TEXT NOT NULL, \
      FOREIGN KEY(operation_id) REFERENCES sync_operations(id) ON DELETE CASCADE)";
    sql t "PRAGMA user_version=11");
  if version <= 11L then (
    sql t "CREATE TABLE mailbox_object_ids ( \
      endpoint TEXT NOT NULL, account TEXT NOT NULL, mailbox_key TEXT NOT NULL, \
      raw_name TEXT NOT NULL, encoding TEXT NOT NULL, \
      account_id TEXT NOT NULL, mailbox_id TEXT NOT NULL, \
      PRIMARY KEY(endpoint,account,mailbox_key), \
      UNIQUE(endpoint,account,account_id,mailbox_id))";
    sql t "PRAGMA user_version=12");
  if version <= 12L then (
    sql t "CREATE TABLE sync_pair_presence ( \
      pair_id TEXT NOT NULL, side TEXT NOT NULL, generation INTEGER NOT NULL, \
      PRIMARY KEY(pair_id,side), \
      CHECK (side IN ('remote','local')), CHECK (generation >= 0), \
      FOREIGN KEY(pair_id) REFERENCES sync_pairs(id) ON DELETE CASCADE)";
    sql t "PRAGMA user_version=13");
  (* Auxiliary indexes do not alter the persisted row representation. *)
  sql t "CREATE INDEX IF NOT EXISTS sync_operations_scope_id ON \
    sync_operations(endpoint,account,mailbox_key,id)";
  sql t "CREATE INDEX IF NOT EXISTS sync_operations_pair_id ON \
    sync_operations(pair_id,id)";
  sql t "CREATE INDEX IF NOT EXISTS sync_pairs_scope_id ON \
    sync_pairs(endpoint,account,mailbox_key,id)";
  sql t "CREATE INDEX IF NOT EXISTS sync_operations_blob_pending ON \
    sync_operations(blob_sha256) WHERE state NOT IN ('committed','rejected')";
  sql t "CREATE INDEX IF NOT EXISTS intents_blob_pending ON \
    intents(digest) WHERE state NOT IN ('confirmed','rejected')";
  ignore (validate_schema t : int64));
  t)
