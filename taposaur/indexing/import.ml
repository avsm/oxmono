module A = Taposaur_archive
module C = Taposaur_catalog

type result = Unchanged | Loaded of int | Failed of string
let stamp archive shard =
  let size, mtime = A.shard_stat archive shard in
  { C.Shard.size; mtime }

let shard ?(force = false) catalog archive ~shard =
  let work () =
    let before = stamp archive shard in
    if not force && C.Shard.unchanged catalog ~shard before then Unchanged else
    let count = C.Shard.replace catalog ~shard before (fun emit ->
      A.iter_index archive ~shard (fun _ row -> emit row);
      if stamp archive shard <> before then
        failwith "Shard changed while it was being read; retry import") in
    Loaded count in
  let failed message = C.Shard.record_error catalog ~shard message;
    Failed message in
  try work () with
  | A.Parse_error e -> failed (A.error_to_string e)
  | Failure e | Invalid_argument e -> failed e
  | Sqlite3.Error e | Sqlite3.SqliteError e -> failed e
  | Eio.Io _ as ex -> failed (Printexc.to_string ex)

type stats = { scanned : int; loaded : int; unchanged : int; failed : int }
let sync ?(force = false) ?(limit = max_int) ?(prefix = "")
    ?(progress = fun _ _ -> ()) catalog archive =
  if limit < 0 then invalid_arg "limit must be nonnegative";
  let scanned = ref 0 and attempted = ref 0 in
  let loaded = ref 0 and unchanged = ref 0 and failed = ref 0 in
  A.iter_shards archive (fun path ->
    incr scanned;
    if !attempted < limit && String.starts_with ~prefix path then begin
      let result = shard ~force catalog archive ~shard:path in
      (match result with
       | Loaded _ -> incr attempted; incr loaded
       | Unchanged -> incr unchanged
       | Failed _ -> incr attempted; incr failed);
      progress path result
    end);
  C.Shard.set_discovered catalog !scanned;
  { scanned = !scanned; loaded = !loaded; unchanged = !unchanged;
    failed = !failed }
