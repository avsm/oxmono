(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let schema_version = 2

type t = {
  sw : Eio.Switch.t;
  path : Eio.Fs.dir_ty Eio.Path.t;
  dir : Eio.Fs.dir_ty Eio.Path.t;
  stem : string;
  db : Sqlite3_eio.t;
  mutable reader : Sqlite3_eio.t option;
  mutable attached : string list;  (** File names attached to the reader. *)
  mutable ignored : string list;  (** File names found not to be archives. *)
}

type archive = {
  file : string;
  range_start : float;
  range_end : float;
  rows : int;
}

type rotation = { moved : int; written : string list }

(* The archive directory is the live file's directory, and archives are
   named from the live file's stem, so [access.db] has [access-2025.db]. *)
let split_path path =
  match Eio.Path.split path with
  | None -> invalid_arg "Proffer_log: path has no directory"
  | Some (dir, name) ->
    let stem =
      match Filename.chop_suffix_opt ~suffix:".db" name with
      | Some s -> s
      | None -> name
    in
    (dir, stem)

(** {1 Schema} *)

let columns =
  "timestamp, remote_addr, forwarded_for, forwarded_proto, method, target, \
   path, host, user_agent, referer, accept, request_headers, status_code, \
   response_content_type, response_body_size, cache_status, duration_us"

(* Every statement takes the schema name of the file it addresses, so the
   same SQL initialises the live file and an attached archive. *)
let schema_sql s = [
  Printf.sprintf {|CREATE TABLE IF NOT EXISTS %s.requests (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  timestamp REAL NOT NULL,
  remote_addr TEXT NOT NULL,
  forwarded_for TEXT,
  forwarded_proto TEXT,
  method TEXT NOT NULL,
  target TEXT NOT NULL,
  path TEXT NOT NULL,
  host TEXT,
  user_agent TEXT,
  referer TEXT,
  accept TEXT,
  request_headers TEXT,
  status_code INTEGER NOT NULL,
  response_content_type TEXT,
  response_body_size INTEGER NOT NULL,
  cache_status TEXT,
  duration_us INTEGER NOT NULL
)|} s;
  Printf.sprintf
    "CREATE TABLE IF NOT EXISTS %s.log_meta \
     (key TEXT PRIMARY KEY, value TEXT NOT NULL)" s;
  (* status_code and cache_status have a handful of distinct values, so an
     index on either narrows nothing, and an earlier schema's indexes on
     them misled the planner. *)
  Printf.sprintf "DROP INDEX IF EXISTS %s.idx_requests_status" s;
  Printf.sprintf "DROP INDEX IF EXISTS %s.idx_requests_cache" s;
  Printf.sprintf
    "CREATE INDEX IF NOT EXISTS %s.idx_requests_timestamp \
     ON requests(timestamp)" s;
  Printf.sprintf
    "CREATE INDEX IF NOT EXISTS %s.idx_requests_path ON requests(path)" s;
  Printf.sprintf
    "CREATE INDEX IF NOT EXISTS %s.idx_requests_status_ts \
     ON requests(status_code, timestamp)" s;
  (* Covers every column the usual aggregates read, so they never touch
     the table, whose rows are dominated by the request header JSON. *)
  Printf.sprintf
    "CREATE INDEX IF NOT EXISTS %s.idx_requests_metrics \
     ON requests(timestamp, status_code, cache_status, duration_us, \
     response_body_size)" s;
]

let check rc = Sqlite3.Rc.check rc

let exec db sql = check (Sqlite3_eio.exec db sql)

(* [query db sql ~f ~init] folds [f] over the rows of [sql]. *)
let query db sql ~f ~init =
  let stmt = Sqlite3_eio.prepare db sql in
  let _rc, v = Sqlite3_eio.fold db stmt ~f ~init in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  v

let query_int db sql =
  query db sql ~init:0 ~f:(fun _ row ->
    match row.(0) with
    | Sqlite3.Data.INT i -> Int64.to_int i
    | Sqlite3.Data.FLOAT f -> Float.to_int f
    | _ -> 0)

let query_float db sql =
  query db sql ~init:0.0 ~f:(fun _ row ->
    match row.(0) with
    | Sqlite3.Data.FLOAT f -> f
    | Sqlite3.Data.INT i -> Int64.to_float i
    | _ -> 0.0)

let user_version db s =
  query_int db (Printf.sprintf "PRAGMA %s.user_version" s)

(* Brings the file named [s] on connection [db] to the current schema. A
   file from before [log_meta] existed has the same [requests] columns, so
   the same statements migrate it as create a fresh one: every statement
   is idempotent and none touches a row. *)
let init_schema db s ~role =
  let v = user_version db s in
  if v > schema_version then
    failwith
      (Printf.sprintf
         "Proffer_log: schema version %d is newer than this library's %d"
         v schema_version);
  exec db "BEGIN IMMEDIATE";
  List.iter (exec db) (schema_sql s);
  exec db
    (Printf.sprintf
       "INSERT OR REPLACE INTO %s.log_meta (key, value) VALUES ('schema', '%d')"
       s schema_version);
  exec db
    (Printf.sprintf
       "INSERT OR IGNORE INTO %s.log_meta (key, value) VALUES ('role', '%s')"
       s role);
  exec db (Printf.sprintf "PRAGMA %s.user_version = %d" s schema_version);
  exec db "COMMIT"

(* Sampled statistics give the planner the cardinalities it needs at a
   cost independent of the table size. A full scan of a multi-gigabyte log
   took the better part of a minute on a cold cache. *)
let analyze db = exec db "PRAGMA analysis_limit=1000"; exec db "ANALYZE"

let open_writer ~sw path =
  let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000 path in
  (* Takes effect only on a file whose header is not yet written, so it
     comes before the journal mode, which writes it. An existing file
     switches when [vacuum] rebuilds it. *)
  exec db "PRAGMA auto_vacuum=INCREMENTAL";
  exec db "PRAGMA journal_mode=WAL";
  exec db "PRAGMA synchronous=NORMAL";
  init_schema db "main" ~role:"live";
  analyze db;
  db

let open_ ~sw path =
  let path = (path :> Eio.Fs.dir_ty Eio.Path.t) in
  let dir, stem = split_path path in
  let db = open_writer ~sw path in
  { sw; path; dir; stem; db; reader = None; attached = []; ignored = [] }

(** {1 Recording} *)

let header_pair_jsont : (string * string) Jsont.t =
  Jsont.map ~kind:"header_pair"
    ~dec:(function [n; v] -> (n, v) | _ -> ("", ""))
    ~enc:(fun (n, v) -> [n; v])
    (Jsont.list Jsont.string)

let headers_jsont : (string * string) list Jsont.t =
  Jsont.list header_pair_jsont

let encode_headers headers =
  match Jsont_bytesrw.encode_string headers_jsont headers with
  | Ok s -> s
  | Error _ -> "[]"

let header headers name =
  List.find_map
    (fun (k, v) ->
      if String.lowercase_ascii k = name then Some v else None)
    headers

let insert_sql =
  Printf.sprintf
    "INSERT INTO requests (%s) VALUES \
     (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14, ?15, \
     ?16, ?17)"
    columns

let bind_opt stmt pos v =
  check (Sqlite3.bind stmt pos (Sqlite3.Data.opt_text v))

let record t ~timestamp (event : Proffer_httpz.event) =
  let headers = event.request_headers in
  (* Prepare a fresh statement per insert. Concurrent fibers from keep-alive
     connections would race on a shared prepared statement. *)
  let stmt = Sqlite3_eio.prepare t.db insert_sql in
  check (Sqlite3.bind_double stmt 1 timestamp);
  check (Sqlite3.bind_text stmt 2 event.remote_addr);
  bind_opt stmt 3 (header headers "x-forwarded-for");
  bind_opt stmt 4 (header headers "x-forwarded-proto");
  check (Sqlite3.bind_text stmt 5 (Proffer.Method.to_string event.meth));
  check (Sqlite3.bind_text stmt 6 event.target);
  check (Sqlite3.bind_text stmt 7 event.path);
  bind_opt stmt 8 (header headers "host");
  bind_opt stmt 9 (header headers "user-agent");
  bind_opt stmt 10 (header headers "referer");
  bind_opt stmt 11 (header headers "accept");
  check (Sqlite3.bind_text stmt 12 (encode_headers headers));
  check (Sqlite3.bind_int stmt 13 (Proffer.Status.code event.status));
  bind_opt stmt 14 event.response_content_type;
  check (Sqlite3.bind_int stmt 15 event.body_size);
  bind_opt stmt 16 event.cache_status;
  check (Sqlite3.bind_int stmt 17 event.duration_us);
  let rc = Sqlite3_eio.step t.db stmt in
  ignore (Sqlite3_eio.finalize t.db stmt : Sqlite3.Rc.t);
  match rc with Sqlite3.Rc.DONE -> () | rc -> check rc

(** {1 Archives} *)

let archive_file stem suffix = Printf.sprintf "%s-%s.db" stem suffix

(* The schema name an archive attaches as, from the part of its file name
   after the stem, with every character SQLite would not accept in an
   identifier replaced. *)
let alias_of_file stem file =
  let prefix = stem ^ "-" in
  let suffix =
    String.sub file (String.length prefix)
      (String.length file - String.length prefix - 3)
  in
  "a_"
  ^ String.map
      (fun c ->
        match c with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> c
        | _ -> '_')
      suffix

let is_archive stem file =
  let prefix = stem ^ "-" in
  String.length file > String.length prefix + 3
  && String.starts_with ~prefix file
  && String.ends_with ~suffix:".db" file

(* Archive file names in the directory, sorted so that attach order and
   view order follow the years. *)
let list_archives dir stem =
  Eio.Path.read_dir dir |> List.filter (is_archive stem) |> List.sort compare

let attach db dir file alias =
  let native = Eio.Path.native_exn Eio.Path.(dir / file) in
  Sqlite3_eio.run db ~label:"sqlite3_attach" (fun raw ->
    let stmt =
      Sqlite3.prepare raw (Printf.sprintf "ATTACH DATABASE ?1 AS %s" alias)
    in
    check (Sqlite3.bind_text stmt 1 native);
    (match Sqlite3.step stmt with
     | Sqlite3.Rc.DONE -> ()
     | rc -> ignore (Sqlite3.finalize stmt : Sqlite3.Rc.t); check rc);
    check (Sqlite3.finalize stmt))

let detach db alias = exec db (Printf.sprintf "DETACH DATABASE %s" alias)

(* Attaches [file] as [alias] when it is an archive this library wrote,
   and detaches and declines any other file that shares the name pattern,
   such as a copy made by hand, so the union view never meets a foreign
   schema. *)
let attach_archive db dir file alias =
  attach db dir file alias;
  let role =
    try
      query db
        (Printf.sprintf "SELECT value FROM %s.log_meta WHERE key = 'role'" alias)
        ~init:None ~f:(fun _ row ->
          match row.(0) with Sqlite3.Data.TEXT s -> Some s | _ -> None)
    with Eio.Exn.Io _ -> None
  in
  role = Some "archive" || (detach db alias; false)

(* The view the analytics read. A WHERE clause on it is pushed into every
   arm, so a range query uses each file's timestamp index. *)
let define_view db stem files =
  exec db "DROP VIEW IF EXISTS temp.requests_all";
  let arms =
    "SELECT * FROM main.requests"
    :: List.map
         (fun f -> Printf.sprintf "SELECT * FROM %s.requests" (alias_of_file stem f))
         files
  in
  exec db
    ("CREATE TEMP VIEW requests_all AS " ^ String.concat " UNION ALL " arms)

(* [(db, attached, ignored)] for the reader on [path]. *)
let open_reader ~sw dir stem path =
  let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000 ~mode:`READONLY path in
  let files, ignored =
    List.partition
      (fun f -> attach_archive db dir f (alias_of_file stem f))
      (list_archives dir stem)
  in
  define_view db stem files;
  (db, files, ignored)

let reader_of_path ~sw path =
  let path = (path :> Eio.Fs.dir_ty Eio.Path.t) in
  let dir, stem = split_path path in
  let db, _, _ = open_reader ~sw dir stem path in
  db

let reader t =
  match t.reader with
  | Some db -> db
  | None ->
    let db, files, ignored = open_reader ~sw:t.sw t.dir t.stem t.path in
    t.reader <- Some db;
    t.attached <- files;
    t.ignored <- ignored;
    db

let refresh t =
  let db = reader t in
  let present = list_archives t.dir t.stem in
  let gone = List.filter (fun f -> not (List.mem f present)) t.attached in
  let fresh =
    List.filter
      (fun f -> not (List.mem f t.attached || List.mem f t.ignored))
      present
  in
  if gone <> [] || fresh <> [] then begin
    List.iter (fun f -> detach db (alias_of_file t.stem f)) gone;
    let added, ignored =
      List.partition (fun f -> attach_archive db t.dir f (alias_of_file t.stem f)) fresh
    in
    let kept = List.filter (fun f -> not (List.mem f gone)) t.attached in
    let files = List.sort compare (kept @ added) in
    define_view db t.stem files;
    t.attached <- files;
    t.ignored <- List.filter (fun f -> List.mem f present) t.ignored @ ignored
  end

let archives t =
  refresh t;
  let db = reader t in
  List.map
    (fun file ->
      let a = alias_of_file t.stem file in
      let range_start =
        query_float db (Printf.sprintf "SELECT MIN(timestamp) FROM %s.requests" a)
      in
      let range_end =
        query_float db (Printf.sprintf "SELECT MAX(timestamp) FROM %s.requests" a)
      in
      let rows = query_int db (Printf.sprintf "SELECT COUNT(*) FROM %s.requests" a) in
      { file; range_start; range_end; rows })
    t.attached

let live_rows t = query_int t.db "SELECT COUNT(*) FROM main.requests"

(** {1 Rotation} *)

(* Runs [f] inside one write transaction on the writer. *)
let transaction db f =
  exec db "BEGIN IMMEDIATE";
  match f () with
  | v -> exec db "COMMIT"; v
  | exception e -> (try exec db "ROLLBACK" with _ -> ()); raise e

let bound_count db sql binds =
  Sqlite3_eio.run db (fun raw ->
    let stmt = Sqlite3.prepare raw sql in
    List.iteri (fun i d -> check (Sqlite3.bind stmt (i + 1) d)) binds;
    (match Sqlite3.step stmt with
     | Sqlite3.Rc.DONE -> ()
     | rc -> ignore (Sqlite3.finalize stmt : Sqlite3.Rc.t); check rc);
    check (Sqlite3.finalize stmt);
    Sqlite3.changes raw)

let bound_strings db sql binds =
  Sqlite3_eio.run db (fun raw ->
    let stmt = Sqlite3.prepare raw sql in
    List.iteri (fun i d -> check (Sqlite3.bind stmt (i + 1) d)) binds;
    let rec loop acc =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
        let acc =
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT s -> s :: acc
          | _ -> acc
        in
        loop acc
      | Sqlite3.Rc.DONE -> List.rev acc
      | rc -> ignore (Sqlite3.finalize stmt : Sqlite3.Rc.t); check rc; []
    in
    let rows = loop [] in
    check (Sqlite3.finalize stmt);
    rows)

let rotate t ~before ?(batch = 20000) ?(progress = fun _ -> ()) () =
  (* A writer connection of its own, so an insert through [t.db] during a
     batch stays outside the batch transaction and waits only for its
     delete. *)
  Eio.Switch.run @@ fun sw ->
  let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000 t.path in
  let where = " WHERE id > ?1 AND id <= ?2 AND timestamp < ?3" in
  let year_where = where ^ " AND strftime('%Y', timestamp, 'unixepoch') = ?4" in
  let bounds =
    query db
      (Printf.sprintf
         "SELECT MIN(id), MAX(id) FROM main.requests WHERE timestamp < %.17g"
         before)
      ~init:None ~f:(fun _ row ->
        match row.(0), row.(1) with
        | Sqlite3.Data.INT lo, Sqlite3.Data.INT hi ->
          Some (Int64.to_int lo, Int64.to_int hi)
        | _ -> None)
  in
  let result =
    match bounds with
    | None -> { moved = 0; written = [] }
    | Some (lo, hi) ->
      let attached = ref [] in
      let ensure_archive year =
        let file = archive_file t.stem year in
        let alias = alias_of_file t.stem file in
        if not (List.mem_assoc file !attached) then begin
          attach db t.dir file alias;
          exec db (Printf.sprintf "PRAGMA %s.journal_mode=WAL" alias);
          init_schema db alias ~role:"archive";
          attached := (file, alias) :: !attached
        end;
        alias
      in
      let moved = ref 0 in
      let last = ref (lo - 1) in
      Fun.protect
        ~finally:(fun () -> List.iter (fun (_, alias) -> detach db alias) !attached)
        (fun () ->
          while !last < hi do
            let top = !last + batch in
            let binds =
              Sqlite3.Data.[ INT (Int64.of_int !last); INT (Int64.of_int top);
                             FLOAT before ]
            in
            let years =
              bound_strings db
                ("SELECT DISTINCT strftime('%Y', timestamp, 'unixepoch') \
                  FROM main.requests" ^ where ^ " ORDER BY 1")
                binds
            in
            (* An archive is attached outside the batch transaction, since
               ATTACH is refused inside one. *)
            let aliases = List.map (fun y -> (y, ensure_archive y)) years in
            (* The copy and the delete share one predicate, so a row leaves
               the live file only once it is in an archive. A timestamp that
               yields no year matches neither and stays. *)
            let n =
              transaction db (fun () ->
                List.fold_left
                  (fun acc (year, alias) ->
                    let binds = binds @ [ Sqlite3.Data.TEXT year ] in
                    let copied =
                      bound_count db
                        (Printf.sprintf
                           "INSERT INTO %s.requests (%s) SELECT %s \
                            FROM main.requests%s"
                           alias columns columns year_where)
                        binds
                    in
                    let removed =
                      bound_count db ("DELETE FROM main.requests" ^ year_where) binds
                    in
                    if copied <> removed then
                      failwith
                        (Printf.sprintf
                           "Proffer_log.rotate: copied %d rows to %s but removed %d"
                           copied alias removed);
                    acc + copied)
                  0 aliases)
            in
            moved := !moved + n;
            last := top;
            progress !moved
          done);
      { moved = !moved; written = List.sort compare (List.map fst !attached) }
  in
  if t.reader <> None then refresh t;
  result

(** {1 Space} *)

(* Pages at a time, so each call holds the write lock briefly. *)
let release_step = 1000

let release_space t =
  if query_int t.db "PRAGMA auto_vacuum" <> 2 then 0
  else
    let rec loop freed =
      let before = query_int t.db "PRAGMA freelist_count" in
      if before = 0 then freed
      else begin
        exec t.db (Printf.sprintf "PRAGMA incremental_vacuum(%d)" release_step);
        let after = query_int t.db "PRAGMA freelist_count" in
        if after < before then loop (freed + before - after) else freed
      end
    in
    loop 0

(** {1 Scheduling} *)

let seconds_until ~now hour =
  let day = 86400. in
  let next = (Float.floor (now /. day) *. day) +. (Float.of_int hour *. 3600.) in
  (if next <= now then next +. day else next) -. now

let schedule ~sw ~clock ~mono t ~live_days ~hour
    ?(on_done = fun _ ~freed:_ -> ()) ?(on_error = fun _ -> ()) () =
  if live_days <= 0 then invalid_arg "Proffer_log.schedule: live_days";
  if hour < 0 || hour > 23 then invalid_arg "Proffer_log.schedule: hour";
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec loop () =
      Eio.Time.Mono.sleep mono (seconds_until ~now:(Eio.Time.now clock) hour);
      (match
         let before =
           Eio.Time.now clock -. (Float.of_int live_days *. 86400.)
         in
         let r = rotate t ~before () in
         (r, release_space t)
       with
       | r, freed -> on_done r ~freed
       | exception (Eio.Cancel.Cancelled _ as e) -> raise e
       | exception e -> on_error e);
      loop ()
    in
    loop ())

(** {1 Merge and vacuum} *)

let merge t ~from =
  let from = (from :> Eio.Fs.dir_ty Eio.Path.t) in
  let dir, file =
    match Eio.Path.split from with
    | Some p -> p
    | None -> invalid_arg "Proffer_log.merge: path has no directory"
  in
  attach t.db dir file "src";
  let n =
    Fun.protect
      ~finally:(fun () -> detach t.db "src")
      (fun () ->
        transaction t.db (fun () ->
          bound_count t.db
            (Printf.sprintf
               "INSERT INTO main.requests (%s) SELECT %s FROM src.requests \
                ORDER BY id"
               columns columns)
            []))
  in
  n

let vacuum t =
  exec t.db "PRAGMA auto_vacuum=INCREMENTAL";
  exec t.db "VACUUM"
