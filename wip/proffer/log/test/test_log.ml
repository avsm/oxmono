(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Exercises Proffer_log on files under a fresh temporary directory:
   recording, the union view, rotation into yearly archives while another
   connection inserts, merging back, migration of a pre-schema file and
   refresh after an archive appears. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then begin
    Printf.printf "FAIL: %s\n%!" name;
    exit 1
  end

let event ?(status = 200) path : Proffer_httpz.event =
  { remote_addr = "127.0.0.1:1"; meth = Httpz.Method.Get; target = path; path;
    request_headers = [ ("Host", "example.org"); ("User-Agent", "test") ];
    status = Option.get (Proffer.Status.of_code status);
    response_content_type = Some "text/html"; cache_status = None;
    body_size = 10; duration_us = 100 }

(* Midnight UTC on 1 January of [year], counted in days from the epoch so
   that the local time zone plays no part. *)
let jan1 year =
  let leap y = (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0 in
  let days = ref 0 in
  for y = 1970 to year - 1 do days := !days + if leap y then 366 else 365 done;
  float_of_int !days *. 86400.

let query_int db sql =
  let stmt = Sqlite3_eio.prepare db sql in
  let _rc, v = Sqlite3_eio.fold db stmt ~init:0 ~f:(fun _ row ->
    match row.(0) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0) in
  ignore (Sqlite3_eio.finalize db stmt : Sqlite3.Rc.t);
  v

let v0_schema =
  "CREATE TABLE requests (id INTEGER PRIMARY KEY AUTOINCREMENT, \
   timestamp REAL NOT NULL, remote_addr TEXT NOT NULL, forwarded_for TEXT, \
   forwarded_proto TEXT, method TEXT NOT NULL, target TEXT NOT NULL, \
   path TEXT NOT NULL, host TEXT, user_agent TEXT, referer TEXT, accept TEXT, \
   request_headers TEXT, status_code INTEGER NOT NULL, \
   response_content_type TEXT, response_body_size INTEGER NOT NULL, \
   cache_status TEXT, duration_us INTEGER NOT NULL)"

let v0_row ts path =
  Printf.sprintf
    "INSERT INTO requests (timestamp, remote_addr, method, target, path, \
     status_code, response_body_size, duration_us) \
     VALUES (%.17g, '::1', 'GET', '%s', '%s', 200, 1, 1)" ts path path

let exec db sql = Sqlite3.Rc.check (Sqlite3_eio.exec db sql)

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmp =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "proffer-log-%d" (Unix.getpid ()))
  in
  let dir = Eio.Path.(fs / tmp) in
  Eio.Path.mkdirs ~perm:0o700 dir;
  let path = Eio.Path.(dir / "access.db") in
  Fun.protect ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote tmp)))
  @@ fun () ->
  (* Three years of rows, a hundred per year, then rotation keeps 2026. *)
  Eio.Switch.run (fun sw ->
    let log = Proffer_log.open_ ~sw path in
    List.iter (fun year ->
      for i = 0 to 99 do
        Proffer_log.record log ~timestamp:(jan1 year +. float_of_int (i * 3600))
          (event (Printf.sprintf "/%d/%d" year i))
      done) [ 2024; 2025; 2026 ];
    (* A timestamp no calendar can place must never be dropped. *)
    Proffer_log.record log ~timestamp:1e300 (event "/far");
    check "live rows" (Proffer_log.live_rows log = 301);
    let r = Proffer_log.reader log in
    check "view without archives" (query_int r "SELECT COUNT(*) FROM requests_all" = 301);
    (* A second writer inserts between batches to show rotation does not
       lock it out. *)
    let other = ref 0 in
    let stop = ref false in
    let rot = ref None in
    Eio.Fiber.both
      (fun () ->
        rot := Some (Proffer_log.rotate log ~before:(jan1 2026) ~batch:30 ());
        stop := true)
      (fun () ->
        Eio.Switch.run (fun sw2 ->
          let w = Proffer_log.open_ ~sw:sw2 path in
          while not !stop do
            Proffer_log.record w ~timestamp:(jan1 2026 +. 1e6) (event "/live");
            incr other;
            Eio.Fiber.yield ()
          done));
    let rot = Option.get !rot in
    check "moved two years" (rot.moved = 200);
    check "two archives written"
      (List.sort compare rot.written = [ "access-2024.db"; "access-2025.db" ]);
    check "live keeps retained year, the unplaceable row and new inserts"
      (Proffer_log.live_rows log = 101 + !other);
    check "other writer progressed" (!other > 0);
    Proffer_log.refresh log;
    let r = Proffer_log.reader log in
    check "view spans archives"
      (query_int r "SELECT COUNT(*) FROM requests_all" = 301 + !other);
    check "range query crosses files"
      (query_int r
         (Printf.sprintf
            "SELECT COUNT(*) FROM requests_all WHERE timestamp >= %.17g AND timestamp < %.17g"
            (jan1 2025) (jan1 2026 +. 1.)) = 101);
    let archives = Proffer_log.archives log in
    check "archive rows" (List.map (fun a -> a.Proffer_log.rows) archives = [ 100; 100 ]);
    check "archive range"
      (match archives with
       | a :: _ -> a.range_start = jan1 2024 && a.range_end = jan1 2024 +. 99. *. 3600.
       | [] -> false);
    (* Rotation again moves nothing. *)
    let again = Proffer_log.rotate log ~before:(jan1 2026) () in
    check "idempotent" (again.moved = 0);
    (* Merge an archive back. *)
    let n = Proffer_log.merge log ~from:Eio.Path.(dir / "access-2024.db") in
    check "merged rows" (n = 100);
    check "live after merge" (Proffer_log.live_rows log = 201 + !other);
    Proffer_log.vacuum log);
  (* A reader opened before an archive exists sees it after refresh. *)
  Eio.Switch.run (fun sw ->
    let log = Proffer_log.open_ ~sw path in
    let r = Proffer_log.reader log in
    let before = query_int r "SELECT COUNT(*) FROM requests_all" in
    (* An archive written by another process, and a stray file that only
       shares the name pattern. *)
    Eio.Switch.run (fun sw2 ->
      let a = Sqlite3_eio.open_path ~sw:sw2 Eio.Path.(dir / "access-1999.db") in
      exec a v0_schema;
      exec a "CREATE TABLE log_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)";
      exec a "INSERT INTO log_meta VALUES ('schema', '2'), ('role', 'archive')";
      exec a (v0_row (jan1 1999) "/old");
      let b = Sqlite3_eio.open_path ~sw:sw2 Eio.Path.(dir / "access-notes.db") in
      exec b "CREATE TABLE notes (body TEXT)");
    Proffer_log.refresh log;
    check "refresh attaches the new archive and ignores the stray file"
      (query_int r "SELECT COUNT(*) FROM requests_all" = before + 1);
    check "archives lists only archives"
      (List.map (fun a -> a.Proffer_log.file) (Proffer_log.archives log)
       = [ "access-1999.db"; "access-2024.db"; "access-2025.db" ]);
    Eio.Path.unlink Eio.Path.(dir / "access-1999.db");
    Proffer_log.refresh log;
    check "refresh detaches a removed archive"
      (query_int r "SELECT COUNT(*) FROM requests_all" = before);
    (* A log from before log_meta existed merges as it is. *)
    Eio.Switch.run (fun sw2 ->
      let a = Sqlite3_eio.open_path ~sw:sw2 Eio.Path.(dir / "import.db") in
      exec a v0_schema;
      exec a (v0_row (jan1 1999) "/old"));
    check "merge reads a pre-schema file"
      (Proffer_log.merge log ~from:Eio.Path.(dir / "import.db") = 1));
  (* A live file from before log_meta existed migrates in place. *)
  let v0 = Eio.Path.(dir / "old.db") in
  Eio.Switch.run (fun sw ->
    let a = Sqlite3_eio.open_path ~sw v0 in
    exec a v0_schema;
    exec a "CREATE INDEX idx_requests_status ON requests(status_code)";
    exec a (v0_row 1.0 "/"));
  Eio.Switch.run (fun sw ->
    let log = Proffer_log.open_ ~sw v0 in
    let r = Proffer_log.reader log in
    check "migrated version"
      (query_int r "PRAGMA user_version" = Proffer_log.schema_version);
    check "migrated meta"
      (query_int r "SELECT COUNT(*) FROM log_meta WHERE key = 'schema'" = 1);
    check "migrated rows kept" (Proffer_log.live_rows log = 1);
    check "legacy index dropped"
      (query_int r
         "SELECT COUNT(*) FROM sqlite_master WHERE name = 'idx_requests_status'" = 0);
    check "metrics index present"
      (query_int r
         "SELECT COUNT(*) FROM sqlite_master WHERE name = 'idx_requests_metrics'" = 1));
  (* A newer file is refused. *)
  Eio.Switch.run (fun sw ->
    let a = Sqlite3_eio.open_path ~sw Eio.Path.(dir / "new.db") in
    exec a "PRAGMA user_version = 99");
  check "newer schema refused"
    (match Eio.Switch.run (fun sw ->
       ignore (Proffer_log.open_ ~sw Eio.Path.(dir / "new.db") : Proffer_log.t)) with
     | () -> false
     | exception Failure _ -> true);
  Printf.printf "test_log: %d checks passed\n" !checks
