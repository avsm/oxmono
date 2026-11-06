module D = Duraml
let time s = Option.get (Ptime.of_float_s s)
let span s = Option.get (Ptime.Span.of_float_s s)
let check = Alcotest.(check bool)
let count = Alcotest.(check int)
let spec ?(queue="work") key =
  D.Spec.v ~queue ~kind:"process-test" ~version:1 ~payload:"data" ~key ()
let open_queue env sw path = Duraml_sqlite.open_ ~sw ~clock:env#clock
  ~random:env#secure_random ~poll_interval:(span 0.01) Eio.Path.(env#fs / path)
(* [with_path prefix f] is [f path] for an unused database path, whose files
   are removed afterwards. *)
let with_path prefix f =
  let path=Filename.temp_file prefix ".db" in Unix.unlink path;
  Fun.protect (fun () -> f path) ~finally:(fun () ->
    List.iter (fun suffix -> let p=path^suffix in
      if Sys.file_exists p then Sys.remove p) ["";"-wal";"-shm"])
(* [scalar db sql] is the first column of the first row [sql] yields. *)
let scalar db sql = let s=Sqlite3.prepare db sql in
  ignore (Sqlite3.step s); let v=Sqlite3.column_text s 0 in
  ignore (Sqlite3.finalize s); v
let child mode path marker = Eio_main.run (fun env -> Eio.Switch.run (fun sw ->
  let ready () = Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(env#fs / marker) "ready" in
  if mode="uncommitted" then begin
    let db=Sqlite3.db_open path in
    Sqlite3.Rc.check (Sqlite3.exec db "BEGIN IMMEDIATE");
    Sqlite3.Rc.check (Sqlite3.exec db
      "UPDATE jobs SET data='broken' WHERE id=1");
    ready (); Eio.Fiber.await_cancel ()
  end else begin
    let q=open_queue env sw path in
    match mode with
    | "enqueue" -> ignore (D.enqueue q (spec "shared"))
    | "drain" ->
        let rec loop () = match D.try_take q ~queue:"work" ~lease:(span 30.)
          with
          | D.Await _ -> ()
          | D.Ready c ->
              if D.settle q c (D.complete (D.Succeeded "ok"))<>D.Applied
              then failwith "Lost claim";
              loop () in loop ()
    | "claim" -> ignore (D.take q ~queue:"work" ~lease:(span 0.1));
        ready (); Eio.Fiber.await_cancel ()
    | "settled" ->
        ignore (D.enqueue q (spec "shared"));
        let c=D.take q ~queue:"work" ~lease:(span 30.) in
        ignore (D.settle q c (D.complete
          ~followups:[spec ~queue:"convert" "successor"] (D.Succeeded "ok")));
        ready (); Eio.Fiber.await_cancel ()
    | _ -> invalid_arg "Unknown child mode"
  end))
let with_store f = Eio_main.run (fun env ->
  let root=Filename.temp_file "duraml-process-" "" in
  Unix.unlink root; Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(env#fs / root))
    (fun () -> Eio.Switch.run (fun sw ->
      let path=Filename.concat root "queue.db" in
      let q=open_queue env sw path in f env sw root path q)))
let spawn env sw mode path marker =
  let exe=if Filename.is_relative Sys.executable_name then
    Filename.concat (Sys.getcwd ()) Sys.executable_name else
      Sys.executable_name in
  Eio.Process.spawn ~sw env#process_mgr [exe;"--child";mode;path;marker]
let wait_ready env marker = Eio.Time.with_timeout_exn env#clock 10. (fun () ->
  while not (Sys.file_exists marker) do Eio.Time.sleep env#clock 0.005 done)
let kill process = Eio.Process.signal process Sys.sigkill;
  check "child killed" true (Eio.Process.await process=`Signaled Sys.sigkill)
let processes () = with_store (fun env sw _ path q ->
  let children=List.init 4 (fun _ -> spawn env sw "enqueue" path "unused") in
  List.iter Eio.Process.await_exn children;
  count "dedup across processes" 1 (List.length (D.list q (D.query ())));
  for i=1 to 30 do ignore (D.enqueue q (spec (string_of_int i))) done;
  let children=List.init 4 (fun _ -> spawn env sw "drain" path "unused") in
  List.iter Eio.Process.await_exn children;
  let jobs=D.list q (D.query ()) in
  count "all jobs" 31 (List.length jobs);
  check "each claimed exactly once" true
    (List.for_all (fun (j:D.job) -> (D.Job.state j)=D.Done && (D.Job.attempt
      j)=1) jobs))
let killed_claim () = with_store (fun env sw root path q ->
  ignore (D.enqueue q (spec "shared"));
  let marker=Filename.concat root "claimed" in
  let process=spawn env sw "claim" path marker in
  wait_ready env marker;kill process;
  let c=D.take q ~queue:"work" ~lease:(span 10.) in
  count "crashed attempt reclaimed" 2 (D.Claim.attempt c);
  check "recovered settlement" true
    (D.settle q c (D.complete (D.Succeeded "recovered"))=D.Applied))
let committed_response () = with_store (fun env sw root path q ->
  let marker=Filename.concat root "committed" in
  let process=spawn env sw "settled" path marker in
  wait_ready env marker;kill process;
  check "committed job found by key" true
    (match D.enqueue q (spec "shared") with D.Existing _ -> true | _ -> false);
  count "committed successor survives kill" 1
    (List.length (D.list q (D.query ~queue:"convert" ())));
  Eio.Switch.run (fun reopened_sw ->
    let reopened=open_queue env reopened_sw path in
    check "stable identity" true (D.identity reopened=D.identity q);
    count "reopened terminal job" 1
      (List.length (D.list reopened (D.query ~state:D.Done ())));
    check "events survive reopen" true (D.events reopened=D.events q)))
let interrupted_transaction () = with_store (fun env sw root path q ->
  ignore (D.enqueue q (spec "shared"));
  let before=D.list q (D.query ()) in
  let marker=Filename.concat root "transaction" in
  let process=spawn env sw "uncommitted" path marker in
  wait_ready env marker;
  check "uncommitted write invisible" true (D.list q (D.query ())=before);
  kill process;
  check "transaction rolled back" true (D.list q (D.query ())=before);
  ignore (D.enqueue q (spec "after-crash")))
let polling () = with_store (fun env sw _ path q ->
  Eio.Time.with_timeout_exn env#clock 10. (fun () -> Eio.Fiber.both
    (fun () -> ignore (D.take q ~queue:"work" ~lease:(span 30.)))
    (fun () -> Eio.Time.sleep env#clock 0.02;
      Eio.Process.await_exn (spawn env sw "enqueue" path "unused"))))
let stale_clock_after_lock () = with_store (fun env sw root path _ ->
  let clock=Eio_mock.Clock.make () in
  let q=Duraml_sqlite.open_ ~sw ~clock ~random:env#secure_random
    Eio.Path.(env#fs / path) in
  ignore (D.enqueue q (spec "shared"));
  let c=D.take q ~queue:"work" ~lease:(span 10.) in
  let marker=Filename.concat root "locked" in
  let process=spawn env sw "uncommitted" path marker in
  wait_ready env marker;
  Eio.Fiber.both
    (fun () -> check "busy wait cannot renew an expired claim" false
      (D.renew q c ~lease:(span 10.)))
    (fun () -> Eio.Time.sleep env#clock 0.03;
      Eio_mock.Clock.set_time clock 20.; kill process))
let cancel_busy_writer () = with_store (fun env sw root path q ->
  ignore (D.enqueue q (spec "shared"));
  let marker=Filename.concat root "busy" in
  let process=spawn env sw "uncommitted" path marker in
  wait_ready env marker;
  Eio.Fiber.both
    (fun () -> check "blocked write cancels" true (try
      Eio.Time.with_timeout_exn env#clock 0.01 (fun () ->
        ignore (D.enqueue q (spec "cancelled-write"))); false
      with Eio.Time.Timeout -> true))
    (fun () -> Eio.Time.sleep env#clock 0.05; kill process);
  ignore (D.enqueue q (spec "after-cancellation"));
  check "connection remains usable" true
    (List.exists (fun (j:D.job) -> (D.Spec.key (D.Job.spec j))=Some
      "after-cancellation")
      (D.list q (D.query ()))))
let refuses () = Eio_main.run (fun env ->
  with_path "duraml-unrelated-" @@ fun path ->
  let db=Sqlite3.db_open path in
  Sqlite3.Rc.check (Sqlite3.exec db "CREATE TABLE unrelated(value TEXT)");
  ignore (Sqlite3.db_close db);
  Eio.Switch.run (fun sw -> check "unrelated database refused" true
    (try ignore (open_queue env sw path);false
     with Eio.Io (D.E (D.Storage _),_) -> true)))
(* A store from a newer schema, or another application's database, is
   refused by both writable and read-only opens and is left unchanged. *)
let refuses_foreign_schema () = Eio_main.run (fun env ->
  List.iter (fun (name,pragma) ->
    with_path "duraml-foreign-" @@ fun path ->
    Eio.Switch.run (fun sw -> ignore (D.enqueue (open_queue env sw path)
      (spec "kept")));
    let db=Sqlite3.db_open path in
    Sqlite3.Rc.check (Sqlite3.exec db pragma);
    let before=scalar db "PRAGMA user_version"
      ^ "/" ^ scalar db "PRAGMA application_id" in
    ignore (Sqlite3.db_close db);
    List.iter (fun read_only -> Eio.Switch.run (fun sw ->
      check name true (try
        ignore (Duraml_sqlite.open_ ~sw ~clock:env#clock
          ~random:env#secure_random ~read_only Eio.Path.(env#fs / path));
        false
      with Eio.Io (D.E (D.Storage _),_) -> true))) [false;true];
    let db=Sqlite3.db_open ~mode:`READONLY path in
    check (name ^ " left unchanged") true
      (scalar db "PRAGMA user_version" ^ "/"
       ^ scalar db "PRAGMA application_id" = before);
    ignore (Sqlite3.db_close db))
    ["newer schema refused","PRAGMA user_version=6";
     "older unknown schema refused","PRAGMA user_version=1";
     "foreign application refused","PRAGMA application_id=1"])
let read_only () = with_store (fun env sw _ path q ->
  ignore (D.enqueue q (spec "inspected"));
  let reader=Duraml_sqlite.open_ ~sw ~clock:env#clock ~random:env#secure_random
    ~read_only:true Eio.Path.(env#fs / path) in
  check "same identity" true (D.identity reader=D.identity q);
  count "reader lists jobs" 1 (List.length (D.list reader (D.query ())));
  count "reader lists events" 1 (List.length (D.events reader));
  check "reader cannot enqueue" true (try
    ignore (D.enqueue reader (spec "refused")); false
    with Eio.Io (D.E (D.Storage _),_) -> true);
  check "reader cannot claim" true (try
    ignore (D.try_take reader ~queue:"work" ~lease:(span 30.)); false
    with Eio.Io (D.E (D.Storage _),_) -> true);
  count "writer unaffected" 1 (List.length (D.list q (D.query ())));
  let missing=Filename.concat (Filename.dirname path) "absent.db" in
  check "uninitialized file refused" true (try
    ignore (Duraml_sqlite.open_ ~sw ~clock:env#clock ~random:env#secure_random
      ~read_only:true Eio.Path.(env#fs / missing)); false
    with Eio.Io _ | Sqlite3.Error _ | Sqlite3.SqliteError _ -> true))
(* A schema-2 store, as written before lanes existed, upgrades in place on
   its first writable open and dispatches its old jobs oldest-first. *)
let upgrade () = Eio_main.run (fun env ->
  with_path "duraml-v2-" @@ fun path ->
  Eio.Switch.run (fun sw ->
  let db=Sqlite3.db_open path in
  let job n at = Printf.sprintf {|{"id":"%d","spec":{"queue":"work",
"kind":"process-test","version":1,"payload":"data","key":"k%d"},
"state":"queued","attempt":0,"available_at":"%s","created_at":"%s",
"updated_at":"%s"}|} n n at at at in
  Sqlite3.Rc.check (Sqlite3.exec db {|
CREATE TABLE meta(key TEXT PRIMARY KEY,value TEXT NOT NULL);
CREATE TABLE jobs(id INTEGER PRIMARY KEY,queue TEXT NOT NULL,key TEXT,
 state TEXT NOT NULL,available TEXT NOT NULL,data TEXT NOT NULL);
CREATE UNIQUE INDEX job_keys ON jobs(queue,key) WHERE key IS NOT NULL;
CREATE INDEX eligible ON jobs(queue,available,id)
 WHERE state IN ('queued','running');
CREATE INDEX browsing ON jobs(queue,state,id);
CREATE TABLE attempts(job INTEGER NOT NULL REFERENCES jobs(id),
 generation INTEGER NOT NULL,data TEXT NOT NULL,PRIMARY KEY(job,generation));
CREATE TABLE events(sequence INTEGER PRIMARY KEY,data TEXT NOT NULL);
PRAGMA application_id=1146442305;
PRAGMA user_version=2;
INSERT INTO meta VALUES('identity','legacy'),('next','3');|});
  (* The younger job is eligible sooner; schema 2 would dispatch it first. *)
  List.iter (fun (n,at,avail) -> Sqlite3.Rc.check (Sqlite3.exec db
    (Printf.sprintf
      "INSERT INTO jobs VALUES(%d,'work','k%d','queued','%s','%s')"
      n n avail (String.map (function '\n' -> ' ' | c -> c) (job n at)))))
    [1,"2026-01-01T00:00:00.000000000000Z","2026-01-01T00:10:00.000000000000Z";
     2,"2026-01-01T00:01:00.000000000000Z","2026-01-01T00:01:00.000000000000Z"];
  Sqlite3.Rc.check (Sqlite3.exec db {|
INSERT INTO events VALUES(1,'{"sequence":"1",
"time":"2026-01-01T00:00:00.000000000000Z",
"kind":"enqueued","job":"1","attempt":0,"detail":""}')|});
  ignore (Sqlite3.db_close db);
  check "old schema refused read-only" true (try
    ignore (Duraml_sqlite.open_ ~sw ~clock:env#clock ~random:env#secure_random
      ~read_only:true Eio.Path.(env#fs / path)); false
    with Eio.Io (D.E (D.Storage _),_) -> true);
  let q=open_queue env sw path in
  check "identity kept" true (D.identity q="legacy");
  count "jobs kept" 2 (List.length (D.list q (D.query ())));
  count "events kept" 1 (List.length (D.events q));
  (match D.try_take q ~queue:"work" ~lease:(span 30.) with
   | D.Ready c ->
       check "oldest job dispatched first" true
         (D.Id.to_string (D.Claim.id c)="1");
       check "unnamed lane" true (D.Spec.lane (D.Claim.spec c)="");
       check "settles" true
         (D.settle q c (D.complete (D.Succeeded "ok"))=D.Applied)
   | D.Await _ -> Alcotest.fail "Expected claim");
  let db=Sqlite3.db_open ~mode:`READONLY path in
  let scalar=scalar db in
  check "schema upgraded" true (scalar "PRAGMA user_version"="5");
  check "payload moved out of the record" true
    (scalar "SELECT data FROM payloads WHERE job=1"="data" &&
     scalar "SELECT json_extract(data,'$.spec.payload') IS NULL \
       FROM jobs WHERE id=1"="1");
  check "lane recorded as served" true
    (scalar "SELECT served FROM lanes WHERE queue='work' AND lane=''"="1");
  check "creation column filled" true
    (scalar "SELECT created FROM jobs WHERE id=2"
     ="2026-01-01T00:01:00.000000000000Z");
  ignore (Sqlite3.db_close db);
  let reader=Duraml_sqlite.open_ ~sw ~clock:env#clock
    ~random:env#secure_random ~read_only:true Eio.Path.(env#fs / path) in
  count "reader sees upgraded store" 2 (List.length (D.list reader (D.query ())))))
(* A schema-4 store, whose records hold payloads and claim specifications,
   upgrades in place, and a claim taken before the upgrade still renews and
   settles. The schema-4 records are made from schema-5 ones. *)
let upgrade_4 () = Eio_main.run (fun env ->
  with_path "duraml-v4-" @@ fun path ->
  let claim=Eio.Switch.run (fun sw ->
    let q=open_queue env sw path in
    ignore (D.enqueue q (spec "running"));
    ignore (D.enqueue q (spec "queued"));
    D.take q ~queue:"work" ~lease:(span 600.)) in
  let db=Sqlite3.db_open path in
  Sqlite3.Rc.check (Sqlite3.exec db {|
UPDATE jobs SET data=json_set(data,'$.spec.payload',
 (SELECT data FROM payloads WHERE job=jobs.id));
UPDATE attempts SET data=json_set(data,'$.claim.spec',
 json_extract((SELECT data FROM jobs WHERE id=attempts.job),'$.spec'));
DROP TABLE payloads;
PRAGMA user_version=4;|});
  ignore (Sqlite3.db_close db);
  Eio.Switch.run (fun sw ->
    let q=open_queue env sw path in
    check "claim renews after upgrade" true
      (D.renew q claim ~lease:(span 600.));
    check "claim settles after upgrade" true
      (D.settle q claim (D.complete (D.Succeeded "ok"))=D.Applied);
    let next=D.take q ~queue:"work" ~lease:(span 600.) in
    check "payload restored" true
      (D.Spec.payload (D.Claim.spec next)="data" &&
       D.Spec.key (D.Claim.spec next)=Some "queued"));
  let db=Sqlite3.db_open ~mode:`READONLY path in
  let scalar=scalar db in
  check "schema 5" true (scalar "PRAGMA user_version"="5");
  check "attempts hold no specification" true
    (scalar "SELECT count(*) FROM attempts \
      WHERE json_extract(data,'$.claim.spec') IS NOT NULL"="0");
  ignore (Sqlite3.db_close db))
(* A renewal leaves the payload row as it is, and a symbolic link to a
   store is refused. *)
let renewal_and_links () = Eio_main.run (fun env ->
  with_path "duraml-renew-" @@ fun path ->
  Eio.Switch.run (fun sw ->
  let q=open_queue env sw path in
  ignore (D.enqueue q (spec "k"));
  let c=D.take q ~queue:"work" ~lease:(span 60.) in
  let db=Sqlite3.db_open path in
  let scalar=scalar db in
  Sqlite3.Rc.check (Sqlite3.exec db {|
CREATE TABLE writes(n INTEGER);
CREATE TRIGGER payload_insert AFTER INSERT ON payloads
 BEGIN INSERT INTO writes VALUES(1); END;
CREATE TRIGGER payload_update AFTER UPDATE ON payloads
 BEGIN INSERT INTO writes VALUES(1); END;|});
  check "renewed" true (D.renew q c ~lease:(span 60.));
  check "payload not written" true (scalar "SELECT count(*) FROM writes"="0");
  ignore (Sqlite3.db_close db);
  let link=path ^ "-link" in
  Unix.symlink path link;
  check "symbolic link refused" true (try
    ignore (Duraml_sqlite.open_ ~sw ~clock:env#clock ~random:env#secure_random
      ~read_only:true Eio.Path.(env#fs / link)); false
    with Eio.Io _ -> true);
  Sys.remove link))
(* A damaged record is a storage failure, and emptied lanes are forgotten. *)
let damaged () = Eio_main.run (fun env ->
  with_path "duraml-damaged-" @@ fun path ->
  Eio.Switch.run (fun sw ->
  let q=open_queue env sw path in
  ignore (D.enqueue q (D.Spec.v ~queue:"work" ~kind:"process-test" ~version:1
    ~payload:"data" ~lane:"once" ()));
  let c=D.take q ~queue:"work" ~lease:(span 30.) in
  ignore (D.settle q c (D.complete (D.Succeeded "ok")));
  ignore (D.enqueue q (spec "bad"));
  let db=Sqlite3.db_open path in
  let scalar=scalar db in
  check "emptied lane forgotten" true
    (scalar "SELECT count(*) FROM lanes WHERE lane='once'"="0");
  Sqlite3.Rc.check (Sqlite3.exec db
    "UPDATE jobs SET data=replace(data,'\"queued\"','\"bogus\"') \
     WHERE key='bad'");
  ignore (Sqlite3.db_close db);
  check "damaged record is a storage failure" true (try
    ignore (D.list q (D.query ())); false
    with Eio.Io (D.E (D.Storage _),_) -> true)))
let () =
  if Array.length Sys.argv=5 && Sys.argv.(1)="--child" then
    child Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
  else Alcotest.run "Duraml SQLite processes" ["sqlite",List.map (fun (n,f) ->
    Alcotest.test_case n `Quick f) ["process contention",processes;
      "kill and reclaim",killed_claim;"commit survives kill",committed_response;
      "interrupted transaction",interrupted_transaction;
      "cross-process wakeup",polling;
      "clock after transaction lock",stale_clock_after_lock;
      "cancel blocked writer",cancel_busy_writer;
      "refuse unrelated store",refuses;
      "refuse foreign schema",refuses_foreign_schema;"read-only inspection",read_only;
      "upgrade from schema 2",upgrade;"upgrade from schema 4",upgrade_4;
      "renewal and links",renewal_and_links;"damaged records",damaged]]
