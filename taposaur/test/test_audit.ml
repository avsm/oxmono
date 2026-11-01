module A = Taposaur_audit
let ( / ) = Eio.Path.( / )
let with_db f =
  let file = Filename.temp_file "taposaur-audit-test" ".db" in
  Fun.protect ~finally:(fun () ->
    List.iter (fun suffix -> try Unix.unlink (file ^ suffix) with _ -> ())
      ["";"-wal";"-shm"])
    (fun () -> Eio_main.run (fun env -> Eio.Switch.run (fun sw ->
      let db = A.open_write ~sw (env#fs / file) in
      f env sw file db)))
let sql path statement =
  let db = Sqlite3.db_open path in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () -> Sqlite3.Rc.check (Sqlite3.exec db statement))
let check = Alcotest.(check bool)
let count = Alcotest.(check int)
let lifecycle () = with_db (fun _ _ _ db ->
  let session = A.session ~component:"test" db in
  A.with_operation ~session ~action:"fetch" ~subject:"doi:10.1/x" (fun nested ->
    A.with_operation ?session:nested ~action:"publish" ~subject:"doi:10.1/x"
      ~result_details:(fun _ -> ["bytes","42"]) (fun _ -> ()));
  let rows = A.query db in
  count "four lifecycle events" 4 (List.length rows);
  check "system actor" true (List.for_all (fun (e : A.event) -> e.actor="system") rows);
  check "one correlated run" true
    (List.for_all (fun (e : A.event) -> e.run=A.run_id session) rows);
  let parent = List.find (fun (e : A.event) -> e.action="fetch" && e.phase=Started) rows in
  let child = List.find (fun (e : A.event) -> e.action="publish" && e.phase=Started) rows in
  check "parent operation" true (child.parent=Some parent.operation);
  count "no incomplete operations" 0 (List.length (A.query ~incomplete:true db));
  let page = A.query ~limit:2 db in
  let last = List.hd (List.rev page) in
  let next = A.query ~before:last.id ~limit:2 db in
  count "cursor next page" 2 (List.length next);
  check "exclusive descending cursor" true
    (List.for_all (fun (e : A.event) -> e.id < last.id) next);
  count "subject filter" 0 (List.length (A.query ~subject:"missing" db));
  count "action filter" 2 (List.length (A.query ~action:"publish" db));
  count "phase filter" 2 (List.length (A.query ~phase:Succeeded db));
  List.iter (fun e -> match Jsont_bytesrw.decode_string Jsont.json (A.to_json e) with
    | Ok _ -> () | Error e -> Alcotest.fail e) rows)
let failures () = with_db (fun env _ file db ->
  let session = A.session ~component:"test" db in
  (try A.with_operation ~session ~action:"fail" ~subject:"paper"
      (fun _ -> failwith "credential=DO_NOT_LOG") with Failure _ -> ());
  let event = List.hd (A.query ~phase:Failed db) in
  Alcotest.(check string) "only safe exception category" "{\"error_kind\":\"failure\"}" event.details;
  (try Eio.Time.with_timeout_exn env#clock 0.01 (fun () ->
      A.with_operation ~session ~action:"cancel" ~subject:"paper"
        (fun _ -> Eio.Time.sleep env#clock 10.))
    with Eio.Time.Timeout -> ());
  count "cancellation terminal" 1 (List.length (A.query ~phase:Cancelled db));
  sql file "CREATE TRIGGER reject_terminal BEFORE INSERT ON events WHEN NEW.phase='succeeded' BEGIN SELECT RAISE(ABORT,'test'); END";
  let changed = ref false in
  (match A.with_operation ~session ~action:"terminal-failure" ~subject:"paper"
      (fun _ -> changed:=true) with
   | () -> Alcotest.fail "Expected uncertain completion"
   | exception A.Completion_unrecorded _ -> ());
  check "operation happened" true !changed;
  count "incomplete result retained" 1 (List.length (A.query ~incomplete:true db));
  sql file "CREATE TRIGGER reject_start BEFORE INSERT ON events WHEN NEW.phase='started' BEGIN SELECT RAISE(ABORT,'test'); END";
  changed:=false;
  (try A.with_operation ~session ~action:"start-failure" ~subject:"paper"
      (fun _ -> changed:=true) with Sqlite3.Error _ | Sqlite3.SqliteError _ -> ());
  check "failed initial logging blocks mutation" false !changed)
let concurrency () = with_db (fun env sw file db ->
  let other = A.open_write ~sw (env#fs / file) in
  let first = A.session ~component:"first" db in
  let second = A.session ~component:"second" ~actor:"operator" other in
  Eio.Fiber.both
    (fun () -> for _=1 to 20 do A.record first ~action:"write" ~subject:"one" () done)
    (fun () -> for _=1 to 20 do A.record second ~action:"write" ~subject:"two" () done);
  count "both writers retained" 40 (List.length (A.query db));
  count "actor filter" 20 (List.length (A.query ~actor:"operator" db));
  let read = A.open_read ~sw (env#fs / file) in
  count "read-only API" 40 (List.length (A.query read));
  List.iter (fun command ->
    match sql file command with
    | () -> Alcotest.fail "Append-only guard missing"
    | exception Sqlite3.Error _ | exception Sqlite3.SqliteError _ -> ())
    ["UPDATE events SET actor='changed'";"DELETE FROM events"];
  (match A.record first ~action:"unsafe" ~subject:"one"
      ~details:["api_token","DO_NOT_LOG"] () with
   | () -> Alcotest.fail "Sensitive field accepted"
   | exception Invalid_argument _ -> ());
  count "no rejected payload stored" 40 (List.length (A.query db)))
let wrong_database () =
  let file = Filename.temp_file "not-audit" ".db" in
  Fun.protect ~finally:(fun () -> Unix.unlink file) (fun () ->
    sql file "CREATE TABLE precious(value TEXT)";
    Eio_main.run (fun env -> Eio.Switch.run (fun sw ->
      match A.open_write ~sw (env#fs / file) with
      | _ -> Alcotest.fail "Unrelated database accepted"
      | exception Failure _ -> ())));
  ()
let () = Alcotest.run "audit" ["durability",[
  Alcotest.test_case "lifecycle and queries" `Quick lifecycle;
  Alcotest.test_case "failure, cancellation and uncertain completion" `Quick failures;
  Alcotest.test_case "concurrent writers and append-only guards" `Quick concurrency;
  Alcotest.test_case "unrelated database refused" `Quick wrong_database]]
