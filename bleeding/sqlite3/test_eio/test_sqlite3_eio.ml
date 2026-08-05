let () =
  Eio_main.run @@ fun env ->
  (* Test 1: open_memory with Switch lifecycle, exec, and query *)
  Eio.Switch.run @@ fun sw ->
  let t = Sqlite3_eio.open_memory ~sw ~busy_timeout:1000 () in
  let rc =
    Sqlite3_eio.exec t
      "CREATE TABLE test (id INTEGER PRIMARY KEY, name TEXT)"
  in
  Sqlite3.Rc.check rc;

  let rc = Sqlite3_eio.exec t "INSERT INTO test VALUES (1, 'alice')" in
  Sqlite3.Rc.check rc;
  let rc = Sqlite3_eio.exec t "INSERT INTO test VALUES (2, 'bob')" in
  Sqlite3.Rc.check rc;

  (* Query via callback *)
  let rows = ref [] in
  let rc =
    Sqlite3_eio.exec t
      ~cb:(fun row _headers ->
        let name = match row.(1) with Some s -> s | None -> "<null>" in
        rows := name :: !rows)
      "SELECT * FROM test ORDER BY id"
  in
  Sqlite3.Rc.check rc;
  assert (List.rev !rows = [ "alice"; "bob" ]);

  (* Test 2: prepare / step / finalize with bind and column access via db *)
  let stmt = Sqlite3_eio.prepare t "SELECT name FROM test WHERE id = ?" in
  let db = Sqlite3_eio.db t in
  Sqlite3.Rc.check (Sqlite3.bind_int stmt 1 2);
  let rc = Sqlite3_eio.step t stmt in
  assert (rc = Sqlite3.Rc.ROW);
  let name = Sqlite3.column_text stmt 0 in
  assert (name = "bob");
  let rc = Sqlite3_eio.step t stmt in
  assert (rc = Sqlite3.Rc.DONE);
  Sqlite3.Rc.check (Sqlite3_eio.finalize t stmt);

  (* Test 3: iter *)
  let stmt2 = Sqlite3_eio.prepare t "SELECT name FROM test ORDER BY id" in
  let names = ref [] in
  let rc =
    Sqlite3_eio.iter t stmt2 ~f:(fun row ->
      match row.(0) with
      | Sqlite3.Data.TEXT s -> names := s :: !names
      | _ -> assert false)
  in
  assert (rc = Sqlite3.Rc.DONE);
  assert (List.rev !names = [ "alice"; "bob" ]);

  (* Test 4: fold *)
  let stmt3 = Sqlite3_eio.prepare t "SELECT id FROM test ORDER BY id" in
  let rc, total =
    Sqlite3_eio.fold t stmt3 ~init:0 ~f:(fun acc row ->
      match row.(0) with
      | Sqlite3.Data.INT n -> acc + Int64.to_int n
      | _ -> assert false)
  in
  assert (rc = Sqlite3.Rc.DONE);
  assert (total = 3);

  (* Test 5: explicit close *)
  let t2 =
    Eio.Switch.run @@ fun sw2 ->
    let t2 = Sqlite3_eio.open_memory ~sw:sw2 () in
    Eio.Resource.close t2;
    t2
  in
  (match Sqlite3_eio.exec t2 "SELECT 1" with
   | _ -> assert false
   | exception Eio.Exn.Io (Sqlite3_eio.E Sqlite3_eio.Closed, _) -> ());

  (* Test 6: fiber cancellation interrupts a long-running query *)
  let t3 = Sqlite3_eio.open_memory ~sw () in
  Sqlite3.Rc.check
    (Sqlite3_eio.exec t3
       "CREATE TABLE big (x INTEGER)");
  Sqlite3.Rc.check
    (Sqlite3_eio.exec t3
       "INSERT INTO big WITH RECURSIVE c(x) AS \
        (VALUES(1) UNION ALL SELECT x+1 FROM c WHERE x < 100000) \
        SELECT x FROM c");
  (* Fiber.both: when the second fiber raises, the first is cancelled.
     Our cancellation handler calls sqlite3_interrupt on the db. *)
  (try
     Eio.Fiber.both
       (fun () ->
         ignore
           (Sqlite3_eio.exec t3
              "SELECT sum(a.x * b.x) FROM big a, big b" : Sqlite3.Rc.t))
       (fun () ->
         Eio.Fiber.yield ();
         failwith "cancel_query")
   with
   | Failure _ -> ()
   | Sqlite3.SqliteError _ -> ()
   | Sqlite3.Error _ -> ());

  (* Test 7: error context on open with non-existent path *)
  let fs = Eio.Stdenv.fs env in
  (match
     Eio.Switch.run @@ fun sw3 ->
     ignore
       (Sqlite3_eio.open_path ~sw:sw3 ~mode:`READONLY
          Eio.Path.(fs / "/nonexistent/path/db.sqlite") : Sqlite3_eio.t)
   with
   | _ -> assert false
   | exception Eio.Exn.Io (Sqlite3_eio.E (Sqlite3_eio.Open_failed _), _) ->
     ());

  (* Test 8: automatic close on switch exit *)
  let t4 =
    Eio.Switch.run @@ fun sw4 ->
    Sqlite3_eio.open_memory ~sw:sw4 ()
  in
  (match Sqlite3_eio.exec t4 "SELECT 1" with
   | _ -> assert false
   | exception Eio.Exn.Io (Sqlite3_eio.E Sqlite3_eio.Closed, _) -> ());

  ignore (db : Sqlite3.db);
  Printf.printf "All tests passed.\n"
