(* Probes of the portability claims in sqlite3_eio.mli. A closure bound at
   portable mode, as in [let (f @ portable) = fun () -> ...], compiles only
   when every Sqlite3_eio value it names is portable. [t] is an Eio resource
   of kind [value], so no portable closure can capture one: [query] takes
   its handle as an argument, and [round_trip] opens its own and also runs
   in a second domain through [Eio.Domain_manager.run]. *)

(* Runs a statement through [run] on a handle passed to it, and the other
   operations that take one. *)
let (query @ portable) = fun t n ->
  Sqlite3.Rc.check (Sqlite3_eio.exec t "CREATE TABLE IF NOT EXISTS n (x)");
  let insert = Sqlite3_eio.prepare t "INSERT INTO n VALUES (?1)" in
  Sqlite3_eio.run t (fun _db ->
    Sqlite3.Rc.check (Sqlite3.bind_int insert 1 n);
    assert (Sqlite3.step insert = Sqlite3.Rc.DONE));
  Sqlite3.Rc.check (Sqlite3_eio.finalize t insert);
  let select = Sqlite3_eio.prepare t "SELECT sum(x) FROM n" in
  assert (Sqlite3_eio.step t select = Sqlite3.Rc.ROW);
  let v = Sqlite3.column_int select 0 in
  Sqlite3.Rc.check (Sqlite3_eio.reset t select);
  let rc, sums =
    Sqlite3_eio.fold t select ~init:[] ~f:(fun acc row -> row :: acc) in
  assert (rc = Sqlite3.Rc.DONE && List.length sums = 1);
  Sqlite3.Rc.check (Sqlite3_eio.finalize t select);
  v

let (round_trip @ portable) = fun n ->
  Eio.Switch.run (fun sw -> query (Sqlite3_eio.open_memory ~sw ()) n)

let (unopenable @ portable) = fun path ->
  Eio.Switch.run (fun sw ->
    match Sqlite3_eio.open_path ~sw ~mode:`READONLY path with
    | _ -> false
    | exception ex -> Eio.Exn.is_io ex)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let t = Sqlite3_eio.open_memory ~sw () in
  assert (query t 20 = 20);
  assert (query t 22 = 42);
  assert (round_trip 21 = 21);
  let dm = Eio.Stdenv.domain_mgr env in
  assert (Eio.Domain_manager.run dm (fun () -> round_trip 50) = 50);
  assert (unopenable Eio.Path.(Eio.Stdenv.fs env / "/nonexistent/db"))
