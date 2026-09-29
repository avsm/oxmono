[@@@alert "-do_not_spawn_domains"]

(* Probes of the portability claims in sqlite3.mli. A closure bound at
   portable mode, as in [let (f @ portable) = fun () -> ...], compiles only
   when every Sqlite3 value it names is portable and every value it captures
   crosses portability. A captured handle is contended there and no Sqlite3
   function takes a contended handle, so [capture_db] and [capture_stmt]
   only return theirs. The other closures take or open their handles, and
   [round_trip] also runs in a second domain. *)

open Sqlite3

let db = db_open ":memory:"
let stmt = prepare db "SELECT ?1 + 1"

(* Each compiles only because its handle type is [value mod portable]. *)
let (capture_db @ portable) = fun () -> db
let (capture_stmt @ portable) = fun () -> stmt

(* Binds [n], steps, reads column 0 and resets [stmt]. *)
let (query @ portable) = fun stmt n ->
  Rc.check (bind_int stmt 1 n);
  let rc = step stmt in
  assert (rc = Rc.ROW);
  let v = column_int stmt 0 in
  Rc.check (reset stmt);
  v

(* Opens a database, registers a portable user function, prepares, binds,
   steps, reads a column and finalizes. *)
let (round_trip @ portable) = fun n ->
  let db = db_open ":memory:" in
  create_fun1 db "twice" (fun v ->
      Data.INT (Int64.mul 2L (Data.to_int64_exn v)));
  let stmt = prepare db "SELECT twice(?1)" in
  let v = query stmt n in
  Rc.check (finalize stmt);
  assert (db_close db);
  v

let () =
  assert (query stmt 41 = 42);
  assert (round_trip 21 = 42);
  let domain = Domain.Safe.spawn (fun () -> round_trip 50) in
  assert (Domain.join domain = 100);
  Rc.check (finalize stmt);
  assert (db_close db)
