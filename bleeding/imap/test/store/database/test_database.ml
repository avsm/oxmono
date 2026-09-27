module D = Database
module S = Sqlite3

let expect what f =
  match f () with
  | exception Invalid_argument _ -> ()
  | _ -> failwith (what ^ " accepted")

let contains ~needle haystack =
  let n = String.length needle in
  let rec from k =
    k + n <= String.length haystack &&
    (String.sub haystack k n = needle || from (k + 1)) in
  from 0

let with_db f =
  Eio.Switch.run (fun sw ->
    let db = Sqlite3_eio.open_memory ~sw () in
    let t = { D.db; mutex = Eio.Mutex.create (); blob_dir = None } in
    D.sql t "CREATE TABLE kv (k INTEGER PRIMARY KEY, v TEXT NOT NULL)";
    f t)

let bind_count t =
  expect "missing value" (fun () ->
    D.run t "INSERT INTO kv VALUES (?,?)" [D.i 1L]);
  expect "extra value" (fun () ->
    D.run t "INSERT INTO kv VALUES (?,?)" [D.i 1L; D.s "a"; D.s "b"]);
  if D.rows t "SELECT k FROM kv" [] <> [] then
    failwith "mismatched bind wrote a row"

let prepared_reuse t =
  D.with_stmt t "INSERT INTO kv VALUES (?,?)" (fun stmt ->
    D.run_prepared t stmt [D.i 1L; D.s "one"];
    (match D.run_prepared t stmt [D.i 1L; D.s "again"] with
     | exception S.SqliteError message ->
       if not (contains ~needle:"UNIQUE constraint failed: kv.k" message)
       then failwith ("error lacks SQLite message: " ^ message)
     | () -> failwith "duplicate key accepted");
    D.run_prepared t stmt [D.i 2L; D.s "two"]);
  D.with_stmt t "SELECT v FROM kv WHERE k=?" (fun stmt ->
    (match D.rows_prepared t stmt [D.i 2L] with
     | [[| S.Data.TEXT "two" |]] -> ()
     | _ -> failwith "reused statement lost a row");
    match D.rows_prepared t stmt [D.i 1L] with
    | [[| S.Data.TEXT "one" |]] -> ()
    | _ -> failwith "reused read statement kept stale state")

let nested t =
  expect "nested transaction" (fun () ->
    D.transaction t (fun () -> D.transaction t (fun () -> ())));
  expect "lock inside transaction" (fun () ->
    D.transaction t (fun () -> D.locked t (fun () -> ())));
  D.transaction t (fun () -> D.run t "INSERT INTO kv VALUES (3,'three')" []);
  let order = ref [] in
  Eio.Fiber.both
    (fun () -> D.transaction t (fun () ->
       Eio.Fiber.yield ();
       order := "first" :: !order))
    (fun () -> D.transaction t (fun () -> order := "second" :: !order));
  if List.rev !order <> ["first"; "second"] then
    failwith "concurrent transactions did not serialize"

let () =
  Eio_main.run (fun _ ->
    with_db bind_count;
    with_db prepared_reuse;
    with_db nested)
