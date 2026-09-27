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
    let t = { D.db; handle = Sqlite3_eio.db db; mutex = Eio.Mutex.create ();
              blob_dir = None } in
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

(* The batch operations keep the reset and error contract of their Eio
   counterparts, and a failed write leaves the statement reusable. *)
let batch_reuse t =
  D.with_stmt t "INSERT INTO kv VALUES (?,?)" (fun stmt ->
    let insert k v =
      D.bind_int64 t stmt 1 k;
      D.bind_text t stmt 2 v;
      D.batch_exec t stmt in
    D.batch t (fun () ->
      insert 1L "one";
      if D.changes t <> 1 then failwith "insert not counted";
      (match insert 1L "again" with
       | exception S.SqliteError message ->
         if not (contains ~needle:"UNIQUE constraint failed: kv.k" message)
         then failwith ("error lacks SQLite message: " ^ message)
       | () -> failwith "duplicate key accepted");
      insert 2L "two"));
  D.with_stmt t "SELECT v FROM kv WHERE k=?" (fun stmt ->
    let read k = D.bind_int64 t stmt 1 k; D.batch_row t stmt in
    D.batch t (fun () ->
      (match read 2L with
       | Some [| S.Data.TEXT "two" |] -> ()
       | _ -> failwith "reused statement lost a row");
      (match read 1L with
       | Some [| S.Data.TEXT "one" |] -> ()
       | _ -> failwith "reused read statement kept stale state");
      if read 3L <> None then failwith "absent key read"));
  match D.batch t (fun () -> raise Exit) with
  | exception Exit -> ()
  | () -> failwith "batch lost its exception"

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
    with_db batch_reuse;
    with_db nested)
