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
    let t = D.v db None in
    D.locked t (fun c ->
      D.sql c "CREATE TABLE kv (k INTEGER PRIMARY KEY, v TEXT NOT NULL)");
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
    D.transaction t (fun _ -> D.transaction t (fun _ -> ())));
  expect "lock inside transaction" (fun () ->
    D.transaction t (fun _ -> D.locked t (fun _ -> ())));
  D.transaction t (fun c -> D.run c "INSERT INTO kv VALUES (3,'three')" []);
  let order = ref [] in
  Eio.Fiber.both
    (fun () -> D.transaction t (fun _ ->
       Eio.Fiber.yield ();
       order := "first" :: !order))
    (fun () -> D.transaction t (fun _ -> order := "second" :: !order));
  if List.rev !order <> ["first"; "second"] then
    failwith "concurrent transactions did not serialize"

module Kinds = struct
  type t : value mod portable contended = D.t
end

(* [write] captures [t], which compiles only while [t] crosses portability
   and contention and every operation it names is portable. It runs here
   and in a second domain, each writing through [transaction]. *)
let shared dm t =
  let (write @ portable) = fun k ->
    let changed = D.transaction t (fun c ->
      D.run c "INSERT INTO kv VALUES (?,'x')" [D.i k];
      D.with_stmt c "INSERT INTO kv VALUES (?,'y')" (fun stmt ->
        D.batch c (fun () ->
          D.bind_int64 c stmt 1 (Int64.add k 100L);
          D.batch_exec c stmt;
          D.changes c))) in
    changed + D.with_stmt_across_locks t "SELECT count(*) FROM kv"
      (fun stmt -> D.locked t (fun c ->
        match D.rows_prepared c stmt [] with
        | [[| S.Data.INT n |]] -> Int64.to_int n
        | _ -> failwith "count")) in
  if write 10L <> 3 then failwith "first write";
  if Eio.Domain_manager.run dm (fun () -> write 11L) <> 5 then
    failwith "write from a second domain"

(* The binding compiles only while [fail] and the value codecs are
   portable. *)
let (codecs @ portable) = fun () ->
  D.text (D.s "x"), D.int (D.i 3L), D.nullable_int (D.ni None),
  D.nullable_text (D.ns (Some "y")),
  (match D.fail "probe" with exception Failure m -> m | () -> "")

let () =
  if codecs () <> ("x", 3L, None, Some "y", "Imap_store: probe") then
    failwith "value codecs";
  Eio_main.run (fun env ->
    with_db (fun t -> D.locked t bind_count);
    with_db (fun t -> D.locked t prepared_reuse);
    with_db (fun t -> D.locked t batch_reuse);
    with_db nested;
    with_db (shared (Eio.Stdenv.domain_mgr env)))
