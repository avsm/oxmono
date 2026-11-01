module D = Sqlite3.Data

let check = Sqlite3.Rc.check

let statement db sql args f =
  let s = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> check (Sqlite3.finalize s))
    (fun () ->
      List.iteri (fun i d -> check (Sqlite3.bind s (i + 1) d)) args;
      f s)

let execute db sql args =
  statement db sql args (fun s -> check (Sqlite3.step s))

let rows db sql args =
  statement db sql args (fun s ->
      let rec loop acc =
        match Sqlite3.step s with
        | ROW -> loop (Sqlite3.row_data s :: acc)
        | DONE -> List.rev acc
        | rc ->
            check rc;
            assert false
      in
      loop [])

let str = function D.TEXT s -> s | _ -> failwith "Invalid database text"
let num = function D.INT n -> Int64.to_int n | _ -> failwith "Invalid integer"
let txt s = D.TEXT s
let int n = D.INT (Int64.of_int n)

let transaction ?(immediate = true) db f =
  check (Sqlite3.exec db (if immediate then "BEGIN IMMEDIATE" else "BEGIN"));
  match
    let x = f () in
    check (Sqlite3.exec db "COMMIT");
    x
  with
  | x -> x
  | exception e ->
      let backtrace = Printexc.get_raw_backtrace () in
      ignore (Sqlite3.exec db "ROLLBACK");
      Printexc.raise_with_backtrace e backtrace
