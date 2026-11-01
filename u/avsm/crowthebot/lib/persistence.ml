let sql db source = Sqlite3.Rc.check (Sqlite3_eio.exec db source)

let statement db source args f =
  let stmt = Sqlite3_eio.prepare db source in
  Fun.protect ~finally:(fun () ->
      Eio.Cancel.protect (fun () -> ignore (Sqlite3_eio.finalize db stmt)))
  @@ fun () ->
  List.iteri (fun i v -> Sqlite3.Rc.check (Sqlite3.bind stmt (i + 1) v)) args;
  f stmt

let execute db source args =
  statement db source args (fun stmt ->
      Sqlite3.Rc.check (Sqlite3_eio.step db stmt))

let rows db source args f =
  statement db source args (fun stmt ->
      let rec loop acc =
        match Sqlite3_eio.step db stmt with
        | Sqlite3.Rc.ROW -> loop (f stmt :: acc)
        | Sqlite3.Rc.DONE -> List.rev acc
        | rc ->
            Sqlite3.Rc.check rc;
            assert false
      in
      loop [])

let text s = Sqlite3.Data.TEXT s
let integer i = Sqlite3.Data.INT (Int64.of_int i)
let optional f = Option.fold ~none:Sqlite3.Data.NULL ~some:f

let string_opt s i =
  match Sqlite3.column s i with
  | Sqlite3.Data.NULL -> None
  | _ -> Some (Sqlite3.column_text s i)

let locked mutex f =
  match
    Eio.Mutex.use_rw ~protect:true mutex (fun () ->
        try Ok (f ()) with exn -> Error (exn, Printexc.get_raw_backtrace ()))
  with
  | Ok result -> result
  | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

let transaction db f =
  sql db "BEGIN IMMEDIATE";
  try
    let result = f () in
    sql db "COMMIT";
    result
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Cancel.protect (fun () -> sql db "ROLLBACK");
    Printexc.raise_with_backtrace exn bt

let last_id db = Int64.to_int (Sqlite3.last_insert_rowid (Sqlite3_eio.db db))
