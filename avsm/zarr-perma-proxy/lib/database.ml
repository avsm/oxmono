type t = { handle : Sqlite3_eio.t; mutex : Eio.Mutex.t }

let run t f = Eio.Mutex.use_ro t.mutex (fun () -> Sqlite3_eio.run t.handle f)
let exec db sql = Sqlite3.Rc.check (Sqlite3.exec db sql)

let statement db sql params f =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      List.iteri
        (fun i value -> Sqlite3.Rc.check (Sqlite3.bind stmt (i + 1) value))
        params;
      f stmt)

let execute db sql params =
  statement db sql params (fun stmt ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          Sqlite3.Rc.check rc;
          failwith "SQLite statement returned rows")

let rows db sql params =
  statement db sql params (fun stmt ->
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> loop (Sqlite3.row_data stmt :: acc)
        | Sqlite3.Rc.DONE -> List.rev acc
        | rc ->
            Sqlite3.Rc.check rc;
            assert false
      in
      loop [])

let integer db sql params =
  match rows db sql params with
  | [ [| Sqlite3.Data.INT n |] ] -> n
  | _ -> failwith "Invalid SQLite integer result"

let transaction db f =
  exec db "BEGIN IMMEDIATE";
  match
    let value = f () in
    exec db "COMMIT";
    value
  with
  | value -> value
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      (try exec db "ROLLBACK" with _ -> ());
      Printexc.raise_with_backtrace exn bt

let open_ ~sw ?(readonly = false) path schema =
  let parent, name =
    match Eio.Path.split path with
    | Some pair -> pair
    | None -> invalid_arg "Database path must name a file"
  in
  Eioext.Path.require_private_dir parent;
  List.iter
    (fun suffix ->
      match Eio.Path.kind ~follow:false Eio.Path.(parent / (name ^ suffix)) with
      | `Not_found | `Regular_file -> ()
      | _ -> invalid_arg "SQLite storage must be regular files")
    [ ""; "-wal"; "-shm"; "-journal" ];
  let handle =
    Sqlite3_eio.open_path ~sw ~busy_timeout:5000
      ?mode:(if readonly then Some `READONLY else None)
      path
  in
  let t = { handle; mutex = Eio.Mutex.create () } in
  run t (fun db ->
      exec db "PRAGMA foreign_keys=ON";
      if not readonly then (
        exec db "PRAGMA journal_mode=WAL";
        exec db "PRAGMA synchronous=FULL");
      let version = integer db "PRAGMA user_version" [] in
      if version = 0L && not readonly then
        transaction db (fun () ->
            exec db schema;
            exec db "PRAGMA user_version=1")
      else if version <> 1L then failwith "Unsupported proxy database version");
  t
