module S = Sqlite3
type read = Read
type write = Write
type 'access t = { db : Sqlite3_eio.t; lock : Eio.Mutex.t }
let check = S.Rc.check
let exec db sql = check (S.exec db sql)
let with_stmt db sql f =
  let s = S.prepare db sql in
  Fun.protect ~finally:(fun () -> ignore (S.finalize s)) (fun () -> f s)
let bind s values = check (S.bind_values s values)
let done_ s = match S.step s with
  | S.Rc.DONE -> () | rc -> check rc; failwith "Expected SQL completion"
let text s = S.Data.TEXT s
let int n = S.Data.INT (Int64.of_int n)
let opt_text = function None -> "" | Some s -> s
(* Publication rolls back expected source and SQL failures. Keep those
   failures from poisoning the lock needed to record errors and retry. *)
let with_lock t f =
  let result = Eio.Mutex.use_rw ~protect:false t.lock (fun () ->
    try Ok (f ()) with ex -> Error (ex, Printexc.get_raw_backtrace ())) in
  match result with
  | Ok value -> value
  | Error (ex, backtrace) -> Printexc.raise_with_backtrace ex backtrace

let run t f = with_lock t (fun () -> Sqlite3_eio.run t.db f)


let transaction db f =
  exec db "BEGIN IMMEDIATE";
  match (let result = f () in exec db "COMMIT"; result) with
  | result -> result
  | exception ex ->
      let bt = Printexc.get_raw_backtrace () in
      (try exec db "ROLLBACK" with _ -> ());
      Printexc.raise_with_backtrace ex bt

let open_db ~sw ~read_only path =
  let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000
    ?mode:(if read_only then Some `READONLY else None) path in
  let t = { db; lock = Eio.Mutex.create () } in
  run t (fun db ->
    let version = with_stmt db "PRAGMA user_version" (fun s ->
      ignore (S.step s); S.column_int s 0) in
    let application = with_stmt db "PRAGMA application_id" (fun s ->
      ignore (S.step s); S.column_int s 0) in
    if version < 0 || version > Schema.current_version then
      failwith "Unsupported catalogue schema";
    if version = 0 then begin
      let tables = with_stmt db
        "SELECT count(*) FROM sqlite_master WHERE name NOT LIKE 'sqlite_%'"
        (fun s -> ignore (S.step s); S.column_int s 0) in
      if tables <> 0 || application <> 0 then
        failwith "Refusing to initialise a nonempty database"
    end else if application <> 1413566547 then
      failwith "Database is not a Taposaur catalogue";
    if version < Schema.current_version then begin
      if read_only then failwith "Run taposaur enrich to upgrade the catalogue";
      transaction db (fun () ->
        List.iter (fun (target, sql) ->
          if version < target then exec db sql) Schema.migrations)
    end;
    if not read_only then exec db "PRAGMA journal_mode=WAL";
    if read_only then exec db "PRAGMA query_only=ON");
  t

let open_read ~sw path : read t = open_db ~sw ~read_only:true path
let open_write ~sw path : write t = open_db ~sw ~read_only:false path

let exclusive t f = with_lock t (fun () ->
  f (fun action -> Sqlite3_eio.run t.db action))
