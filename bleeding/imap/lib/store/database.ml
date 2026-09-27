module S = Sqlite3
module SE = Sqlite3_eio

type blob_dir = Dir : _ Eio.Path.t -> blob_dir
type t = { db : SE.t; mutex : Eio.Mutex.t; blob_dir : blob_dir option;
           schema_version : int64 }

let fail what = failwith ("Imap_store: " ^ what)
let check rc = S.Rc.check rc
let sql t statement = check (SE.exec t.db statement)
let with_stmt t statement f =
  let stmt = Eio.Cancel.protect (fun () -> SE.prepare t.db statement) in
  match f stmt with
  | x -> Eio.Cancel.protect (fun () -> check (SE.finalize t.db stmt)); x
  | exception ex ->
    (try Eio.Cancel.protect (fun () -> ignore (SE.finalize t.db stmt : S.Rc.t)) with _ -> ());
    raise ex
let bind stmt values = check (S.bind_values stmt values)
let run t statement values =
  with_stmt t statement (fun stmt ->
    bind stmt values;
    match SE.step t.db stmt with
    | S.Rc.DONE -> ()
    | S.Rc.ROW -> fail "write returned rows"
    | rc -> check rc)
let run_prepared t stmt values =
  bind stmt values;
  (match SE.step t.db stmt with
   | S.Rc.DONE -> ()
   | S.Rc.ROW -> fail "write returned rows"
   | rc -> check rc);
  check (SE.reset t.db stmt)
let rows t statement values =
  with_stmt t statement (fun stmt ->
    bind stmt values;
    let rec loop acc =
      match SE.step t.db stmt with
      | S.Rc.ROW ->
        let row = Array.init (S.column_count stmt) (S.column stmt) in
        loop (row :: acc)
      | S.Rc.DONE -> List.rev acc
      | rc -> check rc; assert false in
    loop [])
let text = function S.Data.TEXT x -> x | _ -> fail "expected TEXT"
let int = function S.Data.INT x -> x | _ -> fail "expected INTEGER"
let nullable_int = function S.Data.NULL -> None | x -> Some (int x)
let nullable_text = function S.Data.NULL -> None | x -> Some (text x)
let i x = S.Data.INT x
let s x = S.Data.TEXT x
let ni = function None -> S.Data.NULL | Some x -> i x
let ns = function None -> S.Data.NULL | Some x -> s x

let transaction ?(begin_sql="BEGIN IMMEDIATE") t f =
  Eio.Mutex.use_ro t.mutex (fun () ->
    let committed = ref false in
    Fun.protect
      ~finally:(fun () -> if not !committed then
        try Eio.Cancel.protect (fun () -> sql t "ROLLBACK") with _ -> ())
      (fun () ->
        Eio.Cancel.protect (fun () -> sql t begin_sql);
        let result = f () in
        Eio.Cancel.protect (fun () -> sql t "COMMIT");
        committed := true;
        result))

