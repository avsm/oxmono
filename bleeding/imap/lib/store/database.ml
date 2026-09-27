module S = Sqlite3
module SE = Sqlite3_eio

type blob_dir = Dir : _ Eio.Path.t -> blob_dir
type t = { db : SE.t; mutex : Eio.Mutex.t; blob_dir : blob_dir option }

let fail what = failwith ("Imap_store: " ^ what)
let check t rc =
  if not (S.Rc.is_success rc) then
    raise (S.SqliteError (S.Rc.to_string rc ^ ": " ^ S.errmsg (SE.db t.db)))
let sql t statement = check t (SE.exec t.db statement)
let with_stmt t statement f =
  let stmt = Eio.Cancel.protect (fun () -> SE.prepare t.db statement) in
  match f stmt with
  | x -> Eio.Cancel.protect (fun () -> check t (SE.finalize t.db stmt)); x
  | exception ex ->
    (try Eio.Cancel.protect (fun () -> ignore (SE.finalize t.db stmt : S.Rc.t)) with _ -> ());
    raise ex
let bind t stmt values =
  let expected = S.bind_parameter_count stmt in
  let supplied = List.length values in
  if supplied <> expected then
    invalid_arg (Printf.sprintf
      "Imap_store: statement expects %d values, got %d" expected supplied);
  check t (S.bind_values stmt values)

(* A failed step leaves its error code in the statement, which reset
   returns again, so only a reset after success is checked. *)
let with_reset t stmt f =
  match f () with
  | x ->
    Eio.Cancel.protect (fun () -> check t (SE.reset t.db stmt));
    check t (S.clear_bindings stmt);
    x
  | exception ex ->
    let backtrace = Printexc.get_raw_backtrace () in
    (try Eio.Cancel.protect (fun () ->
       ignore (SE.reset t.db stmt : S.Rc.t);
       ignore (S.clear_bindings stmt : S.Rc.t)) with _ -> ());
    Printexc.raise_with_backtrace ex backtrace
let run_prepared t stmt values =
  with_reset t stmt (fun () ->
    bind t stmt values;
    match SE.step t.db stmt with
    | S.Rc.DONE -> ()
    | S.Rc.ROW -> fail "write returned rows"
    | rc -> check t rc; fail ("write step returned " ^ S.Rc.to_string rc))
let rows_prepared t stmt values =
  with_reset t stmt (fun () ->
    bind t stmt values;
    let rec loop acc =
      match SE.step t.db stmt with
      | S.Rc.ROW ->
        let row = Array.init (S.column_count stmt) (S.column stmt) in
        loop (row :: acc)
      | S.Rc.DONE -> List.rev acc
      | rc -> check t rc; fail ("read step returned " ^ S.Rc.to_string rc) in
    loop [])
let run t statement values =
  with_stmt t statement (fun stmt -> run_prepared t stmt values)
let rows t statement values =
  with_stmt t statement (fun stmt -> rows_prepared t stmt values)
let changes t = S.changes (SE.db t.db)
let text = function S.Data.TEXT x -> x | _ -> fail "expected TEXT"
let int = function S.Data.INT x -> x | _ -> fail "expected INTEGER"
let nullable_int = function S.Data.NULL -> None | x -> Some (int x)
let nullable_text = function S.Data.NULL -> None | x -> Some (text x)
let i x = S.Data.INT x
let s x = S.Data.TEXT x
let ni = function None -> S.Data.NULL | Some x -> i x
let ns = function None -> S.Data.NULL | Some x -> s x

(* The mutex is not reentrant, so a nested acquisition would wait on
   itself. Holding it is recorded per fiber to turn that into an error. *)
let held : Eio.Mutex.t list Eio.Fiber.key = Eio.Fiber.create_key ()

let locked t f =
  let owned = Option.value ~default:[] (Eio.Fiber.get held) in
  if List.memq t.mutex owned then
    invalid_arg "Imap_store: nested database transaction";
  Eio.Mutex.use_ro t.mutex (fun () ->
    Eio.Fiber.with_binding held (t.mutex :: owned) f)

let transaction ?(begin_sql="BEGIN IMMEDIATE") t f =
  locked t (fun () ->
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
