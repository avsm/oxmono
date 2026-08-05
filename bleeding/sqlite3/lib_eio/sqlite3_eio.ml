(* -- Errors -- *)

type Eio.Exn.Backend.t += Sqlite3_rc of Sqlite3.Rc.t * string

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
    | Sqlite3_rc (rc, msg) ->
      Fmt.pf f "Sqlite3(%s: %s)" (Sqlite3.Rc.to_string rc) msg;
      true
    | _ -> false)

type error =
  | Open_failed of Eio.Exn.Backend.t
  | Closed

type Eio.Exn.err += E of error

let () =
  Eio.Exn.register_pp (fun f -> function
    | E (Open_failed e) ->
      Fmt.pf f "Sqlite3 Open_failed %a" Eio.Exn.Backend.pp e;
      true
    | E Closed ->
      Fmt.string f "Sqlite3 Closed";
      true
    | _ -> false)

let err e = Eio.Exn.create (E e)

(* -- Resource plumbing -- *)

type db_state = {
  handle : Sqlite3.db;
  mutable closed : bool;
}

type (_, _, _) Eio.Resource.pi +=
  | Sqlite3_db : ('t, 't -> db_state, [> `Sqlite3 ]) Eio.Resource.pi

let close_state st =
  if not st.closed then (
    st.closed <- true;
    ignore (Sqlite3.db_close st.handle : bool))

let handler =
  Eio.Resource.handler
    [
      H (Sqlite3_db, Fun.id);
      H (Eio.Resource.Close, close_state);
    ]

type t = [ `Sqlite3 | `Close ] Eio.Resource.t

let get_state (Eio.Resource.T (t, ops) : t) =
  Eio.Resource.get ops Sqlite3_db t

let db t = (get_state t).handle

let check_open t =
  let st = get_state t in
  if st.closed then raise (err Closed);
  st

(* -- Systhread wrapper with cancellation -- *)

(** Run [fn handle] in a system thread after verifying [t] is open.

    Uses {!Eio.Fiber.first} to race the systhread operation against a
    cancellation watcher.  If the fiber's cancel context fires while the
    systhread is blocked inside SQLite, [sqlite3_interrupt] is called so
    the blocking C call returns promptly. *)
let run t ~label fn =
  let st = check_open t in
  let completed = Atomic.make false in
  Eio.Fiber.first
    (fun () ->
      let x = Eio_unix.run_in_systhread ~label (fun () -> fn st.handle) in
      Atomic.set completed true;
      x)
    (fun () ->
      Fun.protect
        ~finally:(fun () ->
          if not (Atomic.get completed) then
            Sqlite3.interrupt st.handle)
        (fun () -> Eio.Fiber.await_cancel ()))

(* -- Opening -- *)

let open_db ~sw ?busy_timeout ?mode ?uri ?mutex ?cache ?vfs filename =
  let handle =
    try
      Eio_unix.run_in_systhread ~label:"sqlite3_open" (fun () ->
          Sqlite3.db_open ?mode ?uri ?mutex ?cache ?vfs filename)
    with
    | Sqlite3.Error msg ->
      raise (err (Open_failed (Sqlite3_rc (Sqlite3.Rc.CANTOPEN, msg))))
  in
  Option.iter (Sqlite3.busy_timeout handle) busy_timeout;
  let st = { handle; closed = false } in
  let t : t = Eio.Resource.T (st, handler) in
  Eio.Switch.on_release sw (fun () -> close_state st);
  t

let open_path ~sw ?busy_timeout ?mode ?uri ?mutex ?cache ?vfs path =
  let filename = Eio.Path.native_exn path in
  try open_db ~sw ?busy_timeout ?mode ?uri ?mutex ?cache ?vfs filename
  with Eio.Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "opening database %a" Eio.Path.pp path

let open_memory ~sw ?busy_timeout ?mutex ?cache () =
  open_db ~sw ?busy_timeout ?mutex ?cache ":memory:"

(* -- SQL execution -- *)

let exec t ?cb sql =
  run t ~label:"sqlite3_exec" (fun handle ->
      Sqlite3.exec handle ?cb sql)

let exec_no_headers t ~cb sql =
  run t ~label:"sqlite3_exec" (fun handle ->
      Sqlite3.exec_no_headers handle ~cb sql)

let prepare t sql =
  try
    run t ~label:"sqlite3_prepare" (fun handle ->
        Sqlite3.prepare handle sql)
  with
  | (Sqlite3.SqliteError msg | Sqlite3.Error msg) ->
    let bt = Printexc.get_raw_backtrace () in
    let ex = Eio.Exn.create (Eio.Exn.X (Sqlite3_rc (Sqlite3.Rc.ERROR, msg))) in
    Eio.Exn.reraise_with_context ex bt "preparing: %s" sql

let step t stmt =
  run t ~label:"sqlite3_step" (fun _handle -> Sqlite3.step stmt)

let reset t stmt =
  run t ~label:"sqlite3_reset" (fun _handle -> Sqlite3.reset stmt)

let finalize t stmt =
  run t ~label:"sqlite3_finalize" (fun _handle -> Sqlite3.finalize stmt)

let iter t stmt ~f =
  run t ~label:"sqlite3_iter" (fun _handle -> Sqlite3.iter stmt ~f)

let fold t stmt ~f ~init =
  run t ~label:"sqlite3_fold" (fun _handle -> Sqlite3.fold stmt ~f ~init)
