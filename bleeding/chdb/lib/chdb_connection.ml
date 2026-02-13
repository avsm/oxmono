type t = {
  mutable handle : Chdb_raw.connection_handle option;
  path : string option;
  mutex : Mutex.t;
}

let connect ?path () =
  let args = match path with
    | None -> [| "clickhouse"; "--" |]
    | Some p -> [| "clickhouse"; "--path=" ^ p; "--" |]
  in
  match Chdb_raw.connect args with
  | Null ->
    Chdb_error.raise_connection_failed "Failed to create connection"
  | This handle ->
    { handle = Some handle; path; mutex = Mutex.create () }

let close t =
  Mutex.lock t.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) (fun () ->
    match t.handle with
    | None -> ()
    | Some h ->
      Chdb_raw.close_conn h;
      t.handle <- None)

let[@inline always] is_open t = match t.handle with
  | None -> false
  | Some h -> Chdb_raw.conn_is_open h

let with_handle t ~f =
  Mutex.lock t.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) (fun () ->
    match t.handle with
    | None -> raise (Chdb_error.E Chdb_error.Use_after_close)
    | Some h -> f h)

let with_connection ?path ~f () =
  let conn = connect ?path () in
  match f conn with
  | result -> close conn; result
  | exception e -> close conn; raise e

let path t = t.path
