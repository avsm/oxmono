open! Core
open! Async

type t =
  { handle_connection : transport:Rpc.Transport.t -> unit Deferred.t
  ; on_shutdown : (unit -> unit) Vec.t
  }

let create ~implementations ~initial_connection_state : t =
  let on_shutdown = Vec.create () in
  let handle_connection ~transport =
    let%bind connection =
      Async_rpc_kernel.Rpc.Connection.create
        ~implementations
        ~connection_state:initial_connection_state
        transport
      >>| Result.ok_exn
    in
    Vec.push_back on_shutdown (fun () -> don't_wait_for (Rpc.Connection.close connection));
    let%bind () = Rpc.Connection.close_finished connection in
    let%bind () = Rpc.Transport.close transport in
    return ()
  in
  { handle_connection; on_shutdown }
;;

let shutdown t = Vec.iter t.on_shutdown ~f:(fun f -> f ())

let client_connection t =
  let one_transport_end ~pipe_to ~pipe_from =
    let pipe_to_in, _ = pipe_to in
    let _, pipe_from_out = pipe_from in
    Async_rpc_kernel.Pipe_transport.create
      Async_rpc_kernel.Pipe_transport.Kind.string
      pipe_to_in
      pipe_from_out
  in
  let pipe_from_client_to_server = Pipe.create () in
  let pipe_from_server_to_client = Pipe.create () in
  let server_end =
    one_transport_end
      ~pipe_to:pipe_from_client_to_server
      ~pipe_from:pipe_from_server_to_client
  in
  let client_end =
    one_transport_end
      ~pipe_to:pipe_from_server_to_client
      ~pipe_from:pipe_from_client_to_server
  in
  don't_wait_for (t.handle_connection ~transport:server_end);
  Async_rpc_kernel.Rpc.Connection.create
    ?implementations:None
    ~connection_state:(fun _conn -> ())
    client_end
  >>| Result.ok_exn
;;
