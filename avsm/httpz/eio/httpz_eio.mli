(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Httpz Eio - Connection handling for Eio-based servers.

    This module provides Eio integration for httpz, handling the connection
    lifecycle, request parsing, and response writing. It bridges the gap
    between the core {!Httpz} protocol library and Eio's networking primitives.

    {2 Architecture}

    {[
      Eio socket → [Httpz_eio.handle_client] → [Httpz_server.Route.dispatch]
                        ↓                              ↓
                   request parsing              route matching
                        ↓                              ↓
                   [Httpz.parse]              handler execution
                        ↓                              ↓
                  conn state                   [make_respond]
                        ↓                              ↓
                  response writing ←──────────────────┘
    ]}

    {2 Usage with Eio}

    {[
      let handle ~routes flow addr =
        Httpz_eio.handle_client
          ~routes
          ~on_request:(fun ~meth ~path ~status ->
            Logs.info (fun m -> m "%s %s -> %s"
              (Httpz.Method.to_string meth)
              path
              (Httpz.Res.status_to_string status)))
          ~on_error:(fun exn ->
            Logs.err (fun m -> m "Error: %s" (Printexc.to_string exn)))
          flow addr

      let main () =
        Eio_main.run @@ fun env ->
        let net = Eio.Stdenv.net env in
        let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
        Eio.Net.run_server net addr handle
    ]}

    See {!Httpz_server.Route} for defining routes. *)

(** {1 Connection State} *)

type 'a conn constraint 'a = [> [> `Generic ] Eio.Net.stream_socket_ty ]
(** Connection state holding read/write buffers and socket.

    The type parameter constrains the socket type to Eio stream sockets. *)

val create_conn : ([> [> `Generic ] Eio.Net.stream_socket_ty ] as 'a) Eio.Net.stream_socket -> 'a conn
(** [create_conn socket] creates connection state from an Eio socket.

    Allocates internal buffers for request parsing and response writing. *)

(** {1 Response Writing} *)

val make_respond :
  ([> [> `Generic ] Eio.Net.stream_socket_ty ] as 'a) conn ->
  keep_alive:bool ->
  Httpz.Version.t ->
  status:Httpz.Res.status ->
  headers:local_ Httpz_server.Route.resp_header list ->
  Httpz_server.Route.body ->
  unit
(** [make_respond conn ~keep_alive version ~status ~headers body] writes
    an HTTP response to the connection.

    This function is used as the [respond] callback for route handlers.
    It handles:
    - Status line and header serialization
    - Content-Length calculation
    - Connection header based on [keep_alive]
    - Body transmission (string, bigstring, or streaming)

    {b Note:} Typically called indirectly via {!Httpz_server.Route} helpers
    like [html], [json], etc. Direct use is for advanced scenarios. *)

val send_error :
  ([> [> `Generic ] Eio.Net.stream_socket_ty ] as 'a) conn ->
  Httpz.Res.status ->
  string ->
  Httpz.Version.t ->
  unit
(** [send_error conn status message version] sends a simple error response.

    Writes a plain text response with the given status and message body.
    Useful for sending 400, 404, 500 responses outside of normal routing. *)

(** {1 Connection Handling} *)

val handle_client :
  routes:Httpz_server.Route.t ->
  on_request:(meth:Httpz.Method.t -> path:string -> status:Httpz.Res.status -> unit) ->
  on_error:(exn -> unit) ->
  [> [> `Generic ] Eio.Net.stream_socket_ty ] Eio.Net.stream_socket ->
  Eio.Net.Sockaddr.stream ->
  unit
(** [handle_client ~routes ~on_request ~on_error socket addr] handles a
    client connection.

    Processes HTTP requests in a loop until the connection closes:
    1. Reads request data from the socket
    2. Parses the HTTP request using {!Httpz.parse}
    3. Dispatches to matching route via {!Httpz_server.Route.dispatch}
    4. Writes response using {!make_respond}
    5. Continues if keep-alive, otherwise closes

    @param routes Route table for request dispatch
    @param on_request Called after each request completes with method, path,
           and response status. Use for logging/metrics.
    @param on_error Called if an exception occurs. The connection is closed
           after an error.

    {[
      Eio.Net.run_server net addr (fun flow addr ->
        handle_client
          ~routes:my_routes
          ~on_request:(fun ~meth ~path ~status ->
            Log.info "%s %s %d" (Method.to_string meth) path (Res.status_code status))
          ~on_error:(fun exn -> Log.err "%s" (Printexc.to_string exn))
          flow addr)
    ]} *)
