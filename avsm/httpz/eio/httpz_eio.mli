(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Eio integration for httpz - response writing and connection handling

    This module provides Eio-based connection handling for httpz routes.
    It handles request parsing, response writing, and connection lifecycle. *)

(** {1 Connection State} *)

type 'a conn constraint 'a = [> [> `Generic ] Eio.Net.stream_socket_ty ]
(** Connection state with read/write buffers and socket. *)

val create_conn : ([> [> `Generic ] Eio.Net.stream_socket_ty ] as 'a) Eio.Net.stream_socket -> 'a conn
(** Create a new connection from a socket. *)

(** {1 Response Writing} *)

val make_respond :
  ([> [> `Generic ] Eio.Net.stream_socket_ty ] as 'a) conn ->
  keep_alive:bool ->
  Httpz.Version.t ->
  status:Httpz.Res.status ->
  headers:local_ Httpz.Route.resp_header list ->
  Httpz.Route.body ->
  unit
(** [make_respond conn ~keep_alive version ~status ~headers body] writes
    the response directly to the connection. Used as the [respond] callback
    for route handlers. *)

val send_error :
  ([> [> `Generic ] Eio.Net.stream_socket_ty ] as 'a) conn ->
  Httpz.Res.status ->
  string ->
  Httpz.Version.t ->
  unit
(** [send_error conn status message version] sends an error response. *)

(** {1 Connection Handling} *)

val handle_client :
  routes:Httpz.Route.t ->
  on_request:(meth:Httpz.Method.t -> path:string -> status:Httpz.Res.status -> unit) ->
  on_error:(exn -> unit) ->
  [> [> `Generic ] Eio.Net.stream_socket_ty ] Eio.Net.stream_socket ->
  Eio.Net.Sockaddr.stream ->
  unit
(** [handle_client ~routes ~on_request ~on_error flow addr] handles a client
    connection, processing requests until the connection closes.

    @param routes The route table to dispatch requests to.
    @param on_request Called after each request with method, path, and status for logging.
    @param on_error Called if an exception occurs during handling. *)
