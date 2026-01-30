(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>.

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

  3. Neither the name of the copyright holder nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
  SPDX-License-Identifier: BSD-3-Clause
 ---------------------------------------------------------------------------*)

(** HTTP/2 Client with Concurrent Stream Multiplexing.

    This module provides a client for making HTTP/2 requests over
    an established TLS connection. It handles:
    - Connection preface and settings exchange
    - True request/response multiplexing via a background reader fiber
    - Flow control
    - HPACK header compression

    {2 Architecture}

    The client uses a centralized frame reader fiber that dispatches
    incoming frames to per-stream event queues. This allows multiple
    concurrent requests to share a single HTTP/2 connection without
    blocking each other.

    {2 Usage}

    {[
      (* For a single request *)
      Eio.Switch.run @@ fun sw ->
      let response = H2_client.one_request ~sw flow
        ~meth:"GET"
        ~uri:(Uri.of_string "https://example.com/")
        ()
      in
      match response with
      | Ok r -> Printf.printf "Status: %d\n" r.status
      | Error msg -> Printf.printf "Error: %s\n" msg

      (* For multiple concurrent requests on same connection *)
      Eio.Switch.run @@ fun sw ->
      let client = H2_client.create () in
      match H2_client.handshake flow client with
      | Error msg -> failwith msg
      | Ok () ->
          (* Concurrent requests in parallel fibers *)
          let req1 = H2_protocol.make_request ~meth:"GET" ~uri:uri1 () in
          let req2 = H2_protocol.make_request ~meth:"GET" ~uri:uri2 () in
          Eio.Fiber.both
            (fun () -> H2_client.request ~sw flow client req1)
            (fun () -> H2_client.request ~sw flow client req2);
          H2_client.close flow client
    ]}

    See {{:https://datatracker.ietf.org/doc/html/rfc9113}RFC 9113}. *)

(** {1 Client State} *)

(** HTTP/2 client state. *)
type t

val create : ?settings:H2_connection.settings -> unit -> t
(** [create ?settings ()] creates a new HTTP/2 client.
    Default settings are used if not specified. *)

val connection : t -> H2_connection.t
(** [connection t] returns the underlying connection state. *)

val is_open : t -> bool
(** [is_open t] returns true if the connection is still usable. *)

val active_streams : t -> int
(** [active_streams t] returns the number of streams currently
    waiting for responses. Useful for connection pool management. *)

(** {1 Connection Lifecycle} *)

val handshake : [> Eio.Flow.two_way_ty] Eio.Resource.t -> t -> (unit, string) result
(** [handshake flow t] performs the HTTP/2 connection handshake.

    This must be called before making requests. It:
    1. Sends the connection preface and SETTINGS frame
    2. Waits for server SETTINGS
    3. Sends SETTINGS ACK
    4. Waits for server SETTINGS ACK

    @param flow The underlying connection (must be TLS with ALPN "h2")
    @param t Client state
    @return Ok () on success, Error msg on failure *)

val close : [> Eio.Flow.two_way_ty] Eio.Resource.t -> t -> unit
(** [close flow t] gracefully closes the connection.
    Sends GOAWAY and marks connection as closed. *)

(** {1 Background Reader} *)

val start_reader :
  sw:Eio.Switch.t ->
  [> Eio.Flow.two_way_ty] Eio.Resource.t ->
  t ->
  on_goaway:(last_stream_id:int32 -> error_code:H2_frame.error_code -> debug:string -> unit) ->
  unit
(** [start_reader ~sw flow t ~on_goaway] starts the background frame reader fiber.

    The reader fiber dispatches incoming frames to stream handlers and must be
    running for concurrent requests to work. The [sw] parameter should be a
    connection-lifetime switch.

    @param sw Switch for the reader fiber (connection-lifetime)
    @param flow The underlying connection
    @param t Client state
    @param on_goaway Callback invoked when GOAWAY is received from peer *)

(** {1 Making Requests} *)

val request : sw:Eio.Switch.t ->
  [> Eio.Flow.two_way_ty] Eio.Resource.t -> t ->
  H2_protocol.request -> (H2_protocol.response, string) result
(** [request ~sw flow t req] sends a request and waits for the response.

    This function can be called concurrently from multiple fibers on the
    same client. The background frame reader (started on first request)
    dispatches frames to the appropriate stream handlers.

    @param sw Switch for the background reader fiber
    @param flow The underlying connection
    @param t Client state
    @param req The request to send
    @return Response on success, Error msg on failure *)

val request_sync :
  [> Eio.Flow.two_way_ty] Eio.Resource.t -> t ->
  H2_protocol.request -> (H2_protocol.response, string) result
(** [request_sync flow t req] sends a request and waits for the response
    using synchronous I/O without spawning a background reader fiber.

    This is more efficient for single requests but does not support
    concurrent multiplexing. Use this for one-shot requests or when
    connection pooling doesn't require multiplexing.

    @param flow The underlying connection
    @param t Client state
    @param req The request to send
    @return Response on success, Error msg on failure *)

val one_request : sw:Eio.Switch.t ->
  [> Eio.Flow.two_way_ty] Eio.Resource.t ->
  meth:string -> uri:Uri.t -> ?headers:(string * string) list ->
  ?body:string -> unit -> (H2_protocol.response, string) result
(** [one_request ~sw flow ~meth ~uri ?headers ?body ()] makes a single request.

    This is a convenience function that creates a client, performs
    handshake, sends the request, and returns the response.
    Use this for one-off requests; for multiple requests on the same
    connection, use [create], [handshake], and [request] directly.

    @param sw Switch for the background reader fiber
    @param flow The underlying TLS connection
    @param meth HTTP method (GET, POST, etc)
    @param uri Request URI
    @param headers Optional request headers
    @param body Optional request body
    @return Response on success, Error msg on failure *)

val one_request_strings : sw:Eio.Switch.t ->
  [> Eio.Flow.two_way_ty] Eio.Resource.t ->
  meth:string -> scheme:string -> host:string ->
  ?port:int -> ?path:string -> ?query:(string * string list) list ->
  ?headers:(string * string) list ->
  ?body:string -> unit -> (H2_protocol.response, string) result
(** [one_request_strings ~sw flow ~meth ~scheme ~host ?port ?path ?query ?headers ?body ()]
    makes a single request using string components instead of Uri.t.

    This is useful when calling from libraries that have their own Uri module
    which would conflict with the external uri library.

    @param sw Switch for the background reader fiber
    @param flow The underlying TLS connection
    @param meth HTTP method (GET, POST, etc)
    @param scheme URL scheme (http or https)
    @param host Hostname
    @param port Optional port number
    @param path Request path (defaults to "/")
    @param query Optional query parameters
    @param headers Optional request headers
    @param body Optional request body
    @return Response on success, Error msg on failure *)

(** {1 Low-level Frame I/O} *)

val write_frame : [> Eio.Flow.sink_ty] Eio.Resource.t -> H2_frame.frame -> unit
(** [write_frame flow frame] writes a frame to the connection.
    Exposed for testing and advanced use cases. *)

val read_frame : [> Eio.Flow.source_ty] Eio.Resource.t -> H2_frame.frame option
(** [read_frame flow] reads a frame from the connection.
    Returns None on EOF or error. *)
