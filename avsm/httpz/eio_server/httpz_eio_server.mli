(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Httpz Eio server - Connection handling for Eio-based servers.

    This module provides Eio integration for httpz, handling the connection
    lifecycle, request parsing, and response writing. It bridges the gap
    between the core {!Httpz} protocol library and Eio's networking primitives.

    {2 Architecture}

    {[
      Eio socket → [Httpz_eio_server.handle_client] → [Httpz_route.dispatch]
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
        Httpz_eio_server.handle_client
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

    See {!Httpz_route} for defining routes. *)

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
  is_head:bool ->
  keep_alive:bool ->
  Httpz.Version.t ->
  status:Httpz.Res.status ->
  headers:local_ Httpz_route.resp_header list ->
  Httpz_route.body ->
  unit
(** [make_respond conn ~is_head ~keep_alive version ~status ~headers body] writes
    an HTTP response to the connection.

    This function is used as the [respond] callback for route handlers.
    It handles:
    - Status line and header serialization
    - Content-Length calculation
    - Connection header based on [keep_alive]
    - Body transmission (string, bigstring, or streaming)
    - For HEAD requests ([is_head = true]), sends headers with Content-Length
      but suppresses the body

    {b Note:} Typically called indirectly via {!Httpz_route} helpers
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

(** {1 Request Metadata} *)

(** OxCaml mixed block capturing full request/response metadata.
    The [float#] field avoids heap-boxing the timestamp (saves 24 bytes per
    request). Optional fields use [or_null] instead of [option] to avoid
    allocating [Some] boxes (6 fewer heap allocations per request).
    Passed to [on_request] as [@ local] so the record can be
    stack-allocated. *)
type request_info = {
  remote_addr : string;
  meth : Httpz.Method.t;
  target : string;
  path : string;
  host : string or_null;
  user_agent : string or_null;
  referer : string or_null;
  accept : string or_null;
  forwarded_for : string or_null;
  forwarded_proto : string or_null;
  request_headers : (string * string) list;
  status : Httpz.Res.status;
  response_content_type : string or_null;
  cache_status : string or_null;
  timestamp : float#;
  response_body_size : int;
  duration_us : int;
}

(** {1 Connection Handling} *)

val handle_client :
  routes:Httpz_route.t ->
  on_request:(request_info @ local -> unit) ->
  on_error:(exn -> unit) ->
  [> [> `Generic ] Eio.Net.stream_socket_ty ] Eio.Net.stream_socket ->
  Eio.Net.Sockaddr.stream ->
  unit
(** [handle_client ~routes ~on_request ~on_error socket addr] handles a
    client connection.

    Processes HTTP requests in a loop until the connection closes:
    1. Reads request data from the socket
    2. Parses the HTTP request using {!Httpz.parse}
    3. Dispatches to matching route via {!Httpz_route.dispatch}
    4. Writes response using {!make_respond}
    5. Continues if keep-alive, otherwise closes

    @param routes Route table for request dispatch
    @param on_request Called after each request completes with a
           {!request_info} mixed block containing full request/response
           metadata. The record is passed [@ local] so it can be
           stack-allocated — all values must be consumed before the
           callback returns. The [float#] timestamp field avoids
           heap-boxing.
    @param on_error Called if an exception occurs. The connection is closed
           after an error.

    {[
      Eio.Net.run_server net addr (fun flow addr ->
        handle_client
          ~routes:my_routes
          ~on_request:(fun (info @ local) ->
            Log.info "%s %s %s (%dus)"
              (Httpz.Method.to_string info.meth) info.path
              (Httpz.Res.status_to_string info.status) info.duration_us)
          ~on_error:(fun exn -> Log.err "%s" (Printexc.to_string exn))
          flow addr)
    ]} *)

(** {1 Static File Serving} *)

(** Static file serving over Eio, exposed as an {!Httpz_route} route.

    Implements the parts of RFC 7232/7233 that a static server needs:
    strong-ish weak ETags derived from mtime and size, [If-None-Match]
    conditional GET (304), single-range [Range] requests (206), range
    rejection (416), directory index resolution, and path normalisation that
    cannot escape the served root.

    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let root = Eio.Path.(Eio.Stdenv.fs env / "/srv/www") in
      let routes =
        Httpz_eio_server.Static.routes (Httpz_eio_server.Static.create ~sw root)
      in
      ...
    ]} *)
module Static : sig
  type t
  (** A configured static file server rooted at a directory. *)

  val create :
    sw:Eio.Switch.t ->
    ?index:string list ->
    ?max_inline:int ->
    ?chunk_size:int ->
    Eio.Fs.dir_ty Eio.Path.t ->
    t
  (** [create ~sw root] is a static file server serving the contents of [root].

      [root] is confined with {!Eio.Path.open_subtree}: requests cannot escape
      it through [..] or through a symlink pointing outside, and an attempt to
      is answered with 404. Confinement does not depend on which capability the
      caller passes. The directory is held open until [sw] finishes.

      @param index Directory index candidates, tried in order when the
             request resolves to a directory. Defaults to
             [["index.html"]]; an empty list disables index resolution
             (directories then 404).
      @param max_inline Bodies of at most this many bytes are read into a
             single bigstring and written without an intermediate string
             (default 1 MiB). Larger bodies are streamed.
      @param chunk_size Read/write chunk size used when streaming bodies
             larger than [max_inline] (default 64 KiB). *)

  val route : t -> Httpz_route.route
  (** [route t] is a catch-all [GET] route (matching {!Httpz_route.tail}) that
      serves files from [t]. [HEAD] is matched automatically by the router and
      answered with headers only — including an accurate [Content-Length] —
      without reading the file. Add it last if you combine it with other
      routes, since it matches every path. *)

  val routes : t -> Httpz_route.t
  (** [routes t] is [Httpz_route.of_list [route t]], for the common case of a
      server that only serves static files. *)

  val mime_type : string -> string
  (** [mime_type name] guesses a [Content-Type] from the extension of [name],
      falling back to ["application/octet-stream"]. *)

  val etag_opaque : mtime:float -> size:int -> string
  (** [etag_opaque ~mtime ~size] is the opaque entity-tag content (no quotes,
      no [W/] prefix) used for a file with the given modification time and
      size. *)
end
