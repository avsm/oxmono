(** This module provides HTTP/1.1 serving for {!Proffer} with Httpz and Eio.

    The server uses one listening socket and one fiber per connection. Shared
    {!Proffer.Backend} processing keeps dispatch, conditional requests, and HEAD
    behavior consistent with other backends and [proffer.mock].

    Response-path allocation checks exclude the external I/O, application
    callback and monotonic-clock sampling boundaries. Sampling Eio's clock and
    converting its Mtime result may allocate; the sampled timestamp returns
    unboxed to the checked response path.

    {[
      Eio_main.run @@ fun stdenv ->
      Proffer_httpz.run stdenv ~env:My_site.env My_site.site
    ]} *)

type config = {
  backlog : int;
      (** [backlog] is the maximum depth of the kernel accept queue. *)
  max_connections : int;
      (** [max_connections] is the maximum number of concurrently open
          connections. New connections wait in the accept queue while this limit
          is reached. *)
  first_byte_timeout : float;
      (** [first_byte_timeout] is the maximum time in seconds a newly accepted
          connection may send no request bytes. *)
  idle_timeout : float;
      (** [idle_timeout] is the maximum idle time in seconds between requests
          on a persistent connection. *)
  request_timeout : float;
      (** [request_timeout] is the maximum time in seconds from the first
          request byte until the complete head and body arrive. A timeout
          produces 408 Request Timeout and closes the connection. *)
  write_timeout : float;
      (** [write_timeout] is the maximum time in seconds one write to the
          socket may take. A client that stops reading would otherwise pin its
          connection fibre for as long as it cares to. A write that exceeds it
          is reported to [on_error] and the connection closes. *)
}
(** A [config] sets server limits. Derive custom configurations from
    {!default_config} so new fields can acquire defaults. *)

type tls = Httpz_tls.server
(** A [tls] value upgrades a newly accepted connection before HTTP parsing.
    Build one with {!Httpz_tls.val-server}; other compatible Eio flow wrappers
    can be supplied for testing or alternate TLS implementations. *)

val default_config : config
(** [default_config] is a configuration that uses a backlog of 64, permits 512
    open connections, waits 5 seconds for a new connection's first byte and 75
    seconds between persistent requests, allows 15 seconds for a request, and
    30 seconds for one write. *)

type event = {
  remote_addr : string;
      (** [remote_addr] is the TCP peer as ["addr:port"], or the peer's Unix
          socket path. *)
  meth : Proffer.Method.t;  (** [meth] is the method from the request line. *)
  target : string;
      (** [target] is the request target as it was sent, still percent-encoded
          and including the query. A query carries whatever the client put
          there, including bearer tokens and session identifiers, so logging
          [target] logs those too; log {!path} instead unless the query is
          wanted. *)
  path : string;
      (** [path] is the percent-encoded portion of [target] before ['?']. It is
          empty when the server rejects a request before routing. *)
  request_headers : (string * string) list;
      (** [request_headers] is the request fields not consumed for framing, in
          arrival order. Content-Length, Transfer-Encoding, Connection, and
          Expect are omitted. Field-name matching must be case-insensitive.
          The values of Authorization, Proxy-Authorization, and Cookie are
          replaced by ["<redacted>"]; handlers still receive the real values.
      *)
  status : Proffer.Status.t;  (** [status] is the status sent to the client. *)
  response_content_type : string option;
      (** [response_content_type] is the response Content-Type, or [None] when
          none was sent. *)
  cache_status : string option;
      (** [cache_status] is the response X-Cache value, or [None] when absent.
      *)
  body_size : int;
      (** [body_size] is the number of body bytes sent, so it is zero for HEAD
          and contentless statuses. *)
  duration_us : int;
      (** [duration_us] is the number of microseconds from successfully parsing
          the request head to writing the last response byte, measured with the
          monotonic clock. For a handoff it ends when the HTTP response head is
          written and excludes the tunnel or upgraded session lifetime. *)
}
(** An [event] is the information recorded for one parsed request. The
    callback receives it at [local], so it must be inspected or serialized
    before the callback returns. *)

val globalize_event : event @ local -> event
(** [globalize_event event] is a heap copy of [event] that can outlive the
    callback. *)

val run :
  ?sw:Eio.Switch.t ->
  ?port:int ->
  ?addr:Eio.Net.Sockaddr.stream ->
  ?config:config ->
  ?tls:tls ->
  ?on_listening:(Eio.Net.Sockaddr.stream -> unit) ->
  ?on_event:(event @ local -> unit) ->
  ?on_error:(exn -> unit) ->
  ?stop:'a Eio.Promise.t ->
  < net : _ Eio.Net.t;
    clock : _ Eio.Time.clock;
    mono_clock : _ Eio.Time.Mono.t;
    .. > ->
  env:'env ->
  'env Proffer.Site.t ->
  unit
(** [run stdenv ~env site] is a server loop that serves [site] until it is
    stopped. [stdenv] supplies the network and monotonic clock for deadlines
    and elapsed durations. HTTP dates use the process wall clock separately.
    [Eio_main.run]'s environment works as it stands. [env] is passed to every
    handler.

    The server listens on [addr], or on the loopback interface at [port], which
    defaults to 8765. Passing both raises [Invalid_argument]. It runs under
    [sw] and stops when [sw] is cancelled. Without one it creates its own
    switch and only returns on an error.

    Serving is single-domain: connections are fibers, so [env] may hold state
    bound to this domain.

    [tls] defaults to absent, so the listener speaks plaintext HTTP. Pass
    [Httpz_tls.server config] to serve HTTPS. The TLS handshake is bounded by
    [config.first_byte_timeout], independently of the same timeout subsequently
    applied while waiting for the first HTTP request byte. The default
    [on_listening] message reports the corresponding [http] or [https] scheme.

    [on_listening] is called with the bound address after the socket starts
    listening and before the first connection is accepted. This exposes the
    selected port when [port] is 0. By default it prints where the server can
    be reached.

    [on_error] receives handler and connection exceptions and prints them to
    standard error by default. Handler failures produce a plain 500 response
    when possible, and connection failures close that connection. [on_event]
    is called after a response or body-stage rejection is written, including
    rejections with 400, 408, 413, or 417. It is not called when the peer
    disconnects mid-request or a response fails while it is being written.

    Neither callback should raise. Both run in a connection fibre, whose
    failure would otherwise fail the server switch and close the listening
    socket, so an exception escaping either is reported and dropped: an
    [on_event] failure goes to [on_error], and an [on_error] failure is printed
    to standard error along with the exception it was called with.

    Resolving [stop] makes the server stop accepting connections; it returns
    once the connections already open have finished. Cancelling the fibre
    instead stops it at once and truncates whatever is being written.

    {2 Limits}

    A complete request head and body must fit in about 32 KiB. A larger head
    receives 431 Request Header Fields Too Large; a head that fits but names a
    larger body receives 413 Payload Too Large; either closes the connection.
    Use another backend for substantial uploads: a {!Proffer.Multipart} upload
    larger than that window needs one. A response head must fit in about
    30 KiB. An oversized response is reported to [on_error]. When possible the
    server replaces it with a plain 500 response, then closes the connection.

    The configured timeouts bound request reads, and [config.write_timeout]
    bounds each response write. A request refused with 400, 408, 413, 417, or
    431 closes its connection, after reading and discarding up to 64 KiB for
    at most 250 ms of whatever the client is still sending, so that the error
    response is not lost to a reset. An oversized [Expect: 100-continue] request is refused
    from its head alone and never receives 100 Continue.

    {2 Rejections}

    A head the parser refuses is answered without reaching the site: 414 URI
    Too Long for an over-long request target, 501 Not Implemented for an
    unknown method or a transfer coding this server cannot frame, 431 Request
    Header Fields Too Large for a head too large to fit or with too many
    fields, 413 Payload Too Large for an unusable Content-Length, and 400 Bad
    Request for everything else. Every rejection closes the connection.

    A rejection is written with an [HTTP/1.1] status line whatever the version
    on the request line, which
    {{:https://www.rfc-editor.org/rfc/rfc9112#section-2.5}RFC 9112 section 2.5}
    permits: the response uses no feature an HTTP/1.0 client would misread,
    and it carries [Connection: close].

    {2 Framing}

    An unknown-length {!Proffer.Body.Stream} uses chunked transfer coding for
    HTTP/1.1. Non-empty response trailers are declared in [Trailer] and follow
    the terminating zero-size chunk; they force chunked framing even when the
    stream has a declared length. HTTP/1.0 instead closes the connection after
    an unknown-length body and cannot carry trailers; a handler response that
    requires them is replaced with 500 and reported to [on_error]. A stream
    with a declared length and no trailers uses Content-Length, and no byte
    beyond that length reaches the socket: the write that would exceed it
    writes nothing, the mismatch is reported to [on_error], and the connection
    closes. A stream that emits fewer bytes than it declared is reported the
    same way. Successful CONNECT and 101 responses omit HTTP content framing
    and pass the connection, including already-buffered bytes, to the handoff
    callback. See
    {{:https://www.rfc-editor.org/rfc/rfc9112#section-6}RFC 9112 section 6}.

    It raises [Invalid_argument] if [config.backlog] or
    [config.max_connections] is not positive, or if any timeout is not finite
    and positive. *)
