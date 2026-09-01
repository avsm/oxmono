(** This module provides a mock backend for {!Fetch}.

    Any [Fetch.Middleware.request -> Fetch.response] function becomes a client, so code
    written against {!Fetch.type-t} can be tested without sockets. Run it under
    [Eio_mock.Backend.run], or under [run_full] whose auto-advancing clocks suit testing
    {!Fetch.with_limits} and {!Fetch.with_retry}.

    The portable Fetch boundary still validates request URLs, methods, and
    bodies before this backend is called. No bytes cross an HTTP wire, however:
    the mock does not parse or frame response heads, apply backend byte limits,
    enforce Content-Length or Transfer-Encoding, decode content codings, model
    connection reuse, or reject malformed response fields and status lines.
    Its string body also does not exercise streaming backpressure. A
    security-sensitive test that depends on those properties must additionally
    run against [fetch-httpz] or [fetch-curl]. *)

(** [client f] is a client that answers every request with [f request]. *)
val client : (Fetch.Middleware.request -> Fetch.response) -> Fetch.plain

(** [respond body req] is a canned response to [req] carrying [body]. It has status 200,
    no headers, HTTP/1.1, and [req]'s URL. The optional arguments override the first three
    defaults. *)
val respond
  :  ?status:int
  -> ?headers:Http.Header.t
  -> ?version:Fetch.version
  -> string
  -> Fetch.Middleware.request
  -> Fetch.response

module Sse : sig
  (** This module scripts finite Server-Sent Event responses. *)

  type sink
  (** A [sink] accumulates event-stream wire fragments for one scripted
      response. It is valid only during the callback passed to {!respond}. *)

  val send : sink -> ?name:string -> ?id:string -> string -> unit
  (** [send sink data] appends one event. Newlines in [data] become separate
      data fields. [name] and [id] must not contain a newline, and [id] must not
      contain NUL.

      @raise Stdlib.Invalid_argument when these constraints fail. *)

  val comment : sink -> string -> unit
  (** [comment sink text] appends a comment block.

      @raise Stdlib.Invalid_argument for NUL, DEL, or a C0 control other than
      horizontal tab or newline. *)

  val retry : sink -> int -> unit
  (** [retry sink milliseconds] appends a reconnect-delay block.

      @raise Stdlib.Invalid_argument when [milliseconds] is negative. *)

  val respond :
    ?status:int ->
    ?headers:Http.Header.t ->
    ?version:Fetch.version ->
    ?retry:int ->
    (sink -> unit) ->
    Fetch.Middleware.request ->
    Fetch.response
  (** [respond write request] answers with a finite [text/event-stream] body
      assembled synchronously by [write], plus [Cache-Control: no-store].
      [status] defaults to 200, [headers] to empty, [version] to HTTP/1.1, and
      [retry] is omitted by default. Content-Type and Cache-Control replace any
      values supplied in [headers]. Exceptions from [write] or the sink helpers
      propagate before a response is returned. It is the mock counterpart of
      [Proffer.Sse.respond]. *)
end
