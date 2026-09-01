(** This module tests Proffer sites without opening sockets.

    Synthetic requests use the same dispatch, conditional-request, and HEAD
    processing as a network backend. Streaming bodies are collected into strings
    for inspection.

    {1 What the mock does not do}

    A synthetic request is built with {!Proffer.Req.v} and never crosses a
    wire, so nothing here parses or frames HTTP. The mock therefore accepts,
    unchanged, requests a network backend refuses before routing:

    - a target with a malformed percent-escape such as ["/%zz"], an
      over-long target, or an absolute-form target, whose authority a real
      backend cross-checks against Host;
    - a missing or repeated Host field, which HTTP/1.1 requires exactly one
      of;
    - CR, LF, or NUL in a field name or value, and a field name that is not a
      token, all of which are response-splitting material on the wire;
    - a body of any size, where [proffer.httpz] refuses a request that does
      not fit its buffer with 413, and framing fields such as Content-Length
      and Transfer-Encoding, which a backend consumes rather than passes on.

    Response construction is shared, so {!Proffer.Resp.v}'s validation applies
    here as it does anywhere. The collected body does not impose wire framing;
    {!content_length} deliberately preserves the declared length, so a test can
    expose a mismatch instead of silently replacing it with the measured size.
    Request-side validation does not. A test that
    green-lights security-sensitive behaviour against the mock has therefore
    said nothing about production: put it against [proffer.httpz] instead, as
    [proffer/test/test_httpz.ml] does. A handoff response is represented with an
    empty body and no length; its socket callback cannot run without a wire. *)

type response
(** A [response] is one served response as a test reads it. *)

val request :
  ?version:Httpz.Version.t ->
  ?connection_upgrade:bool ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?on_error:(exn -> unit) ->
  ?now:float ->
  'env Proffer.Site.t ->
  'env ->
  Proffer.Method.t ->
  string ->
  response
(** [request site env meth target] is the response produced by dispatching
    one request. A streaming body is run to completion into a buffer, so {!body}
    is always what the client would have received. [on_error] is told of a
    handler exception. An exception raised before a response becomes a 500
    Internal Server Error, and one raised after a response retains that first
    response. [now] is the current time in seconds since the epoch, which
    {!Proffer.Backend.handle} needs to disregard an If-Modified-Since date the
    server has not reached yet. The synthetic request defaults to HTTP/1.1
    with no Connection upgrade option, empty headers, and an empty body. *)

val describe :
  ?version:Httpz.Version.t ->
  ?connection_upgrade:bool ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?on_error:(exn -> unit) ->
  ?now:float ->
  ?meth:Proffer.Method.t ->
  ?target:string ->
  (Proffer.Resp.respond @ local -> unit) @ local ->
  response
(** [describe f] is the response produced by running [f] against a responder
    without a site or route. The result contains the fields a client would see
    and applies conditional request processing to [headers]. The synthetic
    request defaults to GET ["/"], HTTP/1.1 with no Connection upgrade option,
    empty headers, and an empty body. *)

val status : response -> Proffer.Status.t
(** [status r] is the status the backend would send. *)

val headers : response -> Proffer.Headers.t
(** [headers r] is the field block, without Content-Length. *)

val header : response -> Proffer.Headers.name -> string option
(** [header r name] is the first value under [name]. It is always [None] for
    a field httpz does not name; use {!header_other}. *)

val header_other : response -> string -> string option
(** [header_other r spelling] is the first value under a field httpz does not
    name, matched case-insensitively. *)

val body : response -> string
(** [body r] is the body, or [""] for a contentless response. *)

val content_length : response -> int64 option
(** [content_length r] is the response's declared length. It is [None] for an
    unknown-length stream or HEAD and for a 304 produced by conditional request
    processing. Compare it with [String.length (body r)] when testing a
    known-length stream. *)
