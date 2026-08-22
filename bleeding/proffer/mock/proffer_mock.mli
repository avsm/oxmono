(** Run a compiled site against synthetic requests, with no sockets.

    The request goes through the same code a socket backend calls, so a test
    sees the real dispatch, conditional and HEAD behaviour. *)

type response
(** One served response, as a test reads it. *)

val request :
  ?headers:(string * string) list ->
  ?body:string ->
  ?on_error:(exn -> unit) ->
  'env Proffer.Compiled.t ->
  'env ->
  Proffer.Method.t ->
  string ->
  response
(** [request compiled env meth target] dispatches one request. A streaming body
    is run to completion into a buffer, so {!body} is always what the client
    would have received. [on_error] is told of an exception from a handler,
    whose response is then a 500. *)

val describe :
  ?headers:(string * string) list ->
  ?body:string ->
  ?on_error:(exn -> unit) ->
  ?meth:Proffer.Method.t ->
  ?target:string ->
  (Proffer.Resp.respond @ local -> unit) @ local ->
  response
(** [describe f] runs [f] against a responder and reads back what it responded
    with, without a site or a route. It goes through the same
    {!Proffer.Backend} code [request] does, so the field block it returns is
    the rendered one a client would see, and a conditional request against
    [headers] is answered the same way. It is how a test exercises one response
    on its own, now that a response is not a value a handler returns. *)

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
(** [body r] is the body, or [""] for a HEAD or a 304. *)

val content_length : response -> int64 option
(** [content_length r] is the length the backend would announce, which a HEAD
    and a 304 keep even though they send no body. It is [None] when the length
    is unknown. *)
