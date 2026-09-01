(** This module constructs request bodies for HTML form endpoints.

    {!multipart} constructs [multipart/form-data] bodies as specified by
    {{:https://www.rfc-editor.org/rfc/rfc7578}RFC 7578}, framing the parts as
    the
    {{:https://html.spec.whatwg.org/multipage/form-control-infrastructure.html#multipart/form-data-encoding-algorithm}WHATWG
     multipart/form-data encoding algorithm} does. {!urlencoded} constructs
    [application/x-www-form-urlencoded] bodies with the serializer of
    {!Httpz.Urlencoded}, whose codec, {!Httpz.Media.form}, reads a response of
    that type back. *)

type part
(** [part] is a field or file in a [multipart/form-data] body. *)

val field :
  ?content_type:string ->
  ?headers:(string * string) list ->
  string ->
  string ->
  part
(** [field name value] is the form field [name] holding [value].
    [content_type] is the part's [Content-Type], which defaults to absent and
    therefore means [text/plain]. [headers] default to empty and are further
    part headers, sent after the ones the part derives.

    @raise Stdlib.Invalid_argument if [name] contains a backslash, DEL or a
    control byte other than CR and LF; if [content_type] contains a forbidden
    control byte; or if a [headers] name is not a token, is
    [content-disposition] or [content-type] in any case, or has a value
    carrying a forbidden control byte. *)

val file :
  ?headers:(string * string) list ->
  name:string ->
  filename:string ->
  content_type:string ->
  string ->
  part
(** [file ~name ~filename ~content_type content] is an in-memory file-upload
    part. [headers] defaults to empty and is validated as in {!field}.
    @raise Stdlib.Invalid_argument on the same grounds as {!field}, with [filename]
    checked as [name] is. *)

val stream :
  ?headers:(string * string) list ->
  name:string ->
  filename:string ->
  content_type:string ->
  ?length:int64 ->
  _ Eio.Flow.source ->
  part
(** [stream ~name ~filename ~content_type flow] is a file-upload part read from
    [flow] as the request is sent rather than held in memory. The body
    {!multipart} builds from it can be sent at most once, so it is neither
    retried by {!Fetch.with_retry} nor re-sent on a 307.

    [headers] default to empty and [length] defaults to absent. Pass [length]
    when the size is known: the request then carries a
    [Content-Length] and the part is held to that count exactly. A part that
    runs short or long, or whose content contains the boundary, fails the
    request with [Invalid_request] raised from the body flow during the send
    rather than at build time.

    The caller owns [flow] and its closure; Fetch only reads it during the one
    permitted send.

    @raise Stdlib.Invalid_argument on the same grounds as {!val-file}, and if
    [length] is negative. *)

val multipart :
  ?boundary:string -> part list -> Header.headers * Middleware.body
(** [multipart parts] is the [Content-Type] header and body of a
    [multipart/form-data] request carrying [parts] in order. A boundary not
    given is drawn freshly and checked against the parts, so that it occurs
    neither in a part's content nor in the headers a part serializes, and so
    that two requests carrying the same parts are not framed alike. No digest
    of part contents appears in the boundary. Its generator is seeded from the
    system once per process: it is unpredictable, not cryptographic.

    A [name] or [filename] is written into the [Content-Disposition]
    quoted-string with a double quote as [%22], CR as [%0D] and LF as
    [%0A], which is what a browser sends.

    With {!field} and {!val-file} parts alone the body is a replayable [String].
    One {!val-stream} part makes it a one-shot [Stream], which carries a length
    only when every streamed part declared one.

    @raise Stdlib.Invalid_argument if [boundary] is not a token, is longer than
    70 characters, occurs in a part's content or serialized headers, or the
    computed Content-Length overflows [int64]. *)

val urlencoded : (string * string) list -> Header.headers * Middleware.body
(** [urlencoded parameters] is the [Content-Type] header and body of an
    [application/x-www-form-urlencoded] request binding [parameters]. The body
    is {!Httpz.Urlencoded.encode} of [parameters]. It preserves the supplied
    OCaml-string bytes; for valid UTF-8 they are the bytes a browser form sends.
    It is not an OAuth 1.0 signature-base-string encoder. Order and repeated
    names are preserved. *)
