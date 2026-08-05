(** Request bodies for HTML-form endpoints. *)

type part
(** One part of a [multipart/form-data] body. *)

val field : string -> string -> part
(** [field name value] is the form field [name] holding [value]. *)

val file : name:string -> filename:string -> content_type:string -> string -> part
(** [file ~name ~filename ~content_type content] is a file-upload
    part.
    @raise Invalid_argument if [filename] holds a quote or a line
      break. *)

val stream :
  name:string ->
  filename:string ->
  content_type:string ->
  ?length:int64 ->
  _ Eio.Flow.source ->
  part
(** [stream ~name ~filename ~content_type flow] is a file-upload part
    read from [flow] as the request is sent rather than held in memory.
    The body {!multipart} builds from it can be sent at most once, so it
    is neither retried by {!Fetch.with_retry} nor re-sent on a 307.

    Pass [length] when the size is known: the request then carries a
    [Content-Length] and the part is held to that count exactly. A part
    that runs short or long, or whose content contains the boundary,
    fails the request with [Invalid_request] raised from the body flow
    during the send rather than at build time.

    @raise Invalid_argument if [filename] holds a quote or a line
      break, or if [length] is negative. *)

val multipart :
  ?boundary:string -> part list -> Header.headers * Middleware.body
(** [multipart parts] is the [Content-Type] header and body of a
    [multipart/form-data] request carrying [parts] in order. A boundary
    not given is derived so that it occurs in no part.

    With {!field} and {!val-file} parts alone the body is a replayable
    [String]. One {!val-stream} part makes it a one-shot [Stream], which
    carries a length only when every streamed part declared one.

    @raise Invalid_argument if [boundary] is not a token, is longer
      than 70 characters, or occurs in a part. *)

val urlencoded : (string * string) list -> Header.headers * Middleware.body
(** [urlencoded ps] is the [Content-Type] header and body of an
    [application/x-www-form-urlencoded] request binding [ps]. *)
