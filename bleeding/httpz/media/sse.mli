(** This module writes Server-Sent Event fields without owning the output transport. *)

(** A [sink] consumes successive wire fragments. *)
type sink = string -> unit

type slice_sink = #(string * int * int) -> unit
(** A synchronous sink receiving [#(text, offset, length)]. *)

val send_sub : slice_sink @ local -> ?name:string -> ?id:string -> string -> unit @@ portable
val comment_sub : slice_sink @ local -> string -> unit @@ portable
(** Slice variants of [send] and [comment]. The callback may be local and
    is called repeatedly; it is never retained. No line substrings are made. *)

(** [media_type] is ["text/event-stream"]. *)
val media_type : string @@ portable

(** [send sink data] writes one event. CR, LF, and CRLF newlines in [data] become separate
    data fields. [name] and [id] must not contain a newline, and [id] must not contain
    NUL. Violations raise [Invalid_argument]. *)
val send : sink -> ?name:string -> ?id:string -> string -> unit @@ portable

(** [comment sink text] writes a comment block suitable for a keep-alive. CR, LF, and
    CRLF newlines in [text] become separate comment fields. NUL, DEL, and every other C0
    control apart from horizontal tab, carriage return and line feed raise
    [Invalid_argument]. No fragment written to [sink] is empty. *)
val comment : sink -> string -> unit @@ portable

(** [retry sink milliseconds] writes a reconnect-delay block. It raises [Invalid_argument]
    if [milliseconds] is negative. *)
val retry : sink -> int -> unit @@ portable
