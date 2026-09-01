(** This module writes Server-Sent Event fields without owning the output transport. *)

(** A [sink] consumes successive wire fragments. *)
type sink = string -> unit

(** [media_type] is ["text/event-stream"]. *)
val media_type : string @@ portable

(** [send sink data] writes one event. CR, LF, and CRLF newlines in [data] become separate
    data fields. [name] and [id] must not contain a newline, and [id] must not contain
    NUL. Violations raise [Invalid_argument]. *)
val send : sink -> ?name:string -> ?id:string -> string -> unit @@ portable

(** [comment sink text] writes a comment block suitable for a keep-alive. Newlines become
    separate comment fields. NUL, other C0 controls apart from horizontal tab, and DEL
    raise [Invalid_argument]. *)
val comment : sink -> string -> unit @@ portable

(** [retry sink milliseconds] writes a reconnect-delay block. It raises [Invalid_argument]
    if [milliseconds] is negative. *)
val retry : sink -> int -> unit @@ portable
