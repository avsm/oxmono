(** HTTP request type. *)

(** Unboxed request record. Content headers (Content-Length, Transfer-Encoding,
    Connection, Expect) are parsed during header parsing and cached here; they are
    excluded from the returned header list. *)
type t =
  #{ meth : Method.t
   ; target : Span.t
   ; version : Version.t
   ; body_off : int16#
   ; content_length : int64# (** Content-Length value, [-1L] if not present *)
   ; is_chunked : bool (** [true] if Transfer-Encoding: chunked *)
   ; keep_alive : bool (** [true] for keep-alive (considers version default) *)
   ; expect_continue : bool (** [true] if Expect: 100-continue present per
                                {{:https://datatracker.ietf.org/doc/html/rfc7231#section-5.1.1}RFC 7231 Section 5.1.1} *)
   }

(** Check if the complete body is available in the buffer. Returns [true] if body_off +
    content_length <= len, or if there's no body. *)
val body_in_buffer : len:int16# -> t @ local -> bool

(** Get span of body if fully in buffer. Returns span with [len = -1] if body incomplete
    or chunked encoding (use {!Chunk.parse} for chunked). *)
val body_span : len:int16# -> t @ local -> Span.t

(** Returns additional bytes needed for complete body, or [0] if complete. Returns [-1]
    for chunked encoding (unknown length). *)
val body_bytes_needed : len:int16# -> t @ local -> int16#

(** Pretty-print request line using buffer (shows actual values). *)
val pp_with_buf : bytes -> Stdlib.Format.formatter -> t -> unit

(** Pretty-print request structure. *)
val pp : Stdlib.Format.formatter -> t -> unit
