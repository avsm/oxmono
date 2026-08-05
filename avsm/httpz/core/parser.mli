(** HTTP/1.1 request parser.

    Stack-allocated parser with zero heap allocation on the hot path.
    Position is threaded explicitly through all functions. *)

(** Parse error with detailed status. *)
exception Parse_error of Buf_read.status

(** Parser state - unboxed record holding buffer and length. *)
type pstate = #{ buf : bytes; len : int16# }

(** {1 Parser Construction} *)

val make : bytes -> len:int16# -> pstate @@ portable
(** [make buf ~len] creates parser state from buffer and length. *)

(** {1 Request Parsing} *)

val request_line :
  pstate -> pos:int16# -> limits:Buf_read.limits ->
  #(Method.t * Span.t * Target.t * Version.t * int16#) @@ portable
(** [request_line st ~pos ~limits] parses METHOD SP target SP version CRLF.
    Returns [(method, target_span, target, version, new_pos)]. The target is
    validated against the RFC 3986 grammar and against the forms the method
    admits, so its split comes back with it.

    Raises {!Parse_error} with [Uri_too_long] when the target exceeds
    [limits.max_target_length], and with [Partial] when it has not yet been
    terminated by SP or CR. *)

val parse_header : pstate -> pos:int16# -> #(Header_name.t * Span.t * Span.t * int16# * bool) @@ portable
(** [parse_header st ~pos] parses a single header line.
    Returns [(header_name, name_span, value_span, new_pos, has_bare_cr)].

    Raises {!Parse_error} with [Partial] when the line has not yet arrived in
    full, including when the name runs to the end of the buffer. *)

val is_headers_end : pstate -> pos:int16# -> bool @@ portable
(** [is_headers_end st ~pos] returns [true] if at the empty line that ends the
    headers.

    Raises {!Parse_error} with [Partial] when fewer than two bytes remain,
    since a lone CR is indistinguishable from the start of that line. *)

val end_headers : pstate -> pos:int16# -> int16# @@ portable
(** [end_headers st ~pos] skips the empty line at end of headers. *)
