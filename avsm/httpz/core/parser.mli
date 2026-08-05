(** HTTP/1.1 request parser.

    Stack-allocated parser with zero heap allocation on the hot path.
    Position is threaded explicitly through all functions. *)

(** Parse error with detailed status. *)
exception Parse_error of Buf_read.status

(** Parser state - unboxed record holding buffer and length. *)
type pstate = #{ buf : bytes; len : int16# }

(** {1 Parser Construction} *)

val make : bytes -> len:int16# -> pstate
(** [make buf ~len] creates parser state from buffer and length. *)

(** {1 Request Parsing} *)

val request_line : pstate -> pos:int16# -> #(Method.t * Span.t * Version.t * int16#)
(** [request_line st ~pos] parses METHOD SP target SP version CRLF.
    Returns [(method, target_span, version, new_pos)]. *)

val parse_header : pstate -> pos:int16# -> #(Header_name.t * Span.t * Span.t * int16# * bool)
(** [parse_header st ~pos] parses a single header line.
    Returns [(header_name, name_span, value_span, new_pos, has_bare_cr)]. *)

val is_headers_end : pstate -> pos:int16# -> bool
(** [is_headers_end st ~pos] returns [true] if at empty line (end of headers). *)

val end_headers : pstate -> pos:int16# -> int16#
(** [end_headers st ~pos] skips the empty line at end of headers. *)
