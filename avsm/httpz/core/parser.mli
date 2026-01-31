(** Stack-allocated parser combinators for HTTP/1.1 parsing.

    Position is threaded explicitly through all combinators for zero allocation.
    All combinators raise {!Err.Parse_error} on failure. *)

(** Parse error with detailed status. Alias for {!Err.Parse_error}. *)
exception Parse_error of Buf_read.status

(** Parser state - unboxed record holding buffer and length.
    Position is threaded explicitly through functions. *)
type pstate = #{ buf : bytes; len : int16# }

(** {1 Core Functions} *)

(** Create parser state from buffer and length *)
val make : bytes -> len:int16# -> pstate

(** Remaining bytes at position *)
val remaining : pstate -> pos:int16# -> int16#

(** Check if at end of buffer *)
val at_end : pstate -> pos:int16# -> bool

(** {1 Basic Combinators}

    All combinators take [~pos] and return the new position. *)

(** Peek current char without advancing. Raises [Partial] if at end. *)
val peek_char : pstate -> pos:int16# -> char#

(** Peek char at offset from current position. Raises [Partial] if out of bounds. *)
val peek_at : pstate -> pos:int16# -> int16# -> char#

(** Match single character, return new position. Raises [Partial] or [Malformed]. *)
val char : char# -> pstate -> pos:int16# -> int16#

(** Match literal string, return new position. Raises [Partial] or [Malformed]. *)
val string : string -> pstate -> pos:int16# -> int16#

(** Take chars while predicate holds, return span and new position. *)
val take_while : (char# -> bool) -> pstate -> pos:int16# -> #(Span.t * int16#)

(** Skip chars while predicate holds, return new position. *)
val skip_while : (char# -> bool) -> pstate -> pos:int16# -> int16#

(** Take exactly n chars as span, return span and new position. Raises [Partial]. *)
val take : int16# -> pstate -> pos:int16# -> #(Span.t * int16#)

(** Skip exactly n chars, return new position. Raises [Partial]. *)
val skip : int16# -> pstate -> pos:int16# -> int16#

(** Match char satisfying predicate, return char and new position. *)
val satisfy : (char# -> bool) -> pstate -> pos:int16# -> #(char# * int16#)

(** Try parser, return Null and original pos on failure. *)
val optional : (pstate -> pos:int16# -> #('a * int16#)) -> pstate -> pos:int16# -> #('a or_null * int16#)

(** {1 HTTP-Specific Combinators} *)

(** Match CRLF (\\r\\n), return new position. *)
val crlf : pstate -> pos:int16# -> int16#

(** Match SP (space), return new position. *)
val sp : pstate -> pos:int16# -> int16#

(** Take HTTP token chars (for method, header names), return span and new position.
    Must be non-empty. Raises [Malformed] if empty. *)
val token : pstate -> pos:int16# -> #(Span.t * int16#)

(** Skip optional whitespace (OWS = SP / HTAB), return new position. *)
val ows : pstate -> pos:int16# -> int16#

(** Parse HTTP version (HTTP/1.0 or HTTP/1.1), return version and new position. *)
val http_version : pstate -> pos:int16# -> #(Version.t * int16#)

(** Parse HTTP method from token, return method and new position. *)
val parse_method : pstate -> pos:int16# -> #(Method.t * int16#)

(** Parse request target, return span and new position. Raises [Invalid_target] if empty. *)
val parse_target : pstate -> pos:int16# -> #(Span.t * int16#)

(** Parse request line: METHOD SP target SP version CRLF.
    Returns (method, target_span, version, new_pos). *)
val request_line : pstate -> pos:int16# -> #(Method.t * Span.t * Version.t * int16#)

(** Parse a single header line.
    Returns (header_name, name_span, value_span, new_pos). *)
val parse_header : pstate -> pos:int16# -> #(Header_name.t * Span.t * Span.t * int16# * bool)

(** Check if at end of headers (CRLF at current position). *)
val is_headers_end : pstate -> pos:int16# -> bool

(** Skip the empty line at end of headers, return new position. *)
val end_headers : pstate -> pos:int16# -> int16#
