(** This module provides low-level HTTP/1.1 message-head parsing.

    These functions thread an explicit position through a caller-owned buffer. They raise
    {!Parse_error} rather than returning a status. Most applications should use
    {!Httpz.parse} or {!Res.parse}, which catch these exceptions and apply message-level
    rules and limits. *)

(** [Parse_error status] is the exception reporting why a low-level parser stopped. *)
exception Parse_error of Buf_read.status

(** A [pstate] is parser state over the first [len] bytes of [buf]. *)
type pstate =
  #{ buf : bytes (** [buf] is the caller-owned input buffer. *)
   ; len : int16# (** [len] is the exclusive end offset of available input. *)
   }

(** [make buf ~len] is a parser over the first [len] bytes of [buf]. The range is not
    validated. *)
val make : bytes -> len:int16# -> pstate @@ portable

(** [request_line state ~pos ~limits] is
    [(method_, target_span, parsed_target, version, next_pos)] for the request line
    beginning at [pos]. The target is validated against RFC 3986 and the method-specific
    forms in
    {{:https://www.rfc-editor.org/rfc/rfc9112.html#section-3.2} RFC 9112, Section 3.2}.

    It raises [Parse_error] with {!Buf_read.Partial} for incomplete input,
    {!Buf_read.Invalid_method} for invalid method syntax,
    {!Buf_read.Unsupported_method} for a syntactically valid method this parser
    does not implement, {!Buf_read.Invalid_target} for an invalid or
    method-inappropriate target, {!Buf_read.Uri_too_long} when the target
    exceeds [limits.max_target_length], {!Buf_read.Invalid_version} for an
    unsupported version, and {!Buf_read.Malformed} for invalid request-line
    delimiters. *)
val request_line
  :  pstate
  -> pos:int16#
  -> limits:Buf_read.limits
  -> #(Method.t * Span.t * Target.t * Version.t * int16#)
  @@ portable

(** [status_line state ~pos] is [(version, code, reason, next_pos)] for the response
    status line beginning at [pos]. Any exactly three-digit status code is accepted, and
    the reason phrase may be empty.

    It raises [Parse_error] with {!Buf_read.Partial} for incomplete input,
    {!Buf_read.Invalid_version} for an unsupported version,
    {!Buf_read.Invalid_status} for a malformed status code or reason phrase,
    {!Buf_read.Bare_cr_detected} for a bare line ending, or
    {!Buf_read.Malformed} when a required delimiter is absent. *)
val status_line
  :  pstate
  -> pos:int16#
  -> #(Version.t * int16# * Span.t * int16#)
  @@ portable

(** [parse_header state ~pos] is
    [(name, name_span, value_span, next_pos)] for one field line. Obsolete folded lines are
    unfolded in place by replacing their line breaks and leading whitespace with spaces.

    It raises [Parse_error] with {!Buf_read.Partial} for incomplete input,
    {!Buf_read.Bare_cr_detected} for a bare line ending,
    {!Buf_read.Invalid_header} for an invalid field value, or
    {!Buf_read.Malformed} for an invalid field name or missing colon. *)
val parse_header
  :  pstate
  -> pos:int16#
  -> #(Header_name.t * Span.t * Span.t * int16#)
  @@ portable

(** [is_headers_end state ~pos] is [true] when [pos] begins the empty CRLF line ending a
    message head.

    It raises [Parse_error] with {!Buf_read.Partial} when fewer than two bytes remain. *)
val is_headers_end : pstate -> pos:int16# -> bool @@ portable

(** [end_headers state ~pos] is the offset after the CRLF that ends a message head.

    It raises [Parse_error] with {!Buf_read.Partial} if the CRLF is incomplete,
    or {!Buf_read.Malformed} if the available bytes are not CRLF. *)
val end_headers : pstate -> pos:int16# -> int16# @@ portable

(** A [conn_value] is the Connection disposition folded from every Connection field line
    seen so far. *)
type conn_value =
  | Conn_default (** [Conn_default] is no explicit disposition. *)
  | Conn_close (** [Conn_close] is an explicit request to close. *)
  | Conn_keep_alive (** [Conn_keep_alive] is an explicit request to reuse. *)

(** [parse_connection_value buf span ~default] is [default] folded with the Connection
    field value in [span]. A "close" token anywhere, in [span] or already in [default], is
    final. Empty list members are ignored. Raises the parser's [Invalid_header]
    status when a nonempty member is not a token. *)
val parse_connection_value
  :  local_ bytes
  -> Span.t
  -> default:conn_value
  -> conn_value
  @@ portable

(** [content_length_value buf span ~has_cl ~current ~max_content_length] is the
    Content-Length in [span]. [has_cl] and [current] are the length already accepted for
    this message, if any, so that a repeated field naming a different length is rejected.

    It raises [Parse_error] with {!Buf_read.Content_length_overflow} for a
    numeric or configured-limit overflow, {!Buf_read.Ambiguous_framing} for
    conflicting list members or a value different from [current], or
    {!Buf_read.Invalid_header} for other invalid Content-Length syntax. *)
val content_length_value
  :  local_ bytes
  -> Span.t
  -> has_cl:bool
  -> current:int64#
  -> max_content_length:int64#
  -> int64#
  @@ portable
