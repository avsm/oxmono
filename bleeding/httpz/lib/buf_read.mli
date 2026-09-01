(** This module provides buffer-reading primitives, parse results, and resource limits.

    Most applications use {!Httpz.parse} or {!Res.parse} and inspect {!status}. The
    remaining functions support specialized parsers over the same caller-owned buffers.
    Functions documented as unchecked require valid offsets and limits. *)

(** A [status] is the result of parsing an HTTP message head. *)
type status =
  | Complete (** [Complete] means the message head is complete and valid. *)
  | Partial
  (** [Partial] means more input is required before the message can be classified. *)
  | Invalid_method (** [Invalid_method] means the request method is not a valid token. *)
  | Unsupported_method
  (** [Unsupported_method] means the method is valid syntax but is not recognized. *)
  | Invalid_target
  (** [Invalid_target] means the request-target or its method-specific form is invalid. *)
  | Uri_too_long
  (** [Uri_too_long] means the request-target exceeds the configured limit. *)
  | Invalid_version (** [Invalid_version] means the HTTP version is not supported. *)
  | Invalid_status (** [Invalid_status] means the response status line is malformed. *)
  | Invalid_header (** [Invalid_header] means a field name or value is malformed. *)
  | Headers_too_large
  (** [Headers_too_large] means a field count or message-head size limit was exceeded. *)
  | Malformed
  (** [Malformed] means the input violates another required message grammar. *)
  | Content_length_overflow
  (** [Content_length_overflow] means Content-Length exceeds an integer or configured
      bound. *)
  | Ambiguous_framing
  (** [Ambiguous_framing] means message-length fields conflict or otherwise permit
      ambiguous framing. *)
  | Bare_cr_detected
  (** [Bare_cr_detected] means a bare CR or LF occurred in a message head. Strict CRLF
      handling avoids inconsistent parsing between recipients; see
      {{:https://www.rfc-editor.org/rfc/rfc9112.html#section-2.2} RFC 9112, Section 2.2}. *)
  | Missing_host_header
  (** [Missing_host_header] means an HTTP/1.1 request has no valid Host field, or has more
      than one. See
      {{:https://www.rfc-editor.org/rfc/rfc9112.html#section-3.2} RFC 9112, Section 3.2}. *)
  | Unsupported_transfer_encoding
  (** [Unsupported_transfer_encoding] means Transfer-Encoding cannot be framed safely by
      this parser. *)

(** [status_to_string status] is a stable constructor-like description of [status]. *)
val status_to_string : status -> string @@ portable

(** [pp_status formatter status] is the formatter operation that prints
    {!status_to_string}. *)
val pp_status : Stdlib.Format.formatter -> status -> unit @@ portable

(** [valid_field_value buf ~pos ~len] is [true] when every byte from [pos] up to but not
    including [len] is permitted by the RFC 9110 field-value grammar. Here [len] is the
    exclusive end offset, not a byte count. *)
val valid_field_value : local_ bytes -> pos:int16# -> len:int16# -> bool @@ portable

(** A [limits] value sets the resource limits applied while parsing an HTTP message. *)
type limits =
  #{ max_content_length : int64#
   (** [max_content_length] is the greatest accepted Content-Length value. *)
   ; max_header_size : int16#
   (** [max_header_size] is the greatest accepted message-head size in bytes. *)
   ; max_header_count : int16#
   (** [max_header_count] is the greatest accepted field count. *)
   ; max_chunk_size : int
   (** [max_chunk_size] is the greatest accepted chunk size in bytes. *)
   ; max_target_length : int16#
   (** [max_target_length] is the greatest accepted request-target size in bytes. *)
   }

(** [default_limits] is a 100 MiB content-length limit, a 16 KiB message-head limit, a
    100-field limit, a 16 MiB chunk limit, and an 8 KiB request-target limit. *)
val default_limits : limits @@ portable

(** [buffer_size] is the maximum supported parse-buffer size, 32 KiB. *)
val buffer_size : int @@ portable

(** [find_crlf_check_bare_cr buf ~pos ~len] is the first CRLF's CR offset from [pos] up to
    but not including [len], or [-1] if none is complete, together with whether a bare CR
    or LF occurred before it. *)
val find_crlf_check_bare_cr
  :  local_ bytes
  -> pos:int16#
  -> len:int16#
  -> #(int16# * bool)
  @@ portable

(** [i16 value] is [value] as an [int16#]. *)
val i16 : int -> int16# @@ portable

(** [to_int value] is [value] as an [int]. *)
val to_int : int16# -> int @@ portable

(** [peek buf pos] is the byte at [pos]. Bounds are not checked. *)
val peek : local_ bytes -> int16# -> char# @@ portable

(** [left =. right] is character equality. *)
val ( =. ) : char# -> char# -> bool @@ portable

(** [left <>. right] is character inequality. *)
val ( <>. ) : char# -> char# -> bool @@ portable

(** [is_token_char byte] is [true] when [byte] is an HTTP [tchar] as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9110.html#section-5.6.2} RFC 9110, Section 5.6.2}. *)
val is_token_char : char# -> bool @@ portable

(** [skip_token buf ~pos ~limit] is the first offset from [pos] up to but not including
    [limit] that is not an HTTP token character, or [limit]. Bounds are not checked. *)
val skip_token : local_ bytes -> pos:int -> limit:int -> int @@ portable

(** [is_space byte] is [true] for SP or HTAB. *)
val is_space : char# -> bool @@ portable

val[@zero_alloc] is_field_value_char : char# -> bool @@ portable
(** [is_field_value_char byte] is [true] for HTAB, SP, a visible ASCII byte,
    or [obs-text]. These are the bytes allowed in an unfolded RFC 9110 field
    value. *)

val[@zero_alloc] is_qdtext_char : char# -> bool @@ portable
(** [is_qdtext_char byte] is [true] when [byte] is RFC 9110 [qdtext]: an
    unescaped byte allowed inside a quoted string. *)

val[@zero_alloc] is_quoted_pair_char : char# -> bool @@ portable
(** [is_quoted_pair_char byte] is [true] when [byte] may follow a backslash in
    an RFC 9110 quoted pair. *)

(** [is_digit byte] is [true] for an ASCII decimal digit. *)
val is_digit : char# -> bool @@ portable

(** [digit_value byte] is the value of an ASCII decimal digit, or [-1]. *)
val digit_value : char# -> int @@ portable

(** [skip_ows buf ~pos ~len] is the first non-OWS offset from [pos] up to but not
    including [len], or [len]. Here [len] is the exclusive end offset. *)
val skip_ows : local_ bytes -> pos:int16# -> len:int16# -> int16# @@ portable

(** [to_lower byte] is the ASCII-lowercase form of [byte], or [byte] when it is not an
    uppercase ASCII letter. *)
val to_lower : char# -> char# @@ portable
