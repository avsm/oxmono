(** This module implements RFC 3492 Punycode encoding.

    Punycode is the Bootstring encoding used to represent Unicode code points
    with basic ASCII characters. {!encode} and {!decode} operate on the raw
    Punycode payload; they do not add or remove the [xn--] prefix. The complete
    algorithm and its parameters are specified by
    {{:https://www.rfc-editor.org/rfc/rfc3492.html}RFC 3492}.

    {!encode_label} and {!decode_label} are convenient domain-label helpers.
    They enforce the DNS limit of 63 bytes but do not perform the full IDNA
    validation required for resolving an internationalized name; see
    {!Punycode_idna}. *)

type position : immutable_data
(** A [position] is an input location reported as both a byte offset and a
    zero-based Unicode character index. *)

val position_byte_offset : position -> int @@ portable
(** [position_byte_offset position] is the zero-based byte offset recorded in
    [position]. *)

val position_char_index : position -> int @@ portable
(** [position_char_index position] is the zero-based Unicode character index
    recorded in [position]. *)

val pp_position : Format.formatter -> position -> unit @@ portable
(** [pp_position formatter position] is the formatter operation that prints
    [position] as byte and character coordinates. *)

(** An [error_reason] explains why a Punycode operation failed. *)
type error_reason : immutable_data =
  | Overflow of position
      (** [Overflow position] means an intermediate integer exceeded OCaml's
          [int] range. RFC 3492,
          {{:https://www.rfc-editor.org/rfc/rfc3492.html#section-6.4}Section
           6.4}, requires decoders to detect arithmetic overflow. *)
  | Invalid_character of position * Uchar.t
      (** [Invalid_character (position, codepoint)] means [codepoint] was not
          valid at [position]. *)
  | Invalid_digit of position * char
      (** [Invalid_digit (position, byte)] means [byte] was not a Punycode digit
          at [position]. Digits are ASCII letters followed by [0] through [9].
      *)
  | Unexpected_end of position
      (** [Unexpected_end position] means the encoded input ended inside a
          variable-length integer at [position]. *)
  | Invalid_utf8 of position
      (** [Invalid_utf8 position] means a UTF-8 helper received a malformed byte
          sequence at [position]. *)
  | Label_too_long of int
      (** [Label_too_long length] means a domain-label result exceeded
          {!max_label_length}; [length] is its actual byte length. *)
  | Empty_label
      (** [Empty_label] means a domain-label helper received the empty string.
      *)

exception Error of error_reason
(** [Error reason] is the exception reporting a failed Punycode or label
    operation. *)

val pp_error_reason : Format.formatter -> error_reason -> unit @@ portable
(** [pp_error_reason formatter reason] is the formatter operation that prints a
    human-readable explanation of [reason]. *)

val error_reason_to_string : error_reason -> string @@ portable
(** [error_reason_to_string reason] is a human-readable explanation of [reason].
*)

val ace_prefix : string
(** [ace_prefix] is the case-insensitive ["xn--"] prefix used by IDNA A-labels.
    See
    {{:https://www.rfc-editor.org/rfc/rfc5890.html#section-2.3.2.5}RFC 5890,
     Section 2.3.2.5}. *)

val max_label_length : int
(** [max_label_length] is the DNS label limit of 63 bytes. See
    {{:https://www.rfc-editor.org/rfc/rfc1035.html#section-2.3.4}RFC 1035,
     Section 2.3.4}. *)

(** A [case_flag] is a mixed-case annotation from
    {{:https://www.rfc-editor.org/rfc/rfc3492.html#appendix-A}RFC 3492, Appendix
     A}. IDNA does not use this optional annotation. *)
type case_flag =
  | Uppercase
      (** [Uppercase] preserves uppercase for the corresponding letter. *)
  | Lowercase
      (** [Lowercase] preserves lowercase for the corresponding letter. *)

val encode : Uchar.t array -> string
(** [encode codepoints] is the raw Punycode payload for [codepoints]. ASCII
    letters in mixed input are emitted in lowercase. The empty array encodes as
    the empty string.

    It raises [Error] if arithmetic overflows. *)

val decode : string -> Uchar.t array
(** [decode payload] is the array of code points represented by the raw Punycode
    [payload]. The encoded portion accepts either ASCII letter case. The empty
    string decodes as an empty array.

    It raises [Error] if [payload] contains an invalid digit, invalid code
    point, truncated integer, or arithmetic overflow.

    Decoding inserts each code point into the output at an arbitrary position,
    so it costs O(n^2) in the payload length. This is harmless for the 63-byte
    DNS labels the IDNA layer enforces; a caller feeding {!decode} a longer
    payload of its own must bound the length itself. *)

val encode_with_case : Uchar.t array -> case_flag array -> string
(** [encode_with_case codepoints flags] is the raw Punycode payload with the
    optional mixed-case annotation from RFC 3492, Appendix A. [flags] must have
    the same length as [codepoints].

    It raises [Invalid_argument] if the array lengths differ.

    It raises [Error] if arithmetic overflows. *)

val decode_with_case : string -> Uchar.t array * case_flag array
(** [decode_with_case payload] is the decoded code-point array and its
    mixed-case annotations.

    It raises [Error] under the same conditions as {!decode}. *)

val encode_utf8 : string -> string
(** [encode_utf8 value] is the raw Punycode payload, without [xn--], obtained by
    decoding the UTF-8 string [value] to code points.

    It raises [Error] if [value] is malformed UTF-8 or encoding overflows. *)

val decode_utf8 : string -> string
(** [decode_utf8 payload] is the UTF-8 string represented by the raw Punycode
    [payload], without interpreting an [xn--] prefix.

    It raises [Error] under the same conditions as {!decode}. *)

val encode_label : string -> string
(** [encode_label label] is [label] unchanged when it contains only ASCII;
    otherwise it is the raw Punycode encoding prefixed by [xn--]. ASCII letters
    within a non-ASCII label are emitted in lowercase.

    It raises [Error] with {!Empty_label} for an empty label, {!Label_too_long}
    when the result exceeds 63 bytes, or another reason for malformed UTF-8 or
    encoding failure. *)

val decode_label : string -> string
(** [decode_label label] is [label] decoded when it starts with [xn--], ignoring
    the prefix's ASCII case, and is otherwise [label] unchanged.

    It raises [Error] with {!Empty_label} for an empty label,
    {!Label_too_long} when the input exceeds 63 bytes, or another reason when
    an ACE-prefixed label cannot be decoded. *)

val is_basic : Uchar.t -> bool
(** [is_basic codepoint] is [true] when [codepoint] is ASCII. *)

val is_ascii_string : string -> bool
(** [is_ascii_string value] is [true] when every byte in [value] is below
    [0x80]. *)

val has_ace_prefix : string -> bool
(** [has_ace_prefix value] is [true] when [value] begins with [xn--], ignoring
    ASCII case for [x] and [n]. *)
