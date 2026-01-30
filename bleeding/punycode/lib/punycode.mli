(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** RFC 3492 Punycode: A Bootstring encoding of Unicode for IDNA.

    This module implements the Punycode algorithm as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc3492}RFC 3492}, providing
    encoding and decoding of Unicode strings to/from ASCII-compatible encoding
    suitable for use in internationalized domain names.

    Punycode is an instance of Bootstring that uses particular parameter values
    appropriate for IDNA. See
    {{:https://datatracker.ietf.org/doc/html/rfc3492#section-5}RFC 3492 Section
     5} for the specific parameter values.

    {2 References}
    - {{:https://datatracker.ietf.org/doc/html/rfc3492}RFC 3492} - Punycode: A
      Bootstring encoding of Unicode for IDNA
    - {{:https://datatracker.ietf.org/doc/html/rfc5891}RFC 5891} - IDNA Protocol
*)

(** {1 Position Tracking} *)

type position
(** Abstract type representing a position in input for error reporting.
    Positions track both byte offset and Unicode character index. *)

val position_byte_offset : position -> int
(** [position_byte_offset pos] returns the byte offset in the input. *)

val position_char_index : position -> int
(** [position_char_index pos] returns the Unicode character index (0-based). *)

val pp_position : Format.formatter -> position -> unit
(** [pp_position fmt pos] pretty-prints a position as "byte N, char M". *)

(** {1 Error Types} *)

type error =
  | Overflow of position
      (** Arithmetic overflow during encode/decode. This can occur with very
          long strings or extreme Unicode code point values. See
          {{:https://datatracker.ietf.org/doc/html/rfc3492#section-6.4} RFC 3492
           Section 6.4} for overflow handling requirements. *)
  | Invalid_character of position * Uchar.t
      (** A non-basic code point appeared where only basic code points (ASCII <
          128) are allowed. Per
          {{:https://datatracker.ietf.org/doc/html/rfc3492#section-3.1} RFC 3492
           Section 3.1}, basic code points must be segregated at the beginning
          of the encoded string. *)
  | Invalid_digit of position * char
      (** An invalid Punycode digit was encountered during decoding. Valid
          digits are a-z, A-Z (values 0-25) and 0-9 (values 26-35). See
          {{:https://datatracker.ietf.org/doc/html/rfc3492#section-5} RFC 3492
           Section 5} for digit-value mappings. *)
  | Unexpected_end of position
      (** The input ended prematurely during decoding of a delta value. See
          {{:https://datatracker.ietf.org/doc/html/rfc3492#section-6.2} RFC 3492
           Section 6.2} decoding procedure. *)
  | Invalid_utf8 of position  (** Malformed UTF-8 sequence in input string. *)
  | Label_too_long of int
      (** Encoded label exceeds 63 bytes (DNS limit per
          {{:https://datatracker.ietf.org/doc/html/rfc1035}RFC 1035}). The int
          is the actual length. *)
  | Empty_label  (** Empty label is not valid for encoding. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error fmt e] pretty-prints an error with position information. *)

val error_to_string : error -> string
(** [error_to_string e] converts an error to a human-readable string. *)

(** {1 Constants}

    Punycode parameters as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc3492#section-5}RFC 3492 Section
     5}. *)

val ace_prefix : string
(** The ACE prefix ["xn--"] used for Punycode-encoded domain labels. See
    {{:https://datatracker.ietf.org/doc/html/rfc3492#section-5} RFC 3492 Section
     5} which notes that IDNA prepends this prefix. *)

val max_label_length : int
(** Maximum length of a domain label in bytes (63), per
    {{:https://datatracker.ietf.org/doc/html/rfc1035}RFC 1035}. *)

(** {1 Case Flags for Mixed-Case Annotation}

    {{:https://datatracker.ietf.org/doc/html/rfc3492#appendix-A}RFC 3492
     Appendix A} describes an optional mechanism for preserving case information
    through the encoding/decoding round-trip. This is useful when the original
    string's case should be recoverable.

    Note: Mixed-case annotation is not used by the ToASCII and ToUnicode
    operations of IDNA. *)

type case_flag =
  | Uppercase
  | Lowercase  (** Case annotation for a character. *)

(** {1 Core Punycode Operations}

    These functions implement the Bootstring algorithms from
    {{:https://datatracker.ietf.org/doc/html/rfc3492#section-6}RFC 3492 Section
     6}. They operate on arrays of Unicode code points ([Uchar.t array]). The
    encoded output is a plain ASCII string without the ACE prefix. *)

val encode : Uchar.t array -> (string, error) result
(** [encode codepoints] encodes an array of Unicode code points to a Punycode
    ASCII string.

    Implements the encoding procedure from
    {{:https://datatracker.ietf.org/doc/html/rfc3492#section-6.3}RFC 3492
     Section 6.3}:

    1. Basic code points (ASCII < 128) are copied literally to the beginning of
    the output per
    {{:https://datatracker.ietf.org/doc/html/rfc3492#section-3.1} Section 3.1
     (Basic code point segregation)} 2. A delimiter ('-') is appended if there
    are any basic code points 3. Non-basic code points are encoded as deltas
    using the generalized variable-length integer representation from
    {{:https://datatracker.ietf.org/doc/html/rfc3492#section-3.3}Section 3.3}

    Example:
    {[
      encode [| Uchar.of_int 0x4ED6; Uchar.of_int 0x4EEC; ... |]
      (* = Ok "ihqwcrb4cv8a8dqg056pqjye" *)
    ]} *)

val decode : string -> (Uchar.t array, error) result
(** [decode punycode] decodes a Punycode ASCII string to an array of Unicode
    code points.

    Implements the decoding procedure from
    {{:https://datatracker.ietf.org/doc/html/rfc3492#section-6.2}RFC 3492
     Section 6.2}.

    The input should be the Punycode portion only, without the ACE prefix. The
    decoder is case-insensitive for the encoded portion, as required by
    {{:https://datatracker.ietf.org/doc/html/rfc3492#section-5}RFC 3492 Section
     5}: "A decoder MUST recognize the letters in both uppercase and lowercase
    forms".

    Example:
    {[
      decode "ihqwcrb4cv8a8dqg056pqjye"
      (* = Ok [| U+4ED6; U+4EEC; U+4E3A; ... |] (Chinese simplified) *)
    ]} *)

(** {1 Mixed-Case Annotation}

    These functions support round-trip case preservation as described in
    {{:https://datatracker.ietf.org/doc/html/rfc3492#appendix-A}RFC 3492
     Appendix A}. *)

val encode_with_case :
  Uchar.t array -> case_flag array -> (string, error) result
(** [encode_with_case codepoints case_flags] encodes with case annotation.

    Per
    {{:https://datatracker.ietf.org/doc/html/rfc3492#appendix-A}RFC 3492
     Appendix A}:
    - For basic (ASCII) letters, the output preserves the case flag directly
    - For non-ASCII characters, the case of the final digit in each delta
      encoding indicates the flag (uppercase = suggested uppercase)

    The [case_flags] array must have the same length as [codepoints].

    @raise Invalid_argument if array lengths don't match. *)

val decode_with_case : string -> (Uchar.t array * case_flag array, error) result
(** [decode_with_case punycode] decodes and extracts case annotations.

    Per
    {{:https://datatracker.ietf.org/doc/html/rfc3492#appendix-A}RFC 3492
     Appendix A}, returns both the decoded code points and an array of case
    flags indicating the suggested case for each character based on the
    uppercase/lowercase form of the encoding digits. *)

(** {1 UTF-8 String Operations}

    Convenience functions that work directly with UTF-8 encoded OCaml strings.
    These combine UTF-8 decoding/encoding with the core Punycode operations. *)

val encode_utf8 : string -> (string, error) result
(** [encode_utf8 s] encodes a UTF-8 string to Punycode (no ACE prefix).

    This is equivalent to decoding [s] from UTF-8 to code points, then calling
    {!encode}.

    Example:
    {[
      encode_utf8 "münchen"
      (* = Ok "mnchen-3ya" *)
    ]} *)

val decode_utf8 : string -> (string, error) result
(** [decode_utf8 punycode] decodes Punycode to a UTF-8 string (no ACE prefix).

    This is equivalent to calling {!decode} then encoding the result as UTF-8.

    Example:
    {[
      decode_utf8 "mnchen-3ya"
      (* = Ok "münchen" *)
    ]} *)

(** {1 Domain Label Operations}

    These functions handle the ACE prefix automatically and enforce DNS label
    length limits per
    {{:https://datatracker.ietf.org/doc/html/rfc1035}RFC 1035}. *)

val encode_label : string -> (string, error) result
(** [encode_label label] encodes a domain label for use in DNS.

    If the label contains only ASCII characters, it is returned unchanged.
    Otherwise, it is Punycode-encoded with the ACE prefix ("xn--") prepended, as
    specified in
    {{:https://datatracker.ietf.org/doc/html/rfc3492#section-5} RFC 3492 Section
     5}.

    Returns {!Error} {!Label_too_long} if the result exceeds 63 bytes.

    Example:
    {[
      encode_label "münchen"
        (* = Ok "xn--mnchen-3ya" *)
        encode_label "example"
      (* = Ok "example" *)
    ]} *)

val decode_label : string -> (string, error) result
(** [decode_label label] decodes a domain label.

    If the label starts with the ACE prefix ("xn--", case-insensitive), it is
    Punycode-decoded. Otherwise, it is returned unchanged.

    Example:
    {[
      decode_label "xn--mnchen-3ya"
        (* = Ok "münchen" *)
        decode_label "example"
      (* = Ok "example" *)
    ]} *)

(** {1 Validation}

    Predicate functions for checking code point and string properties. *)

val is_basic : Uchar.t -> bool
(** [is_basic u] is [true] if [u] is a basic code point (ASCII, < 128).

    Per
    {{:https://datatracker.ietf.org/doc/html/rfc3492#section-5}RFC 3492 Section
     5}, basic code points for Punycode are the ASCII code points (0..7F). *)

val is_ascii_string : string -> bool
(** [is_ascii_string s] is [true] if [s] contains only ASCII characters (all
    bytes < 128). *)

val has_ace_prefix : string -> bool
(** [has_ace_prefix s] is [true] if [s] starts with the ACE prefix "xn--"
    (case-insensitive comparison). *)
