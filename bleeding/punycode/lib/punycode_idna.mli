(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** IDNA (Internationalized Domain Names in Applications) support.

    This module provides ToASCII and ToUnicode operations as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc5891}RFC 5891} (IDNA 2008),
    using Punycode ({{:https://datatracker.ietf.org/doc/html/rfc3492}RFC 3492})
    for encoding.

    IDNA allows domain names to contain non-ASCII Unicode characters by encoding
    them using Punycode with an ACE prefix. This module handles the conversion
    between Unicode domain names and their ASCII-compatible encoding (ACE) form.

    {2 References}
    - {{:https://datatracker.ietf.org/doc/html/rfc5891}RFC 5891} -
      Internationalized Domain Names in Applications (IDNA): Protocol
    - {{:https://datatracker.ietf.org/doc/html/rfc5892}RFC 5892} - The Unicode
      Code Points and Internationalized Domain Names for Applications (IDNA)
    - {{:https://datatracker.ietf.org/doc/html/rfc5893}RFC 5893} - Right-to-Left
      Scripts for Internationalized Domain Names for Applications (IDNA)
    - {{:https://datatracker.ietf.org/doc/html/rfc3492}RFC 3492} - Punycode: A
      Bootstring encoding of Unicode for IDNA *)

(** {1 Error Types} *)

type error =
  | Punycode_error of Punycode.error
      (** Error during Punycode encoding/decoding. See {!Punycode.error} for
          details. *)
  | Invalid_label of string
      (** Label violates IDNA constraints. The string describes the violation.
          See
          {{:https://datatracker.ietf.org/doc/html/rfc5891#section-4} RFC 5891
           Section 4} for label validation requirements. *)
  | Domain_too_long of int
      (** Domain name exceeds 253 bytes, per
          {{:https://datatracker.ietf.org/doc/html/rfc1035}RFC 1035}. The int is
          the actual length. *)
  | Normalization_failed
      (** Unicode normalization (NFC) failed. Per
          {{:https://datatracker.ietf.org/doc/html/rfc5891#section-4.2.1} RFC
           5891 Section 4.2.1}, labels must be in NFC form. *)
  | Verification_failed
      (** ToASCII/ToUnicode verification step failed (round-trip check). Per
          {{:https://datatracker.ietf.org/doc/html/rfc5891#section-4.2} RFC 5891
           Section 4.2}, the result of encoding must decode back to the original
          input. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error fmt e] pretty-prints an error. *)

val error_to_string : error -> string
(** [error_to_string e] converts an error to a human-readable string. *)

(** {1 Constants} *)

val max_domain_length : int
(** Maximum length of a domain name in bytes (253), per
    {{:https://datatracker.ietf.org/doc/html/rfc1035}RFC 1035}. *)

(** {1 ToASCII Operation}

    Converts an internationalized domain name to its ASCII-compatible encoding
    (ACE) form suitable for DNS lookup.

    See
    {{:https://datatracker.ietf.org/doc/html/rfc5891#section-4} RFC 5891 Section
     4} for the complete ToASCII specification. *)

val to_ascii :
  ?check_hyphens:bool ->
  ?check_bidi:bool ->
  ?check_joiners:bool ->
  ?use_std3_rules:bool ->
  ?transitional:bool ->
  string ->
  (string, error) result
(** [to_ascii domain] converts an internationalized domain name to ASCII.

    Implements the ToASCII operation from
    {{:https://datatracker.ietf.org/doc/html/rfc5891#section-4.1}RFC 5891
     Section 4.1}.

    For each label in the domain: 1. If all ASCII, pass through (with optional
    STD3 validation) 2. Otherwise, normalize to NFC per
    {{:https://datatracker.ietf.org/doc/html/rfc5891#section-4.2.1}Section
     4.2.1} and Punycode-encode with ACE prefix

    Optional parameters (per
    {{:https://datatracker.ietf.org/doc/html/rfc5891#section-4} RFC 5891 Section
     4} processing options):
    - [check_hyphens]: Validate hyphen placement per
      {{:https://datatracker.ietf.org/doc/html/rfc5891#section-4.2.3.1}Section
       4.2.3.1} (default: true)
    - [check_bidi]: Check bidirectional text rules per
      {{:https://datatracker.ietf.org/doc/html/rfc5893}RFC 5893} (default:
      false, not implemented)
    - [check_joiners]: Check contextual joiner rules per
      {{:https://datatracker.ietf.org/doc/html/rfc5892#appendix-A.1}RFC 5892
       Appendix A.1} (default: false, not implemented)
    - [use_std3_rules]: Apply STD3 hostname rules per
      {{:https://datatracker.ietf.org/doc/html/rfc5891#section-4.2.3.2}Section
       4.2.3.2} (default: false)
    - [transitional]: Use IDNA 2003 transitional processing (default: false)

    Example:
    {[
      to_ascii "münchen.example.com"
      (* = Ok "xn--mnchen-3ya.example.com" *)
    ]} *)

val label_to_ascii :
  ?check_hyphens:bool ->
  ?use_std3_rules:bool ->
  string ->
  (string, error) result
(** [label_to_ascii label] converts a single label to ASCII.

    This implements the core ToASCII operation for one label, as described in
    {{:https://datatracker.ietf.org/doc/html/rfc5891#section-4.1}RFC 5891
     Section 4.1}. *)

(** {1 ToUnicode Operation}

    Converts an ASCII-compatible encoded domain name back to Unicode.

    See
    {{:https://datatracker.ietf.org/doc/html/rfc5891#section-4.2} RFC 5891
     Section 4.2} for the complete ToUnicode specification. *)

val to_unicode : string -> (string, error) result
(** [to_unicode domain] converts an ACE domain name to Unicode.

    Implements the ToUnicode operation from
    {{:https://datatracker.ietf.org/doc/html/rfc5891#section-4.2}RFC 5891
     Section 4.2}.

    For each label in the domain: 1. If it has the ACE prefix ("xn--"),
    Punycode-decode it per
    {{:https://datatracker.ietf.org/doc/html/rfc3492#section-6.2}RFC 3492
     Section 6.2} 2. Otherwise, pass through unchanged

    Example:
    {[
      to_unicode "xn--mnchen-3ya.example.com"
      (* = Ok "münchen.example.com" *)
    ]} *)

val label_to_unicode : string -> (string, error) result
(** [label_to_unicode label] converts a single ACE label to Unicode.

    This implements the core ToUnicode operation for one label, as described in
    {{:https://datatracker.ietf.org/doc/html/rfc5891#section-4.2}RFC 5891
     Section 4.2}. *)

(** {1 Domain Name Integration}

    Functions that work with the
    {{:https://github.com/hannesm/domain-name}domain-name} library types.

    These provide integration with the [Domain_name] module for applications
    that use that library for domain name handling. *)

val domain_to_ascii :
  ?check_hyphens:bool ->
  ?use_std3_rules:bool ->
  [ `raw ] Domain_name.t ->
  ([ `raw ] Domain_name.t, error) result
(** [domain_to_ascii domain] converts a domain name to ASCII form.

    Applies {!to_ascii} to the string representation and returns the result as a
    [Domain_name.t].

    Example:
    {[
      let d = Domain_name.of_string_exn "münchen.example.com" in
      domain_to_ascii d
      (* = Ok (Domain_name.of_string_exn "xn--mnchen-3ya.example.com") *)
    ]} *)

val domain_to_unicode :
  [ `raw ] Domain_name.t -> ([ `raw ] Domain_name.t, error) result
(** [domain_to_unicode domain] converts a domain name to Unicode form.

    Applies {!to_unicode} to the string representation and returns the result as
    a [Domain_name.t]. *)

(** {1 Validation} *)

val is_idna_valid : string -> bool
(** [is_idna_valid domain] checks if a domain name is valid for IDNA processing.

    Returns [true] if {!to_ascii} would succeed on the domain. *)

val is_ace_label : string -> bool
(** [is_ace_label label] is [true] if the label has the ACE prefix "xn--"
    (case-insensitive). This indicates the label is Punycode-encoded per
    {{:https://datatracker.ietf.org/doc/html/rfc3492#section-5}RFC 3492 Section
     5}. *)

(** {1 Normalization} *)

val normalize_nfc : string -> string
(** [normalize_nfc s] returns the NFC-normalized form of UTF-8 string [s].

    Per
    {{:https://datatracker.ietf.org/doc/html/rfc5891#section-4.2.1} RFC 5891
     Section 4.2.1}, domain labels must be normalized to NFC (Unicode
    Normalization Form C) before encoding.

    See {{:http://www.unicode.org/reports/tr15/}Unicode Standard Annex #15} for
    details on Unicode normalization forms. *)
