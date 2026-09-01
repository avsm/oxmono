(** This module provides internationalized domain-name conversion helpers.

    This module converts UTF-8 labels to NFC and Punycode A-labels and decodes
    [xn--] labels for display. Punycode is defined by
    {{:https://www.rfc-editor.org/rfc/rfc3492.html}RFC 3492}; the IDNA 2008
    protocol is defined by
    {{:https://www.rfc-editor.org/rfc/rfc5891.html}RFC 5891}.

    {b Validation scope.} This implementation enforces label and domain byte
    limits, NFC normalization for encoded labels, canonical A-label round-trip
    checks, an optional ASCII STD3 check, and the leading, trailing, and
    positions-3-and-4 hyphen rules. It does not implement the RFC 5892
    code-point tables, RFC 5893 bidirectional rules, contextual joiner rules, or
    UTS #46 mapping. Applications that accept untrusted internationalized names
    must apply those checks separately before DNS use. *)

(** An [error_reason] explains why an IDNA helper failed. *)
type error_reason : immutable_data =
  | Punycode_error of Punycode.error_reason
      (** [Punycode_error reason] means a raw Punycode operation failed with
          [reason]. *)
  | Invalid_label of string
      (** [Invalid_label reason] reports why a label failed one of this
          module's configured checks. The payload is explanatory text, not the
          rejected input. *)
  | Domain_too_long of int
      (** [Domain_too_long length] means an ASCII domain exceeded
          {!max_domain_length}; [length] is its actual byte length. *)
  | Verification_failed
      (** [Verification_failed] means encoding did not decode back to the NFC
          input. *)

exception Error of error_reason
(** [Error reason] is the exception reporting a failed conversion or validation
    step. *)

val pp_error_reason : Format.formatter -> error_reason -> unit @@ portable
(** [pp_error_reason formatter reason] is the formatter operation that prints a
    human-readable explanation of [reason]. *)

val error_reason_to_string : error_reason -> string @@ portable
(** [error_reason_to_string reason] is a human-readable explanation of [reason].
*)

val max_domain_length : int
(** [max_domain_length] is 253 bytes, the maximum dotted ASCII presentation
    form without a trailing root dot. *)

val to_ascii : ?check_hyphens:bool -> ?use_std3_rules:bool -> string -> string
(** [to_ascii ?check_hyphens ?use_std3_rules domain] is the ASCII domain
    obtained by converting each dot-separated UTF-8 label and enforcing
    {!max_domain_length}.

    [check_hyphens] defaults to [true] and rejects leading or trailing hyphens,
    and [--] in positions 3 and 4 unless the label begins with [xn--]. Apparent
    A-labels are always decoded and checked for a canonical non-ASCII
    round-trip. [use_std3_rules] defaults to [false].
    When enabled, an already-ASCII label must contain only letters, digits, and
    hyphens and must not begin or end with a hyphen. A single trailing empty
    label is preserved as the DNS root dot; other empty labels are rejected.

    It raises [Error] when a label fails conversion or a configured check, or
    when the ASCII domain excluding its root dot is longer than 253 bytes. *)

val label_to_ascii :
  ?check_hyphens:bool -> ?use_std3_rules:bool -> string -> string
(** [label_to_ascii ?check_hyphens ?use_std3_rules label] is the NFC-normalized
    Punycode form of a non-ASCII label, or an ASCII label unchanged after the
    requested checks. The defaults match {!to_ascii}.

    The DNS length check applies to the resulting A-label. A generous input
    bound limits work on U-labels that cannot plausibly produce a DNS label.
    It raises [Error] when [label] is empty, too long, malformed UTF-8, or fails
    a configured check. *)

val to_unicode : string -> string
(** [to_unicode domain] is [domain] with every dot-separated [xn--] label
    decoded and every other label unchanged. It is intended for display; it
    validates the A-label round-trip but does not validate IDNA code-point,
    bidi, or joiner rules.

    A-labels must be at most 63 octets, the DNS limit; the cap also bounds the
    quadratic cost of Punycode decoding. The total domain length is not checked.

    A trailing DNS root dot is preserved.

    It raises [Error] when an A-label is longer than 63 octets, an ACE-prefixed
    label is not valid Punycode, the input is malformed UTF-8, or a non-ASCII
    label exceeds the defensive U-label input bound. *)
