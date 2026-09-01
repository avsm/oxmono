(** This module provides public-suffix and registrable-domain lookup.

    The {{:https://publicsuffix.org/list/} Public Suffix List} records domain suffixes
    beneath which independent parties may register names. For [www.example.co.uk], [co.uk]
    is the public suffix and [example.co.uk] is the registrable domain: the public suffix
    plus one label.

    The embedded list includes both its ICANN section, which describes DNS delegations,
    and its Private section, which describes services such as shared hosting. Normal,
    wildcard, and exception rules are supported. When more than one rule matches, an
    exception wins; otherwise the longest rule wins. An implicit wildcard matches names
    absent from the list.

    Input may use Unicode or ASCII Punycode. Before lookup, {!Punycode_idna.to_ascii}
    normalizes Unicode labels to NFC and encodes them as Punycode A-labels; results use
    lower-case ASCII. That helper implements only part of the
    {{:https://www.rfc-editor.org/rfc/rfc5891.html} IDNA2008 protocol}, so callers must
    separately validate untrusted internationalized names for DNS use. A trailing dot
    denoting an absolute DNS name is preserved. *)

type section =
  | ICANN
  (** [ICANN] selects rules for domains delegated by ICANN or present in the IANA root
      zone. *)
  | Private
  (** [Private] selects rules supplied by operators that allocate names beneath their own
      domains. *)

type error =
  | Empty_domain (** [Empty_domain] means the input is empty or contains no labels. *)
  | Invalid_domain of string
  (** [Invalid_domain reason] means the input is not a valid domain name for [reason]. *)
  | Leading_dot
  (** [Leading_dot] means the input starts with a dot. A trailing dot is permitted, but a
      leading dot does not denote a DNS name. *)
  | Punycode_error of string
  (** [Punycode_error reason] means IDNA conversion failed for [reason]. *)
  | No_public_suffix (** [No_public_suffix] means no public suffix could be derived. *)
  | Domain_is_public_suffix
  (** [Domain_is_public_suffix] means the input is itself a public suffix, so it has no
      registrable domain. *)

(** [pp_error ppf error] is [()] after writing a human-readable description of [error] on
    [ppf]. *)
val pp_error : Format.formatter -> error -> unit

(** [error_to_string error] is a human-readable description of [error]. *)
val error_to_string : error -> string

(** {1 Lookup} *)

(** [public_suffix domain] is the public suffix of [domain]. For example,
    [public_suffix "www.example.co.uk"] is [Ok "co.uk"]. A name not covered by an explicit
    rule uses the implicit wildcard, making its final label the public suffix. See the
    {{:https://github.com/publicsuffix/list/wiki/Format#formal-algorithm} Public Suffix
      List matching algorithm}. *)
val public_suffix : string -> (string, error) result

(** [public_suffix_with_section domain] is the public suffix of [domain] and the section
    containing its prevailing rule. The implicit wildcard is reported as {!ICANN}. *)
val public_suffix_with_section : string -> (string * section, error) result

(** [registrable_domain domain] is the public suffix of [domain] plus its immediately
    preceding label. For example, [registrable_domain "www.example.co.uk"] is
    [Ok "example.co.uk"]. It is [Error Domain_is_public_suffix] when [domain] contains no
    label before its public suffix. *)
val registrable_domain : string -> (string, error) result

(** [registrable_domain_with_section domain] is the registrable domain of [domain] and the
    section containing its prevailing rule. *)
val registrable_domain_with_section : string -> (string * section, error) result

(** {1 Predicates} *)

(** [is_public_suffix domain] is [Ok true] if [domain] is exactly a public suffix.
    Wildcards and exceptions are applied, so [foo.ck] is a public suffix under [*.ck],
    while the exception [!www.ck] makes [www.ck] registrable. *)
val is_public_suffix : string -> (bool, error) result

(** [is_registrable_domain domain] is [Ok true] if [domain] consists of a public suffix
    and exactly one preceding label. *)
val is_registrable_domain : string -> (bool, error) result

(** {1 Embedded list information} *)

(** [rule_count] is the total number of embedded rules. *)
val rule_count : int

(** [icann_rule_count] is the number of embedded ICANN rules. *)
val icann_rule_count : int

(** [private_rule_count] is the number of embedded Private rules. *)
val private_rule_count : int

(** [version] is the version identifier recorded in the embedded source list. *)
val version : string

(** [commit] is the upstream commit identifier recorded in the embedded source list. *)
val commit : string
