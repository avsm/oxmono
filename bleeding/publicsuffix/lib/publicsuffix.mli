(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Public Suffix List implementation for OCaml

    This library provides functions to query the Mozilla Public Suffix List
    (PSL) to determine public suffixes and registrable domains. It implements
    the algorithm specified at
    {{:https://publicsuffix.org/list/} publicsuffix.org}.

    {1 Overview}

    The Public Suffix List is a cross-vendor initiative to provide an accurate
    list of domain name suffixes under which Internet users can directly
    register names. A "public suffix" is one under which Internet users can
    register names. Some examples of public suffixes are [.com], [.co.uk] and
    [.pvt.k12.ma.us].

    The "registrable domain" is the public suffix plus one additional label. For
    example, for the domain [www.example.com], the public suffix is [.com] and
    the registrable domain is [example.com].

    Domain names follow the specifications in
    {{:https://datatracker.ietf.org/doc/html/rfc1034}RFC 1034} and
    {{:https://datatracker.ietf.org/doc/html/rfc1035}RFC 1035}, which define the
    Domain Name System concepts and implementation.

    {1 Sections}

    The PSL is divided into two sections:

    - {b ICANN}: Domains delegated by ICANN or in the IANA root zone database.
      These are official TLDs and their subdivisions.

    - {b Private}: Domains submitted by private parties (e.g., [blogspot.com],
      [github.io]). Some applications may want to treat these differently.

    {1 Rule Types}

    The PSL supports three types of rules:

    - {b Normal}: A standard domain suffix (e.g., [com], [co.uk])
    - {b Wildcard}: Matches any label in that position (e.g., [*.jp] matches
      anything under [.jp])
    - {b Exception}: Overrides a wildcard rule (e.g., [!city.kobe.jp] allows
      [city.kobe.jp] to be a registrable domain despite [*.kobe.jp])

    {1 Example Usage}

    {[
      let psl = Publicsuffix.create () in

      (* Get the public suffix of a domain *)
      Publicsuffix.public_suffix psl "www.example.com" (* Returns: Ok "com" *)
        Publicsuffix.public_suffix psl
        "www.example.co.uk" (* Returns: Ok "co.uk" *)
        (* Get the registrable domain *)
        Publicsuffix.registrable_domain psl
        "www.example.com" (* Returns: Ok "example.com" *)
        (* Check if a domain is a public suffix *)
        Publicsuffix.is_public_suffix psl "com" (* Returns: Ok true *)
        Publicsuffix.is_public_suffix psl "example.com"
      (* Returns: Ok false *)
    ]}

    {1 Internationalized Domain Names}

    The library handles internationalized domain names (IDN) by converting them
    to Punycode (ASCII-compatible encoding) before lookup, following the
    IDNA2008 protocol defined in
    {{:https://datatracker.ietf.org/doc/html/rfc5890}RFC 5890} (IDNA
    Definitions) and {{:https://datatracker.ietf.org/doc/html/rfc5891}RFC 5891}
    (IDNA Protocol). The conversion is performed using [Punycode_idna.to_ascii].

    Punycode encoding, specified in
    {{:https://datatracker.ietf.org/doc/html/rfc3492}RFC 3492}, uniquely and
    reversibly transforms Unicode strings into ASCII-compatible strings using
    the "xn--" prefix (ACE prefix). See the [Punycode] library for the core
    encoding implementation. Both Unicode and Punycode input are accepted:

    {[
      Publicsuffix.registrable_domain psl
        "www.食狮.com.cn" (* Returns: Ok "食狮.com.cn" *)
        Publicsuffix.registrable_domain psl "www.xn--85x722f.com.cn"
      (* Returns: Ok "xn--85x722f.com.cn" *)
    ]}

    {1 Trailing Dots}

    Per the PSL specification, trailing dots (indicating fully-qualified domain
    names) are preserved in the output:

    {[
      Publicsuffix.public_suffix psl "example.com" (* Returns: Ok "com" *)
        Publicsuffix.public_suffix psl "example.com."
      (* Returns: Ok "com." *)
    ]}

    {1 References}

    This library implementation is based on the following specifications:

    - {{:https://publicsuffix.org/list/} Public Suffix List Specification} - The
      algorithm and list format
    - {{:https://datatracker.ietf.org/doc/html/rfc1034}RFC 1034} - Domain Names:
      Concepts and Facilities
    - {{:https://datatracker.ietf.org/doc/html/rfc1035}RFC 1035} - Domain Names:
      Implementation and Specification
    - {{:https://datatracker.ietf.org/doc/html/rfc3492}RFC 3492} - Punycode: A
      Bootstring encoding of Unicode for IDNA
    - {{:https://datatracker.ietf.org/doc/html/rfc5890}RFC 5890} -
      Internationalized Domain Names for Applications (IDNA): Definitions
    - {{:https://datatracker.ietf.org/doc/html/rfc5891}RFC 5891} -
      Internationalized Domain Names in Applications (IDNA): Protocol

    {1 Related Libraries}

    - [Punycode] - Core Punycode encoding/decoding implementation
    - [Punycode_idna] - IDNA ToASCII/ToUnicode operations used for IDN
      conversion *)

(** {1 Types} *)

(** Section of the Public Suffix List where a rule originates *)
type section =
  | ICANN  (** Domains delegated by ICANN or in the IANA root zone *)
  | Private  (** Domains submitted by private parties *)

type t
(** A handle to the parsed Public Suffix List *)

(** {1 Errors} *)

(** Errors that can occur during PSL operations *)
type error =
  | Empty_domain  (** The input domain was empty *)
  | Invalid_domain of string
      (** The domain could not be parsed as a valid domain name. Domain names
          must conform to the syntax specified in
          {{:https://datatracker.ietf.org/doc/html/rfc1035}RFC 1035}. *)
  | Leading_dot
      (** The domain has a leading dot (e.g., [.example.com]). Per
          {{:https://datatracker.ietf.org/doc/html/rfc1035}RFC 1035}, domain
          names should not have leading dots. *)
  | Punycode_error of string
      (** Failed to convert internationalized domain to Punycode encoding. The
          string contains the error message from [Punycode_idna]. See
          {{:https://datatracker.ietf.org/doc/html/rfc3492}RFC 3492} for
          Punycode encoding requirements and
          {{:https://datatracker.ietf.org/doc/html/rfc5891}RFC 5891} for IDNA
          protocol requirements. *)
  | No_public_suffix
      (** The domain has no public suffix (should not happen with valid domains)
      *)
  | Domain_is_public_suffix
      (** The domain is itself a public suffix and has no registrable domain *)

val pp_error : Format.formatter -> error -> unit
(** Pretty-print an error *)

val error_to_string : error -> string
(** Convert an error to a human-readable string *)

(** {1 Creation} *)

val create : unit -> t
(** Create a PSL instance using the embedded Public Suffix List data. The data
    is compiled into the library at build time. *)

(** {1 Core Operations} *)

val public_suffix : t -> string -> (string, error) result
(** [public_suffix t domain] returns the public suffix portion of [domain].

    The public suffix is determined by the PSL algorithm:
    - Match against all rules, taking the longest match
    - Exception rules ([!]) take priority over all other rules
    - If no rules match, the implicit [*] rule applies (returns the TLD)

    Domain names are processed according to
    {{:https://datatracker.ietf.org/doc/html/rfc1035}RFC 1035} syntax.
    Internationalized domain names (IDN) are automatically converted to Punycode
    per {{:https://datatracker.ietf.org/doc/html/rfc3492}RFC 3492} before
    matching.

    @param t The PSL instance
    @param domain The domain name to query (Unicode or Punycode)
    @return [Ok suffix] with the public suffix, or [Error e] on failure

    Examples:
    - [public_suffix t "www.example.com"] returns [Ok "com"]
    - [public_suffix t "www.example.co.uk"] returns [Ok "co.uk"]
    - [public_suffix t "test.k12.ak.us"] returns [Ok "k12.ak.us"]
    - [public_suffix t "city.kobe.jp"] returns [Ok "jp"] (exception rule) *)

val public_suffix_with_section : t -> string -> (string * section, error) result
(** [public_suffix_with_section t domain] is like {!public_suffix} but also
    returns the section (ICANN or Private) where the matching rule was found.

    If the implicit [*] rule was used (no explicit rule matched), the section is
    [ICANN].

    @return [Ok (suffix, section)] or [Error e] on failure *)

val registrable_domain : t -> string -> (string, error) result
(** [registrable_domain t domain] returns the registrable domain portion.

    The registrable domain is the public suffix plus one additional label. This
    is the highest-level domain that can be registered by a user.

    Domain labels follow the naming restrictions specified in
    {{:https://datatracker.ietf.org/doc/html/rfc1035}RFC 1035}.
    Internationalized domain names are handled per
    {{:https://datatracker.ietf.org/doc/html/rfc5891}RFC 5891}.

    @param t The PSL instance
    @param domain The domain name to query
    @return [Ok domain] with the registrable domain, or [Error e] on failure

    Returns [Error Domain_is_public_suffix] if the domain is itself a public
    suffix (e.g., [com] or [co.uk]).

    Examples:
    - [registrable_domain t "www.example.com"] returns [Ok "example.com"]
    - [registrable_domain t "example.com"] returns [Ok "example.com"]
    - [registrable_domain t "com"] returns [Error Domain_is_public_suffix] *)

val registrable_domain_with_section :
  t -> string -> (string * section, error) result
(** [registrable_domain_with_section t domain] is like {!registrable_domain} but
    also returns the section where the matching rule was found.

    @return [Ok (domain, section)] or [Error e] on failure *)

(** {1 Predicates} *)

val is_public_suffix : t -> string -> (bool, error) result
(** [is_public_suffix t domain] returns [true] if [domain] is exactly a public
    suffix according to the PSL.

    Note: This returns [true] if the domain matches a rule exactly, not if it's
    under a wildcard. For example:
    - [is_public_suffix t "com"] returns [Ok true]
    - [is_public_suffix t "example.com"] returns [Ok false]
    - [is_public_suffix t "foo.ck"] returns [Ok true] (due to [*.ck] rule)
    - [is_public_suffix t "www.ck"] returns [Ok false] (due to [!www.ck]
      exception) *)

val is_registrable_domain : t -> string -> (bool, error) result
(** [is_registrable_domain t domain] returns [true] if [domain] is exactly a
    registrable domain (public suffix plus one label, no more).

    Examples:
    - [is_registrable_domain t "example.com"] returns [Ok true]
    - [is_registrable_domain t "www.example.com"] returns [Ok false]
    - [is_registrable_domain t "com"] returns [Ok false] *)

(** {1 Statistics} *)

val rule_count : t -> int
(** Total number of rules in the embedded PSL *)

val icann_rule_count : t -> int
(** Number of ICANN section rules *)

val private_rule_count : t -> int
(** Number of private section rules *)

(** {1 Version Information} *)

val version : t -> string
(** Version string from the embedded PSL data.

    Returns the version identifier from the Public Suffix List source file,
    typically in the format ["YYYY-MM-DD_HH-MM-SS_UTC"]. *)

val commit : t -> string
(** Commit hash from the embedded PSL data.

    Returns the git commit hash from the Public Suffix List repository
    corresponding to the version of the data embedded in this library. *)
