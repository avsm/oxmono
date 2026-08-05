(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Queries against the Mozilla Public Suffix List.

    Internal to [fetch.cookies], which uses it for the [Domain=] check of
    RFC 6265 §5.3 step 5. It is vendored from ocaml-publicsuffix along
    with a PSL table generated at vendoring time, so the full API is kept
    for upstream comparison even though only {!v} and
    {!is_public_suffix} are called here.

    A {e public suffix} is a domain under which users may register names,
    such as [com], [co.uk] or [pvt.k12.ma.us]. A {e registrable domain}
    is a public suffix plus one label, so for [www.example.com] the
    public suffix is [com] and the registrable domain is [example.com].

    The list holds three kinds of rule: a normal suffix ([com]), a
    wildcard matching any one label in that position ([*.jp]), and an
    exception overriding a wildcard ([!city.kobe.jp]). The longest match
    wins, an exception beats every other rule, and where nothing matches
    the implicit [*] rule applies. Rules are split into an ICANN section
    and a section submitted by private parties.

    Trailing dots are preserved, so [public_suffix t "example.com."] is
    [Ok "com."].

    Unlike upstream, this copy has no IDNA support: {!Fetch}'s URLs only
    ever yield ASCII hosts, so a non-ASCII domain gives
    [Punycode_error] rather than being converted. Give hosts in punycode
    form.

    @see <https://publicsuffix.org/list/> The list and its algorithm. *)

(** {1 Types} *)

type section =
  | ICANN  (** Delegated by ICANN, or in the IANA root zone. *)
  | Private  (** Submitted by a private party, such as [github.io]. *)
(** Which section of the list a rule came from. *)

type t
(** A handle on the parsed list. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] prints a summary of [t]. *)

(** {1 Errors} *)

type error =
  | Empty_domain  (** The domain was empty. *)
  | Invalid_domain of string  (** The domain was not a valid name. *)
  | Leading_dot  (** The domain began with a dot. *)
  | Punycode_error of string
      (** The domain was not ASCII. This copy does no IDNA conversion. *)
  | No_public_suffix  (** No public suffix was found. *)
  | Domain_is_public_suffix
      (** The domain is a public suffix, so it has no registrable
          domain. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error fmt e] prints [e]. *)

val error_to_string : error -> string
(** [error_to_string e] is [e] as a human-readable string. *)

(** {1 Creation} *)

val v : unit -> t
(** [v ()] is a handle on the list embedded at build time. *)

(** {1 Queries} *)

val public_suffix : t -> string -> (string, error) result
(** [public_suffix t domain] is the public suffix of [domain], so
    [public_suffix t "www.example.co.uk"] is [Ok "co.uk"]. *)

val public_suffix_with_section : t -> string -> (string * section, error) result
(** [public_suffix_with_section t domain] is {!public_suffix} together
    with the section the matching rule came from. The implicit [*] rule
    counts as [ICANN]. *)

val registrable_domain : t -> string -> (string, error) result
(** [registrable_domain t domain] is the public suffix of [domain] plus
    one label, so [registrable_domain t "www.example.com"] is
    [Ok "example.com"]. It is [Error Domain_is_public_suffix] if
    [domain] is itself a public suffix. *)

val registrable_domain_with_section :
  t -> string -> (string * section, error) result
(** [registrable_domain_with_section t domain] is {!registrable_domain}
    together with the section the matching rule came from. *)

val is_public_suffix : t -> string -> (bool, error) result
(** [is_public_suffix t domain] is [true] if [domain] is exactly a public
    suffix. [foo.ck] is one by the [*.ck] wildcard, while [www.ck] is not
    because of the [!www.ck] exception. *)

val is_registrable_domain : t -> string -> (bool, error) result
(** [is_registrable_domain t domain] is [true] if [domain] is a public
    suffix plus exactly one label. *)

(** {1 Provenance of the embedded data} *)

val rule_count : t -> int
(** [rule_count t] is the number of rules embedded. *)

val icann_rule_count : t -> int
(** [icann_rule_count t] is the number of ICANN-section rules. *)

val private_rule_count : t -> int
(** [private_rule_count t] is the number of private-section rules. *)

val version : t -> string
(** [version t] is the list's version stamp, of the form
    ["YYYY-MM-DD_HH-MM-SS_UTC"]. *)

val commit : t -> string
(** [commit t] is the git commit of the list the data came from. *)
