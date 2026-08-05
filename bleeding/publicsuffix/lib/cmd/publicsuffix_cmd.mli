(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
---------------------------------------------------------------------------*)

(** Reusable Cmdliner terms for the publicsuffix library.

    This module provides argument parsers and term functions that can be
    composed to build command-line tools that work with the Public Suffix List.
*)

(** {1 Argument terms} *)

val domain_arg : string Cmdliner.Term.t
(** Cmdliner term for parsing a domain name from a positional argument. *)

(** {1 Term functions} *)

val registrable_term :
  Publicsuffix.t -> (string, Publicsuffix.error) result Cmdliner.Term.t
(** Term that gets the registrable domain for a given domain. *)

val suffix_term :
  Publicsuffix.t -> (string, Publicsuffix.error) result Cmdliner.Term.t
(** Term that gets the public suffix for a given domain. *)

val is_suffix_term :
  Publicsuffix.t -> (bool, Publicsuffix.error) result Cmdliner.Term.t
(** Term that checks if a domain is a public suffix. *)

val is_registrable_term :
  Publicsuffix.t -> (bool, Publicsuffix.error) result Cmdliner.Term.t
(** Term that checks if a domain is a registrable domain. *)

val registrable_section_term :
  Publicsuffix.t ->
  (string * Publicsuffix.section, Publicsuffix.error) result Cmdliner.Term.t
(** Term that gets the registrable domain with section information. *)

val suffix_section_term :
  Publicsuffix.t ->
  (string * Publicsuffix.section, Publicsuffix.error) result Cmdliner.Term.t
(** Term that gets the public suffix with section information. *)

val stats_term : Publicsuffix.t -> (int * int * int) Cmdliner.Term.t
(** Term that returns statistics about the Public Suffix List as a tuple of
    (total_rules, icann_rules, private_rules). *)

val version_term : Publicsuffix.t -> (string * string) Cmdliner.Term.t
(** Term that returns version information about the Public Suffix List as a
    tuple of (version, commit). *)
