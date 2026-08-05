(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The Public Suffix List, compiled into a trie.

    Internal to [fetch.cookies], and generated rather than written:
    ocaml-publicsuffix's [gen_psl] reads [public_suffix_list.dat] and
    emits this module, so the list needs no file I/O or parsing at
    runtime. Query it through {!Publicsuffix}.

    Labels are stored lowercase, in punycode, and in reverse order with
    the top-level domain first, so [example.co.uk] is walked as [uk],
    then [co], then [example]. *)

type section =
  | ICANN  (** Delegated by ICANN, or in the IANA root zone. *)
  | Private  (** Submitted by a private party. *)
(** Which section of the list a rule came from. *)

type rule_type =
  | Normal  (** A suffix matching exactly, such as [com]. *)
  | Wildcard  (** [*.] matching any one label in that position. *)
  | Exception  (** [!] overriding a wildcard. *)
(** The three kinds of rule the list defines. *)

type trie_node = {
  rule : (rule_type * section) option;
      (** Set if a rule ends at this node. *)
  children : (string * trie_node) list;  (** Exact label matches. *)
  wildcard_child : trie_node option;  (** The [*] match, if any. *)
}
(** A node of the suffix trie. *)

val root : unit -> trie_node
(** [root ()] is the trie's root, from which labels are walked
    top-level-domain first. *)

(** {1 Provenance} *)

val rule_count : int
(** [rule_count] is the number of rules embedded, of every kind and
    section. *)

val icann_rule_count : int
(** [icann_rule_count] is the number of ICANN-section rules. *)

val private_rule_count : int
(** [private_rule_count] is the number of private-section rules. *)

val version : string
(** [version] is the list's version stamp, of the form
    ["YYYY-MM-DD_HH-MM-SS_UTC"]. *)

val commit : string
(** [commit] is the git commit of the list the data came from. *)
