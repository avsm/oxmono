(** This module contains internal data generated from the Public Suffix List. *)

type section =
  | ICANN (** [ICANN] marks a rule from the ICANN section. *)
  | Private (** [Private] marks a rule from the Private section. *)

type rule_type =
  | Normal (** [Normal] matches its labels exactly. *)
  | Wildcard (** [Wildcard] matches any leftmost label. *)
  | Exception (** [Exception] overrides a wildcard match. *)

(** A [trie_node] is a node in the reverse-label suffix trie. *)
type trie_node =
  { rule : (rule_type * section) option
  ; children : (string * trie_node) list
  ; wildcard : section option
  }

(** [root] is the root of the embedded suffix trie. *)
val root : trie_node

(** [rule_count] is the total number of embedded rules. *)
val rule_count : int

(** [icann_rule_count] is the number of embedded ICANN rules. *)
val icann_rule_count : int

(** [private_rule_count] is the number of embedded Private rules. *)
val private_rule_count : int

(** [version] is the version recorded in the source list. *)
val version : string

(** [commit] is the commit recorded in the source list. *)
val commit : string
