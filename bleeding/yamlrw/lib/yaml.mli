(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Full YAML representation with anchors, tags, and aliases *)

type t =
  [ `Scalar of Scalar.t
  | `Alias of string
  | `A of t Sequence.t
  | `O of (t, t) Mapping.t ]

(** {2 Pretty Printing} *)

val pp : Format.formatter -> t -> unit

(** {2 Equality} *)

val equal : t -> t -> bool

(** {2 Conversion from Value} *)

val of_value : Value.t -> t
(** Construct from JSON-compatible Value *)

(** {2 Alias Resolution} *)

val default_max_alias_nodes : int
(** Default maximum nodes during alias expansion (10 million) *)

val default_max_alias_depth : int
(** Default maximum alias nesting depth (100) *)

val resolve_aliases : ?max_nodes:int -> ?max_depth:int -> t -> t
(** Resolve aliases by replacing them with referenced nodes.

    @param max_nodes Maximum number of nodes to create during expansion
    @param max_depth Maximum depth of alias-within-alias resolution
    @raise Error.Yamlrw_error if limits exceeded or undefined alias found *)

(** {2 Conversion to Value} *)

val to_value :
  ?resolve_aliases_first:bool ->
  ?max_nodes:int ->
  ?max_depth:int ->
  t ->
  Value.t
(** Convert to JSON-compatible Value.

    @param resolve_aliases_first
      Whether to resolve aliases before conversion (default true)
    @param max_nodes Maximum nodes during alias expansion
    @param max_depth Maximum alias nesting depth
    @raise Error.Yamlrw_error if unresolved aliases encountered *)

(** {2 Node Accessors} *)

val anchor : t -> string option
(** Get anchor from any node *)

val tag : t -> string option
(** Get tag from any node *)
