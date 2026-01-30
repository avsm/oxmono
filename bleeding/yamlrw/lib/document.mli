(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML document with directives and content *)

type t = {
  version : (int * int) option;
  tags : (string * string) list;
  root : Yaml.t option;
  implicit_start : bool;
  implicit_end : bool;
}

val make :
  ?version:int * int ->
  ?tags:(string * string) list ->
  ?implicit_start:bool ->
  ?implicit_end:bool ->
  Yaml.t option ->
  t
(** Create a document *)

(** {2 Accessors} *)

val version : t -> (int * int) option
val tags : t -> (string * string) list
val root : t -> Yaml.t option
val implicit_start : t -> bool
val implicit_end : t -> bool

(** {2 Modifiers} *)

val with_version : int * int -> t -> t
val with_tags : (string * string) list -> t -> t
val with_root : Yaml.t -> t -> t

(** {2 Comparison} *)

val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
