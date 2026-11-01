(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML scalar values with metadata *)

type t

val make :
  ?anchor:string ->
  ?tag:string ->
  ?plain_implicit:bool ->
  ?quoted_implicit:bool ->
  ?style:Scalar_style.t ->
  string ->
  t
(** Create a scalar value *)

(** {2 Accessors} *)

val value : t -> string
val anchor : t -> string option
val tag : t -> string option
val style : t -> Scalar_style.t
val plain_implicit : t -> bool
val quoted_implicit : t -> bool

(** {2 Modifiers} *)

val with_anchor : string -> t -> t
val with_tag : string -> t -> t
val with_style : Scalar_style.t -> t -> t

(** {2 Comparison} *)

val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
val compare : t -> t -> int
