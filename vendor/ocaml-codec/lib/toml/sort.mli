(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** TOML value sorts, used as labels in error contexts and {!Loc.Path} frames.

    A {e sort} is the closed enumeration of node categories the TOML grammar
    defines: string, integer, float, boolean, datetime (offset / local), date,
    time, array, table. A {e kind} is the specific human-readable label for one
    instance, built from a sort plus an identifier. *)

type t =
  | String  (** Strings *)
  | Int  (** Integers *)
  | Float  (** Floating-point numbers *)
  | Bool  (** Booleans *)
  | Datetime  (** Offset datetimes *)
  | Datetime_local  (** Local datetimes *)
  | Date  (** Local dates *)
  | Time  (** Local times *)
  | Array  (** Arrays *)
  | Table  (** Tables (objects) *)

val to_string : t -> string
(** [to_string sort] is the TOML-spec name of [sort]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats sorts using {!to_string}. *)

val or_kind : kind:string -> t -> string
(** [or_kind ~kind sort] is [kind] if non-empty, [to_string sort] otherwise. *)

val kinded_string : kind:string -> string -> string
(** [kinded_string ~kind s] is [s] when [kind] is empty, [kind] followed by a
    space and [s] otherwise. *)

val kinded : kind:string -> t -> string
(** [kinded ~kind sort] is {!kinded_string} applied to [to_string sort]. *)
