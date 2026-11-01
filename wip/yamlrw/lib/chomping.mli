(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Block scalar chomping indicators *)

type t =
  | Strip  (** Remove final line break and trailing empty lines *)
  | Clip  (** Keep final line break, remove trailing empty lines (default) *)
  | Keep  (** Keep final line break and trailing empty lines *)

val to_string : t -> string
(** Convert chomping mode to string *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a chomping mode *)

val of_char : char -> t option
(** Parse chomping indicator from character *)

val to_char : t -> char option
(** Convert chomping mode to indicator character (None for Clip) *)

val equal : t -> t -> bool
(** Test equality of two chomping modes *)
