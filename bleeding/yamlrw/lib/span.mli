(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Source spans representing ranges in input *)

type t = { start : Position.t; stop : Position.t }

val make : start:Position.t -> stop:Position.t -> t
(** Create a span from start and stop positions *)

val point : Position.t -> t
(** Create a zero-width span at a single position *)

val merge : t -> t -> t
(** Merge two spans into one covering both *)

val extend : t -> Position.t -> t
(** Extend a span to a new stop position *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a span *)

val to_string : t -> string
(** Convert span to string *)

val compare : t -> t -> int
(** Compare two spans *)

val equal : t -> t -> bool
(** Test equality of two spans *)
