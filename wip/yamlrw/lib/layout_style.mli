(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Collection layout styles *)

type t =
  [ `Any  (** Let emitter choose *)
  | `Block  (** Indentation-based *)
  | `Flow  (** Inline with brackets *) ]

val to_string : t -> string
(** Convert style to string representation *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a style *)

val equal : t -> t -> bool
(** Test equality of two styles *)

val compare : t -> t -> int
(** Compare two styles *)
