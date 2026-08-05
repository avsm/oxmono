(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Scalar formatting styles *)

type t =
  [ `Any  (** Let emitter choose *)
  | `Plain  (** Unquoted: foo *)
  | `Single_quoted  (** 'foo' *)
  | `Double_quoted  (** "foo" *)
  | `Literal  (** | block *)
  | `Folded  (** > block *) ]

val to_string : t -> string
(** Convert style to string representation *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a style *)

val equal : t -> t -> bool
(** Test equality of two styles *)

val compare : t -> t -> int
(** Compare two styles *)
