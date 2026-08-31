(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Byte ranges of a store object.

    A range is expressed without knowing the object size, so that a
    store can turn it into an HTTP [Range] header without a prior
    [HEAD]. {!resolve} turns it into a concrete span once the size is
    known. *)

type t =
  | From_start of { off : int; len : int option }
      (** Bytes [off] onwards, at most [len] of them. [None] runs to the
          end of the object. *)
  | Suffix of int  (** The last [n] bytes of the object. *)

val resolve : size:int -> t -> int * int
(** [resolve ~size r] is the [(start, length)] span that [r] denotes in
    an object of [size] bytes. The span is always within the object: a
    range starting beyond the end has length [0], and a range or suffix
    longer than the object is truncated. Raises [Invalid_argument] if
    [size] is negative or [r] carries a negative offset, length or
    suffix. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf r] prints a one line rendering of [r]. *)
