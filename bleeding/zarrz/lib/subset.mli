(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Rectangular regions of a C-order array.

    A subset names a hyper-rectangle inside an enclosing array by its
    origin and its extent. It carries no reference to the array, so the
    enclosing shape is passed at every operation as [~outer].

    Every operation here walks the region as maximal contiguous element
    runs. A run is a stretch that is contiguous in the source array and
    in the destination array at the same time, so {!copy}, {!gather} and
    {!scatter} turn each run into one [Base_bigstring.blit] and never
    touch an element. {!iter_runs} exposes the same walk to a caller
    that moves the bytes itself. *)

type t = { start : int iarray; shape : int iarray }
(** The region starting at [start] and extending [shape] elements in each
    dimension. Both have the rank of the enclosing array. *)

val rank : t -> int
(** [rank t] is the number of dimensions of [t]. *)

val num_elements : t -> int
(** [num_elements t] is the product of [t.shape]. Raises
    [Invalid_argument] if a dimension is negative or the product
    overflows. *)

val validate : outer:int iarray -> t -> unit
(** [validate ~outer t] checks that [t] indexes an array of shape
    [outer]. Raises [Invalid_argument] if the ranks differ, a start or
    extent is negative, or [start.(d) + shape.(d)] exceeds [outer.(d)] in
    any dimension. *)

val iter_runs :
  outer:int iarray -> t -> f:(src:int -> dst:int -> len:int -> unit) -> unit
(** [iter_runs ~outer t ~f] calls [f] once per maximal contiguous run of
    [t] inside an array of shape [outer], in increasing [dst] order.
    [src] is the linear C-order index in the enclosing array of the first
    element of the run, [dst] the linear index of that element in the
    dense C-order layout of [t] alone, and [len] the number of elements
    in the run. All three count elements, not bytes.

    Runs coalesce across every trailing dimension the subset spans in
    full, so a subset equal to the whole array yields exactly one run and
    a rectangle of full rows yields one run per row. A subset with no
    elements yields no runs, and a rank 0 subset yields one run of one
    element.

    Validates as {!validate} does. *)

val copy :
  elem_size:int ->
  src:Base_bigstring.t ->
  src_outer:int iarray ->
  src_start:int iarray ->
  dst:Base_bigstring.t ->
  dst_outer:int iarray ->
  dst_start:int iarray ->
  shape:int iarray ->
  unit
(** [copy ~elem_size ~src ~src_outer ~src_start ~dst ~dst_outer
    ~dst_start ~shape] copies the block of extent [shape] at [src_start]
    of the C-order array of extent [src_outer] held in [src] onto the
    block at [dst_start] of the C-order array of extent [dst_outer] held
    in [dst]. [elem_size] is the element size in bytes and every other
    argument counts elements.

    A trailing dimension coalesces into a run only when [shape] spans it
    in full on both sides, so a block that is a whole array on both sides
    moves in one [Base_bigstring.blit]. The result is unspecified if the
    two blocks overlap in one buffer.

    Raises [Invalid_argument] if the ranks differ, if either block falls
    outside its array, or if either buffer is shorter than the array it
    holds. Buffers longer than needed are accepted and the excess is
    untouched. *)

val gather :
  elem_size:int ->
  src:Base_bigstring.t ->
  outer:int iarray ->
  t ->
  dst:Base_bigstring.t ->
  unit
(** [gather ~elem_size ~src ~outer t ~dst] copies the region [t] out of
    [src], which holds an array of shape [outer], into [dst], which
    receives it densely in C order. It is {!copy} with [t.shape] as the
    destination array and its origin as the destination block. The same
    length rules and exceptions apply. *)

val scatter :
  elem_size:int ->
  src:Base_bigstring.t ->
  dst:Base_bigstring.t ->
  outer:int iarray ->
  t ->
  unit
(** [scatter ~elem_size ~src ~dst ~outer t] is the inverse of {!gather}.
    It copies the dense C-order block [src] into the region [t] of [dst],
    which holds an array of shape [outer]. The same length rules and
    exceptions apply. *)
