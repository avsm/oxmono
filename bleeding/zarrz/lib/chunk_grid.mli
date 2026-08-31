(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Regular chunk grids.

    The only grid Zarr V3 defines in its core. Every chunk is stored at
    the full chunk shape, edge chunks included, and the part of an edge
    chunk that lies beyond the array holds fill values. Use {!clip} to
    get the part that is inside the array.

    Every function taking chunk or array indices raises
    [Invalid_argument] when their dimensionality is not the grid's. *)

type t
(** The type for regular chunk grids. *)

val v : array_shape:int array -> chunk_shape:int array -> (t, string) result
(** [v ~array_shape ~chunk_shape] is the grid of [chunk_shape] chunks
    over an array of [array_shape]. It is an error if the two shapes
    have different lengths, if any chunk length is not positive, or if
    any array length is negative. The shapes are copied. *)

val of_ext : Ext.t -> array_shape:int array -> (t, string) result
(** [of_ext e ~array_shape] is the grid described by the [chunk_grid]
    member [e]. The name must be ["regular"], [must_understand] must be
    [true], which the spec requires of this extension point, and the
    configuration must be present, an object, and have [chunk_shape] as
    its only member.
    [chunk_shape] is an array of positive integers no greater than
    2{^52}, of the length of [array_shape]. *)

val to_ext : t -> Ext.t
(** [to_ext t] is the [chunk_grid] member for [t]. *)

val dimensionality : t -> int
(** [dimensionality t] is the number of dimensions of [t]. *)

val array_shape : t -> int array
(** [array_shape t] is a copy of the shape of the array [t] covers. *)

val chunk_shape : t -> int array
(** [chunk_shape t] is a copy of the shape of every chunk of [t]. *)

val grid_shape : t -> int array
(** [grid_shape t] is a copy of the number of chunks along each
    dimension, the ceiling of the array length divided by the chunk
    length. *)

val chunk_origin : t -> int array -> int array
(** [chunk_origin t i] is the array index of the first element of chunk
    [i], namely [i.(d) * chunk_shape.(d)] in each dimension. It is not
    checked that [i] is within {!grid_shape}. *)

val chunk_indices : t -> int array -> int array
(** [chunk_indices t j] is the index of the chunk holding array index
    [j], namely [j.(d) / chunk_shape.(d)] in each dimension. It is not
    checked that [j] is within {!array_shape}. *)

val clip : t -> int array -> (int array * int array) option
(** [clip t i] is [Some (start, shape)], the part of chunk [i] that lies
    within the array. [start] is {!chunk_origin}[ t i] and [shape] is
    the chunk shape truncated at the array bound, so it is smaller than
    the chunk shape only on an edge chunk. It is [None] when [i] is
    outside {!grid_shape}, which includes every index when the array has
    a zero length dimension. *)

val chunks_overlapping :
  t -> start:int array -> shape:int array -> (int array -> unit) -> unit
(** [chunks_overlapping t ~start ~shape f] calls [f] on the index of
    each chunk that intersects the array subset starting at [start] and
    extending by [shape], in C order. Indices beyond {!grid_shape} are
    not visited, so a subset reaching past the array yields only the
    chunks that exist, and a [shape] with a zero length dimension yields
    none. Each [f] gets a fresh array. A zero dimensional grid has one
    chunk, so [f] is called once with [[||]]. *)
