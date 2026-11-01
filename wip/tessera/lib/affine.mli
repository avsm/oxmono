(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** GDAL-order 2-D affine transforms.

    A transform maps a pixel coordinate to a world coordinate as
    [x = c + a * col + b * row] and [y = f + d * col + e * row]. This is
    the six-element order GDAL writes and the Zarr geo-embeddings
    convention stores in [spatial:transform], namely
    [[| px; 0.; ox; 0.; -.px; oy |]] for a north-up grid of pixel size
    [px] with upper-left corner [(ox, oy)].

    Column and row are measured from the upper-left corner of the grid,
    so integer values name pixel corners. The four helpers {!col_of_x},
    {!row_of_y}, {!x_of_col} and {!y_of_row} work in pixel centres
    instead, the convention the Zarr [x] and [y] coordinate arrays use.
    They ignore the rotation terms [b] and [d], which the Tessera store
    always writes as zero. *)

type t = {
  a : float;  (** [x] per column. *)
  b : float;  (** [x] per row, the rotation term. *)
  c : float;  (** [x] at column and row zero. *)
  d : float;  (** [y] per column, the rotation term. *)
  e : float;  (** [y] per row, negative for a north-up grid. *)
  f : float;  (** [y] at column and row zero. *)
}

val of_spatial : float array -> t
(** [of_spatial a] is the transform whose fields are the six elements of
    [a] in order.

    @raise Invalid_argument if [a] does not have exactly six elements. *)

val to_spatial : t -> float array
(** [to_spatial t] is the six-element array {!of_spatial} accepts. *)

val apply : t -> col:float -> row:float -> float * float
(** [apply t ~col ~row] is the world coordinate [(x, y)] of the pixel
    coordinate [(col, row)]. *)

val invert : t -> t
(** [invert t] is the transform mapping world coordinates back to pixel
    coordinates, so [apply (invert t) ~col:x ~row:y] is the [(col, row)]
    that [apply t] sends to [(x, y)].

    @raise Invalid_argument if [t] is singular, that is if [a * e - b * d]
    is zero or not finite. *)

val col_of_x : t -> x:float -> float
(** [col_of_x t ~x] is [(x -. t.c) /. t.a -. 0.5], the fractional column
    whose pixel centre lies at [x]. Rotation is ignored. *)

val row_of_y : t -> y:float -> float
(** [row_of_y t ~y] is [(y -. t.f) /. t.e -. 0.5], the fractional row
    whose pixel centre lies at [y]. Rotation is ignored. *)

val x_of_col : t -> col:float -> float
(** [x_of_col t ~col] is [t.c +. (col +. 0.5) *. t.a], the [x] of the
    centre of column [col]. Rotation is ignored. *)

val y_of_row : t -> row:float -> float
(** [y_of_row t ~row] is [t.f +. (row +. 0.5) *. t.e], the [y] of the
    centre of row [row]. Rotation is ignored. *)

val equal : t -> t -> bool
(** [equal x y] is field-by-field float equality. It is exact, so it
    holds only for transforms that came from the same numbers. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints the six coefficients of [t] on [ppf]. *)
