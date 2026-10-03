(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Linear colour gradients over terminal cells.

    A gradient is a list of colour stops, evenly spaced from position [0] to
    position [1], interpolated in RGB ({!Color.blend}). It is laid over a grid
    of cells along a direction: the cell at row [r] and column [c] sits at
    offset [dx * c + dy * r] along it, and a stretch of [length] cells runs from
    the first stop to the last. Beyond that stretch the gradient spreads as SVG
    1.1's [spreadMethod] says
    ({{:https://www.w3.org/TR/SVG11/pservers.html#LinearGradientElementSpreadMethodAttribute}SVG
      1.1, 13.2.2}): it pads with its end colours, reflects back and forth, or
    repeats.

    {!Style.fg_gradient} and {!Style.bg_gradient} make a gradient a style, so a
    {!Span}, a {!Border} or a {!Guide} can carry one; each widget lays it over
    its own cells, row [0] and column [0] being its top-left cell. *)

type spread = [ `Pad | `Reflect | `Repeat ]
(** The type for what a gradient does past its stretch (SVG's [spreadMethod]).
*)

type t
(** The type for linear gradients over cells. *)

val v :
  ?spread:spread -> ?direction:int * int -> length:int -> Color.t list -> t
(** [v ~length stops] is the gradient through [stops] over [length] cells, along
    [direction] [(dx, dy)] (defaults to [(1, 0)], left to right), spread by
    [spread] (defaults to [`Pad]).

    {b Raises.} [Invalid_argument] if [stops] is empty or [length] is not
    positive. *)

val stops : t -> Color.t list
(** [stops g] are the colour stops of [g], in order. *)

val at : t -> float -> Color.t
(** [at g p] is the colour of [g] at position [p], clamped to 0 to 1: with [n]
    stops, [p] falls between stop [i] and stop [i + 1] where [i] is the whole
    part of [p * (n - 1)], and is their {!Color.blend} by the fractional part. A
    single stop is that stop. *)

val cell : t -> row:int -> column:int -> Color.t
(** [cell g ~row ~column] is the colour of [g] on the cell at [row] and
    [column]: [at g p] where, with [k] the cell's offset along the direction and
    [l] the length,
    - [`Pad]: [p] is [k / l];
    - [`Repeat]: [p] is [(k mod l) / l];
    - [`Reflect]: [p] is [1 - |2x - 1|] with [x] being [(k mod 2l) / 2l], so the
      colours run out along the stops and back over [2l] cells.

    A negative offset is taken modulo the period like a positive one. *)

val shift : row:int -> column:int -> t -> t
(** [shift ~row ~column g] is [g] moved so that its cell at row [0] and column
    [0] is the one [g] has at [row] and [column]:
    [cell (shift ~row ~column g) ~row:r ~column:c] is
    [cell g ~row:(row + r) ~column:(column + c)]. A renderer that draws a block
    a row at a time shifts a gradient to each row to lay it over the whole
    block. *)

val map : (Color.t -> Color.t) -> t -> t
(** [map f g] is [g] with [f] applied to each of its stops. *)

val equal : t -> t -> bool
(** [equal g0 g1] is [true] iff [g0] and [g1] have equal stops, direction,
    length, spread and {!shift}. *)

val pp : t Fmt.t
(** [pp] formats a gradient for diagnostics, in an unspecified format. *)
