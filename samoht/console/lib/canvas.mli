(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Grids of styled cells.

    A canvas is a rectangle of cells, each one terminal cell wide and carrying
    its own {!Style.t}: the surface for pixel art, title cards and scenes, drawn
    a cell at a time and rendered by Console. Row [0] is the top row and column
    [0] the left one; a gradient in a cell's style takes its colour on the
    cell's own row and column.

    A canvas is a value: {!map} and the constructors make new ones. An animation
    is a function from a frame to a canvas, which [Console.Display.set_scene]
    draws through {!rows} at every frame of a live display. *)

(** {1:cells Cells} *)

type cell
(** The type for cells: a glyph one terminal cell wide and its style. *)

val cell : ?style:Style.t -> string -> cell
(** [cell ~style g] is the glyph [g] drawn in [style] (defaults to
    {!Style.none}).

    {b Raises.} [Invalid_argument] if [g] is not exactly one terminal cell wide
    ({!Width.string_width}) or holds a terminal control. Use {!cells} to cut
    untrusted text into cells. *)

val cells : ?style:Style.t -> string -> cell list
(** [cells ~style s] is [s] as a row of cells in [style], one for each glyph of
    [s] after {!Console.sanitize} (without newlines): a zero-width code point
    joins the cell before it, and a glyph wider than one cell becomes U+FFFD, so
    the row is exactly as wide as it is long. *)

val glyph : cell -> string
(** [glyph c] is the glyph of [c]. *)

val style : cell -> Style.t
(** [style c] is the style of [c]. *)

val with_style : Style.t -> cell -> cell
(** [with_style s c] is [c] drawn in [s]. *)

(** {1:canvases Canvases} *)

type t
(** The type for canvases. *)

val v : width:int -> height:int -> (row:int -> column:int -> cell) -> t
(** [v ~width ~height f] is the canvas whose cell at [row] and [column] is
    [f ~row ~column].

    {b Raises.} [Invalid_argument] if [width] or [height] is negative. *)

val of_rows : cell list list -> t
(** [of_rows rows] is the canvas whose rows are [rows], top to bottom.

    {b Raises.} [Invalid_argument] if the rows are not all as long. *)

val width : t -> int
(** [width c] is the number of columns of [c]. *)

val height : t -> int
(** [height c] is the number of rows of [c]. *)

val get : t -> row:int -> column:int -> cell
(** [get c ~row ~column] is the cell of [c] at [row] and [column].

    {b Raises.} [Invalid_argument] if the position is outside [c]. *)

val map : (row:int -> column:int -> cell -> cell) -> t -> t
(** [map f c] is [c] with [f ~row ~column] applied to each of its cells. *)

(** {1:rendering Rendering} *)

val rows : t -> Span.t list
(** [rows c] is [c] as one span per row. Adjacent cells whose styles draw the
    same SGR sequence (at {!Color.val-depth}, a gradient taken on each cell) are
    one styled run, so a row writes a sequence only where its look changes. *)

val pp : t Fmt.t
(** [pp] formats [c] one row per line, each row cut to the formatter's margin,
    following its style renderer ({!Span.pp}). *)
