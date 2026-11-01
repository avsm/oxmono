(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tables with headers and aligned columns.

    A table is a list of {!type-column}s and rows of cells, drawn in a
    {!Border}. A table wider than the room it is given narrows its columns until
    it fits. When even the narrowest columns and the frame do not fit, each line
    is cut with an ellipsis, and no column is left out. *)

(** {1:columns Columns} *)

type overflow = [ `Wrap | `Truncate ]
(** The type for what a cell wider than its column does: [`Wrap] goes on over
    several lines ({!View.wrap}), while [`Truncate] keeps one line, ended with
    an ellipsis ({!View.truncate}). *)

type column
(** The type for column specifications. *)

val column :
  ?align:View.align ->
  ?width:int ->
  ?min_width:int ->
  ?max_width:int ->
  ?overflow:overflow ->
  ?shrinkable:bool ->
  ?style:Style.t ->
  View.t ->
  column
(** [column header] is a column headed [header], in the table's header style
    under its own styles, such that:

    - [align] places the cells in the column (defaults to [`Left]);
    - [width] is its width, in cells, at its natural size (defaults to the width
      of its widest cell), within [min_width] and [max_width]. It narrows to fit
      as any column does, and a cell wider than it overflows;
    - [min_width] and [max_width] bound its width, in cells (default to none: it
      narrows to one cell to fit). The table goes below [min_width] only when
      nothing else fits;
    - [overflow] is what a cell wider than the column does (defaults to
      [`Wrap]). Under [`Wrap], the column keeps the width of its widest word
      while another column can narrow, so that a word stays whole, and a cell
      laid out in its room keeps its {!View.fit} [min_width] instead: a
      {!Bar.sparkline} narrows to one cell;
    - [shrinkable] is [false] for a column that is the last to narrow (defaults
      to [true]);
    - [style] is the style of its cells (defaults to {!Style.none}).

    @raise Invalid_argument
      if [width], [min_width] or [max_width] is negative, or [min_width] is
      above [max_width]. *)

(** {1:tables Tables} *)

val view :
  ?border:Border.t ->
  ?gap:int ->
  ?header_style:Style.t ->
  ?rules:bool ->
  column list ->
  View.t list list ->
  View.t
(** [view ~rules columns rows] is the table of [rows] under [columns], laid out
    in its room ({!View.fit}): no wider than the room, its columns narrowing
    until it fits, and each cell laid out at the width of its column.

    - Its frame is [border] if given, else the border of the formatter's theme
      ({!Theme.get}).
    - [gap] is the spaces between two cells (defaults to [2]). In a frame, each
      cell has [gap / 2] of them on its left and the rest on its right, inside
      the frame's lines.
    - The headers are in [header_style] (defaults to the theme's
      {!Theme.header}).
    - If [rules] (defaults to [false]), a rule separates each two rows, in the
      glyphs and the style of the header's rule. A frame that moves
      ({!Theme.val-motion}) moves the ends of the rules with its edges.

    A cell may hold line feeds, which start new lines inside its row. A row with
    fewer cells than there are columns ends in a cell that spans the columns
    left. In a table without a frame, whose border has no glyph in its vertical
    lines ({!Border.none}, {!Border.simple}), a line ends at its last cell that
    is not empty.

    - The lines of a framed table that fits are all as wide as one another.
    - The cells of a right-aligned column end in the same column.
    - A column is as wide as its widest cell, so a table whose cells change in a
      display gives its columns a [width] to keep still.

    @raise Invalid_argument
      if [gap] is negative, or a row is empty or has more cells than there are
      columns. *)
