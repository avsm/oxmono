(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Horizontal lines with an optional label.

    A rule separates the parts of a program's output. It is a line as wide as
    the formatter's margin, in the muted colour of its theme ({!Theme.get}),
    with an optional label set in it. *)

(** {1:rules Rules} *)

val view :
  ?style:Style.t ->
  ?glyph:string ->
  ?align:View.align ->
  ?label:View.t ->
  unit ->
  View.t
(** [view ()] is a horizontal line as wide as its room ({!View.fit}). At its
    natural size, its label has two glyphs on each side, and without a label it
    is four glyphs.
    - [glyph] draws it (defaults to a light horizontal line, U+2500). A [glyph]
      that is not ASCII is drawn as [-] on a formatter that is not {!Fmt.utf_8}.
    - [style] is its style (defaults to the muted colour of the formatter's
      theme, {!Theme.get}).
    - [label], if given, sits in the line at [align] (defaults to [`Center]),
      with a space on each side, and is cut with an ellipsis to the room.

    @raise Invalid_argument
      if [glyph], with each control character a space, is not one terminal cell
      wide. *)
