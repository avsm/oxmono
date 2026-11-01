(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Box-drawing borders for panels and tables.

    A border is the glyphs that draw a box and the style they are drawn in.
    {!Panel} draws its corners and edges; {!Table} also draws the crossings of
    its column and row rules. *)

(** {1:borders Borders} *)

type chars = {
  top_left : string;
  top : string;
  top_right : string;
  left : string;
  right : string;
  bottom_left : string;
  bottom : string;
  bottom_right : string;
  vertical : string;  (** The line between two columns. *)
  horizontal : string;
      (** The line of a rule across the box: under a header, between rows. *)
  cross : string;  (** The crossing of a horizontal and a vertical line. *)
  top_cross : string;  (** The tee on the top edge, pointing down. *)
  bottom_cross : string;  (** The tee on the bottom edge, pointing up. *)
  left_cross : string;  (** The tee on the left edge, pointing right. *)
  right_cross : string;  (** The tee on the right edge, pointing left. *)
}
(** The type for the glyphs of a border, one per part of the box. It is plain
    data the program supplies: every field is needed to draw a box, and none
    constrains another.

    A glyph may take any number of cells. Each vertical line of the box is as
    wide as its widest glyph, and a narrower glyph is followed by spaces. A
    horizontal line repeats its glyph over its length, and where a whole copy
    does not fit, the glyph is cut between two characters and ended with spaces.

    Fields may be added to this record, so a custom border starts from a
    predefined one, as [v { (chars single) with top = "=" }] does. *)

type t
(** The type for borders. *)

val v : ?style:Style.t -> chars -> t
(** [v ~style chars] is the border drawn with [chars] in [style]. Without
    [style], or with {!Style.none}, it is drawn in the muted colour of the
    formatter's theme ({!Theme.muted}). *)

val chars : t -> chars
(** [chars b] are the glyphs of [b], each control character in them a space. *)

val style : t -> Style.t
(** [style b] is the style [b]'s glyphs are drawn in, {!Style.none} for the
    theme's muted colour. *)

val with_style : Style.t -> t -> t
(** [with_style s b] is [b] drawn in [s]. *)

(** {1:predefined Predefined borders} *)

val none : t
(** [none] is the border drawn with empty strings: a box without edges. *)

val simple : t
(** [simple] is the border drawn with a Unicode light horizontal line under a
    table's header and between its rows. It has no frame and no line between the
    columns. *)

val ascii : t
(** [ascii] is the border drawn with [+], [-] and [|]. *)

val single : t
(** [single] is the border drawn with Unicode light box-drawing lines. *)

val double : t
(** [double] is the border drawn with Unicode double box-drawing lines. *)

val dos : t
(** [dos] is the border drawn as Norton Commander drew its panels in code page
    437: a frame of Unicode double box-drawing lines, divided by light ones. *)

val rounded : t
(** [rounded] is like {!single} with Unicode arc corners. *)

val heavy : t
(** [heavy] is the border drawn with Unicode heavy box-drawing lines. *)

val hidden : t
(** [hidden] is the border drawn with spaces: the spacing of a box without its
    lines. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a border's corners and edges, for diagnostics. *)
