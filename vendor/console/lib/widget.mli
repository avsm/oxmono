(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What the widgets share. *)

type look = Look.t
(** The type for how a widget is drawn: in a theme, in UTF-8 or ASCII alone,
    live or not. *)

val border : look -> Border.t option -> Border.t
(** [border look b] is [b] if given, else the border of [look]'s theme, in
    {!muted} if it has no style; in the glyphs of {!Border.ascii}, in the same
    style, when [look] is not UTF-8 and one of its glyphs is not ASCII, each
    glyph [b] leaves empty still empty. *)

val guide : look -> Guide.t option -> Guide.t
(** [guide look g] is like {!border} for the guide of a tree. *)

val glyph : look -> string -> ascii:string -> string
(** [glyph look g ~ascii] is [g], or [ascii] when [look] is not UTF-8 and [g] is
    not ASCII. *)

val ascii_theme : look -> bool
(** [ascii_theme] is {!Look.ascii_theme}. *)

val line : look -> string -> string
(** [line look g] is [g], the glyph of a line, or [-] when [look] is not UTF-8
    and [g] is not ASCII, or in an {!ascii_theme}. *)

val spinner : look -> time:float -> string
(** [spinner look ~time] is the frame of [look]'s spinner [time] seconds in, as
    {!glyph} shows it, with {!Spinner.ascii}'s frame for ASCII. *)

val mark : look -> Theme.mark -> string
(** [mark look m] is the mark of [m] in [look]'s theme, as {!glyph} shows it,
    with {!Theme.ascii}'s mark for ASCII. *)

val bar : look -> Theme.bar
(** [bar look] is the glyphs of a bar in [look]: its theme's, or [`Ascii] when
    [look] is not UTF-8. *)

val ellipsis : look -> string
(** [ellipsis] is {!Look.ellipsis}. *)

val truncate : look -> int -> View.t -> View.t
(** [truncate look w s] is {!View.truncate}[ w s] with {!ellipsis}. *)

val muted : look -> Style.t
(** [muted look] is the style of the muted colour of [look]'s theme. *)

val accent : look -> Style.t
(** [accent look] is the style of the main colour of [look]'s theme. *)

val one_line : View.t -> View.t
(** [one_line s] is [s] with each line feed a space. *)

val cells : string -> int
(** [cells s] is the number of cells [s] takes on a terminal. *)

val widest : string list -> int
(** [widest gs] is the {!cells} of the widest of [gs], [0] if there is none. *)

val fill : int -> string -> string
(** [fill n g] is [g] repeated over exactly [n] cells: the copies of [g] that
    fit, then the characters of one more that fit, then spaces. It is [""] if
    [n] is not positive, and [n] spaces if [g] takes no cell. *)

val rails : Guide.t -> bool list -> string
(** [rails g lasts] are the guides beside a node whose ancestors' flags,
    innermost first, are [lasts]: [true] when the ancestor is the last of its
    siblings, which leaves a {!Guide.space} under it, else a {!Guide.pipe}. *)

val branch : Guide.t -> bool list -> string
(** [branch g (own :: up)] is the guide before a node whose flag is [own] and
    whose ancestors' are [up]: {!rails} of [up], then {!Guide.last} if [own]
    else {!Guide.branch}. It is [""] for a root, whose flags are [[]]. *)

val pad_glyph : int -> string -> string
(** [pad_glyph n g] is [g] followed by the spaces that make it [n] cells wide.
*)

val pad : int -> View.t
(** [pad n] is [n] spaces, none if [n] is not positive. *)

val framed_kind : View.t list -> look -> View_core.kind
(** [framed_kind parts look] is how a frame around [parts] changes in [look]:
    with the time if [look] is live and its theme's frames move
    ({!Theme.val-motion}), and otherwise as its parts change. *)
