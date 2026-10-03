(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal text styles.

    Styles can be composed: [Style.(bold + fg Color.red + underline)]. *)

(** {1:style_type Style type} *)

type t
(** A composable style specification. *)

(** {1:constructors Constructors} *)

val none : t
(** [none] is the empty style. *)

val bold : t
(** [bold] is the style of bold text. *)

val faint : t
(** [faint] is the style of faint text. *)

val italic : t
(** [italic] is the style of italic text. *)

val underline : t
(** [underline] is the style of underlined text. *)

val blink : t
(** [blink] is the style of blinking text. *)

val reverse : t
(** [reverse] is the style of reverse video text. *)

val strikethrough : t
(** [strikethrough] is the style of struck-through text. *)

val fg : Color.t -> t
(** [fg c] is the style with foreground colour [c]. *)

val bg : Color.t -> t
(** [bg c] is the style with background colour [c]. *)

val fg_gradient : Gradient.t -> t
(** [fg_gradient g] is the style whose foreground is [g], laid over the cells it
    styles: each cell takes the colour {!Gradient.cell} gives its row and
    column. *)

val bg_gradient : Gradient.t -> t
(** [bg_gradient g] is like {!fg_gradient} for the background. *)

(** {1:composition Composition} *)

val ( + ) : t -> t -> t
(** [s1 + s2] combines two styles. Later styles override earlier ones for
    conflicting attributes. *)

val merge : t list -> t
(** [merge styles] merges a list of styles left to right. *)

(** {1:colours Colours} *)

val at : row:int -> column:int -> t -> t
(** [at ~row ~column s] is [s] on the cell at [row] and [column]: each gradient
    of [s] replaced by its colour on that cell, the rest of [s] unchanged. *)

val has_gradient : t -> bool
(** [has_gradient s] is [true] iff the foreground or the background of [s] is a
    gradient, so that {!val-at} gives different styles on different cells. *)

val shows_on_space : t -> bool
(** [shows_on_space s] is [true] iff a space drawn in [s] looks different from
    an unstyled one: [s] sets a background, reverse video, underline or
    strikethrough. *)

val foreground : t -> Color.t option
(** [foreground s] is the foreground colour of [s], if it sets one. A gradient
    foreground is its colour on row [0], column [0]. *)

val background : t -> Color.t option
(** [background s] is like {!foreground} for the background. *)

val map_color : ?fg:(Color.t -> Color.t) -> ?bg:(Color.t -> Color.t) -> t -> t
(** [map_color ~fg ~bg s] is [s] with [fg] applied to its foreground colour and
    [bg] to its background colour (each defaults to the identity), every stop of
    a gradient included. *)

(** {1:ansi_escape_codes ANSI escape codes} *)

val to_ansi : t -> string
(** [to_ansi s] is the ANSI escape sequence that enables
    [at ~row:0 ~column:0 s], or the empty string for {!none}. Its colours are
    written at {!Color.val-depth}. *)

val reset : string
(** [reset] is the ANSI escape sequence to reset all styling. *)

(** {1:fmt_integration Fmt integration} *)

val styled : t -> 'a Fmt.t -> 'a Fmt.t
(** [styled style pp] applies [style] when the target formatter's style renderer
    permits ANSI, and otherwise renders with [pp] unchanged.

    Example: [Fmt.pr "%a" (Style.styled Style.bold Fmt.string) "hello"]. *)

(** {1:operations Operations} *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same style. *)

val pp : t Fmt.t
(** [pp] pretty-prints [s]. *)

val is_none : t -> bool
(** [is_none s] is [true] if [s] has no styling. *)
