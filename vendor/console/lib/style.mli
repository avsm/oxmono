(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal text styles.

    A style is a set of attributes and colours, composed with {!( + )}:
    [Style.(bold + fg Color.red + underline)]. *)

(** {1:styles Styles} *)

type t
(** The type for styles. *)

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

val reverse : t
(** [reverse] is the style of reverse video text. *)

val strikethrough : t
(** [strikethrough] is the style of struck-through text. *)

val fg : Color.t -> t
(** [fg c] is the style with foreground colour [c]. *)

val bg : Color.t -> t
(** [bg c] is the style with background colour [c]. *)

val ( + ) : t -> t -> t
(** [s0 + s1] is [s0] with the attributes of [s1] added and the colours that
    [s1] sets replacing those of [s0]. {!none} is its unit and it is
    associative: [s + none], [none + s] and [s] are equal, and so are
    [s0 + (s1 + s2)] and [(s0 + s1) + s2]. *)

val equal : t -> t -> bool
(** [equal s0 s1] is [true] iff [s0] and [s1] are the same style. *)

val compare : t -> t -> int
(** [compare s0 s1] is a total order on styles, compatible with {!equal}. *)

(** {1:fmt Formatting} *)

val styled : t -> 'a Fmt.t -> 'a Fmt.t
(** [styled s pp_v] formats like [pp_v] in [s] iff the formatter's style
    renderer ({!Fmt.style_renderer}) is [`Ansi_tty], and like [pp_v] otherwise.
    Its colours are those the formatter's {!Color.val-depth} shows.

    - In the printer of another [styled], [s] is added to the outer style
      ({!( + )}), which is on again after it.
    - The escape codes take no room on the line, so the boxes and breaks of
      [pp_v] lay out as they do without colour. *)

val pp : t Fmt.t
(** [pp] formats a style for diagnostics, in an unspecified format. *)

(**/**)

val sgr : Color.depth -> t -> string
(** [sgr d s] is the escape code that selects [s], its colours
    {!Color.downsample}d to [d], or [""] if [s] sets nothing. *)
