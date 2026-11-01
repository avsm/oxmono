(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal colours.

    A colour is one of the 16 named ANSI colours, an entry of the xterm
    256-colour palette, or a 24-bit RGB colour.

    A colour is kept as given and written at the {!val-depth} of the formatter
    it is printed on ({!downsample}). *)

(** {1:colours Colours} *)

type t
(** The type for terminal colours. *)

val rgb : int -> int -> int -> t
(** [rgb r g b] is the 24-bit colour with red [r], green [g] and blue [b].

    @raise Invalid_argument if a component is not in \[[0];[255]\]. *)

val of_hex : string -> (t, [ `Msg of string ]) result
(** [of_hex s] is the colour written [s] in hexadecimal: ["RRGGBB"] or ["RGB"],
    optionally after a ["#"]. ["RGB"] is ["RRGGBB"] with each digit doubled. *)

val ansi_256 : int -> t
(** [ansi_256 n] is the entry [n] of the xterm 256-colour palette.

    @raise Invalid_argument if [n] is not in \[[0];[255]\]. *)

(** {1:predefined Predefined colours} *)

val black : t
(** [black] is black. *)

val red : t
(** [red] is red. *)

val green : t
(** [green] is green. *)

val yellow : t
(** [yellow] is yellow. *)

val blue : t
(** [blue] is blue. *)

val magenta : t
(** [magenta] is magenta. *)

val cyan : t
(** [cyan] is cyan. *)

val white : t
(** [white] is white. *)

val bright_black : t
(** [bright_black] is bright black. *)

val bright_red : t
(** [bright_red] is bright red. *)

val bright_green : t
(** [bright_green] is bright green. *)

val bright_yellow : t
(** [bright_yellow] is bright yellow. *)

val bright_blue : t
(** [bright_blue] is bright blue. *)

val bright_magenta : t
(** [bright_magenta] is bright magenta. *)

val bright_cyan : t
(** [bright_cyan] is bright cyan. *)

val bright_white : t
(** [bright_white] is bright white. *)

(** {1:rgb RGB} *)

val to_rgb : t -> int * int * int
(** [to_rgb c] is the red, green and blue components of [c]. A named or palette
    colour is taken at xterm's default value, which a terminal's own scheme may
    redefine. *)

val blend : t -> t -> float -> t
(** [blend c0 c1 f] is the RGB colour [f] of the way from [c0] to [c1]. [f] is
    clamped to 0 to 1, and NaN is 0. Each component is
    [a + round (f * (b - a))]. [blend c0 c1 0.] is [c0] and [blend c0 c1 1.] is
    [c1], as RGB. The fraction comes last so that [blend c0 c1] is the gradient
    from [c0] to [c1], whereas {!scale} takes its factor first. *)

val scale : float -> t -> t
(** [scale f c] is the RGB colour whose components are those of [c] times [f],
    rounded and clamped to 0 to 255, a NaN component being 0. The factor comes
    first so that [scale 0.5] is a function that darkens by half, whereas
    {!blend} takes its fraction last. *)

(** {1:preds Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal c0 c1] is [true] iff [c0] and [c1] are the same colour as given:
    [red] and [ansi_256 1] are not equal, although a terminal may draw them
    alike. *)

val compare : t -> t -> int
(** [compare c0 c1] is a total order on colours, compatible with {!equal}. *)

(** {1:depths Colour depth} *)

type depth = [ `Ansi_16 | `Ansi_256 | `True_color ]
(** The type for the colours a terminal shows: the 16 named colours, the
    xterm-256 palette, or any 24-bit colour. *)

val depth : Format.formatter -> depth
(** [depth ppf] is the colour depth of [ppf]. Every printer of this library
    {!downsample}s the colours it writes on [ppf] to it. If {!set_depth} has
    never been called on [ppf], this is [`Ansi_16], the colours every terminal
    shows. [Console_tty.setup] sets it from the environment. *)

val set_depth : Format.formatter -> depth -> unit
(** [set_depth ppf d] sets the colour depth of [ppf] to [d]. *)

val downsample : depth -> t -> t
(** [downsample d c] is [c] as a terminal of depth [d] shows it:
    - [c] itself at [`True_color], and for a colour already in the palette of
      [d];
    - at [`Ansi_256], the nearest entry of the colour cube or of the grey ramp,
      whichever is nearer in RGB, as tmux's [colour_find_rgb] chooses;
    - at [`Ansi_16], the named colour nearest by the CIE 1976 colour difference
      (the distance in CIE L*a*b*, D65 white), against the xterm defaults of the
      16.

    It is idempotent: [downsample d (downsample d c)] is [downsample d c]. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats an RGB colour as [#rrggbb], which {!of_hex} reads back, and a
    named or palette colour by its name or number. *)

(**/**)

val sgr : depth -> t -> Sgr.color
(** [sgr d c] is [downsample d c] as an escape code selects it. *)
