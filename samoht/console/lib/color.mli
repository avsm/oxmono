(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal colors.

    Supports ANSI 16-color palette, 256-color palette, and true color (RGB/hex).

    A colour is kept as given and degraded only where it is drawn: {!to_fg_code}
    and {!to_bg_code} write it at the process's {{!depths}colour depth}, the
    nearest entry of the terminal's palette standing for a colour it cannot
    show. *)

(** {1:color_types Color types} *)

type ansi =
  [ `Black
  | `Red
  | `Green
  | `Yellow
  | `Blue
  | `Magenta
  | `Cyan
  | `White
  | `Bright_black
  | `Bright_red
  | `Bright_green
  | `Bright_yellow
  | `Bright_blue
  | `Bright_magenta
  | `Bright_cyan
  | `Bright_white ]
(** Standard 16-color ANSI palette. *)

type t
(** A terminal color. *)

(** {1:constructors Constructors} *)

val ansi : ansi -> t
(** [ansi c] is the named 16-colour palette colour [c]. *)

val rgb : int -> int -> int -> t
(** [rgb r g b] is the 24-bit colour with the given components (each 0 to 255).
    {b Raises.} [Invalid_argument] if a component is out of range. *)

val hex : string -> t
(** [hex s] is the colour parsed from a hex string like ["#RRGGBB"] or
    ["RRGGBB"]. {b Raises.} [Invalid_argument] if the string is malformed. *)

val palette : int -> t
(** [palette n] is the 256-colour palette entry [n]. {b Raises.}
    [Invalid_argument] if [n] is not 0 to 255. *)

(** {1:predefined_colors Predefined colors} *)

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

(** {1:depths Colour depth} *)

type depth = [ `Ansi_16 | `Ansi_256 | `True_color ]
(** The type for the colours a terminal shows: the 16 named colours, the
    xterm-256 palette, or any 24-bit colour. *)

val depth_of_env : (string -> string option) -> depth
(** [depth_of_env getenv] is the colour depth the environment [getenv] reads
    announces:
    - [`True_color] if [COLORTERM] is [truecolor] or [24bit], or [TERM] ends in
      [-direct] (the terminfo convention for direct colour);
    - [`Ansi_256] if [TERM] contains [256color];
    - [`Ansi_16] otherwise. *)

val depth : unit -> depth
(** [depth ()] is the depth {!to_fg_code} and {!to_bg_code} write at (defaults
    to [`True_color], which writes every colour as given). *)

val set_depth : depth -> unit
(** [set_depth d] sets {!val-depth} to [d]. The terminal a process draws on is
    one for the whole process, so this is set once, by the application, usually
    through [Console_eio.setup]. *)

val downsample : depth -> t -> t
(** [downsample d c] is [c] as a terminal of depth [d] shows it:
    - [c] itself at [`True_color], and for a colour already in the palette of
      [d];
    - at [`Ansi_256], the nearest entry of the colour cube or of the grey ramp,
      whichever is nearer in RGB, as tmux's [colour_find_rgb] chooses;
    - at [`Ansi_16], the named colour nearest by the CIE 1976 colour difference
      (the distance in CIE L*a*b*, D65 white), against the xterm defaults of the
      16. *)

(** {1:rgb RGB} *)

val to_rgb : t -> int * int * int
(** [to_rgb c] is the red, green and blue components of [c]. A named or palette
    colour is taken at xterm's default value, which a terminal's own scheme may
    redefine. *)

val blend : t -> t -> float -> t
(** [blend c0 c1 t] is the RGB colour [t] of the way from [c0] to [c1], [t]
    clamped to 0 to 1: each component is [a + round (t * (b - a))]. *)

val scale : float -> t -> t
(** [scale f c] is the RGB colour whose components are those of [c] times [f],
    rounded and clamped to 0 to 255: [scale 0.5] darkens by half. *)

(** {1:ansi_codes ANSI codes} *)

val to_fg_code : t -> string
(** [to_fg_code c] is the ANSI escape sequence that sets the foreground to
    [downsample (depth ()) c] (without the leading [ESC\[]). *)

val to_bg_code : t -> string
(** [to_bg_code c] is the ANSI escape sequence that sets the background to
    [downsample (depth ()) c] (without the leading [ESC\[]). *)

(** {1:operations Operations} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same colour. *)

val pp : t Fmt.t
(** [pp] pretty-prints [c]. *)
