(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Low-level ANSI SGR primitives: a foreground colour and the escape sequences
    that switch graphic attributes on. The styling the live display and {!Bar}
    render with; {!Style} and {!Color} are the richer, user-facing palette. *)

val color_code : Color.t -> string
(** [color_code c] is the SGR sequence that sets the foreground to [c]. *)

val dim : string
(** [dim] is the SGR sequence that starts dim (faint) text. *)

val bold : string
(** [bold] is the SGR sequence that starts bold text. *)

val reset_code : string
(** [reset_code] is the SGR reset sequence. *)

val styled : string -> string -> string
(** [styled code s] wraps [s] in [code] and a {!reset_code}. *)

val dimmed : string -> string
(** [dimmed s] renders [s] dim, or [""] unchanged. *)

val should_style : Format.formatter -> bool
(** [should_style ppf] is whether [ppf] takes ANSI: [Fmt.style_renderer ppf] is
    [`Ansi_tty]. It is the one answer every surface in this library renders by,
    set by [Console_eio.setup] from [--color=auto|always|never], [NO_COLOR] and
    [TERM] ({!Console.style_renderer}), so they are honoured wherever they are
    read rather than once per surface. *)
