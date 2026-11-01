(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What the environment says about colour. *)

val dumb : getenv:(string -> string option) -> bool
(** [dumb ~getenv] is [true] iff [TERM] is unset, empty or [dumb]: a terminal
    that has no cursor to move. *)

val renderer :
  getenv:(string -> string option) -> is_tty:bool -> Fmt.style_renderer
(** [renderer ~getenv ~is_tty] is [`None] if [NO_COLOR] is set and not empty or
    {!dumb} holds, and otherwise [`Ansi_tty] iff [is_tty]. *)

val depth : getenv:(string -> string option) -> Console.Color.depth
(** [depth ~getenv] is the colour depth the environment announces:
    - [`True_color] if [COLORTERM] is [truecolor] or [24bit], or [TERM] ends in
      [-direct];
    - [`Ansi_256] if [TERM] contains [256color];
    - [`Ansi_16] otherwise. *)

val utf_8 : getenv:(string -> string option) -> bool
(** [utf_8 ~getenv] is [true] iff the locale's character set is UTF-8: that of
    the first of [LC_ALL], [LC_CTYPE] and [LANG] that is set and not empty. An
    unset locale is not UTF-8. *)
