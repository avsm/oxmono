(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** How a formatter draws what is printed on it. *)

type t = { theme : Theme.t; utf_8 : bool; live : bool }
(** The type for what a formatter says of how to draw: its theme, whether it
    shows UTF-8 or ASCII alone, and whether a live display draws in colour, in
    which alone the theme's frames move. *)

val v : ?live:bool -> Format.formatter -> t
(** [v ppf] is the look of [ppf]: its {!Theme.get} and its {!Fmt.utf_8}, [live]
    (defaults to [false]). *)

val default : t
(** [default] is the look of a formatter given nothing: {!Theme.default}, in
    UTF-8, not live. *)

val ascii_theme : t -> bool
(** [ascii_theme l] is [true] iff the theme of [l] draws in ASCII on any
    formatter: its bars are [`Ascii], or the lines of its frames are ASCII. *)

val ellipsis : t -> string
(** [ellipsis l] ends a line cut in [l]: U+2026 in UTF-8, three dots in ASCII
    and in an {!ascii_theme}. *)

val equal : t -> t -> bool
(** [equal l0 l1] is [true] iff [l0] and [l1] have the same theme, physically,
    and the same [utf_8] and [live]. *)

val pp : t Fmt.t
(** [pp] formats a look for diagnostics, in an unspecified format. *)
