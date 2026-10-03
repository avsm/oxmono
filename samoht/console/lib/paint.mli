(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Internal: a widget's output, cell by cell.

    A painter writes a widget's rows to a formatter and knows the row and the
    column of the next cell, so the glyphs it inks in a style with a gradient
    take the gradient's colour on their own cell. A run of inked glyphs opens
    one SGR sequence, writes another only where the next cell's style differs,
    and closes it before anything else is written. *)

type t
(** The type for painters. *)

val v : Format.formatter -> t
(** [v ppf] is a painter at row [0], column [0] of [ppf]. It styles iff
    [Fmt.style_renderer ppf] is [`Ansi_tty]. *)

val ink : t -> Style.t -> string -> unit
(** [ink p s glyphs] writes [glyphs] in [s], each cell in [Style.at] its row and
    column. A space that [s] would not change ({!Style.shows_on_space}) is
    written unstyled, closing the run. *)

val text : t -> string -> unit
(** [text p s] closes any open run and writes [s], already rendered, advancing
    the column by its display width. *)

val newline : t -> unit
(** [newline p] closes any open run and ends the row ({!Render.newline}). *)

val close : t -> unit
(** [close p] closes any open run. *)
