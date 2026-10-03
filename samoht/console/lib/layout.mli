(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Horizontal layout of rendered text blocks.

    A block is a multi-line string -- typically the output of
    {!Panel.to_string}, {!Table.to_string} or {!Tree.pp}. {!hcat} places several
    blocks side by side, padding each to its own width so the columns stay
    aligned even when lines carry ANSI colour codes or wide (CJK) glyphs. *)

val hcat : ?gutter:int -> ?align:[ `Top | `Bottom ] -> string list -> string
(** [hcat blocks] joins [blocks] side by side into one multi-line string.

    Each block is split on newlines and every line is padded on the right to
    that block's widest line -- ANSI escapes and wide glyphs counted as the
    terminal renders them (see {!Width.string_width}) -- so each column keeps a
    fixed width regardless of styling. Adjacent columns are separated by
    [gutter] spaces (default [1]).

    When the blocks differ in height the shorter ones are padded with blank
    lines: at the bottom for [`Top] (the default), at the top for [`Bottom]. No
    padding or gutter is emitted past the last non-empty column of a row, so
    rows carry no trailing whitespace. [hcat []] is [""]; [hcat [b]] is [b]
    unchanged. *)

val hcat_anim :
  ?gutter:int ->
  ?align:[ `Top | `Bottom ] ->
  string Anim.t list ->
  string Anim.t
(** [hcat_anim blocks] is {!hcat} lifted over animations: every frame samples
    each block in [blocks] at the current elapsed time (see {!Anim.all}) and
    joins the results side by side. Build the blocks from {!Span.anim},
    {!Panel.anim}, {!Tree.anim}, {!Bar.anim} or any [string Anim.t], then drive
    the joined row in a live {!Display} view. The column widths are recomputed
    each frame, so a block that changes width over time shifts its neighbours --
    fix a block's width (e.g. {!Panel.v}'s [?width]) to keep the columns steady.
*)

val vcat : ?gutter:int -> string list -> string
(** [vcat blocks] joins [blocks] from top to bottom. [gutter] is the number of
    blank lines inserted between adjacent blocks (default [0]). [vcat []] is
    [""]. {b Raises.} [Invalid_argument] if [gutter] is negative. *)

val vcat_anim : ?gutter:int -> string Anim.t list -> string Anim.t
(** [vcat_anim blocks] is {!vcat} lifted over animations, sampling every block
    at the same elapsed time. *)
