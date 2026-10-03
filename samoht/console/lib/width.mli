(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Text width calculation in terminal cells.

    Default measurement follows Unicode grapheme clusters (UAX #29) and East
    Asian width (UAX #11), including combining marks, ZWJ emoji, flags and
    variation selectors. ANSI escape sequences count as zero. Supplying
    [char_width] selects code-point measurement for a custom terminal policy. *)

val default_char_width : Uchar.t -> int
(** [default_char_width u] is Uucp's Unicode terminal-width hint: [0] for
    non-printing and combining code points, [1] for narrow code points, and [2]
    for wide CJK and emoji code points. *)

val string_width : ?char_width:(Uchar.t -> int) -> string -> int
(** [string_width s] is the display width of [s] in terminal columns: UTF-8 is
    decoded and grapheme clusters are measured as a terminal renders them. ANSI
    CSI/OSC escape sequences count as zero. An explicit [char_width] replaces
    grapheme measurement with the supplied code-point policy. *)

val truncate : ?char_width:(Uchar.t -> int) -> int -> string -> string
(** [truncate width s] is [s] cut to fit within [width] terminal columns,
    preserving ANSI escape sequences, closing an active SGR style or OSC 8
    hyperlink when cut, and adding no ellipsis. *)

val ellipsize :
  ?char_width:(Uchar.t -> int) ->
  ?at:[ `Middle | `End ] ->
  int ->
  string ->
  string
(** [ellipsize width s] is [s] fitted into [width] terminal columns with a
    single ellipsis standing where the cut fell. [at] says where it falls.

    [`Middle], the default, replaces the middle, so both ends of the value
    survive: the leading characters that identify a hash, and the trailing ones
    that name a file. [`End] keeps the leading characters alone and marks the
    right-hand edge, for a value whose tail carries nothing a reader needs.

    [s] already within [width] is returned unchanged, and a [width] of one is
    the ellipsis alone.

    Unlike {!truncate}, which ends a value wherever the room does and leaves it
    reading as a line that wraps, this marks the cut. Escape sequences count as
    zero columns and are never cut; a style or hyperlink left open at either end
    is closed. *)

val shorten : ?char_width:(Uchar.t -> int) -> int -> string -> string
(** [shorten width s] is [s] when it fits in [width] terminal columns, and
    otherwise the longest run of its leading words that does: the words it drops
    are taken from its end, whole. A word ends at a space outside any bracket,
    so a group such as ["(300s bound)"] is dropped as one word. What is kept
    does not end on a separator (a space, a comma, a semicolon, a colon) nor on
    a word that only leads into the ones dropped (an article, a preposition, a
    conjunction: ["building in"] is ["building"]). No word is ever cut, so [s]
    whose first word is wider than [width] is [""]: a path, an id or a digest is
    kept whole or not at all.

    Escape sequences count as zero columns and a style or hyperlink left open is
    closed. *)

val fold :
  ?char_width:(Uchar.t -> int) -> ?split:bool -> int -> string -> string list
(** [fold width s] is [s] broken at spaces into lines of at most [width]
    terminal columns, the way [fold -s] breaks a line, with the spaces at each
    break dropped. [s] that fits, or a [width] of zero or less, is [[s]].

    A word wider than [width] is never cut: it stands whole on a line of its
    own, which is wider than [width]. With [~split:true] it is instead continued
    on the next line where the room ends, so that every line fits and every
    character is still printed, for a surface that cannot let a line wrap.

    A style or hyperlink open at a break is closed at the end of the line and
    opened again at the start of the next. *)

val pad_right : ?char_width:(Uchar.t -> int) -> int -> string -> string
(** [pad_right width s] is [s] padded with spaces on the right to reach [width]
    columns, or [s] unchanged if it is already wider. *)

val pad_left : ?char_width:(Uchar.t -> int) -> int -> string -> string
(** [pad_left width s] is [s] padded with spaces on the left to reach [width]
    columns, or [s] unchanged if it is already wider. *)

val center : ?char_width:(Uchar.t -> int) -> int -> string -> string
(** [center width s] is [s] centred within [width] columns with spaces on both
    sides, or [s] unchanged if it is already wider. *)

val wrap :
  ?char_width:(Uchar.t -> int) -> ?indent:int -> int -> string -> string
(** [wrap ?char_width ?indent width text] is [text] word-wrapped to [width]
    terminal columns, broken at spaces, each line prefixed with [indent] spaces
    (default [0]). A backquoted run is one word, so a command in backquotes is
    never broken across lines; a word wider than a line takes a line of its own.
*)
