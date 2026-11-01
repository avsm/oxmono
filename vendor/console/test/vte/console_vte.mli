(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** An emulated terminal, and what a terminal recording put on its screen.

    The tests read console's output on the screen of an emulator, matrix's
    [Vte], which reads its bytes as xterm does. One choice of console's widths
    holds on it too: a spacing mark takes a cell of its own, as in xterm and
    VTE.

    A recording of a program that redraws lines in place holds every redraw, so
    reading it as text does not say what the screen showed. {!frames} replays a
    recording and reads the screen after each write, with the time of the write.
*)

(** {1:terminal Terminal} *)

type t
(** The type for emulated terminals. *)

val v : ?raw:bool -> rows:int -> cols:int -> unit -> t
(** [v ~rows ~cols ()] is a blank terminal of [rows] by [cols] cells, its cursor
    at the top left. A line feed also returns the cursor to the first column, as
    a tty with output processing on makes it; with [raw] (default [false]) it
    keeps the column, as with output processing off. *)

val feed : t -> string -> unit
(** [feed t s] is the terminal reading [s]. An escape sequence or a character
    that [s] leaves unended is read with the bytes of the next [feed]. *)

val vte : t -> Vte.t
(** [vte t] is the emulator behind [t]. *)

val rows : t -> int
(** [rows t] is the height of [t]. *)

val cols : t -> int
(** [cols t] is the width of [t]. *)

val resize : t -> rows:int -> cols:int -> unit
(** [resize t ~rows ~cols] gives [t] a new size and blanks its screen. *)

val cursor_pos : t -> int * int
(** [cursor_pos t] is the cursor's row and column. After a character in the last
    column, the column is the width of [t]: the next character wraps. *)

val cursor_visible : t -> bool
(** [cursor_visible t] is [true] iff the cursor is shown. *)

val to_string : t -> string
(** [to_string t] is the visible screen, a line per row. *)

val scrollback_lines : t -> string list
(** [scrollback_lines t] is what has scrolled off the top, oldest first. *)

val scrollback_size : t -> int
(** [scrollback_size t] is the number of {!scrollback_lines}. *)

val grid : t -> Grid.t
(** [grid t] is the visible screen's cells. *)

val fg : Grid.t -> int -> Ansi.Color.t
(** [fg g i] is the foreground colour of cell [i] of [g]. *)

val bg : Grid.t -> int -> Ansi.Color.t
(** [bg g i] is the background colour of cell [i] of [g]. *)

val screen : ?scrollback:bool -> t -> string list
(** [screen t] is the visible screen: one string per row, trailing blanks
    removed, and the blank rows at the bottom dropped. [scrollback] (default
    [false]) puts the lines that have already scrolled off the top in front of
    it. *)

(** {1:recordings Recordings} *)

val records : string -> (float * string) list
(** [records recording] is what the recorded program wrote and when, oldest
    first: one element per write, carrying the seconds since the epoch that the
    capture stamped it with.

    [recording] is the raw byte stream BSD [script(1) -r] writes, a sequence of

    {v <uint64 len><int64 tv_sec><int32 tv_usec><int32 kind><len bytes> v}

    little-endian throughout, with [kind] ['s'] for the session header, ['o']
    for a write the program made and ['e'] for the end. Only the ['o'] records
    are returned; a trailing partial record is dropped, since a capture of a
    program that was still running ends in one. It does not read util-linux's
    [script], which keeps its timings in a second file, and it does not read
    asciicast. *)

val frames : rows:int -> cols:int -> string -> (float * string list) list
(** [frames ~rows ~cols recording] is the screen after each write in
    [recording], with its timestamp: the writes of {!records} replayed into a
    [rows] by [cols] terminal, each paired with {!screen} taken straight after
    it.

    The timestamp is seconds from the recording's first record, not the epoch
    seconds {!records} carries, so two captures of the same command are
    comparable and a diff over one is stable. Empty writes make no frame: they
    change nothing on the screen. *)
