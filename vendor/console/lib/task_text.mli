(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The quantities and caller text of tasks, as their rows draw them. *)

val bytes : int64 -> string
(** [bytes n] is [n] bytes in the largest binary unit below 1024 of it, a
    negative [n] being [0 B]. *)

val bytes_short : int64 -> string
(** [bytes_short n] is [n] bytes in at most five characters, as a count that
    gives way shows them: a number of at most three digits, then the first
    letter of its unit, such as ["512B"], ["1.5K"] or ["256M"]. A count that
    would take four digits of a unit is shown in the next, such as ["1.0M"] for
    1000 KiB. *)

val duration : float -> string
(** [duration s] is [s] seconds as a row draws its elapsed time, rounded to its
    last digit: tenths of a second below a minute (["6.4s"]), then minutes and
    seconds (["1m05s"]), then hours and minutes (["1h02m"]). A negative or NaN
    [s] is ["0.0s"]. *)

val duration_cells : int
(** [duration_cells] is the cells a row's timer is right-aligned in: the widest
    {!duration} below a hundred hours. *)

val clean : string -> string
(** [clean s] is [s] on one line: each control character, line feeds included, a
    space. *)

val kept_bytes : int
(** [kept_bytes] is the most bytes of a label or a line that a display keeps:

    4096. *)

val capped : string -> string @@ portable
(** [capped s] is [s] if it has at most {!kept_bytes} bytes, and otherwise its
    longest prefix of whole grapheme clusters that leaves room for three dots,
    then ["..."]: at most {!kept_bytes} bytes in all. *)

val label : string -> string
(** [label s] is [capped (clean s)], the label or the message of a task as a
    display keeps it. *)

val lines : string -> string list
(** [lines s] are the lines of [s], each {!clean}, ending where
    {!Console_text.sanitize} ends a line, a tab in them gone to the next stop of
    eight columns. One trailing line end ends the last line rather than starting
    an empty one; [lines ""] is [[]]. *)
