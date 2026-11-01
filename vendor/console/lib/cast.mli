(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Recordings of a terminal in
    {{:https://docs.asciinema.org/manual/asciicast/v2/}asciicast v2}.

    This module is internal to the [console] package: [Console] does not expose
    it, and [console.tty] and [console-eio] read it through [Console__]. *)

val file : unit -> string option
(** [file ()] is the file that [CONSOLE_CAST] names, when it is set and not
    empty. *)

val deterministic : unit -> bool
(** [deterministic ()] is [true] iff [CONSOLE_DETERMINISTIC_CLOCK] is set and
    not empty, or a recording is asked for ({!file}). *)

type header = { width : int; height : int; term : string option }
(** The type for the first line of a recording: the terminal's size, and its
    [TERM] if known. *)

val header : header Jsont.t
(** [header] maps the header of a recording, [version] 2, the terminal's [TERM]
    in its [env]. *)

type event = { time : float; data : string }
(** The type for the bytes a terminal was given at a time, in seconds. *)

val event : event Jsont.t
(** [event] maps an output event, the array [[time, "o", data]]. A byte of
    [data] that is not part of a UTF-8 character is written as U+FFFD, so that a
    recording is valid UTF-8. *)

val to_line : 'a Jsont.t -> 'a -> string
(** [to_line t v] is [v] on one line, ended. A time is written to the
    microsecond. *)
