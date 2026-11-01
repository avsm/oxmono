(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Running a display for a scheduler.

    This module is internal to console: [Console] does not expose it, and
    [console-eio] reads it through [Console__]. *)

val when_idle : Display.t -> (unit -> unit) -> unit
(** [when_idle display f] calls [f] at once if [display] is neither drawing nor
    writing, and as soon as it is done otherwise. A signal handler, which runs
    at any safe point of the program, calls [display] through it, so that it
    never works from a frame half drawn or half written. *)

val closed : Display.t -> bool
(** [closed display] is [true] once [display] is closed. *)

val watch :
  Display.t -> Format.formatter_out_functions -> Format.formatter_out_functions
(** [watch display out] is [out], which also tells [display] whether what it
    writes ends its line. A driver sends on through it what it takes from a
    formatter that writes where [display] does, so that the next line [display]
    prints without a terminal starts on a line of its own. *)
