(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Displays, with what {!Progress} and a driver need. {!Display} is their
    public face and documents the values both have. *)

type t
(** The type for displays. *)

val v :
  ?size:(unit -> int * int) ->
  ?once_width:(unit -> int) ->
  ?write_error:(exn -> bool) ->
  ?delay:float ->
  ?fallback:View.t ->
  now:(unit -> float) ->
  Format.formatter ->
  View.t ->
  t
(** [v ~size ~once_width ~write_error ~delay ~fallback ~now ppf view] is
    {!Display.v}, which also takes as the end of its output an exception that
    [write_error] says is a write error (defaults to none but [Sys_error]), and
    draws [fallback] (defaults to {!View.empty}) in place of a view that raises.
    Without [size], what it prints once is [once_width ()] cells wide, and whole
    without [once_width]. *)

val set_view : t -> View.t -> unit
val print : t -> View.t -> unit
val with_output : t -> (Format.formatter -> 'a) -> 'a
val redraw : t -> unit
val close : t -> unit
val pp : t Fmt.t

(** {1:internal For progress and drivers} *)

val touch : t -> unit
(** [touch d] says that what the view shows changed: it is drawn again, at most
    ten times a second. *)

val told : t -> unit
(** [told d] says that [d] is told of every change to the tasks its view reads
    ({!touch}): a redraw draws a view that reads the tasks and no other state
    only after a change or as the time moves. *)

val write : t -> string -> unit
(** [write d s] writes [s] as it is, in turn with what [d] writes. *)

val is_live : t -> bool
(** [is_live d] is [true] iff [d] redraws a region on a terminal. *)

val print_lines : t -> View.t list -> unit
(** [print_lines d lines] is {!print} of [lines], each one line that fits. *)

val closed : t -> bool
(** [closed d] is [true] iff [d] closed. *)

val width : t -> int
(** [width d] is the terminal's width, read at the last redraw. *)

val look : ?live:bool -> t -> Look.t
(** [look d] is the look [d] draws in, live iff [live] (defaults to [false]) and
    [d] draws in colour. *)

val on_close : t -> (unit -> unit) -> unit
(** [on_close d f] has [d] call [f] as it closes, before its last frame. *)

val on_output : t -> (string -> unit) -> unit
(** [on_output d f] gives [f] what {!with_output} prints, rather than printing
    it. *)

val when_idle : t -> (unit -> unit) -> unit
(** [when_idle d f] calls [f] at once if [d] is neither drawing nor writing, and
    as soon as it is done otherwise. *)

val watch :
  t -> Format.formatter_out_functions -> Format.formatter_out_functions
(** [watch d out] is [out], which also tells [d] whether what it writes ends its
    line, so that the next line [d] prints without a terminal starts on a line
    of its own. *)
