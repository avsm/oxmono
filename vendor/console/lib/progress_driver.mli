(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Showing tasks for a scheduler.

    This module is internal to console: [Console] does not expose it, and
    [console-eio] reads it through [Console__]. *)

val show :
  write_error:(exn -> bool) ->
  ?progress:Progress.t ->
  ?layout:Progress.Layout.t ->
  ?view:View.t ->
  ?keep_ended:bool ->
  ?commit:(Task.t -> View.t option) ->
  ?plain:(Task.Event.t -> View.t option) ->
  ?delay:float ->
  ?size:(unit -> int * int) ->
  Format.formatter ->
  Task.t ->
  Display.t
(** [show ~write_error ppf root] is {!Progress.show}[ ppf root] whose display
    takes as the end of its output an exception that [write_error] says is a
    write error, such as a closed pipe. *)
