(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Views laid out as a display lays them out, for the tests of console's
    widgets. *)

open Console

val at :
  ?theme:Theme.t ->
  ?utf_8:bool ->
  ?live:bool ->
  ?width:int ->
  time:float ->
  View.t ->
  string
(** [at ~time v] is the text of [v] laid out as a display draws it [time]
    seconds in, in [theme] (defaults to {!Theme.default}), in UTF-8 iff [utf_8]
    (defaults to [true]), live iff [live] (defaults to [true]), in a room
    [width] wide if given. *)

val laid : ?live:bool -> ?tasks:bool -> unit -> View.t * int ref
(** [laid ()] is a view as wide as its room, three cells at its natural size,
    which reads state outside the view iff [live] (defaults to [false]), the
    tasks iff [tasks] (defaults to [false]), and the count of the times it was
    laid out. *)
