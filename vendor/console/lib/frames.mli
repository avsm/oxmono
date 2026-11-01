(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The frames of a spinner, which {!Theme} and {!Spinner} share. *)

val braille : string list
(** See {!Spinner.braille}. *)

val ascii : string list
(** See {!Spinner.ascii}. *)

val still : string list
(** [still] is the one frame of a spinner that does not move. *)

val v : string -> string list -> string list
(** [v fn frames] is [frames], each on one line: a control character in it is a
    space.

    @raise Invalid_argument naming [fn] if [frames] is empty. *)

val at : string list -> time:float -> string
(** [at frames ~time] is the frame shown [time] seconds in, ten frames a second:
    the first one for a [time] that is not positive or not finite. *)
