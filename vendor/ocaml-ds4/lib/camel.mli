(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A camel that says things.

    The camel faces right, towards its own words, which run in a column beside
    it. Use {!say} for text already in hand. A model streams its reply a piece
    at a time, so {!speaker} gathers the pieces into lines and returns each one
    as it completes, which keeps a reply readable while it is still arriving. *)

val art : string list
(** [art] is the camel, one line per element and without trailing newlines. *)

val orange : string
(** [orange] is the colour the camel is drawn in, as a terminal escape. *)

val say : ?width:int -> ?color:bool -> string -> string
(** [say text] is [text] laid out beside the camel, wrapped to [width] columns,
    which defaults to 64. A word longer than the width is cut, having nowhere to
    break. The camel is coloured unless [color] is false, and its words never
    are. *)

type speaker
(** A camel part way through saying something. *)

val speaker : ?color:bool -> unit -> speaker
(** [speaker ()] is a camel that has not yet said anything. It is drawn in
    colour unless [color] is false. *)

val speak : ?width:int -> speaker -> string -> string
(** [speak t chunk] is the lines of [chunk] that are now complete, each beside
    the next line of the camel. A partial line is held until it is finished or
    until {!finish}. *)

val finish : speaker -> string
(** [finish t] is the last partial line, followed by whatever of the camel the
    text was too short to reach, and readies [t] to speak again. It is empty if
    [t] has said nothing. *)
