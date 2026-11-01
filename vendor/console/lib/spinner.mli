(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Spinners, for work of unknown length.

    A spinner is a short cycle of frames, such as braille dots or a turning
    ASCII bar. Its {!view} goes in any view: a {!Display} turns it at each
    frame, while printed once it shows its first frame. Each running task's row
    has the spinner of its theme ({!Theme.val-spinner}).

    A spinner of one frame does not move. A program offers that to users who
    prefer no animation with {!Theme.still}, which also stops the frames of
    tables and panels. *)

val view : ?frames:string list -> unit -> View.t
(** [view ()] shows [frames] in turn, from the first to the last and then from
    the first again, ten frames a second of the time it is drawn at
    ({!View.fit}): the first frame at time [0.]. [frames] defaults to the
    spinner of the theme it is printed in. A frame is drawn on one line, a
    control character in it a space, and on a formatter that is not {!Fmt.utf_8}
    a spinner whose frames are not all ASCII is drawn as {!ascii}. Every frame
    takes the cells of the widest, so the view does not change size as it turns,
    and a room narrower than that cuts every frame to the room.

    @raise Invalid_argument if [frames] is empty. *)

val braille : string list
(** [braille] are the ten braille patterns U+280B, U+2819, U+2839, U+2838,
    U+283C, U+2834, U+2826, U+2827, U+2807 and U+280F. *)

val ascii : string list
(** [ascii] are the four frames [|], [/], [-] and a backslash, for terminals or
    fonts without braille. *)
