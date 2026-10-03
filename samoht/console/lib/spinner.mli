(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Animated spinners.

    A spinner is a short cyclic sequence of frames -- braille dots, a spinning
    ASCII bar -- shown beside an indeterminate task. The core is pure: {!frame}
    and {!at} pick the glyph to show and the caller draws it. {!Console.Display}
    animates one per running row, and a {!Theme.t} carries the one a display
    uses. *)

type t
(** A spinner: a non-empty, cyclic sequence of frames. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] writes a short representation of [t] to [ppf]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] have the same frames in order. *)

(** {1:spinners Spinners} *)

val v : string array -> t
(** [v frames] is a spinner cycling [frames] left to right.

    {b Raises.} [Invalid_argument] if [frames] is empty. *)

val frames : t -> string array
(** [frames t] are [t]'s frames, in order. *)

val braille : t
(** [braille] cycles the ten braille dots [⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏]. *)

val ascii : t
(** [ascii] cycles the four ASCII frames "|", "/", "-", and backslash, for
    terminals or fonts without braille. *)

(** {1:frames Frames} *)

val frame : t -> int -> string
(** [frame t i] is the [i]th frame. [i] wraps around the sequence, so every
    integer names a frame and [frame t] never raises. *)

val at : ?fps:float -> t -> elapsed:float -> string
(** [at t ~elapsed] is the frame to show [elapsed] seconds into the animation,
    advancing at [fps] frames per second (default [10.]). A non-positive
    [elapsed] shows the first frame. *)

val anim : ?fps:float -> t -> string Anim.t
(** [anim t] is [t] as an {!Anim.t}: its frame at [elapsed] is
    [at ?fps t ~elapsed]. *)
