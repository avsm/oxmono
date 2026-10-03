(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Time-varying values.

    An animation is a value sampled at an elapsed time: [frame a ~elapsed] is
    what [a] looks like [elapsed] seconds in. It is the common shape every
    animatable component shares -- {!Spinner.anim}, {!Bar.anim}, {!Panel.anim}
    -- so a renderer (the live {!Display}, or [Console_eio]) can drive any of
    them by sampling at the current time each tick. A still value is {!const};
    {!all} samples several at once so a {!Layout} can join them into one frame.
*)

type 'a t
(** A value that varies with elapsed time. *)

val v : (elapsed:float -> 'a) -> 'a t
(** [v f] is the animation whose frame at [elapsed] is [f ~elapsed]. *)

val const : 'a -> 'a t
(** [const x] is the still animation: every frame is [x]. *)

val frame : 'a t -> elapsed:float -> 'a
(** [frame a ~elapsed] is [a] sampled [elapsed] seconds in. A non-positive
    [elapsed] is the first frame. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f a] applies [f] to every frame of [a]. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f a b] samples [a] and [b] at the same elapsed time and combines their
    frames with [f]. *)

val all : 'a t list -> 'a list t
(** [all xs] samples every animation in [xs] at the same elapsed time:
    [frame (all xs) ~elapsed] is [List.map (fun a -> frame a ~elapsed) xs]. It
    is how a {!Layout} combines several animated blocks into one frame. *)
