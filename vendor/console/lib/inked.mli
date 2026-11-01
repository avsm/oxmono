(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Values drawn in a style.

    The representation shared by {!Border.t} and {!Guide.t}. *)

type 'a t
(** The type for values of type ['a] drawn in a {!Style.t}. *)

val v : ?style:Style.t -> 'a -> 'a t
(** [v ~style x] is [x] drawn in [style] (defaults to {!Style.none}). *)

val get : 'a t -> 'a
(** [get t] is the value of [t]. *)

val style : 'a t -> Style.t
(** [style t] is the style of [t]. *)

val with_style : Style.t -> 'a t -> 'a t
(** [with_style s t] is [t] drawn in [s]. *)
