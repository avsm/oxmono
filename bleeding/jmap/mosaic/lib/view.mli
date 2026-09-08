(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The terminal side of the client.

    The three functions here are the Mosaic half of the application record.
    {!render} draws a {!Model.t} and {!subscriptions} says which terminal events
    the client wants, so that the whole of [Mosaic] is confined to this module
    and to the executable that runs it. *)

val render : Model.t -> Model.msg Mosaic.t
(** [render t] is the view of [t]. It is a title line, the screen [t] names, and
    a status line holding {!Model.field-status} and the keys that screen takes.
*)

val subscriptions : Model.t -> Model.msg Mosaic.Sub.t
(** [subscriptions t] is the events the client wants for [t], which is every key
    event whatever holds the focus. A key the client does not act on is dropped
    rather than dispatched. *)

val key_of_event : Mosaic.Event.key -> Model.key option
(** [key_of_event ev] is the key [ev] stands for, and [None] for a key the
    client does not act on. A control character and a character with the control
    modifier both give {!Model.Ctrl}. *)
