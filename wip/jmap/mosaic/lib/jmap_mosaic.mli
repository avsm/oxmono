(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A terminal mail client on JMAP.

    The client is written in the Elm Architecture that
    {{:https://github.com/tmattio/mosaic} Mosaic} runs. {!Model} is the state,
    the messages that change it and the JMAP work it wants done, and it mentions
    neither the terminal nor the network. {!View} draws a model and reads the
    keyboard. {!Io} performs one unit of that work against a
    {!Jmap_eio.Client.t}.

    The split is what makes the client testable. {!Model.update} runs under
    Alcotest with scripted messages, and {!Io.perform} runs against a server
    with a scripted dispatch, neither of them needing a terminal. *)

module Model = Model
(** The state of the client, its messages and its actions. *)

module View = View
(** The Mosaic view of a model and the events it wants. *)

module Io = Io
(** The JMAP requests the actions of a model stand for. *)

module Login = Login
(** The adapter between the login model and shared connection profiles. *)
