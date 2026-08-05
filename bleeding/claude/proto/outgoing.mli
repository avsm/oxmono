(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Outgoing messages to the Claude CLI.

    This module provides encoding for all message types that can be sent to the
    Claude CLI. *)

type t =
  | Message of Message.t
  | Control_request of Control.request_envelope
  | Control_response of Control.response_envelope

val jsont : t Jsont.t
(** Codec for outgoing messages. *)

val to_json : t -> Jsont.json
(** [to_json t] converts an outgoing message to JSON. *)

val of_json : Jsont.json -> t
(** [of_json json] parses an outgoing message from JSON.
    @raise Invalid_argument if parsing fails. *)
