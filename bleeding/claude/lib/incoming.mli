(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Incoming messages from the Claude CLI.

    This module defines a discriminated union of all possible message types that
    can be received from the Claude CLI, with a single jsont codec.

    The codec uses the "type" field to discriminate between message types:
    - "user", "assistant", "system", "result" -> Message variant
    - "control_response" -> Control_response variant
    - "control_request" -> Control_request variant

    This provides a clean, type-safe way to decode incoming messages in a single
    operation. *)

type t =
  | Message of Message.t
  | Control_response of Sdk_control.control_response
  | Control_request of Sdk_control.control_request

val jsont : t Jsont.t
(** Codec for incoming messages. Uses the "type" field to discriminate. Use
    [Jsont.pp_value jsont ()] for pretty-printing. *)
