(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Incoming messages from the Claude CLI.

    The codec uses the "type" field to discriminate between message types:
    - "user", "assistant", "system", "result" -> Message variant
    - "control_response" -> Control_response variant
    - "control_request" -> Control_request variant
    - "rate_limit_event" -> Rate_limit_event variant
    - "stream_event" -> Stream_event variant *)

type rate_limit_status = Proto.Incoming.rate_limit_status
type rate_limit_type = Proto.Incoming.rate_limit_type
type rate_limit_info = Proto.Incoming.rate_limit_info
type rate_limit_event = Proto.Incoming.rate_limit_event
type stream_event = Proto.Incoming.stream_event

type t =
  | Message of Message.t
  | Control_response of Sdk_control.control_response
  | Control_request of Sdk_control.control_request
  | Rate_limit_event of rate_limit_event
  | Stream_event of stream_event

val jsont : t Jsont.t
