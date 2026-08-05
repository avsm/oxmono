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
    - "rate_limit_event" -> Rate_limit_event variant
    - "stream_event" -> Stream_event variant *)

(** {1 Rate Limit Types} *)

type rate_limit_status = [ `Allowed | `Allowed_warning | `Rejected ]
type rate_limit_type =
  [ `Five_hour | `Seven_day | `Seven_day_opus | `Seven_day_sonnet | `Overage ]

type rate_limit_info = {
  status : rate_limit_status;
  resets_at : int option;
  rate_limit_type : rate_limit_type option;
  utilization : float option;
  overage_status : rate_limit_status option;
  overage_resets_at : int option;
  overage_disabled_reason : string option;
}

type rate_limit_event = {
  rate_limit_info : rate_limit_info;
  uuid : string;
  session_id : string;
}

(** {1 Stream Event} *)

type stream_event = {
  uuid : string;
  session_id : string;
  event : Jsont.json;
  parent_tool_use_id : string option;
}

(** {1 Incoming Message} *)

type t =
  | Message of Message.t
  | Control_response of Control.response_envelope
  | Control_request of Control.request_envelope
  | Rate_limit_event of rate_limit_event
  | Stream_event of stream_event

val jsont : t Jsont.t
(** Codec for incoming messages. Uses the "type" field to discriminate. *)

val rate_limit_status_jsont : rate_limit_status Jsont.t
val rate_limit_type_jsont : rate_limit_type Jsont.t
val rate_limit_info_jsont : rate_limit_info Jsont.t
val rate_limit_event_jsont : rate_limit_event Jsont.t
val stream_event_jsont : stream_event Jsont.t
