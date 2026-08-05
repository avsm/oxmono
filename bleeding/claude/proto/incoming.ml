(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Incoming messages from Claude CLI.

    This uses the Control module's request_envelope_jsont and
    response_envelope_jsont for control messages, and Message.jsont for
    conversation messages. The top-level discriminator is the "type" field. *)

(** Rate limit status. *)
type rate_limit_status = [ `Allowed | `Allowed_warning | `Rejected ]

let rate_limit_status_jsont : rate_limit_status Jsont.t =
  Jsont.enum ~kind:"RateLimitStatus"
    [ ("allowed", `Allowed);
      ("allowed_warning", `Allowed_warning);
      ("rejected", `Rejected) ]

(** Rate limit type (which window). *)
type rate_limit_type =
  [ `Five_hour | `Seven_day | `Seven_day_opus | `Seven_day_sonnet | `Overage ]

let rate_limit_type_jsont : rate_limit_type Jsont.t =
  Jsont.enum ~kind:"RateLimitType"
    [ ("five_hour", `Five_hour);
      ("seven_day", `Seven_day);
      ("seven_day_opus", `Seven_day_opus);
      ("seven_day_sonnet", `Seven_day_sonnet);
      ("overage", `Overage) ]

(** Rate limit info payload. *)
type rate_limit_info = {
  status : rate_limit_status;
  resets_at : int option;
  rate_limit_type : rate_limit_type option;
  utilization : float option;
  overage_status : rate_limit_status option;
  overage_resets_at : int option;
  overage_disabled_reason : string option;
}

let rate_limit_info_jsont : rate_limit_info Jsont.t =
  Jsont.Object.map ~kind:"RateLimitInfo"
    (fun status resets_at rate_limit_type utilization
         overage_status overage_resets_at overage_disabled_reason _unknown ->
      { status; resets_at; rate_limit_type; utilization;
        overage_status; overage_resets_at; overage_disabled_reason })
  |> Jsont.Object.mem "status" rate_limit_status_jsont
       ~enc:(fun r -> r.status)
  |> Jsont.Object.opt_mem "resetsAt" Jsont.int
       ~enc:(fun r -> r.resets_at)
  |> Jsont.Object.opt_mem "rateLimitType" rate_limit_type_jsont
       ~enc:(fun r -> r.rate_limit_type)
  |> Jsont.Object.opt_mem "utilization" Jsont.number
       ~enc:(fun r -> r.utilization)
  |> Jsont.Object.opt_mem "overageStatus" rate_limit_status_jsont
       ~enc:(fun r -> r.overage_status)
  |> Jsont.Object.opt_mem "overageResetsAt" Jsont.int
       ~enc:(fun r -> r.overage_resets_at)
  |> Jsont.Object.opt_mem "overageDisabledReason" Jsont.string
       ~enc:(fun r -> r.overage_disabled_reason)
  |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun _ -> Unknown.empty)
  |> Jsont.Object.finish

(** Rate limit event. *)
type rate_limit_event = {
  rate_limit_info : rate_limit_info;
  uuid : string;
  session_id : string;
}

let rate_limit_event_jsont : rate_limit_event Jsont.t =
  Jsont.Object.map ~kind:"RateLimitEvent"
    (fun rate_limit_info uuid session_id _unknown ->
      { rate_limit_info; uuid; session_id })
  |> Jsont.Object.mem "rate_limit_info" rate_limit_info_jsont
       ~enc:(fun r -> r.rate_limit_info)
  |> Jsont.Object.mem "uuid" Jsont.string
       ~enc:(fun r -> r.uuid)
  |> Jsont.Object.mem "session_id" Jsont.string
       ~enc:(fun r -> r.session_id)
  |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun _ -> Unknown.empty)
  |> Jsont.Object.finish

(** Stream event for partial message updates during streaming. *)
type stream_event = {
  uuid : string;
  session_id : string;
  event : Jsont.json;
  parent_tool_use_id : string option;
}

let stream_event_jsont : stream_event Jsont.t =
  Jsont.Object.map ~kind:"StreamEvent"
    (fun uuid session_id event parent_tool_use_id _unknown ->
      { uuid; session_id; event; parent_tool_use_id })
  |> Jsont.Object.mem "uuid" Jsont.string
       ~enc:(fun r -> r.uuid)
  |> Jsont.Object.mem "session_id" Jsont.string
       ~enc:(fun r -> r.session_id)
  |> Jsont.Object.mem "event" Jsont.json
       ~enc:(fun r -> r.event)
  |> Jsont.Object.opt_mem "parent_tool_use_id" Jsont.string
       ~enc:(fun r -> r.parent_tool_use_id)
  |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun _ -> Unknown.empty)
  |> Jsont.Object.finish

type t =
  | Message of Message.t
  | Control_response of Control.response_envelope
  | Control_request of Control.request_envelope
  | Rate_limit_event of rate_limit_event
  | Stream_event of stream_event

let jsont : t Jsont.t =
  let case_control_request =
    Jsont.Object.Case.map "control_request" Control.request_envelope_jsont
      ~dec:(fun v -> Control_request v)
  in
  let case_control_response =
    Jsont.Object.Case.map "control_response" Control.response_envelope_jsont
      ~dec:(fun v -> Control_response v)
  in
  let case_user =
    Jsont.Object.Case.map "user" Message.User.incoming_jsont ~dec:(fun v ->
        Message (Message.User v))
  in
  let case_assistant =
    Jsont.Object.Case.map "assistant" Message.Assistant.incoming_jsont
      ~dec:(fun v -> Message (Message.Assistant v))
  in
  let case_system =
    Jsont.Object.Case.map "system" Message.System.jsont ~dec:(fun v ->
        Message (Message.System v))
  in
  let case_result =
    Jsont.Object.Case.map "result" Message.Result.jsont ~dec:(fun v ->
        Message (Message.Result v))
  in
  let case_rate_limit_event =
    Jsont.Object.Case.map "rate_limit_event" rate_limit_event_jsont
      ~dec:(fun v -> Rate_limit_event v)
  in
  let case_stream_event =
    Jsont.Object.Case.map "stream_event" stream_event_jsont
      ~dec:(fun v -> Stream_event v)
  in
  let enc_case = function
    | Control_request v -> Jsont.Object.Case.value case_control_request v
    | Control_response v -> Jsont.Object.Case.value case_control_response v
    | Rate_limit_event v -> Jsont.Object.Case.value case_rate_limit_event v
    | Stream_event v -> Jsont.Object.Case.value case_stream_event v
    | Message msg -> (
        match msg with
        | Message.User u -> Jsont.Object.Case.value case_user u
        | Message.Assistant a -> Jsont.Object.Case.value case_assistant a
        | Message.System s -> Jsont.Object.Case.value case_system s
        | Message.Result r -> Jsont.Object.Case.value case_result r)
  in
  let cases =
    Jsont.Object.Case.
      [
        make case_control_request;
        make case_control_response;
        make case_user;
        make case_assistant;
        make case_system;
        make case_result;
        make case_rate_limit_event;
        make case_stream_event;
      ]
  in
  Jsont.Object.map ~kind:"Incoming" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
       ~tag_to_string:Fun.id ~tag_compare:String.compare
  |> Jsont.Object.finish
