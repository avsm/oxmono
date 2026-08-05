(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src =
  Logs.Src.create "claude.incoming" ~doc:"Incoming messages from Claude CLI"

module Log = (val Logs.src_log src : Logs.LOG)

(** Incoming messages from Claude CLI.

    This uses the Sdk_control module's control_request_jsont and
    control_response_jsont for control messages, and Message.jsont for
    conversation messages. The top-level discriminator is the "type" field. *)

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

let jsont : t Jsont.t =
  let case_control_request =
    Jsont.Object.Case.map "control_request" Sdk_control.control_request_jsont
      ~dec:(fun v -> Control_request v)
  in
  let case_control_response =
    Jsont.Object.Case.map "control_response" Sdk_control.control_response_jsont
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
    Jsont.Object.Case.map "rate_limit_event" Proto.Incoming.rate_limit_event_jsont
      ~dec:(fun v -> Rate_limit_event v)
  in
  let case_stream_event =
    Jsont.Object.Case.map "stream_event" Proto.Incoming.stream_event_jsont
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
