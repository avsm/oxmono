(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Outgoing messages to Claude CLI.

    This uses the Message.jsont for conversation messages and Control envelope
    codecs for control messages. The top-level discriminator is the "type"
    field. *)

type t =
  | Message of Message.t
  | Control_request of Control.request_envelope
  | Control_response of Control.response_envelope

let jsont : t Jsont.t =
  (* Message types use "user", "assistant", "system", "result" as type values.
     Control uses "control_request" and "control_response".

     We use case_mem for all types. For Message, we use Message.jsont which
     already handles the inner "type" discrimination. *)
  let case_control_request =
    Jsont.Object.Case.map "control_request" Control.request_envelope_jsont
      ~dec:(fun v -> Control_request v)
  in
  let case_control_response =
    Jsont.Object.Case.map "control_response" Control.response_envelope_jsont
      ~dec:(fun v -> Control_response v)
  in
  (* For messages, we need to handle all four message types *)
  let case_user =
    Jsont.Object.Case.map "user" Message.User.outgoing_jsont ~dec:(fun v ->
        Message (Message.User v))
  in
  let case_assistant =
    Jsont.Object.Case.map "assistant" Message.Assistant.jsont ~dec:(fun v ->
        Message (Message.Assistant v))
  in
  let case_system =
    Jsont.Object.Case.map "system" Message.System.jsont ~dec:(fun v ->
        Message (Message.System v))
  in
  let case_result =
    Jsont.Object.Case.map "result" Message.Result.jsont ~dec:(fun v ->
        Message (Message.Result v))
  in
  let enc_case = function
    | Control_request v -> Jsont.Object.Case.value case_control_request v
    | Control_response v -> Jsont.Object.Case.value case_control_response v
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
      ]
  in
  Jsont.Object.map ~kind:"Outgoing" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
       ~tag_to_string:Fun.id ~tag_compare:String.compare
  |> Jsont.Object.finish

let to_json t =
  match Jsont.Json.encode jsont t with
  | Ok json -> json
  | Error e -> invalid_arg ("to_json: " ^ e)

let of_json json =
  match Jsont.Json.decode jsont json with
  | Ok v -> v
  | Error e -> invalid_arg ("of_json: " ^ e)
