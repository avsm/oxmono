(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "claude.content_block" ~doc:"Claude content blocks"

module Log = (val Logs.src_log src : Logs.LOG)

module Text = struct
  type t = Proto.Content_block.Text.t

  let text = Proto.Content_block.Text.text
  let of_proto proto = proto
  let to_proto t = t
end

module Tool_use = struct
  type t = Proto.Content_block.Tool_use.t

  let id = Proto.Content_block.Tool_use.id
  let name = Proto.Content_block.Tool_use.name
  let input t = Proto.Content_block.Tool_use.input t |> Tool_input.of_json
  let of_proto proto = proto
  let to_proto t = t
end

module Tool_result = struct
  type t = Proto.Content_block.Tool_result.t

  let tool_use_id = Proto.Content_block.Tool_result.tool_use_id
  let content = Proto.Content_block.Tool_result.content
  let is_error = Proto.Content_block.Tool_result.is_error
  let of_proto proto = proto
  let to_proto t = t
end

module Thinking = struct
  type t = Proto.Content_block.Thinking.t

  let thinking = Proto.Content_block.Thinking.thinking
  let signature = Proto.Content_block.Thinking.signature
  let of_proto proto = proto
  let to_proto t = t
end

type t =
  | Text of Text.t
  | Tool_use of Tool_use.t
  | Tool_result of Tool_result.t
  | Thinking of Thinking.t

let text s =
  let proto = Proto.Content_block.text s in
  match proto with
  | Proto.Content_block.Text proto_text -> Text (Text.of_proto proto_text)
  | _ -> failwith "Internal error: Proto.Content_block.text returned non-Text"

let tool_use ~id ~name ~input =
  let json_input = Tool_input.to_json input in
  let proto = Proto.Content_block.tool_use ~id ~name ~input:json_input in
  match proto with
  | Proto.Content_block.Tool_use proto_tool_use ->
      Tool_use (Tool_use.of_proto proto_tool_use)
  | _ ->
      failwith
        "Internal error: Proto.Content_block.tool_use returned non-Tool_use"

let tool_result ~tool_use_id ?content ?is_error () =
  let proto =
    Proto.Content_block.tool_result ~tool_use_id ?content ?is_error ()
  in
  match proto with
  | Proto.Content_block.Tool_result proto_tool_result ->
      Tool_result (Tool_result.of_proto proto_tool_result)
  | _ ->
      failwith
        "Internal error: Proto.Content_block.tool_result returned \
         non-Tool_result"

let thinking ~thinking ~signature =
  let proto = Proto.Content_block.thinking ~thinking ~signature in
  match proto with
  | Proto.Content_block.Thinking proto_thinking ->
      Thinking (Thinking.of_proto proto_thinking)
  | _ ->
      failwith
        "Internal error: Proto.Content_block.thinking returned non-Thinking"

let of_proto proto =
  match proto with
  | Proto.Content_block.Text t -> Text (Text.of_proto t)
  | Proto.Content_block.Tool_use t -> Tool_use (Tool_use.of_proto t)
  | Proto.Content_block.Tool_result t -> Tool_result (Tool_result.of_proto t)
  | Proto.Content_block.Thinking t -> Thinking (Thinking.of_proto t)

let to_proto = function
  | Text t -> Proto.Content_block.Text (Text.to_proto t)
  | Tool_use t -> Proto.Content_block.Tool_use (Tool_use.to_proto t)
  | Tool_result t -> Proto.Content_block.Tool_result (Tool_result.to_proto t)
  | Thinking t -> Proto.Content_block.Thinking (Thinking.to_proto t)

let log_received t =
  let proto = to_proto t in
  Log.debug (fun m ->
      m "Received content block: %a"
        (Jsont.pp_value Proto.Content_block.jsont ())
        proto)

let log_sending t =
  let proto = to_proto t in
  Log.debug (fun m ->
      m "Sending content block: %a"
        (Jsont.pp_value Proto.Content_block.jsont ())
        proto)
