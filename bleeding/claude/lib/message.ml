(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "claude.message" ~doc:"Claude messages"

module Log = (val Logs.src_log src : Logs.LOG)

module User = struct
  type t = Proto.Message.User.t

  let of_string s = Proto.Message.User.create_string s

  let of_blocks blocks =
    Proto.Message.User.create_blocks (List.map Content_block.to_proto blocks)

  let with_tool_result ~tool_use_id ~content ?is_error () =
    Proto.Message.User.create_with_tool_result ~tool_use_id ~content ?is_error
      ()

  let as_text t =
    match Proto.Message.User.content t with
    | Proto.Message.User.String s -> Some s
    | Proto.Message.User.Blocks _ -> None

  let blocks t =
    match Proto.Message.User.content t with
    | Proto.Message.User.String s -> [ Content_block.text s ]
    | Proto.Message.User.Blocks bs -> List.map Content_block.of_proto bs

  let of_proto proto = proto
  let to_proto t = t

  (* Internal wire format functions *)
  let incoming_jsont = Proto.Message.User.incoming_jsont

  let to_json t =
    match Jsont.Json.encode Proto.Message.User.jsont t with
    | Ok json -> json
    | Error e -> invalid_arg ("User.to_json: " ^ e)
end

module Assistant = struct
  type error = Proto.Message.Assistant.error
  type t = Proto.Message.Assistant.t

  let content t =
    List.map Content_block.of_proto (Proto.Message.Assistant.content t)

  let model t = Proto.Message.Assistant.model t
  let error t = Proto.Message.Assistant.error t

  let text_blocks t =
    List.filter_map
      (function
        | Content_block.Text text -> Some (Content_block.Text.text text)
        | _ -> None)
      (content t)

  let tool_uses t =
    List.filter_map
      (function Content_block.Tool_use tool -> Some tool | _ -> None)
      (content t)

  let thinking_blocks t =
    List.filter_map
      (function Content_block.Thinking thinking -> Some thinking | _ -> None)
      (content t)

  let has_tool_use t =
    List.exists
      (function Content_block.Tool_use _ -> true | _ -> false)
      (content t)

  let combined_text t = String.concat "\n" (text_blocks t)
  let of_proto proto = proto
  let to_proto t = t

  (* Internal wire format functions *)
  let incoming_jsont = Proto.Message.Assistant.incoming_jsont

  let to_json t =
    match Jsont.Json.encode Proto.Message.Assistant.jsont t with
    | Ok json -> json
    | Error e -> invalid_arg ("Assistant.to_json: " ^ e)
end

module System = struct
  type t = Proto.Message.System.t

  let is_init = function Proto.Message.System.Init _ -> true | _ -> false
  let is_error = function Proto.Message.System.Error _ -> true | _ -> false
  let session_id = Proto.Message.System.session_id
  let model = Proto.Message.System.model
  let cwd = Proto.Message.System.cwd
  let error_message = Proto.Message.System.error_msg
  let of_proto proto = proto
  let to_proto t = t

  (* Internal wire format functions *)
  let jsont = Proto.Message.System.jsont

  let to_json t =
    match Jsont.Json.encode Proto.Message.System.jsont t with
    | Ok json -> json
    | Error e -> invalid_arg ("System.to_json: " ^ e)
end

module Result = struct
  module Usage = struct
    type t = Proto.Message.Result.Usage.t

    let input_tokens = Proto.Message.Result.Usage.input_tokens
    let output_tokens = Proto.Message.Result.Usage.output_tokens
    let total_tokens = Proto.Message.Result.Usage.total_tokens

    let cache_creation_input_tokens =
      Proto.Message.Result.Usage.cache_creation_input_tokens

    let cache_read_input_tokens =
      Proto.Message.Result.Usage.cache_read_input_tokens

    let of_proto proto = proto
  end

  type t = Proto.Message.Result.t

  let duration_ms = Proto.Message.Result.duration_ms
  let duration_api_ms = Proto.Message.Result.duration_api_ms
  let is_error = Proto.Message.Result.is_error
  let num_turns = Proto.Message.Result.num_turns
  let session_id = Proto.Message.Result.session_id
  let total_cost_usd = Proto.Message.Result.total_cost_usd
  let usage t = Option.map Usage.of_proto (Proto.Message.Result.usage t)
  let result_text = Proto.Message.Result.result
  let structured_output = Proto.Message.Result.structured_output
  let of_proto proto = proto
  let to_proto t = t

  (* Internal wire format functions *)
  let jsont = Proto.Message.Result.jsont

  let to_json t =
    match Jsont.Json.encode Proto.Message.Result.jsont t with
    | Ok json -> json
    | Error e -> invalid_arg ("Result.to_json: " ^ e)
end

type t =
  | User of User.t
  | Assistant of Assistant.t
  | System of System.t
  | Result of Result.t

let of_proto = function
  | Proto.Message.User u -> User (User.of_proto u)
  | Proto.Message.Assistant a -> Assistant (Assistant.of_proto a)
  | Proto.Message.System s -> System (System.of_proto s)
  | Proto.Message.Result r -> Result (Result.of_proto r)

let to_proto = function
  | User u -> Proto.Message.User (User.to_proto u)
  | Assistant a -> Proto.Message.Assistant (Assistant.to_proto a)
  | System s -> Proto.Message.System (System.to_proto s)
  | Result r -> Proto.Message.Result (Result.to_proto r)

let is_user = function User _ -> true | _ -> false
let is_assistant = function Assistant _ -> true | _ -> false
let is_system = function System _ -> true | _ -> false
let is_result = function Result _ -> true | _ -> false

let is_error = function
  | Result r -> Result.is_error r
  | System s -> System.is_error s
  | _ -> false

let extract_text = function
  | User u -> User.as_text u
  | Assistant a ->
      let text = Assistant.combined_text a in
      if text = "" then None else Some text
  | _ -> None

let extract_tool_uses = function
  | Assistant a -> Assistant.tool_uses a
  | _ -> []

let get_session_id = function
  | System s -> System.session_id s
  | Result r -> Some (Result.session_id r)
  | _ -> None

(* Wire format conversion *)
let to_json = function
  | User u -> User.to_json u
  | Assistant a -> Assistant.to_json a
  | System s -> System.to_json s
  | Result r -> Result.to_json r

(* Convenience constructors *)
let user_string s = User (User.of_string s)
let user_blocks blocks = User (User.of_blocks blocks)
let pp fmt t = Jsont.pp_value Proto.Message.jsont () fmt (to_proto t)
let log_received t = Log.info (fun m -> m "← %a" pp t)
let log_sending t = Log.info (fun m -> m "→ %a" pp t)
