(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Text = struct
  type t = Content_block.Text.t

  let content = Content_block.Text.text
  let of_block block = block
end

module Tool_use = struct
  type t = Content_block.Tool_use.t

  let id = Content_block.Tool_use.id
  let name = Content_block.Tool_use.name
  let input = Content_block.Tool_use.input
  let of_block block = block
end

module Thinking = struct
  type t = Content_block.Thinking.t

  let content = Content_block.Thinking.thinking
  let signature = Content_block.Thinking.signature
  let of_block block = block
end

module Init = struct
  type t = Message.System.t

  let session_id = Message.System.session_id
  let model = Message.System.model
  let cwd = Message.System.cwd
  let of_system sys = if Message.System.is_init sys then Some sys else None
end

module Error = struct
  type t =
    | System_error of Message.System.t
    | Assistant_error of Message.Assistant.t * Message.Assistant.error

  let message = function
    | System_error sys ->
        Option.value (Message.System.error_message sys) ~default:"Unknown error"
    | Assistant_error (_, err) -> (
        match err with
        | `Authentication_failed -> "Authentication failed"
        | `Billing_error -> "Billing error"
        | `Rate_limit -> "Rate limit exceeded"
        | `Invalid_request -> "Invalid request"
        | `Server_error -> "Server error"
        | `Unknown -> "Unknown error")

  let is_system_error = function System_error _ -> true | _ -> false
  let is_assistant_error = function Assistant_error _ -> true | _ -> false

  let of_system sys =
    if Message.System.is_error sys then Some (System_error sys) else None

  let of_assistant msg =
    match Message.Assistant.error msg with
    | Some err -> Some (Assistant_error (msg, err))
    | None -> None
end

module Complete = struct
  type t = Message.Result.t

  let duration_ms = Message.Result.duration_ms
  let num_turns = Message.Result.num_turns
  let session_id = Message.Result.session_id
  let total_cost_usd = Message.Result.total_cost_usd
  let usage = Message.Result.usage
  let result_text = Message.Result.result_text
  let structured_output = Message.Result.structured_output
  let of_result result = result
end

type t =
  | Text of Text.t
  | Tool_use of Tool_use.t
  | Tool_result of Content_block.Tool_result.t
  | Thinking of Thinking.t
  | Init of Init.t
  | Error of Error.t
  | Complete of Complete.t

let of_message = function
  | Message.User _ ->
      (* User messages are inputs, not responses *)
      []
  | Message.Assistant msg -> (
      (* Check for assistant error first *)
      match Error.of_assistant msg with
      | Some err -> [ Error err ]
      | None ->
          (* Convert content blocks to response events *)
          Message.Assistant.content msg
          |> List.map (function
            | Content_block.Text text -> Text (Text.of_block text)
            | Content_block.Tool_use tool -> Tool_use (Tool_use.of_block tool)
            | Content_block.Tool_result result -> Tool_result result
            | Content_block.Thinking thinking ->
                Thinking (Thinking.of_block thinking)))
  | Message.System sys -> (
      (* System messages can be Init or Error *)
      match Init.of_system sys with
      | Some init -> [ Init init ]
      | None -> (
          match Error.of_system sys with
          | Some err -> [ Error err ]
          | None -> []))
  | Message.Result result ->
      (* Result messages become Complete events *)
      [ Complete (Complete.of_result result) ]
