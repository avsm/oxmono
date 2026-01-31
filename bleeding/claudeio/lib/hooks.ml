(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "claude.hooks" ~doc:"Claude hooks system"

module Log = (val Logs.src_log src : Logs.LOG)

(** {1 PreToolUse Hook} *)

module PreToolUse = struct
  type input = {
    session_id : string;
    transcript_path : string;
    tool_name : string;
    tool_input : Tool_input.t;
  }

  type decision = Allow | Deny | Ask

  type output = {
    decision : decision option;
    reason : string option;
    updated_input : Tool_input.t option;
  }

  let allow ?reason ?updated_input () =
    { decision = Some Allow; reason; updated_input }

  let deny ?reason () = { decision = Some Deny; reason; updated_input = None }
  let ask ?reason () = { decision = Some Ask; reason; updated_input = None }
  let continue () = { decision = None; reason = None; updated_input = None }

  type callback = input -> output

  let input_of_proto proto =
    {
      session_id = Proto.Hooks.PreToolUse.Input.session_id proto;
      transcript_path = Proto.Hooks.PreToolUse.Input.transcript_path proto;
      tool_name = Proto.Hooks.PreToolUse.Input.tool_name proto;
      tool_input =
        Tool_input.of_json (Proto.Hooks.PreToolUse.Input.tool_input proto);
    }

  let output_to_proto output =
    match output.decision with
    | None -> Proto.Hooks.PreToolUse.Output.continue ()
    | Some Allow ->
        let updated_input =
          Option.map Tool_input.to_json output.updated_input
        in
        Proto.Hooks.PreToolUse.Output.allow ?reason:output.reason ?updated_input
          ()
    | Some Deny -> Proto.Hooks.PreToolUse.Output.deny ?reason:output.reason ()
    | Some Ask -> Proto.Hooks.PreToolUse.Output.ask ?reason:output.reason ()
end

(** {1 PostToolUse Hook} *)

module PostToolUse = struct
  type input = {
    session_id : string;
    transcript_path : string;
    tool_name : string;
    tool_input : Tool_input.t;
    tool_response : Jsont.json;
  }

  type output = {
    block : bool;
    reason : string option;
    additional_context : string option;
  }

  let continue ?additional_context () =
    { block = false; reason = None; additional_context }

  let block ?reason ?additional_context () =
    { block = true; reason; additional_context }

  type callback = input -> output

  let input_of_proto proto =
    {
      session_id = Proto.Hooks.PostToolUse.Input.session_id proto;
      transcript_path = Proto.Hooks.PostToolUse.Input.transcript_path proto;
      tool_name = Proto.Hooks.PostToolUse.Input.tool_name proto;
      tool_input =
        Tool_input.of_json (Proto.Hooks.PostToolUse.Input.tool_input proto);
      tool_response = Proto.Hooks.PostToolUse.Input.tool_response proto;
    }

  let output_to_proto output =
    if output.block then
      Proto.Hooks.PostToolUse.Output.block ?reason:output.reason
        ?additional_context:output.additional_context ()
    else
      Proto.Hooks.PostToolUse.Output.continue
        ?additional_context:output.additional_context ()
end

(** {1 UserPromptSubmit Hook} *)

module UserPromptSubmit = struct
  type input = {
    session_id : string;
    transcript_path : string;
    prompt : string;
  }

  type output = {
    block : bool;
    reason : string option;
    additional_context : string option;
  }

  let continue ?additional_context () =
    { block = false; reason = None; additional_context }

  let block ?reason () = { block = true; reason; additional_context = None }

  type callback = input -> output

  let input_of_proto proto =
    {
      session_id = Proto.Hooks.UserPromptSubmit.Input.session_id proto;
      transcript_path = Proto.Hooks.UserPromptSubmit.Input.transcript_path proto;
      prompt = Proto.Hooks.UserPromptSubmit.Input.prompt proto;
    }

  let output_to_proto output =
    if output.block then
      Proto.Hooks.UserPromptSubmit.Output.block ?reason:output.reason ()
    else
      Proto.Hooks.UserPromptSubmit.Output.continue
        ?additional_context:output.additional_context ()
end

(** {1 Stop Hook} *)

module Stop = struct
  type input = {
    session_id : string;
    transcript_path : string;
    stop_hook_active : bool;
  }

  type output = { block : bool; reason : string option }

  let continue () = { block = false; reason = None }
  let block ?reason () = { block = true; reason }

  type callback = input -> output

  let input_of_proto proto =
    {
      session_id = Proto.Hooks.Stop.Input.session_id proto;
      transcript_path = Proto.Hooks.Stop.Input.transcript_path proto;
      stop_hook_active = Proto.Hooks.Stop.Input.stop_hook_active proto;
    }

  let output_to_proto output =
    if output.block then Proto.Hooks.Stop.Output.block ?reason:output.reason ()
    else Proto.Hooks.Stop.Output.continue ()
end

(** {1 SubagentStop Hook} *)

module SubagentStop = struct
  type input = Stop.input
  type output = Stop.output

  let continue = Stop.continue
  let block = Stop.block

  type callback = input -> output

  let input_of_proto = Stop.input_of_proto

  (* Since Proto.Hooks.SubagentStop.Output.t = Proto.Hooks.Stop.Output.t,
     we can use Stop.output_to_proto directly *)
  let output_to_proto = Stop.output_to_proto
end

(** {1 PreCompact Hook} *)

module PreCompact = struct
  type input = { session_id : string; transcript_path : string }
  type callback = input -> unit

  let input_of_proto proto =
    {
      session_id = Proto.Hooks.PreCompact.Input.session_id proto;
      transcript_path = Proto.Hooks.PreCompact.Input.transcript_path proto;
    }
end

(** {1 Hook Configuration} *)

(* Internal representation of hooks *)
type hook_entry =
  | PreToolUseHook of (string option * PreToolUse.callback)
  | PostToolUseHook of (string option * PostToolUse.callback)
  | UserPromptSubmitHook of UserPromptSubmit.callback
  | StopHook of Stop.callback
  | SubagentStopHook of SubagentStop.callback
  | PreCompactHook of PreCompact.callback

type t = hook_entry list

let empty = []

let on_pre_tool_use ?pattern callback config =
  PreToolUseHook (pattern, callback) :: config

let on_post_tool_use ?pattern callback config =
  PostToolUseHook (pattern, callback) :: config

let on_user_prompt_submit callback config =
  UserPromptSubmitHook callback :: config

let on_stop callback config = StopHook callback :: config
let on_subagent_stop callback config = SubagentStopHook callback :: config
let on_pre_compact callback config = PreCompactHook callback :: config

(** {1 Internal - Conversion to Wire Format} *)

let get_callbacks config =
  (* Group hooks by event type *)
  let pre_tool_use_hooks = ref [] in
  let post_tool_use_hooks = ref [] in
  let user_prompt_submit_hooks = ref [] in
  let stop_hooks = ref [] in
  let subagent_stop_hooks = ref [] in
  let pre_compact_hooks = ref [] in

  List.iter
    (function
      | PreToolUseHook (pattern, callback) ->
          pre_tool_use_hooks := (pattern, callback) :: !pre_tool_use_hooks
      | PostToolUseHook (pattern, callback) ->
          post_tool_use_hooks := (pattern, callback) :: !post_tool_use_hooks
      | UserPromptSubmitHook callback ->
          user_prompt_submit_hooks :=
            (None, callback) :: !user_prompt_submit_hooks
      | StopHook callback -> stop_hooks := (None, callback) :: !stop_hooks
      | SubagentStopHook callback ->
          subagent_stop_hooks := (None, callback) :: !subagent_stop_hooks
      | PreCompactHook callback ->
          pre_compact_hooks := (None, callback) :: !pre_compact_hooks)
    config;

  (* Convert each group to wire format *)
  let result = [] in

  (* PreToolUse *)
  let result =
    if !pre_tool_use_hooks <> [] then
      let wire_callbacks =
        List.map
          (fun (pattern, callback) ->
            let wire_callback json =
              (* Decode JSON to Proto input *)
              let proto_input =
                match
                  Jsont.Json.decode Proto.Hooks.PreToolUse.Input.jsont json
                with
                | Ok input -> input
                | Error msg ->
                    Log.err (fun m ->
                        m "PreToolUse: failed to decode input: %s" msg);
                    raise (Invalid_argument ("PreToolUse input: " ^ msg))
              in
              (* Convert to typed input *)
              let typed_input = PreToolUse.input_of_proto proto_input in
              (* Invoke user callback *)
              let typed_output = callback typed_input in
              (* Convert back to Proto output *)
              let proto_output = PreToolUse.output_to_proto typed_output in
              (* Encode as hook_specific_output *)
              let hook_specific_output =
                match
                  Jsont.Json.encode Proto.Hooks.PreToolUse.Output.jsont
                    proto_output
                with
                | Ok json -> json
                | Error msg -> failwith ("PreToolUse output encoding: " ^ msg)
              in
              (* Return wire format result *)
              Proto.Hooks.continue ~hook_specific_output ()
            in
            (pattern, wire_callback))
          !pre_tool_use_hooks
      in
      (Proto.Hooks.Pre_tool_use, wire_callbacks) :: result
    else result
  in

  (* PostToolUse *)
  let result =
    if !post_tool_use_hooks <> [] then
      let wire_callbacks =
        List.map
          (fun (pattern, callback) ->
            let wire_callback json =
              let proto_input =
                match
                  Jsont.Json.decode Proto.Hooks.PostToolUse.Input.jsont json
                with
                | Ok input -> input
                | Error msg ->
                    Log.err (fun m ->
                        m "PostToolUse: failed to decode input: %s" msg);
                    raise (Invalid_argument ("PostToolUse input: " ^ msg))
              in
              let typed_input = PostToolUse.input_of_proto proto_input in
              let typed_output = callback typed_input in
              let proto_output = PostToolUse.output_to_proto typed_output in
              let hook_specific_output =
                match
                  Jsont.Json.encode Proto.Hooks.PostToolUse.Output.jsont
                    proto_output
                with
                | Ok json -> json
                | Error msg -> failwith ("PostToolUse output encoding: " ^ msg)
              in
              if typed_output.block then
                Proto.Hooks.block ~hook_specific_output ()
              else Proto.Hooks.continue ~hook_specific_output ()
            in
            (pattern, wire_callback))
          !post_tool_use_hooks
      in
      (Proto.Hooks.Post_tool_use, wire_callbacks) :: result
    else result
  in

  (* UserPromptSubmit *)
  let result =
    if !user_prompt_submit_hooks <> [] then
      let wire_callbacks =
        List.map
          (fun (pattern, callback) ->
            let wire_callback json =
              let proto_input =
                match
                  Jsont.Json.decode Proto.Hooks.UserPromptSubmit.Input.jsont
                    json
                with
                | Ok input -> input
                | Error msg ->
                    Log.err (fun m ->
                        m "UserPromptSubmit: failed to decode input: %s" msg);
                    raise (Invalid_argument ("UserPromptSubmit input: " ^ msg))
              in
              let typed_input = UserPromptSubmit.input_of_proto proto_input in
              let typed_output = callback typed_input in
              let proto_output =
                UserPromptSubmit.output_to_proto typed_output
              in
              let hook_specific_output =
                match
                  Jsont.Json.encode Proto.Hooks.UserPromptSubmit.Output.jsont
                    proto_output
                with
                | Ok json -> json
                | Error msg ->
                    failwith ("UserPromptSubmit output encoding: " ^ msg)
              in
              if typed_output.block then
                Proto.Hooks.block ~hook_specific_output ()
              else Proto.Hooks.continue ~hook_specific_output ()
            in
            (pattern, wire_callback))
          !user_prompt_submit_hooks
      in
      (Proto.Hooks.User_prompt_submit, wire_callbacks) :: result
    else result
  in

  (* Stop *)
  let result =
    if !stop_hooks <> [] then
      let wire_callbacks =
        List.map
          (fun (pattern, callback) ->
            let wire_callback json =
              let proto_input =
                match Jsont.Json.decode Proto.Hooks.Stop.Input.jsont json with
                | Ok input -> input
                | Error msg ->
                    Log.err (fun m -> m "Stop: failed to decode input: %s" msg);
                    raise (Invalid_argument ("Stop input: " ^ msg))
              in
              let typed_input = Stop.input_of_proto proto_input in
              let typed_output = callback typed_input in
              let proto_output = Stop.output_to_proto typed_output in
              let hook_specific_output =
                match
                  Jsont.Json.encode Proto.Hooks.Stop.Output.jsont proto_output
                with
                | Ok json -> json
                | Error msg -> failwith ("Stop output encoding: " ^ msg)
              in
              if typed_output.block then
                Proto.Hooks.block ~hook_specific_output ()
              else Proto.Hooks.continue ~hook_specific_output ()
            in
            (pattern, wire_callback))
          !stop_hooks
      in
      (Proto.Hooks.Stop, wire_callbacks) :: result
    else result
  in

  (* SubagentStop *)
  let result =
    if !subagent_stop_hooks <> [] then
      let wire_callbacks =
        List.map
          (fun (pattern, callback) ->
            let wire_callback json =
              let proto_input =
                match
                  Jsont.Json.decode Proto.Hooks.SubagentStop.Input.jsont json
                with
                | Ok input -> input
                | Error msg ->
                    Log.err (fun m ->
                        m "SubagentStop: failed to decode input: %s" msg);
                    raise (Invalid_argument ("SubagentStop input: " ^ msg))
              in
              let typed_input = SubagentStop.input_of_proto proto_input in
              let typed_output = callback typed_input in
              let proto_output = SubagentStop.output_to_proto typed_output in
              let hook_specific_output =
                match
                  Jsont.Json.encode Proto.Hooks.SubagentStop.Output.jsont
                    proto_output
                with
                | Ok json -> json
                | Error msg -> failwith ("SubagentStop output encoding: " ^ msg)
              in
              if typed_output.block then
                Proto.Hooks.block ~hook_specific_output ()
              else Proto.Hooks.continue ~hook_specific_output ()
            in
            (pattern, wire_callback))
          !subagent_stop_hooks
      in
      (Proto.Hooks.Subagent_stop, wire_callbacks) :: result
    else result
  in

  (* PreCompact *)
  let result =
    if !pre_compact_hooks <> [] then
      let wire_callbacks =
        List.map
          (fun (pattern, callback) ->
            let wire_callback json =
              let proto_input =
                match
                  Jsont.Json.decode Proto.Hooks.PreCompact.Input.jsont json
                with
                | Ok input -> input
                | Error msg ->
                    Log.err (fun m ->
                        m "PreCompact: failed to decode input: %s" msg);
                    raise (Invalid_argument ("PreCompact input: " ^ msg))
              in
              let typed_input = PreCompact.input_of_proto proto_input in
              (* Invoke user callback (returns unit) *)
              callback typed_input;
              (* PreCompact has no specific output *)
              Proto.Hooks.continue ()
            in
            (pattern, wire_callback))
          !pre_compact_hooks
      in
      (Proto.Hooks.Pre_compact, wire_callbacks) :: result
    else result
  in

  List.rev result
