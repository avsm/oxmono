(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Claude Code Hooks System - Wire Format

    This module defines the wire format for hook configuration. *)

(** Hook events that can be intercepted *)
type event =
  | Pre_tool_use
  | Post_tool_use
  | User_prompt_submit
  | Stop
  | Subagent_stop
  | Pre_compact

let event_to_string = function
  | Pre_tool_use -> "PreToolUse"
  | Post_tool_use -> "PostToolUse"
  | User_prompt_submit -> "UserPromptSubmit"
  | Stop -> "Stop"
  | Subagent_stop -> "SubagentStop"
  | Pre_compact -> "PreCompact"

let event_of_string = function
  | "PreToolUse" -> Pre_tool_use
  | "PostToolUse" -> Post_tool_use
  | "UserPromptSubmit" -> User_prompt_submit
  | "Stop" -> Stop
  | "SubagentStop" -> Subagent_stop
  | "PreCompact" -> Pre_compact
  | s -> raise (Invalid_argument (Printf.sprintf "Unknown hook event: %s" s))

let event_jsont : event Jsont.t =
  Jsont.enum
    [
      ("PreToolUse", Pre_tool_use);
      ("PostToolUse", Post_tool_use);
      ("UserPromptSubmit", User_prompt_submit);
      ("Stop", Stop);
      ("SubagentStop", Subagent_stop);
      ("PreCompact", Pre_compact);
    ]

(** Context provided to hook callbacks *)
module Context = struct
  type t = { signal : unit option; unknown : Unknown.t }

  let create ?signal () =
    let signal = Option.map (fun () -> ()) signal in
    { signal; unknown = Unknown.empty }

  let signal t = t.signal
  let unknown t = t.unknown

  let jsont : t Jsont.t =
    let make unknown = { signal = None; unknown } in
    Jsont.Object.map ~kind:"Context" make
    |> Jsont.Object.keep_unknown Unknown.mems ~enc:unknown
    |> Jsont.Object.finish
end

(** Hook decision control *)
type decision = Continue | Block

let decision_jsont : decision Jsont.t =
  Jsont.enum [ ("continue", Continue); ("block", Block) ]

type result = {
  decision : decision option;
  system_message : string option;
  hook_specific_output : Jsont.json option;
  unknown : Unknown.t;
}
(** Generic hook result *)

let result_jsont : result Jsont.t =
  let make decision system_message hook_specific_output unknown =
    { decision; system_message; hook_specific_output; unknown }
  in
  Jsont.Object.map ~kind:"Result" make
  |> Jsont.Object.opt_mem "decision" decision_jsont ~enc:(fun r -> r.decision)
  |> Jsont.Object.opt_mem "systemMessage" Jsont.string ~enc:(fun r ->
      r.system_message)
  |> Jsont.Object.opt_mem "hookSpecificOutput" Jsont.json ~enc:(fun r ->
      r.hook_specific_output)
  |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun r -> r.unknown)
  |> Jsont.Object.finish

(** {1 PreToolUse Hook} *)
module PreToolUse = struct
  module Input = struct
    type t = {
      session_id : string;
      transcript_path : string;
      tool_name : string;
      tool_input : Jsont.json;
      unknown : Unknown.t;
    }

    let session_id t = t.session_id
    let transcript_path t = t.transcript_path
    let tool_name t = t.tool_name
    let tool_input t = t.tool_input
    let unknown t = t.unknown

    let jsont : t Jsont.t =
      let make session_id transcript_path tool_name tool_input unknown =
        { session_id; transcript_path; tool_name; tool_input; unknown }
      in
      Jsont.Object.map ~kind:"PreToolUseInput" make
      |> Jsont.Object.mem "session_id" Jsont.string ~enc:session_id
      |> Jsont.Object.mem "transcript_path" Jsont.string ~enc:transcript_path
      |> Jsont.Object.mem "tool_name" Jsont.string ~enc:tool_name
      |> Jsont.Object.mem "tool_input" Jsont.json ~enc:tool_input
      |> Jsont.Object.keep_unknown Unknown.mems ~enc:unknown
      |> Jsont.Object.finish
  end

  type permission_decision = [ `Allow | `Deny | `Ask ]

  let permission_decision_jsont : permission_decision Jsont.t =
    Jsont.enum [ ("allow", `Allow); ("deny", `Deny); ("ask", `Ask) ]

  module Output = struct
    type t = {
      permission_decision : permission_decision option;
      permission_decision_reason : string option;
      updated_input : Jsont.json option;
      unknown : Unknown.t;
    }

    let jsont : t Jsont.t =
      let make _hook_event_name permission_decision permission_decision_reason
          updated_input unknown =
        {
          permission_decision;
          permission_decision_reason;
          updated_input;
          unknown;
        }
      in
      Jsont.Object.map ~kind:"PreToolUseOutput" make
      |> Jsont.Object.mem "hookEventName" Jsont.string ~enc:(fun _ ->
          "PreToolUse")
      |> Jsont.Object.opt_mem "permissionDecision" permission_decision_jsont
           ~enc:(fun o -> o.permission_decision)
      |> Jsont.Object.opt_mem "permissionDecisionReason" Jsont.string
           ~enc:(fun o -> o.permission_decision_reason)
      |> Jsont.Object.opt_mem "updatedInput" Jsont.json ~enc:(fun o ->
          o.updated_input)
      |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun o -> o.unknown)
      |> Jsont.Object.finish

    let allow ?reason ?updated_input () =
      {
        permission_decision = Some `Allow;
        permission_decision_reason = reason;
        updated_input;
        unknown = Unknown.empty;
      }

    let deny ?reason () =
      {
        permission_decision = Some `Deny;
        permission_decision_reason = reason;
        updated_input = None;
        unknown = Unknown.empty;
      }

    let ask ?reason () =
      {
        permission_decision = Some `Ask;
        permission_decision_reason = reason;
        updated_input = None;
        unknown = Unknown.empty;
      }

    let continue () =
      {
        permission_decision = None;
        permission_decision_reason = None;
        updated_input = None;
        unknown = Unknown.empty;
      }
  end
end

(** {1 PostToolUse Hook} *)
module PostToolUse = struct
  module Input = struct
    type t = {
      session_id : string;
      transcript_path : string;
      tool_name : string;
      tool_input : Jsont.json;
      tool_response : Jsont.json;
      unknown : Unknown.t;
    }

    let session_id t = t.session_id
    let transcript_path t = t.transcript_path
    let tool_name t = t.tool_name
    let tool_input t = t.tool_input
    let tool_response t = t.tool_response
    let unknown t = t.unknown

    let jsont : t Jsont.t =
      let make session_id transcript_path tool_name tool_input tool_response
          unknown =
        {
          session_id;
          transcript_path;
          tool_name;
          tool_input;
          tool_response;
          unknown;
        }
      in
      Jsont.Object.map ~kind:"PostToolUseInput" make
      |> Jsont.Object.mem "session_id" Jsont.string ~enc:session_id
      |> Jsont.Object.mem "transcript_path" Jsont.string ~enc:transcript_path
      |> Jsont.Object.mem "tool_name" Jsont.string ~enc:tool_name
      |> Jsont.Object.mem "tool_input" Jsont.json ~enc:tool_input
      |> Jsont.Object.mem "tool_response" Jsont.json ~enc:tool_response
      |> Jsont.Object.keep_unknown Unknown.mems ~enc:unknown
      |> Jsont.Object.finish
  end

  module Output = struct
    type t = {
      decision : decision option;
      reason : string option;
      additional_context : string option;
      unknown : Unknown.t;
    }

    let jsont : t Jsont.t =
      let make _hook_event_name decision reason additional_context unknown =
        { decision; reason; additional_context; unknown }
      in
      Jsont.Object.map ~kind:"PostToolUseOutput" make
      |> Jsont.Object.mem "hookEventName" Jsont.string ~enc:(fun _ ->
          "PostToolUse")
      |> Jsont.Object.opt_mem "decision" decision_jsont ~enc:(fun o ->
          o.decision)
      |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(fun o -> o.reason)
      |> Jsont.Object.opt_mem "additionalContext" Jsont.string ~enc:(fun o ->
          o.additional_context)
      |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun o -> o.unknown)
      |> Jsont.Object.finish

    let continue ?additional_context () =
      {
        decision = None;
        reason = None;
        additional_context;
        unknown = Unknown.empty;
      }

    let block ?reason ?additional_context () =
      {
        decision = Some Block;
        reason;
        additional_context;
        unknown = Unknown.empty;
      }
  end
end

(** {1 UserPromptSubmit Hook} *)
module UserPromptSubmit = struct
  module Input = struct
    type t = {
      session_id : string;
      transcript_path : string;
      prompt : string;
      unknown : Unknown.t;
    }

    let session_id t = t.session_id
    let transcript_path t = t.transcript_path
    let prompt t = t.prompt
    let unknown t = t.unknown

    let jsont : t Jsont.t =
      let make session_id transcript_path prompt unknown =
        { session_id; transcript_path; prompt; unknown }
      in
      Jsont.Object.map ~kind:"UserPromptSubmitInput" make
      |> Jsont.Object.mem "session_id" Jsont.string ~enc:session_id
      |> Jsont.Object.mem "transcript_path" Jsont.string ~enc:transcript_path
      |> Jsont.Object.mem "prompt" Jsont.string ~enc:prompt
      |> Jsont.Object.keep_unknown Unknown.mems ~enc:unknown
      |> Jsont.Object.finish
  end

  module Output = struct
    type t = {
      decision : decision option;
      reason : string option;
      additional_context : string option;
      unknown : Unknown.t;
    }

    let jsont : t Jsont.t =
      let make _hook_event_name decision reason additional_context unknown =
        { decision; reason; additional_context; unknown }
      in
      Jsont.Object.map ~kind:"UserPromptSubmitOutput" make
      |> Jsont.Object.mem "hookEventName" Jsont.string ~enc:(fun _ ->
          "UserPromptSubmit")
      |> Jsont.Object.opt_mem "decision" decision_jsont ~enc:(fun o ->
          o.decision)
      |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(fun o -> o.reason)
      |> Jsont.Object.opt_mem "additionalContext" Jsont.string ~enc:(fun o ->
          o.additional_context)
      |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun o -> o.unknown)
      |> Jsont.Object.finish

    let continue ?additional_context () =
      {
        decision = None;
        reason = None;
        additional_context;
        unknown = Unknown.empty;
      }

    let block ?reason () =
      {
        decision = Some Block;
        reason;
        additional_context = None;
        unknown = Unknown.empty;
      }
  end
end

(** {1 Stop Hook} *)
module Stop = struct
  module Input = struct
    type t = {
      session_id : string;
      transcript_path : string;
      stop_hook_active : bool;
      unknown : Unknown.t;
    }

    let session_id t = t.session_id
    let transcript_path t = t.transcript_path
    let stop_hook_active t = t.stop_hook_active
    let unknown t = t.unknown

    let jsont : t Jsont.t =
      let make session_id transcript_path stop_hook_active unknown =
        { session_id; transcript_path; stop_hook_active; unknown }
      in
      Jsont.Object.map ~kind:"StopInput" make
      |> Jsont.Object.mem "session_id" Jsont.string ~enc:session_id
      |> Jsont.Object.mem "transcript_path" Jsont.string ~enc:transcript_path
      |> Jsont.Object.mem "stop_hook_active" Jsont.bool ~enc:stop_hook_active
      |> Jsont.Object.keep_unknown Unknown.mems ~enc:unknown
      |> Jsont.Object.finish
  end

  module Output = struct
    type t = {
      decision : decision option;
      reason : string option;
      unknown : Unknown.t;
    }

    let jsont : t Jsont.t =
      let make _hook_event_name decision reason unknown =
        { decision; reason; unknown }
      in
      Jsont.Object.map ~kind:"StopOutput" make
      |> Jsont.Object.mem "hookEventName" Jsont.string ~enc:(fun _ -> "Stop")
      |> Jsont.Object.opt_mem "decision" decision_jsont ~enc:(fun o ->
          o.decision)
      |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(fun o -> o.reason)
      |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun o -> o.unknown)
      |> Jsont.Object.finish

    let continue () =
      { decision = None; reason = None; unknown = Unknown.empty }

    let block ?reason () =
      { decision = Some Block; reason; unknown = Unknown.empty }
  end
end

(** {1 SubagentStop Hook} - Same structure as Stop *)
module SubagentStop = struct
  module Input = struct
    type t = Stop.Input.t

    let jsont = Stop.Input.jsont
    let session_id = Stop.Input.session_id
    let transcript_path = Stop.Input.transcript_path
    let stop_hook_active = Stop.Input.stop_hook_active
    let unknown = Stop.Input.unknown
  end

  module Output = struct
    type t = Stop.Output.t

    let jsont : t Jsont.t =
      let make _hook_event_name decision reason unknown : t =
        { Stop.Output.decision; reason; unknown }
      in
      Jsont.Object.map ~kind:"SubagentStopOutput" make
      |> Jsont.Object.mem "hookEventName" Jsont.string ~enc:(fun _ ->
          "SubagentStop")
      |> Jsont.Object.opt_mem "decision" decision_jsont ~enc:(fun (o : t) ->
          o.Stop.Output.decision)
      |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(fun (o : t) ->
          o.Stop.Output.reason)
      |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun (o : t) ->
          o.Stop.Output.unknown)
      |> Jsont.Object.finish

    let continue = Stop.Output.continue
    let block = Stop.Output.block
  end
end

(** {1 PreCompact Hook} *)
module PreCompact = struct
  module Input = struct
    type t = {
      session_id : string;
      transcript_path : string;
      unknown : Unknown.t;
    }

    let session_id t = t.session_id
    let transcript_path t = t.transcript_path
    let unknown t = t.unknown

    let jsont : t Jsont.t =
      let make session_id transcript_path unknown =
        { session_id; transcript_path; unknown }
      in
      Jsont.Object.map ~kind:"PreCompactInput" make
      |> Jsont.Object.mem "session_id" Jsont.string ~enc:session_id
      |> Jsont.Object.mem "transcript_path" Jsont.string ~enc:transcript_path
      |> Jsont.Object.keep_unknown Unknown.mems ~enc:unknown
      |> Jsont.Object.finish
  end

  module Output = struct
    type t = unit

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PreCompactOutput" (fun _hook_event_name -> ())
      |> Jsont.Object.mem "hookEventName" Jsont.string ~enc:(fun () ->
          "PreCompact")
      |> Jsont.Object.finish

    let continue () = ()
  end
end

(** {1 Result Builders} *)
let continue ?system_message ?hook_specific_output () =
  {
    decision = None;
    system_message;
    hook_specific_output;
    unknown = Unknown.empty;
  }

let block ?system_message ?hook_specific_output () =
  {
    decision = Some Block;
    system_message;
    hook_specific_output;
    unknown = Unknown.empty;
  }
