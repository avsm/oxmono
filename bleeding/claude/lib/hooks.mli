(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Fully typed hook callbacks.

    Hooks allow you to intercept and control events in Claude Code sessions,
    using fully typed OCaml values instead of raw JSON.

    {1 Overview}

    This module provides a high-level, type-safe interface to hooks. Each hook
    type has:
    - Fully typed input records using {!Tool_input.t}
    - Fully typed output records
    - Helper functions for common responses
    - Conversion functions to/from wire format ({!Proto.Hooks})

    {1 Example Usage}

    {[
      open Eio.Std

      (* Block dangerous bash commands *)
      let block_rm_rf input =
        if input.Hooks.PreToolUse.tool_name = "Bash" then
          match Tool_input.get_string input.tool_input "command" with
          | Some cmd when String.contains cmd "rm -rf" ->
              Hooks.PreToolUse.deny ~reason:"Dangerous command" ()
          | _ -> Hooks.PreToolUse.continue ()
        else Hooks.PreToolUse.continue ()

      let hooks =
        Hooks.empty
        |> Hooks.on_pre_tool_use ~pattern:"Bash" block_rm_rf

      let options = Claude.Options.create ~hooks () in
      let client = Claude.Client.create ~options ~sw ~process_mgr () in
    ]} *)

val src : Logs.Src.t
(** The log source for hooks *)

(** {1 Hook Types} *)

(** PreToolUse hook - fires before tool execution *)
module PreToolUse : sig
  (** {2 Input} *)

  type input = {
    session_id : string;
    transcript_path : string;
    tool_name : string;
    tool_input : Tool_input.t;
  }
  (** Input provided to PreToolUse hooks. *)

  (** {2 Output} *)

  type decision =
    | Allow
    | Deny
    | Ask  (** Permission decision for tool usage. *)

  type output = {
    decision : decision option;
    reason : string option;
    updated_input : Tool_input.t option;
  }
  (** Output from PreToolUse hooks. *)

  (** {2 Response Builders} *)

  val allow : ?reason:string -> ?updated_input:Tool_input.t -> unit -> output
  (** [allow ?reason ?updated_input ()] creates an allow response.
      @param reason Optional explanation for allowing
      @param updated_input Optional modified tool input *)

  val deny : ?reason:string -> unit -> output
  (** [deny ?reason ()] creates a deny response.
      @param reason Optional explanation for denying *)

  val ask : ?reason:string -> unit -> output
  (** [ask ?reason ()] creates an ask response to prompt the user.
      @param reason Optional explanation for asking *)

  val continue : unit -> output
  (** [continue ()] creates a continue response with no decision. *)

  (** {2 Callback Type} *)

  type callback = input -> output
  (** Callback function type for PreToolUse hooks. *)

  (** {2 Conversion Functions} *)

  val input_of_proto : Proto.Hooks.PreToolUse.Input.t -> input
  (** [input_of_proto proto] converts wire format input to typed input. *)

  val output_to_proto : output -> Proto.Hooks.PreToolUse.Output.t
  (** [output_to_proto output] converts typed output to wire format. *)
end

(** PostToolUse hook - fires after tool execution *)
module PostToolUse : sig
  (** {2 Input} *)

  type input = {
    session_id : string;
    transcript_path : string;
    tool_name : string;
    tool_input : Tool_input.t;
    tool_response : Jsont.json; (* Response varies by tool *)
  }
  (** Input provided to PostToolUse hooks. Note: [tool_response] remains as
      {!type:Jsont.json} since response schemas vary by tool. *)

  (** {2 Output} *)

  type output = {
    block : bool;
    reason : string option;
    additional_context : string option;
  }
  (** Output from PostToolUse hooks. *)

  (** {2 Response Builders} *)

  val continue : ?additional_context:string -> unit -> output
  (** [continue ?additional_context ()] creates a continue response.
      @param additional_context Optional context to add to the transcript *)

  val block : ?reason:string -> ?additional_context:string -> unit -> output
  (** [block ?reason ?additional_context ()] creates a block response.
      @param reason Optional explanation for blocking
      @param additional_context Optional context to add to the transcript *)

  (** {2 Callback Type} *)

  type callback = input -> output
  (** Callback function type for PostToolUse hooks. *)

  (** {2 Conversion Functions} *)

  val input_of_proto : Proto.Hooks.PostToolUse.Input.t -> input
  (** [input_of_proto proto] converts wire format input to typed input. *)

  val output_to_proto : output -> Proto.Hooks.PostToolUse.Output.t
  (** [output_to_proto output] converts typed output to wire format. *)
end

(** UserPromptSubmit hook - fires when user submits a prompt *)
module UserPromptSubmit : sig
  (** {2 Input} *)

  type input = {
    session_id : string;
    transcript_path : string;
    prompt : string;
  }
  (** Input provided to UserPromptSubmit hooks. *)

  (** {2 Output} *)

  type output = {
    block : bool;
    reason : string option;
    additional_context : string option;
  }
  (** Output from UserPromptSubmit hooks. *)

  (** {2 Response Builders} *)

  val continue : ?additional_context:string -> unit -> output
  (** [continue ?additional_context ()] creates a continue response.
      @param additional_context Optional context to add to the transcript *)

  val block : ?reason:string -> unit -> output
  (** [block ?reason ()] creates a block response.
      @param reason Optional explanation for blocking *)

  (** {2 Callback Type} *)

  type callback = input -> output
  (** Callback function type for UserPromptSubmit hooks. *)

  (** {2 Conversion Functions} *)

  val input_of_proto : Proto.Hooks.UserPromptSubmit.Input.t -> input
  (** [input_of_proto proto] converts wire format input to typed input. *)

  val output_to_proto : output -> Proto.Hooks.UserPromptSubmit.Output.t
  (** [output_to_proto output] converts typed output to wire format. *)
end

(** Stop hook - fires when conversation stops *)
module Stop : sig
  (** {2 Input} *)

  type input = {
    session_id : string;
    transcript_path : string;
    stop_hook_active : bool;
  }
  (** Input provided to Stop hooks. *)

  (** {2 Output} *)

  type output = { block : bool; reason : string option }
  (** Output from Stop hooks. *)

  (** {2 Response Builders} *)

  val continue : unit -> output
  (** [continue ()] creates a continue response. *)

  val block : ?reason:string -> unit -> output
  (** [block ?reason ()] creates a block response.
      @param reason Optional explanation for blocking *)

  (** {2 Callback Type} *)

  type callback = input -> output
  (** Callback function type for Stop hooks. *)

  (** {2 Conversion Functions} *)

  val input_of_proto : Proto.Hooks.Stop.Input.t -> input
  (** [input_of_proto proto] converts wire format input to typed input. *)

  val output_to_proto : output -> Proto.Hooks.Stop.Output.t
  (** [output_to_proto output] converts typed output to wire format. *)
end

(** SubagentStop hook - fires when a subagent stops *)
module SubagentStop : sig
  (** {2 Input} *)

  type input = Stop.input
  (** Same structure as Stop.input *)

  (** {2 Output} *)

  type output = Stop.output
  (** Same structure as Stop.output *)

  (** {2 Response Builders} *)

  val continue : unit -> output
  (** [continue ()] creates a continue response. *)

  val block : ?reason:string -> unit -> output
  (** [block ?reason ()] creates a block response.
      @param reason Optional explanation for blocking *)

  (** {2 Callback Type} *)

  type callback = input -> output
  (** Callback function type for SubagentStop hooks. *)

  (** {2 Conversion Functions} *)

  val input_of_proto : Proto.Hooks.SubagentStop.Input.t -> input
  (** [input_of_proto proto] converts wire format input to typed input. *)

  val output_to_proto : output -> Proto.Hooks.SubagentStop.Output.t
  (** [output_to_proto output] converts typed output to wire format. *)
end

(** PreCompact hook - fires before message compaction *)
module PreCompact : sig
  (** {2 Input} *)

  type input = { session_id : string; transcript_path : string }
  (** Input provided to PreCompact hooks. *)

  (** {2 Callback Type} *)

  type callback = input -> unit
  (** Callback function type for PreCompact hooks. PreCompact hooks have no
      output - they are notification-only. *)

  (** {2 Conversion Functions} *)

  val input_of_proto : Proto.Hooks.PreCompact.Input.t -> input
  (** [input_of_proto proto] converts wire format input to typed input. *)
end

(** {1 Hook Configuration} *)

type t
(** Hook configuration.

    Hooks are configured using a builder pattern:
    {[
      Hooks.empty
      |> Hooks.on_pre_tool_use ~pattern:"Bash" bash_handler
      |> Hooks.on_post_tool_use post_handler
    ]} *)

val empty : t
(** [empty] is an empty hook configuration with no callbacks. *)

val on_pre_tool_use : ?pattern:string -> PreToolUse.callback -> t -> t
(** [on_pre_tool_use ?pattern callback config] adds a PreToolUse hook.
    @param pattern
      Optional regex pattern to match tool names (e.g., "Bash|Edit")
    @param callback Function to invoke on matching events *)

val on_post_tool_use : ?pattern:string -> PostToolUse.callback -> t -> t
(** [on_post_tool_use ?pattern callback config] adds a PostToolUse hook.
    @param pattern Optional regex pattern to match tool names
    @param callback Function to invoke on matching events *)

val on_user_prompt_submit : UserPromptSubmit.callback -> t -> t
(** [on_user_prompt_submit callback config] adds a UserPromptSubmit hook.
    @param callback Function to invoke on prompt submission *)

val on_stop : Stop.callback -> t -> t
(** [on_stop callback config] adds a Stop hook.
    @param callback Function to invoke on conversation stop *)

val on_subagent_stop : SubagentStop.callback -> t -> t
(** [on_subagent_stop callback config] adds a SubagentStop hook.
    @param callback Function to invoke on subagent stop *)

val on_pre_compact : PreCompact.callback -> t -> t
(** [on_pre_compact callback config] adds a PreCompact hook.
    @param callback Function to invoke before message compaction *)

(** {1 Internal - for client use} *)

val get_callbacks :
  t ->
  (Proto.Hooks.event
  * (string option * (Jsont.json -> Proto.Hooks.result)) list)
  list
(** [get_callbacks config] returns hook configuration in format suitable for
    registration with the CLI.

    This function converts typed callbacks into wire format handlers that:
    - Parse JSON input using Proto.Hooks types
    - Convert to typed input using input_of_proto
    - Invoke the user's typed callback
    - Convert output back to wire format using output_to_proto

    This is an internal function used by {!Client} - you should not need to call
    it directly. *)
