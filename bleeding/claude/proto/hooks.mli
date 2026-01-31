(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Claude Code Hooks System - Wire Format

    This module defines the wire format for hook configuration. Hooks allow you
    to intercept and control events in Claude Code sessions, such as tool usage,
    prompt submission, and session stops.

    {1 Overview}

    Hooks are organized by event type, with each event having:
    - A typed input structure (accessible via submodules)
    - A typed output structure for responses
    - Helper functions for common responses

    This is the wire format module - it does not include the callback system or
    Eio dependencies. For the full hooks system with callbacks, see the [Hooks]
    module in the [lib] directory. *)

(** {1 Hook Events} *)

(** Hook event types *)
type event =
  | Pre_tool_use  (** Fires before a tool is executed *)
  | Post_tool_use  (** Fires after a tool completes *)
  | User_prompt_submit  (** Fires when user submits a prompt *)
  | Stop  (** Fires when conversation stops *)
  | Subagent_stop  (** Fires when a subagent stops *)
  | Pre_compact  (** Fires before message compaction *)

val event_to_string : event -> string
(** [event_to_string event] converts an event to its wire format string. Wire
    format: "PreToolUse", "PostToolUse", "UserPromptSubmit", "Stop",
    "SubagentStop", "PreCompact" *)

val event_of_string : string -> event
(** [event_of_string s] parses an event from its wire format string.
    @raise Invalid_argument if the string is not a valid event. *)

val event_jsont : event Jsont.t
(** [event_jsont] is the Jsont codec for hook events. *)

(** {1 Context} *)

module Context : sig
  (** Context provided to hook callbacks. *)

  type t
  (** The type of hook context. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for hook context. Preserves unknown fields. *)

  val create : ?signal:unit -> unit -> t
  (** [create ?signal ()] creates a new context.
      @param signal Optional abort signal support (future use) *)

  val signal : t -> unit option
  (** [signal t] returns the optional abort signal. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns the unknown fields. *)
end

(** {1 Decisions} *)

(** Hook decision control *)
type decision =
  | Continue  (** Allow the action to proceed *)
  | Block  (** Block the action *)

val decision_jsont : decision Jsont.t
(** [decision_jsont] is the Jsont codec for hook decisions. Wire format:
    "continue", "block" *)

(** {1 Typed Hook Modules} *)

(** PreToolUse hook - fires before tool execution *)
module PreToolUse : sig
  (** {2 Input} *)

  module Input : sig
    type t
    (** Typed input for PreToolUse hooks *)

    val jsont : t Jsont.t
    (** [jsont] is the Jsont codec for PreToolUse input. *)

    val session_id : t -> string
    (** [session_id t] returns the session ID. *)

    val transcript_path : t -> string
    (** [transcript_path t] returns the transcript file path. *)

    val tool_name : t -> string
    (** [tool_name t] returns the tool name being invoked. *)

    val tool_input : t -> Jsont.json
    (** [tool_input t] returns the tool's input as raw JSON. *)

    val unknown : t -> Unknown.t
    (** [unknown t] returns the unknown fields. *)
  end

  (** {2 Output} *)

  type permission_decision = [ `Allow | `Deny | `Ask ]
  (** Permission decision for tool usage. Wire format: "allow", "deny", "ask" *)

  val permission_decision_jsont : permission_decision Jsont.t
  (** [permission_decision_jsont] is the Jsont codec for permission decisions.
  *)

  module Output : sig
    type t
    (** Typed output for PreToolUse hooks *)

    val jsont : t Jsont.t
    (** [jsont] is the Jsont codec for PreToolUse output. *)

    val allow : ?reason:string -> ?updated_input:Jsont.json -> unit -> t
    (** [allow ?reason ?updated_input ()] creates an allow response.
        @param reason Optional explanation for allowing
        @param updated_input Optional modified tool input *)

    val deny : ?reason:string -> unit -> t
    (** [deny ?reason ()] creates a deny response.
        @param reason Optional explanation for denying *)

    val ask : ?reason:string -> unit -> t
    (** [ask ?reason ()] creates an ask response to prompt the user.
        @param reason Optional explanation for asking *)

    val continue : unit -> t
    (** [continue ()] creates a continue response with no decision. *)
  end
end

(** PostToolUse hook - fires after tool execution *)
module PostToolUse : sig
  (** {2 Input} *)

  module Input : sig
    type t
    (** Typed input for PostToolUse hooks *)

    val jsont : t Jsont.t
    (** [jsont] is the Jsont codec for PostToolUse input. *)

    val session_id : t -> string
    (** [session_id t] returns the session ID. *)

    val transcript_path : t -> string
    (** [transcript_path t] returns the transcript file path. *)

    val tool_name : t -> string
    (** [tool_name t] returns the tool name that was invoked. *)

    val tool_input : t -> Jsont.json
    (** [tool_input t] returns the tool's input as raw JSON. *)

    val tool_response : t -> Jsont.json
    (** [tool_response t] returns the tool's response as raw JSON. *)

    val unknown : t -> Unknown.t
    (** [unknown t] returns the unknown fields. *)
  end

  (** {2 Output} *)

  module Output : sig
    type t
    (** Typed output for PostToolUse hooks *)

    val jsont : t Jsont.t
    (** [jsont] is the Jsont codec for PostToolUse output. *)

    val continue : ?additional_context:string -> unit -> t
    (** [continue ?additional_context ()] creates a continue response.
        @param additional_context Optional context to add to the transcript *)

    val block : ?reason:string -> ?additional_context:string -> unit -> t
    (** [block ?reason ?additional_context ()] creates a block response.
        @param reason Optional explanation for blocking
        @param additional_context Optional context to add to the transcript *)
  end
end

(** UserPromptSubmit hook - fires when user submits a prompt *)
module UserPromptSubmit : sig
  (** {2 Input} *)

  module Input : sig
    type t
    (** Typed input for UserPromptSubmit hooks *)

    val jsont : t Jsont.t
    (** [jsont] is the Jsont codec for UserPromptSubmit input. *)

    val session_id : t -> string
    (** [session_id t] returns the session ID. *)

    val transcript_path : t -> string
    (** [transcript_path t] returns the transcript file path. *)

    val prompt : t -> string
    (** [prompt t] returns the user's prompt text. *)

    val unknown : t -> Unknown.t
    (** [unknown t] returns the unknown fields. *)
  end

  (** {2 Output} *)

  module Output : sig
    type t
    (** Typed output for UserPromptSubmit hooks *)

    val jsont : t Jsont.t
    (** [jsont] is the Jsont codec for UserPromptSubmit output. *)

    val continue : ?additional_context:string -> unit -> t
    (** [continue ?additional_context ()] creates a continue response.
        @param additional_context Optional context to add to the transcript *)

    val block : ?reason:string -> unit -> t
    (** [block ?reason ()] creates a block response.
        @param reason Optional explanation for blocking *)
  end
end

(** Stop hook - fires when conversation stops *)
module Stop : sig
  (** {2 Input} *)

  module Input : sig
    type t
    (** Typed input for Stop hooks *)

    val jsont : t Jsont.t
    (** [jsont] is the Jsont codec for Stop input. *)

    val session_id : t -> string
    (** [session_id t] returns the session ID. *)

    val transcript_path : t -> string
    (** [transcript_path t] returns the transcript file path. *)

    val stop_hook_active : t -> bool
    (** [stop_hook_active t] returns whether stop hooks are active. *)

    val unknown : t -> Unknown.t
    (** [unknown t] returns the unknown fields. *)
  end

  (** {2 Output} *)

  module Output : sig
    type t
    (** Typed output for Stop hooks *)

    val jsont : t Jsont.t
    (** [jsont] is the Jsont codec for Stop output. *)

    val continue : unit -> t
    (** [continue ()] creates a continue response. *)

    val block : ?reason:string -> unit -> t
    (** [block ?reason ()] creates a block response.
        @param reason Optional explanation for blocking *)
  end
end

(** SubagentStop hook - fires when a subagent stops *)
module SubagentStop : sig
  (** {2 Input} *)

  module Input : sig
    type t = Stop.Input.t
    (** Same structure as Stop.Input *)

    val jsont : t Jsont.t
    val session_id : t -> string
    val transcript_path : t -> string
    val stop_hook_active : t -> bool
    val unknown : t -> Unknown.t
  end

  (** {2 Output} *)

  module Output : sig
    type t = Stop.Output.t
    (** Same structure as Stop.Output *)

    val jsont : t Jsont.t
    val continue : unit -> t
    val block : ?reason:string -> unit -> t
  end
end

(** PreCompact hook - fires before message compaction *)
module PreCompact : sig
  (** {2 Input} *)

  module Input : sig
    type t
    (** Typed input for PreCompact hooks *)

    val jsont : t Jsont.t
    (** [jsont] is the Jsont codec for PreCompact input. *)

    val session_id : t -> string
    (** [session_id t] returns the session ID. *)

    val transcript_path : t -> string
    (** [transcript_path t] returns the transcript file path. *)

    val unknown : t -> Unknown.t
    (** [unknown t] returns the unknown fields. *)
  end

  (** {2 Output} *)

  module Output : sig
    type t = unit
    (** PreCompact has no specific output *)

    val jsont : t Jsont.t
    (** [jsont] is the Jsont codec for PreCompact output (unit codec). *)

    val continue : unit -> t
    (** [continue ()] returns unit. *)
  end
end

(** {1 Generic Hook Result} *)

type result = {
  decision : decision option;
  system_message : string option;
  hook_specific_output : Jsont.json option;
  unknown : Unknown.t;
}
(** Generic result structure for hooks *)

val result_jsont : result Jsont.t
(** [result_jsont] is the Jsont codec for hook results. *)

val continue :
  ?system_message:string -> ?hook_specific_output:Jsont.json -> unit -> result
(** [continue ?system_message ?hook_specific_output ()] creates a continue
    result.
    @param system_message Optional message to add to system context
    @param hook_specific_output Optional hook-specific output data *)

val block :
  ?system_message:string -> ?hook_specific_output:Jsont.json -> unit -> result
(** [block ?system_message ?hook_specific_output ()] creates a block result.
    @param system_message Optional message to add to system context
    @param hook_specific_output Optional hook-specific output data *)
