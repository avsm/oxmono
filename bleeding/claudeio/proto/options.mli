(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Wire format for Claude configuration options.

    This module provides the protocol-level wire format encoding/decoding for
    configuration options used in JSON configuration files. It handles JSON
    serialization and deserialization with proper field name mappings
    (camelCase).

    This is the protocol-level module without Eio types or logging. *)

(** {1 Setting Sources} *)

(** The type of setting sources, indicating where configuration was loaded from.
*)
type setting_source =
  | User  (** User-level settings *)
  | Project  (** Project-level settings *)
  | Local  (** Local directory settings *)

(** {1 Configuration Type} *)

type t
(** The type of configuration options.

    This represents all configurable options for Claude interactions, encoded in
    JSON format. *)

val jsont : t Jsont.t
(** [jsont] is the Jsont codec for configuration options.

    Wire format uses camelCase field names:
    - allowedTools (array of strings)
    - disallowedTools (array of strings)
    - maxThinkingTokens (int)
    - systemPrompt (string)
    - appendSystemPrompt (string)
    - permissionMode (string via Permissions.Mode.jsont)
    - model (string via Model.jsont)
    - continueConversation (bool)
    - resume (string)
    - maxTurns (int)
    - permissionPromptToolName (string)
    - settings (string)
    - addDirs (array of strings)
    - maxBudgetUsd (float)
    - fallbackModel (string via Model.jsont)
    - settingSources (array of "user", "project", "local")
    - maxBufferSize (int)
    - user (string)
    - outputFormat (object via Structured_output.jsont)

    Unknown fields are preserved for forward compatibility. *)

val empty : t
(** [empty] is an empty configuration with all fields set to their default
    values.

    Default values:
    - Lists default to empty
    - [maxThinkingTokens] defaults to 8000
    - [continueConversation] defaults to false
    - All optional fields default to [None] *)

(** {1 Accessor Functions} *)

val allowed_tools : t -> string list
(** [allowed_tools t] returns the list of allowed tool names. Empty list means
    all tools are allowed (unless explicitly disallowed). *)

val disallowed_tools : t -> string list
(** [disallowed_tools t] returns the list of disallowed tool names. *)

val max_thinking_tokens : t -> int option
(** [max_thinking_tokens t] returns the maximum number of tokens Claude can use
    for internal thinking. *)

val system_prompt : t -> string option
(** [system_prompt t] returns the system prompt to use for Claude. *)

val append_system_prompt : t -> string option
(** [append_system_prompt t] returns additional text to append to the system
    prompt. *)

val permission_mode : t -> Permissions.Mode.t option
(** [permission_mode t] returns the permission mode controlling how tool
    invocations are authorized. *)

val model : t -> Model.t option
(** [model t] returns the Claude model to use for interactions. *)

val continue_conversation : t -> bool
(** [continue_conversation t] returns whether to continue from a previous
    conversation. *)

val resume : t -> string option
(** [resume t] returns the session ID to resume from. *)

val max_turns : t -> int option
(** [max_turns t] returns the maximum number of conversation turns to allow. *)

val permission_prompt_tool_name : t -> string option
(** [permission_prompt_tool_name t] returns the tool name to use for permission
    prompts. *)

val settings : t -> string option
(** [settings t] returns the path to the settings file. *)

val add_dirs : t -> string list
(** [add_dirs t] returns additional directories to include in the context. *)

val max_budget_usd : t -> float option
(** [max_budget_usd t] returns the maximum budget in USD for API calls. *)

val fallback_model : t -> Model.t option
(** [fallback_model t] returns the fallback model to use if the primary model
    fails. *)

val setting_sources : t -> setting_source list option
(** [setting_sources t] returns the list of setting sources to load from. *)

val max_buffer_size : t -> int option
(** [max_buffer_size t] returns the maximum buffer size for I/O operations. *)

val user : t -> string option
(** [user t] returns the user identifier for the session. *)

val output_format : t -> Structured_output.t option
(** [output_format t] returns the structured output format configuration. *)

val unknown : t -> Unknown.t
(** [unknown t] returns the unknown fields preserved from JSON parsing. *)

(** {1 Builder Functions} *)

val with_allowed_tools : string list -> t -> t
(** [with_allowed_tools tools t] sets the allowed tools. *)

val with_disallowed_tools : string list -> t -> t
(** [with_disallowed_tools tools t] sets the disallowed tools. *)

val with_max_thinking_tokens : int -> t -> t
(** [with_max_thinking_tokens tokens t] sets the maximum thinking tokens. *)

val with_system_prompt : string -> t -> t
(** [with_system_prompt prompt t] sets the system prompt. *)

val with_append_system_prompt : string -> t -> t
(** [with_append_system_prompt prompt t] sets the text to append to the system
    prompt. *)

val with_permission_mode : Permissions.Mode.t -> t -> t
(** [with_permission_mode mode t] sets the permission mode. *)

val with_model : Model.t -> t -> t
(** [with_model model t] sets the Claude model. *)

val with_continue_conversation : bool -> t -> t
(** [with_continue_conversation continue t] sets whether to continue
    conversation. *)

val with_resume : string -> t -> t
(** [with_resume session_id t] sets the session ID to resume from. *)

val with_max_turns : int -> t -> t
(** [with_max_turns turns t] sets the maximum number of turns. *)

val with_permission_prompt_tool_name : string -> t -> t
(** [with_permission_prompt_tool_name tool t] sets the permission prompt tool
    name. *)

val with_settings : string -> t -> t
(** [with_settings path t] sets the settings file path. *)

val with_add_dirs : string list -> t -> t
(** [with_add_dirs dirs t] sets the additional directories. *)

val with_max_budget_usd : float -> t -> t
(** [with_max_budget_usd budget t] sets the maximum budget. *)

val with_fallback_model : Model.t -> t -> t
(** [with_fallback_model model t] sets the fallback model. *)

val with_setting_sources : setting_source list -> t -> t
(** [with_setting_sources sources t] sets the setting sources. *)

val with_max_buffer_size : int -> t -> t
(** [with_max_buffer_size size t] sets the maximum buffer size. *)

val with_user : string -> t -> t
(** [with_user user t] sets the user identifier. *)

val with_output_format : Structured_output.t -> t -> t
(** [with_output_format format t] sets the structured output format. *)
