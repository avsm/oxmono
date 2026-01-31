(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Configuration options for Claude sessions.

    This module provides comprehensive configuration options for controlling
    Claude's behavior, including tool permissions, system prompts, models,
    execution environment, cost controls, and structured outputs.

    {2 Overview}

    Options control all aspects of Claude's behavior:
    - {b Permissions}: Which tools Claude can use and how permission is granted
    - {b Models}: Which AI model to use and fallback options
    - {b Environment}: Working directory, environment variables, settings
    - {b Cost Control}: Budget limits to prevent runaway spending
    - {b Hooks}: Intercept and modify tool execution
    - {b Structured Output}: JSON schema validation for responses
    - {b Session Management}: Continue or resume conversations

    {2 Builder Pattern}

    Options use a functional builder pattern - each [with_*] function returns a
    new options value with the specified field updated:

    {[
      let options =
        Options.default
        |> Options.with_model `Sonnet_4_5
        |> Options.with_max_budget_usd 1.0
        |> Options.with_permission_mode Permissions.Mode.Accept_edits
    ]}

    {2 Common Configuration Scenarios}

    {3 CI/CD: Isolated, Reproducible Builds}

    {[
      let ci_config =
        Options.default |> Options.with_no_settings (* Ignore user config *)
        |> Options.with_max_budget_usd 0.50 (* 50 cent limit *)
        |> Options.with_permission_mode Permissions.Mode.Bypass_permissions
        |> Options.with_model `Haiku_4
    ]}

    {3 Production: Cost Control with Fallback}

    {[
      let prod_config =
        Options.default
        |> Options.with_model `Sonnet_4_5
        |> Options.with_fallback_model `Haiku_4
        |> Options.with_max_budget_usd 10.0 (* $10 daily limit *)
        |> Options.with_max_buffer_size 5_000_000
    ]}

    {3 Development: User Settings with Overrides}

    {[
      let dev_config =
        Options.default
        |> Options.with_max_budget_usd 1.0
        |> Options.with_permission_mode Permissions.Mode.Default
    ]}

    {2 Advanced Options}

    {3 Budget Control}

    Use {!with_max_budget_usd} to set hard spending limits. Claude will
    terminate the session if the budget is exceeded, preventing runaway costs.

    {3 Settings Isolation}

    Use {!with_no_settings} to control which configuration files are loaded.
    This is critical for reproducible builds in CI/CD environments.

    {3 Model Fallback}

    Use {!with_fallback_model} to specify an alternative model when the primary
    model is unavailable or overloaded. This improves reliability. *)

val src : Logs.Src.t
(** The log source for options operations *)

(** {1 Types} *)

type t
(** The type of configuration options. *)

val default : t
(** [default] returns the default configuration with sensible defaults:
    - No tool restrictions
    - 8000 max thinking tokens
    - Default allow permission callback
    - No custom prompts or model override *)

(** {1 Builder Pattern} *)

val with_allowed_tools : string list -> t -> t
(** [with_allowed_tools tools t] sets the allowed tools. *)

val with_disallowed_tools : string list -> t -> t
(** [with_disallowed_tools tools t] sets the disallowed tools. *)

val with_max_thinking_tokens : int -> t -> t
(** [with_max_thinking_tokens tokens t] sets the maximum thinking tokens. *)

val with_system_prompt : string -> t -> t
(** [with_system_prompt prompt t] sets the system prompt override. *)

val with_append_system_prompt : string -> t -> t
(** [with_append_system_prompt prompt t] sets the system prompt append. *)

val with_permission_mode : Permissions.Mode.t -> t -> t
(** [with_permission_mode mode t] sets the permission mode. *)

val with_permission_callback : Permissions.callback -> t -> t
(** [with_permission_callback callback t] sets the permission callback. *)

val with_model : Proto.Model.t -> t -> t
(** [with_model model t] sets the model override using a typed Model.t. *)

val with_cwd : [> Eio.Fs.dir_ty ] Eio.Path.t -> t -> t
(** [with_cwd cwd t] sets the working directory. *)

val with_env : (string * string) list -> t -> t
(** [with_env env t] sets the environment variables. *)

val with_continue_conversation : bool -> t -> t
(** [with_continue_conversation continue t] sets whether to continue
    conversation. *)

val with_resume : string -> t -> t
(** [with_resume session_id t] sets the session ID to resume. *)

val with_max_turns : int -> t -> t
(** [with_max_turns turns t] sets the maximum number of turns. *)

val with_permission_prompt_tool_name : string -> t -> t
(** [with_permission_prompt_tool_name tool t] sets the permission prompt tool
    name. *)

val with_settings : string -> t -> t
(** [with_settings path t] sets the path to settings file. *)

val with_add_dirs : string list -> t -> t
(** [with_add_dirs dirs t] sets the additional allowed directories. *)

val with_debug_stderr : [> Eio.Flow.sink_ty ] Eio.Flow.sink -> t -> t
(** [with_debug_stderr sink t] sets the debug output sink. *)

val with_hooks : Hooks.t -> t -> t
(** [with_hooks hooks t] sets the hooks configuration. *)

val with_max_budget_usd : float -> t -> t
(** [with_max_budget_usd budget t] sets the maximum spending limit in USD. The
    session will terminate if this limit is exceeded. *)

val with_fallback_model : Proto.Model.t -> t -> t
(** [with_fallback_model model t] sets the fallback model using a typed Model.t.
*)

val with_no_settings : t -> t
(** [with_no_settings t] disables all settings loading (user, project, local).
    Useful for CI/CD environments where you want isolated, reproducible
    behavior. *)

val with_max_buffer_size : int -> t -> t
(** [with_max_buffer_size size t] sets the maximum stdout buffer size in bytes.
*)

val with_user : string -> t -> t
(** [with_user user t] sets the Unix user for subprocess execution. *)

val with_output_format : Proto.Structured_output.t -> t -> t
(** [with_output_format format t] sets the structured output format. *)

val with_extra_args : (string * string option) list -> t -> t
(** [with_extra_args args t] sets the additional CLI flags. *)

val with_mcp_server : name:string -> Mcp_server.t -> t -> t
(** [with_mcp_server ~name server t] adds an in-process MCP server.

    Multiple servers can be added. Tools from server "foo" are accessed as
    [mcp__foo__<tool_name>]. *)

(** {1 Accessors} *)

val allowed_tools : t -> string list
(** [allowed_tools t] returns the list of allowed tools. *)

val disallowed_tools : t -> string list
(** [disallowed_tools t] returns the list of disallowed tools. *)

val max_thinking_tokens : t -> int
(** [max_thinking_tokens t] returns the maximum thinking tokens. *)

val system_prompt : t -> string option
(** [system_prompt t] returns the optional system prompt override. *)

val append_system_prompt : t -> string option
(** [append_system_prompt t] returns the optional system prompt append. *)

val permission_mode : t -> Permissions.Mode.t option
(** [permission_mode t] returns the optional permission mode. *)

val permission_callback : t -> Permissions.callback option
(** [permission_callback t] returns the optional permission callback. *)

val model : t -> Proto.Model.t option
(** [model t] returns the optional model override. *)

val cwd : t -> Eio.Fs.dir_ty Eio.Path.t option
(** [cwd t] returns the optional working directory. *)

val env : t -> (string * string) list
(** [env t] returns the environment variables. *)

val continue_conversation : t -> bool
(** [continue_conversation t] returns whether to continue an existing
    conversation. *)

val resume : t -> string option
(** [resume t] returns the optional session ID to resume. *)

val max_turns : t -> int option
(** [max_turns t] returns the optional maximum number of turns. *)

val permission_prompt_tool_name : t -> string option
(** [permission_prompt_tool_name t] returns the optional tool name for
    permission prompts. *)

val settings : t -> string option
(** [settings t] returns the optional path to settings file. *)

val add_dirs : t -> string list
(** [add_dirs t] returns the list of additional allowed directories. *)

val debug_stderr : t -> Eio.Flow.sink_ty Eio.Flow.sink option
(** [debug_stderr t] returns the optional debug output sink. *)

val hooks : t -> Hooks.t option
(** [hooks t] returns the optional hooks configuration. *)

val max_budget_usd : t -> float option
(** [max_budget_usd t] returns the optional spending limit in USD. *)

val fallback_model : t -> Proto.Model.t option
(** [fallback_model t] returns the optional fallback model. *)

val setting_sources : t -> Proto.Options.setting_source list option
(** [setting_sources t] returns the optional list of setting sources to load. *)

val max_buffer_size : t -> int option
(** [max_buffer_size t] returns the optional stdout buffer size in bytes. *)

val user : t -> string option
(** [user t] returns the optional Unix user for subprocess execution. *)

val output_format : t -> Proto.Structured_output.t option
(** [output_format t] returns the optional structured output format. *)

val extra_args : t -> (string * string option) list
(** [extra_args t] returns the additional CLI flags. *)

val mcp_servers : t -> (string * Mcp_server.t) list
(** [mcp_servers t] returns the list of in-process MCP servers. *)

(** {1 Logging} *)

val log_options : t -> unit
(** [log_options t] logs the current options configuration. *)

(** {1 Advanced: Wire Format Conversion} *)

module Advanced : sig
  val to_wire : t -> Proto.Options.t
  (** [to_wire t] converts to wire format (excludes Eio types and callbacks).
      This is used internally by the client to send options to the Claude CLI.
  *)
end
