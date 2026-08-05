(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "claudeio.options" ~doc:"Claude configuration options"

module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  allowed_tools : string list;
  disallowed_tools : string list;
  max_thinking_tokens : int;
  system_prompt : string option;
  append_system_prompt : string option;
  permission_mode : Permissions.Mode.t option;
  permission_callback : Permissions.callback option;
  model : Proto.Model.t option;
  cwd : Eio.Fs.dir_ty Eio.Path.t option;
  env : (string * string) list;
  continue_conversation : bool;
  resume : string option;
  max_turns : int option;
  permission_prompt_tool_name : string option;
  settings : string option;
  add_dirs : string list;
  extra_args : (string * string option) list;
  debug_stderr : Eio.Flow.sink_ty Eio.Flow.sink option;
  hooks : Hooks.t option;
  max_budget_usd : float option;
  fallback_model : Proto.Model.t option;
  setting_sources : Proto.Options.setting_source list option;
  max_buffer_size : int option;
  user : string option;
  output_format : Proto.Structured_output.t option;
  mcp_servers : (string * Mcp_server.t) list;
}

let default =
  {
    allowed_tools = [];
    disallowed_tools = [];
    max_thinking_tokens = 8000;
    system_prompt = None;
    append_system_prompt = None;
    permission_mode = None;
    permission_callback = Some Permissions.default_allow;
    model = None;
    cwd = None;
    env = [];
    continue_conversation = false;
    resume = None;
    max_turns = None;
    permission_prompt_tool_name = None;
    settings = None;
    add_dirs = [];
    extra_args = [];
    debug_stderr = None;
    hooks = None;
    max_budget_usd = None;
    fallback_model = None;
    setting_sources = None;
    max_buffer_size = None;
    user = None;
    output_format = None;
    mcp_servers = [];
  }

(* Accessors *)
let allowed_tools t = t.allowed_tools
let disallowed_tools t = t.disallowed_tools
let max_thinking_tokens t = t.max_thinking_tokens
let system_prompt t = t.system_prompt
let append_system_prompt t = t.append_system_prompt
let permission_mode t = t.permission_mode
let permission_callback t = t.permission_callback
let model t = t.model
let cwd t = t.cwd
let env t = t.env
let continue_conversation t = t.continue_conversation
let resume t = t.resume
let max_turns t = t.max_turns
let permission_prompt_tool_name t = t.permission_prompt_tool_name
let settings t = t.settings
let add_dirs t = t.add_dirs
let extra_args t = t.extra_args
let debug_stderr t = t.debug_stderr
let hooks t = t.hooks
let max_budget_usd t = t.max_budget_usd
let fallback_model t = t.fallback_model
let setting_sources t = t.setting_sources
let max_buffer_size t = t.max_buffer_size
let user t = t.user
let output_format t = t.output_format
let mcp_servers t = t.mcp_servers

(* Builders *)
let with_allowed_tools tools t = { t with allowed_tools = tools }
let with_disallowed_tools tools t = { t with disallowed_tools = tools }
let with_max_thinking_tokens tokens t = { t with max_thinking_tokens = tokens }
let with_system_prompt prompt t = { t with system_prompt = Some prompt }

let with_append_system_prompt prompt t =
  { t with append_system_prompt = Some prompt }

let with_permission_mode mode t = { t with permission_mode = Some mode }

let with_permission_callback callback t =
  { t with permission_callback = Some callback }

let with_model model t = { t with model = Some model }
let with_cwd cwd t = { t with cwd = Some (cwd :> Eio.Fs.dir_ty Eio.Path.t) }
let with_env env t = { t with env }

let with_continue_conversation continue t =
  { t with continue_conversation = continue }

let with_resume session_id t = { t with resume = Some session_id }
let with_max_turns turns t = { t with max_turns = Some turns }

let with_permission_prompt_tool_name tool t =
  { t with permission_prompt_tool_name = Some tool }

let with_settings path t = { t with settings = Some path }
let with_add_dirs dirs t = { t with add_dirs = dirs }
let with_extra_args args t = { t with extra_args = args }

let with_debug_stderr sink t =
  { t with debug_stderr = Some (sink :> Eio.Flow.sink_ty Eio.Flow.sink) }

let with_hooks hooks t = { t with hooks = Some hooks }
let with_max_budget_usd budget t = { t with max_budget_usd = Some budget }
let with_fallback_model model t = { t with fallback_model = Some model }
let with_no_settings t = { t with setting_sources = Some [] }
let with_max_buffer_size size t = { t with max_buffer_size = Some size }
let with_user user t = { t with user = Some user }
let with_output_format format t = { t with output_format = Some format }

let with_mcp_server ~name server t =
  { t with mcp_servers = (name, server) :: t.mcp_servers }

let log_options t =
  Log.debug (fun m ->
      m "Options: model=%s fallback=%s max_thinking_tokens=%d max_budget=%s"
        (match t.model with
        | None -> "default"
        | Some m -> Proto.Model.to_string m)
        (match t.fallback_model with
        | None -> "none"
        | Some m -> Proto.Model.to_string m)
        t.max_thinking_tokens
        (match t.max_budget_usd with
        | None -> "unlimited"
        | Some b -> Printf.sprintf "$%.2f" b))

module Advanced = struct
  let to_wire (t : t) : Proto.Options.t =
    let base = Proto.Options.empty in
    let base = Proto.Options.with_allowed_tools t.allowed_tools base in
    let base = Proto.Options.with_disallowed_tools t.disallowed_tools base in
    let base =
      Proto.Options.with_max_thinking_tokens t.max_thinking_tokens base
    in
    let base =
      match t.system_prompt with
      | None -> base
      | Some p -> Proto.Options.with_system_prompt p base
    in
    let base =
      match t.append_system_prompt with
      | None -> base
      | Some p -> Proto.Options.with_append_system_prompt p base
    in
    let base =
      match t.permission_mode with
      | None -> base
      | Some m ->
          Proto.Options.with_permission_mode (Permissions.Mode.to_proto m) base
    in
    let base =
      match t.model with
      | None -> base
      | Some m -> Proto.Options.with_model m base
    in
    let base =
      Proto.Options.with_continue_conversation t.continue_conversation base
    in
    let base =
      match t.resume with
      | None -> base
      | Some r -> Proto.Options.with_resume r base
    in
    let base =
      match t.max_turns with
      | None -> base
      | Some turns -> Proto.Options.with_max_turns turns base
    in
    let base =
      match t.permission_prompt_tool_name with
      | None -> base
      | Some tool -> Proto.Options.with_permission_prompt_tool_name tool base
    in
    let base =
      match t.settings with
      | None -> base
      | Some s -> Proto.Options.with_settings s base
    in
    let base = Proto.Options.with_add_dirs t.add_dirs base in
    let base =
      match t.max_budget_usd with
      | None -> base
      | Some b -> Proto.Options.with_max_budget_usd b base
    in
    let base =
      match t.fallback_model with
      | None -> base
      | Some m -> Proto.Options.with_fallback_model m base
    in
    let base =
      match t.setting_sources with
      | None -> base
      | Some sources -> Proto.Options.with_setting_sources sources base
    in
    let base =
      match t.max_buffer_size with
      | None -> base
      | Some size -> Proto.Options.with_max_buffer_size size base
    in
    let base =
      match t.user with
      | None -> base
      | Some u -> Proto.Options.with_user u base
    in
    let base =
      match t.output_format with
      | None -> base
      | Some format -> Proto.Options.with_output_format format base
    in
    base
end
