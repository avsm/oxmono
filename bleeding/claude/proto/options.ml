(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Wire format for Claude configuration options. *)

(** Setting sources *)
type setting_source = User | Project | Local

let setting_source_jsont : setting_source Jsont.t =
  Jsont.enum [ ("user", User); ("project", Project); ("local", Local) ]

type t = {
  allowed_tools : string list;
  disallowed_tools : string list;
  max_thinking_tokens : int option;
  system_prompt : string option;
  append_system_prompt : string option;
  permission_mode : Permissions.Mode.t option;
  model : Model.t option;
  continue_conversation : bool;
  resume : string option;
  max_turns : int option;
  permission_prompt_tool_name : string option;
  settings : string option;
  add_dirs : string list;
  max_budget_usd : float option;
  fallback_model : Model.t option;
  setting_sources : setting_source list option;
  max_buffer_size : int option;
  user : string option;
  output_format : Structured_output.t option;
  unknown : Unknown.t;
}
(** Configuration type *)

let empty =
  {
    allowed_tools = [];
    disallowed_tools = [];
    max_thinking_tokens = None;
    system_prompt = None;
    append_system_prompt = None;
    permission_mode = None;
    model = None;
    continue_conversation = false;
    resume = None;
    max_turns = None;
    permission_prompt_tool_name = None;
    settings = None;
    add_dirs = [];
    max_budget_usd = None;
    fallback_model = None;
    setting_sources = None;
    max_buffer_size = None;
    user = None;
    output_format = None;
    unknown = Unknown.empty;
  }

(** Accessor functions *)
let allowed_tools t = t.allowed_tools

let disallowed_tools t = t.disallowed_tools
let max_thinking_tokens t = t.max_thinking_tokens
let system_prompt t = t.system_prompt
let append_system_prompt t = t.append_system_prompt
let permission_mode t = t.permission_mode
let model t = t.model
let continue_conversation t = t.continue_conversation
let resume t = t.resume
let max_turns t = t.max_turns
let permission_prompt_tool_name t = t.permission_prompt_tool_name
let settings t = t.settings
let add_dirs t = t.add_dirs
let max_budget_usd t = t.max_budget_usd
let fallback_model t = t.fallback_model
let setting_sources t = t.setting_sources
let max_buffer_size t = t.max_buffer_size
let user t = t.user
let output_format t = t.output_format
let unknown t = t.unknown

(** Builder functions *)
let with_allowed_tools allowed_tools t = { t with allowed_tools }

let with_disallowed_tools disallowed_tools t = { t with disallowed_tools }

let with_max_thinking_tokens max_thinking_tokens t =
  { t with max_thinking_tokens = Some max_thinking_tokens }

let with_system_prompt system_prompt t =
  { t with system_prompt = Some system_prompt }

let with_append_system_prompt append_system_prompt t =
  { t with append_system_prompt = Some append_system_prompt }

let with_permission_mode permission_mode t =
  { t with permission_mode = Some permission_mode }

let with_model model t = { t with model = Some model }

let with_continue_conversation continue_conversation t =
  { t with continue_conversation }

let with_resume resume t = { t with resume = Some resume }
let with_max_turns max_turns t = { t with max_turns = Some max_turns }

let with_permission_prompt_tool_name permission_prompt_tool_name t =
  { t with permission_prompt_tool_name = Some permission_prompt_tool_name }

let with_settings settings t = { t with settings = Some settings }
let with_add_dirs add_dirs t = { t with add_dirs }

let with_max_budget_usd max_budget_usd t =
  { t with max_budget_usd = Some max_budget_usd }

let with_fallback_model fallback_model t =
  { t with fallback_model = Some fallback_model }

let with_setting_sources setting_sources t =
  { t with setting_sources = Some setting_sources }

let with_max_buffer_size max_buffer_size t =
  { t with max_buffer_size = Some max_buffer_size }

let with_user user t = { t with user = Some user }

let with_output_format output_format t =
  { t with output_format = Some output_format }

(** JSON codec *)
let jsont : t Jsont.t =
  let make allowed_tools disallowed_tools max_thinking_tokens system_prompt
      append_system_prompt permission_mode model continue_conversation resume
      max_turns permission_prompt_tool_name settings add_dirs max_budget_usd
      fallback_model setting_sources max_buffer_size user output_format unknown
      =
    {
      allowed_tools;
      disallowed_tools;
      max_thinking_tokens;
      system_prompt;
      append_system_prompt;
      permission_mode;
      model;
      continue_conversation;
      resume;
      max_turns;
      permission_prompt_tool_name;
      settings;
      add_dirs;
      max_budget_usd;
      fallback_model;
      setting_sources;
      max_buffer_size;
      user;
      output_format;
      unknown;
    }
  in
  Jsont.Object.(
    map ~kind:"Options" make
    |> mem "allowedTools" (Jsont.list Jsont.string) ~enc:allowed_tools
         ~dec_absent:[]
    |> mem "disallowedTools" (Jsont.list Jsont.string) ~enc:disallowed_tools
         ~dec_absent:[]
    |> opt_mem "maxThinkingTokens" Jsont.int ~enc:max_thinking_tokens
    |> opt_mem "systemPrompt" Jsont.string ~enc:system_prompt
    |> opt_mem "appendSystemPrompt" Jsont.string ~enc:append_system_prompt
    |> opt_mem "permissionMode" Permissions.Mode.jsont ~enc:permission_mode
    |> opt_mem "model" Model.jsont ~enc:model
    |> mem "continueConversation" Jsont.bool ~enc:continue_conversation
         ~dec_absent:false
    |> opt_mem "resume" Jsont.string ~enc:resume
    |> opt_mem "maxTurns" Jsont.int ~enc:max_turns
    |> opt_mem "permissionPromptToolName" Jsont.string
         ~enc:permission_prompt_tool_name
    |> opt_mem "settings" Jsont.string ~enc:settings
    |> mem "addDirs" (Jsont.list Jsont.string) ~enc:add_dirs ~dec_absent:[]
    |> opt_mem "maxBudgetUsd" Jsont.number ~enc:max_budget_usd
    |> opt_mem "fallbackModel" Model.jsont ~enc:fallback_model
    |> opt_mem "settingSources"
         (Jsont.list setting_source_jsont)
         ~enc:setting_sources
    |> opt_mem "maxBufferSize" Jsont.int ~enc:max_buffer_size
    |> opt_mem "user" Jsont.string ~enc:user
    |> opt_mem "outputFormat" Structured_output.jsont ~enc:output_format
    |> keep_unknown Unknown.mems ~enc:unknown
    |> finish)
