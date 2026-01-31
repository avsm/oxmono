(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Permission system wire format for Claude tool invocations.

    This module provides the wire format encoding/decoding for permission types
    used in the Claude protocol. It handles JSON serialization and
    deserialization with proper field name mappings. *)

(** Permission modes *)
module Mode = struct
  type t = Default | Accept_edits | Plan | Bypass_permissions

  let to_string = function
    | Default -> "default"
    | Accept_edits -> "acceptEdits"
    | Plan -> "plan"
    | Bypass_permissions -> "bypassPermissions"

  let of_string = function
    | "default" -> Default
    | "acceptEdits" -> Accept_edits
    | "plan" -> Plan
    | "bypassPermissions" -> Bypass_permissions
    | s ->
        raise
          (Invalid_argument (Printf.sprintf "Mode.of_string: unknown mode %s" s))

  let jsont : t Jsont.t =
    Jsont.enum
      [
        ("default", Default);
        ("acceptEdits", Accept_edits);
        ("plan", Plan);
        ("bypassPermissions", Bypass_permissions);
      ]
end

(** Permission behaviors *)
module Behavior = struct
  type t = Allow | Deny | Ask

  let to_string = function Allow -> "allow" | Deny -> "deny" | Ask -> "ask"

  let of_string = function
    | "allow" -> Allow
    | "deny" -> Deny
    | "ask" -> Ask
    | s ->
        raise
          (Invalid_argument
             (Printf.sprintf "Behavior.of_string: unknown behavior %s" s))

  let jsont : t Jsont.t =
    Jsont.enum [ ("allow", Allow); ("deny", Deny); ("ask", Ask) ]
end

(** Permission rules *)
module Rule = struct
  type t = {
    tool_name : string;
    rule_content : string option;
    unknown : Unknown.t;
  }

  let create ~tool_name ?rule_content ?(unknown = Unknown.empty) () =
    { tool_name; rule_content; unknown }

  let tool_name t = t.tool_name
  let rule_content t = t.rule_content
  let unknown t = t.unknown

  let jsont : t Jsont.t =
    let make tool_name rule_content unknown =
      { tool_name; rule_content; unknown }
    in
    Jsont.Object.map ~kind:"Rule" make
    |> Jsont.Object.mem "toolName" Jsont.string ~enc:tool_name
    |> Jsont.Object.opt_mem "ruleContent" Jsont.string ~enc:rule_content
    |> Jsont.Object.keep_unknown Unknown.mems ~enc:unknown
    |> Jsont.Object.finish
end

(** Permission updates *)
module Update = struct
  type destination =
    | User_settings
    | Project_settings
    | Local_settings
    | Session

  let destination_jsont : destination Jsont.t =
    Jsont.enum
      [
        ("userSettings", User_settings);
        ("projectSettings", Project_settings);
        ("localSettings", Local_settings);
        ("session", Session);
      ]

  type update_type =
    | Add_rules
    | Replace_rules
    | Remove_rules
    | Set_mode
    | Add_directories
    | Remove_directories

  let update_type_jsont : update_type Jsont.t =
    Jsont.enum
      [
        ("addRules", Add_rules);
        ("replaceRules", Replace_rules);
        ("removeRules", Remove_rules);
        ("setMode", Set_mode);
        ("addDirectories", Add_directories);
        ("removeDirectories", Remove_directories);
      ]

  type t = {
    update_type : update_type;
    rules : Rule.t list option;
    behavior : Behavior.t option;
    mode : Mode.t option;
    directories : string list option;
    destination : destination option;
    unknown : Unknown.t;
  }

  let create ~update_type ?rules ?behavior ?mode ?directories ?destination
      ?(unknown = Unknown.empty) () =
    { update_type; rules; behavior; mode; directories; destination; unknown }

  let update_type t = t.update_type
  let rules t = t.rules
  let behavior t = t.behavior
  let mode t = t.mode
  let directories t = t.directories
  let destination t = t.destination
  let unknown t = t.unknown

  let jsont : t Jsont.t =
    let make update_type rules behavior mode directories destination unknown =
      { update_type; rules; behavior; mode; directories; destination; unknown }
    in
    Jsont.Object.map ~kind:"Update" make
    |> Jsont.Object.mem "type" update_type_jsont ~enc:update_type
    |> Jsont.Object.opt_mem "rules" (Jsont.list Rule.jsont) ~enc:rules
    |> Jsont.Object.opt_mem "behavior" Behavior.jsont ~enc:behavior
    |> Jsont.Object.opt_mem "mode" Mode.jsont ~enc:mode
    |> Jsont.Object.opt_mem "directories" (Jsont.list Jsont.string)
         ~enc:directories
    |> Jsont.Object.opt_mem "destination" destination_jsont ~enc:destination
    |> Jsont.Object.keep_unknown Unknown.mems ~enc:unknown
    |> Jsont.Object.finish
end

(** Permission context for callbacks *)
module Context = struct
  type t = { suggestions : Update.t list; unknown : Unknown.t }

  let create ?(suggestions = []) ?(unknown = Unknown.empty) () =
    { suggestions; unknown }

  let suggestions t = t.suggestions
  let unknown t = t.unknown

  let jsont : t Jsont.t =
    let make suggestions unknown = { suggestions; unknown } in
    Jsont.Object.map ~kind:"Context" make
    |> Jsont.Object.mem "suggestions" (Jsont.list Update.jsont) ~enc:suggestions
         ~dec_absent:[]
    |> Jsont.Object.keep_unknown Unknown.mems ~enc:unknown
    |> Jsont.Object.finish
end

(** Permission results *)
module Result = struct
  type t =
    | Allow of {
        updated_input : Jsont.json option;
        updated_permissions : Update.t list option;
        unknown : Unknown.t;
      }
    | Deny of { message : string; interrupt : bool; unknown : Unknown.t }

  let allow ?updated_input ?updated_permissions ?(unknown = Unknown.empty) () =
    Allow { updated_input; updated_permissions; unknown }

  let deny ~message ~interrupt ?(unknown = Unknown.empty) () =
    Deny { message; interrupt; unknown }

  let jsont : t Jsont.t =
    let allow_record =
      let make updated_input updated_permissions unknown =
        Allow { updated_input; updated_permissions; unknown }
      in
      Jsont.Object.map ~kind:"AllowRecord" make
      |> Jsont.Object.mem "updatedInput" (Jsont.option Jsont.json)
           ~enc:(function
             | Allow { updated_input; _ } -> updated_input | _ -> None)
           ~dec_absent:None
      |> Jsont.Object.opt_mem "updatedPermissions" (Jsont.list Update.jsont)
           ~enc:(function
           | Allow { updated_permissions; _ } -> updated_permissions
           | _ -> None)
      |> Jsont.Object.keep_unknown Unknown.mems ~enc:(function
        | Allow { unknown; _ } -> unknown
        | _ -> Unknown.empty)
      |> Jsont.Object.finish
    in
    let deny_record =
      let make message interrupt unknown =
        Deny { message; interrupt; unknown }
      in
      Jsont.Object.map ~kind:"DenyRecord" make
      |> Jsont.Object.mem "message" Jsont.string ~enc:(function
        | Deny { message; _ } -> message
        | _ -> "")
      |> Jsont.Object.mem "interrupt" Jsont.bool ~enc:(function
        | Deny { interrupt; _ } -> interrupt
        | _ -> false)
      |> Jsont.Object.keep_unknown Unknown.mems ~enc:(function
        | Deny { unknown; _ } -> unknown
        | _ -> Unknown.empty)
      |> Jsont.Object.finish
    in
    let case_allow =
      Jsont.Object.Case.map "allow" allow_record ~dec:(fun v -> v)
    in
    let case_deny =
      Jsont.Object.Case.map "deny" deny_record ~dec:(fun v -> v)
    in

    let enc_case = function
      | Allow _ as v -> Jsont.Object.Case.value case_allow v
      | Deny _ as v -> Jsont.Object.Case.value case_deny v
    in

    let cases = Jsont.Object.Case.[ make case_allow; make case_deny ] in

    Jsont.Object.map ~kind:"Result" Fun.id
    |> Jsont.Object.case_mem "behavior" Jsont.string ~enc:Fun.id ~enc_case cases
         ~tag_to_string:Fun.id ~tag_compare:String.compare
    |> Jsont.Object.finish
end
