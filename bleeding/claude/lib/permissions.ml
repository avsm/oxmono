(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "claude.permission" ~doc:"Claude permission system"

module Log = (val Logs.src_log src : Logs.LOG)

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

  let of_proto : Proto.Permissions.Mode.t -> t = function
    | Proto.Permissions.Mode.Default -> Default
    | Proto.Permissions.Mode.Accept_edits -> Accept_edits
    | Proto.Permissions.Mode.Plan -> Plan
    | Proto.Permissions.Mode.Bypass_permissions -> Bypass_permissions

  let to_proto : t -> Proto.Permissions.Mode.t = function
    | Default -> Proto.Permissions.Mode.Default
    | Accept_edits -> Proto.Permissions.Mode.Accept_edits
    | Plan -> Proto.Permissions.Mode.Plan
    | Bypass_permissions -> Proto.Permissions.Mode.Bypass_permissions
end

(** Permission rules *)
module Rule = struct
  type t = { tool_name : string; rule_content : string option }

  let create ~tool_name ?rule_content () = { tool_name; rule_content }
  let tool_name t = t.tool_name
  let rule_content t = t.rule_content

  let of_proto (proto : Proto.Permissions.Rule.t) : t =
    {
      tool_name = Proto.Permissions.Rule.tool_name proto;
      rule_content = Proto.Permissions.Rule.rule_content proto;
    }

  let to_proto (t : t) : Proto.Permissions.Rule.t =
    Proto.Permissions.Rule.create ~tool_name:t.tool_name
      ?rule_content:t.rule_content ()
end

(** Permission decisions *)
module Decision = struct
  type t =
    | Allow of { updated_input : Tool_input.t option }
    | Deny of { message : string; interrupt : bool }

  let allow ?updated_input () = Allow { updated_input }
  let deny ~message ~interrupt = Deny { message; interrupt }
  let is_allow = function Allow _ -> true | Deny _ -> false
  let is_deny = function Allow _ -> false | Deny _ -> true

  let updated_input = function
    | Allow { updated_input } -> updated_input
    | Deny _ -> None

  let deny_message = function
    | Allow _ -> None
    | Deny { message; _ } -> Some message

  let deny_interrupt = function
    | Allow _ -> false
    | Deny { interrupt; _ } -> interrupt

  let to_proto_result ~original_input (t : t) : Proto.Permissions.Result.t =
    match t with
    | Allow { updated_input } ->
        let updated_input_json =
          match updated_input with
          | Some input -> Some (Tool_input.to_json input)
          | None -> Some (Tool_input.to_json original_input)
          (* Return original when not modified *)
        in
        Proto.Permissions.Result.allow ?updated_input:updated_input_json ()
    | Deny { message; interrupt } ->
        Proto.Permissions.Result.deny ~message ~interrupt ()
end

type context = {
  tool_name : string;
  input : Tool_input.t;
  suggested_rules : Rule.t list;
}
(** Permission context *)

let extract_rules_from_proto_updates updates =
  List.concat_map
    (fun update ->
      match Proto.Permissions.Update.rules update with
      | Some rules -> List.map Rule.of_proto rules
      | None -> [])
    updates

type callback = context -> Decision.t
(** Permission callback type *)

(** Default callbacks *)
let default_allow _ctx = Decision.allow ()

let discovery log ctx =
  List.iter (fun rule -> log := rule :: !log) ctx.suggested_rules;
  Decision.allow ()

(** Logging *)
let log_permission_check ~tool_name ~decision =
  match decision with
  | Decision.Allow _ ->
      Log.info (fun m -> m "Permission granted for tool: %s" tool_name)
  | Decision.Deny { message; _ } ->
      Log.warn (fun m ->
          m "Permission denied for tool %s: %s" tool_name message)
