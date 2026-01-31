(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Permission control for tool usage.

    This module provides a permission system for controlling which tools Claude
    can invoke and how they can be used. It includes support for permission
    modes, rules, decisions, and callbacks. *)

val src : Logs.Src.t
(** The log source for permission operations. *)

(** {1 Permission Modes} *)

module Mode : sig
  (** Permission modes control the overall behavior of the permission system. *)

  (** The type of permission modes. *)
  type t =
    | Default  (** Standard permission mode with normal checks *)
    | Accept_edits  (** Automatically accept file edits *)
    | Plan  (** Planning mode with restricted execution *)
    | Bypass_permissions  (** Bypass all permission checks *)

  val to_string : t -> string
  (** [to_string t] converts a mode to its string representation. *)

  val of_string : string -> t
  (** [of_string s] parses a mode from its string representation.
      @raise Invalid_argument if the string is not a valid mode. *)

  val of_proto : Proto.Permissions.Mode.t -> t
  (** [of_proto proto] converts from the protocol representation. *)

  val to_proto : t -> Proto.Permissions.Mode.t
  (** [to_proto t] converts to the protocol representation. *)
end

(** {1 Permission Rules} *)

module Rule : sig
  (** Rules define specific permissions for tools. *)

  type t
  (** The type of permission rules. *)

  val create : tool_name:string -> ?rule_content:string -> unit -> t
  (** [create ~tool_name ?rule_content ()] creates a new rule.
      @param tool_name The name of the tool this rule applies to
      @param rule_content Optional rule specification or pattern *)

  val tool_name : t -> string
  (** [tool_name t] returns the tool name. *)

  val rule_content : t -> string option
  (** [rule_content t] returns the optional rule content. *)

  val of_proto : Proto.Permissions.Rule.t -> t
  (** [of_proto proto] converts from the protocol representation. *)

  val to_proto : t -> Proto.Permissions.Rule.t
  (** [to_proto t] converts to the protocol representation. *)
end

(** {1 Permission Decisions} *)

module Decision : sig
  (** Decisions represent the outcome of a permission check. *)

  type t
  (** The type of permission decisions. *)

  val allow : ?updated_input:Tool_input.t -> unit -> t
  (** [allow ?updated_input ()] creates an allow decision.
      @param updated_input Optional modified tool input *)

  val deny : message:string -> interrupt:bool -> t
  (** [deny ~message ~interrupt] creates a deny decision.
      @param message The reason for denying permission
      @param interrupt Whether to interrupt further execution *)

  val is_allow : t -> bool
  (** [is_allow t] returns true if the decision allows the operation. *)

  val is_deny : t -> bool
  (** [is_deny t] returns true if the decision denies the operation. *)

  val updated_input : t -> Tool_input.t option
  (** [updated_input t] returns the optional updated tool input if the decision
      is allow. *)

  val deny_message : t -> string option
  (** [deny_message t] returns the denial message if the decision is deny. *)

  val deny_interrupt : t -> bool
  (** [deny_interrupt t] returns whether to interrupt if the decision is deny.
  *)

  val to_proto_result :
    original_input:Tool_input.t -> t -> Proto.Permissions.Result.t
  (** [to_proto_result ~original_input t] converts to the protocol result
      representation. When the decision allows without modification, the
      original_input is returned. *)
end

(** {1 Permission Context} *)

type context = {
  tool_name : string;  (** Name of the tool being invoked *)
  input : Tool_input.t;  (** Tool input parameters *)
  suggested_rules : Rule.t list;  (** Suggested permission rules *)
}
(** The context provided to permission callbacks. *)

val extract_rules_from_proto_updates :
  Proto.Permissions.Update.t list -> Rule.t list
(** [extract_rules_from_proto_updates updates] extracts rules from protocol
    permission updates. Used internally to convert protocol suggestions into
    context rules. *)

(** {1 Permission Callbacks} *)

type callback = context -> Decision.t
(** The type of permission callbacks. Callbacks are invoked when Claude attempts
    to use a tool, allowing custom permission logic.

    The callback receives a typed context with the tool name, input, and
    suggested rules, and returns a decision to allow or deny the operation. *)

val default_allow : callback
(** [default_allow] always allows tool invocations. *)

val discovery : Rule.t list ref -> callback
(** [discovery log] creates a callback that collects suggested rules into the
    provided reference while allowing all operations. Useful for discovering
    what permissions an operation requires. *)

(** {1 Logging} *)

val log_permission_check : tool_name:string -> decision:Decision.t -> unit
(** [log_permission_check ~tool_name ~decision] logs a permission check result.
*)
