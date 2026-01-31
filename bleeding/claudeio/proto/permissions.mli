(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Permission system wire format for Claude tool invocations.

    This module provides the wire format encoding/decoding for permission types
    used in the Claude protocol. It handles JSON serialization and
    deserialization with proper field name mappings. *)

(** {1 Permission Modes} *)

module Mode : sig
  (** Permission modes control the overall behavior of the permission system. *)

  (** The type of permission modes. *)
  type t =
    | Default  (** Standard permission mode with normal checks *)
    | Accept_edits  (** Automatically accept file edits *)
    | Plan  (** Planning mode with restricted execution *)
    | Bypass_permissions  (** Bypass all permission checks *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for permission modes. Wire format uses
      camelCase: "default", "acceptEdits", "plan", "bypassPermissions". *)

  val to_string : t -> string
  (** [to_string t] converts a mode to its wire format string representation. *)

  val of_string : string -> t
  (** [of_string s] parses a mode from its wire format string representation.
      @raise Invalid_argument if the string is not a valid mode. *)
end

(** {1 Permission Behaviors} *)

module Behavior : sig
  (** Behaviors determine how permission requests are handled. *)

  (** The type of permission behaviors. *)
  type t =
    | Allow  (** Allow the operation *)
    | Deny  (** Deny the operation *)
    | Ask  (** Ask the user for permission *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for permission behaviors. Wire format uses
      lowercase: "allow", "deny", "ask". *)

  val to_string : t -> string
  (** [to_string t] converts a behavior to its wire format string
      representation. *)

  val of_string : string -> t
  (** [of_string s] parses a behavior from its wire format string
      representation.
      @raise Invalid_argument if the string is not a valid behavior. *)
end

(** {1 Permission Rules} *)

module Rule : sig
  (** Rules define specific permissions for tools. *)

  type t
  (** The type of permission rules. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for permission rules. Preserves unknown fields
      for forward compatibility. *)

  val create :
    tool_name:string -> ?rule_content:string -> ?unknown:Unknown.t -> unit -> t
  (** [create ~tool_name ?rule_content ?unknown ()] creates a new rule.
      @param tool_name The name of the tool this rule applies to
      @param rule_content Optional rule specification or pattern
      @param unknown Optional unknown fields to preserve *)

  val tool_name : t -> string
  (** [tool_name t] returns the tool name. *)

  val rule_content : t -> string option
  (** [rule_content t] returns the optional rule content. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns the unknown fields. *)
end

(** {1 Permission Updates} *)

module Update : sig
  (** Updates modify permission settings. *)

  (** The destination for permission updates. *)
  type destination =
    | User_settings  (** Apply to user settings *)
    | Project_settings  (** Apply to project settings *)
    | Local_settings  (** Apply to local settings *)
    | Session  (** Apply to current session only *)

  (** The type of permission update. *)
  type update_type =
    | Add_rules  (** Add new rules *)
    | Replace_rules  (** Replace existing rules *)
    | Remove_rules  (** Remove rules *)
    | Set_mode  (** Set permission mode *)
    | Add_directories  (** Add allowed directories *)
    | Remove_directories  (** Remove allowed directories *)

  type t
  (** The type of permission updates. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for permission updates. Wire format uses
      camelCase for destination ("userSettings", "projectSettings",
      "localSettings", "session") and update_type ("addRules", "replaceRules",
      "removeRules", "setMode", "addDirectories", "removeDirectories"). *)

  val create :
    update_type:update_type ->
    ?rules:Rule.t list ->
    ?behavior:Behavior.t ->
    ?mode:Mode.t ->
    ?directories:string list ->
    ?destination:destination ->
    ?unknown:Unknown.t ->
    unit ->
    t
  (** [create ~update_type ?rules ?behavior ?mode ?directories ?destination
       ?unknown ()] creates a new permission update.
      @param update_type The type of update to perform
      @param rules Optional list of rules to add/remove/replace
      @param behavior Optional behavior to set
      @param mode Optional permission mode to set
      @param directories Optional directories to add/remove
      @param destination Optional destination for the update
      @param unknown Optional unknown fields to preserve *)

  val update_type : t -> update_type
  (** [update_type t] returns the update type. *)

  val rules : t -> Rule.t list option
  (** [rules t] returns the optional list of rules. *)

  val behavior : t -> Behavior.t option
  (** [behavior t] returns the optional behavior. *)

  val mode : t -> Mode.t option
  (** [mode t] returns the optional mode. *)

  val directories : t -> string list option
  (** [directories t] returns the optional list of directories. *)

  val destination : t -> destination option
  (** [destination t] returns the optional destination. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns the unknown fields. *)
end

(** {1 Permission Context} *)

module Context : sig
  (** Context provided to permission callbacks. *)

  type t
  (** The type of permission context. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for permission context. Preserves unknown
      fields for forward compatibility. *)

  val create : ?suggestions:Update.t list -> ?unknown:Unknown.t -> unit -> t
  (** [create ?suggestions ?unknown ()] creates a new context.
      @param suggestions Optional list of suggested permission updates
      @param unknown Optional unknown fields to preserve *)

  val suggestions : t -> Update.t list
  (** [suggestions t] returns the list of suggested updates. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns the unknown fields. *)
end

(** {1 Permission Results} *)

module Result : sig
  (** Results of permission checks. *)

  type t =
    | Allow of {
        updated_input : Jsont.json option;  (** Modified tool input *)
        updated_permissions : Update.t list option;
            (** Permission updates to apply *)
        unknown : Unknown.t;  (** Unknown fields *)
      }
    | Deny of {
        message : string;  (** Reason for denial *)
        interrupt : bool;  (** Whether to interrupt execution *)
        unknown : Unknown.t;  (** Unknown fields *)
      }
        (** The type of permission results. Wire format uses a discriminated
            union with "behavior" field set to "allow" or "deny". *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for permission results. Preserves unknown
      fields for forward compatibility. *)

  val allow :
    ?updated_input:Jsont.json ->
    ?updated_permissions:Update.t list ->
    ?unknown:Unknown.t ->
    unit ->
    t
  (** [allow ?updated_input ?updated_permissions ?unknown ()] creates an allow
      result.
      @param updated_input Optional modified tool input
      @param updated_permissions Optional permission updates to apply
      @param unknown Optional unknown fields to preserve *)

  val deny : message:string -> interrupt:bool -> ?unknown:Unknown.t -> unit -> t
  (** [deny ~message ~interrupt ?unknown ()] creates a deny result.
      @param message The reason for denying permission
      @param interrupt Whether to interrupt further execution
      @param unknown Optional unknown fields to preserve *)
end
