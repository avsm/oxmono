(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** SDK Control Protocol for Claude.

    This module defines the typed SDK control protocol for bidirectional
    communication between the SDK and the Claude CLI. It handles:

    - Permission requests (tool usage authorization)
    - Hook callbacks (intercepting and modifying tool execution)
    - Dynamic control (changing settings mid-conversation)
    - Server introspection (querying capabilities)

    {2 Protocol Overview}

    The SDK control protocol is a JSON-based request/response protocol that runs
    alongside the main message stream. It enables:

    1. {b Callbacks}: Claude asks the SDK for permission or hook execution 2.
    {b Control}: SDK changes Claude's behavior dynamically 3. {b Introspection}:
    SDK queries server metadata

    {2 Request/Response Flow}

    {v
      SDK                          Claude CLI
       |                               |
       |-- Initialize (with hooks) --> |
       |<-- Permission Request --------|  (for tool usage)
       |-- Allow/Deny Response ------> |
       |                               |
       |<-- Hook Callback -------------|  (pre/post tool)
       |-- Hook Result -------------> |
       |                               |
       |-- Set Model ---------------> |  (dynamic control)
       |<-- Success Response ----------|
       |                               |
       |-- Get Server Info ----------> |
       |<-- Server Info Response ------|
    v}

    {2 Usage}

    Most users won't interact with this module directly. The {!Client} module
    handles the protocol automatically. However, this module is exposed for:

    - Understanding the control protocol
    - Implementing custom control logic
    - Debugging control message flow
    - Advanced SDK extensions

    {2 Dynamic Control Examples}

    See {!Client.set_permission_mode}, {!Client.set_model}, and
    {!Client.get_server_info} for high-level APIs that use this protocol. *)

val src : Logs.Src.t
(** The log source for SDK control operations *)

(** {1 Request Types} *)

module Request : sig
  (** SDK control request types. *)

  type interrupt = { subtype : [ `Interrupt ]; unknown : Unknown.t }
  (** Interrupt request to stop execution. *)

  type permission = {
    subtype : [ `Can_use_tool ];
    tool_name : string;
    input : Jsont.json;
    permission_suggestions : Proto.Permissions.Update.t list option;
    blocked_path : string option;
    unknown : Unknown.t;
  }
  (** Permission request for tool usage. *)

  type initialize = {
    subtype : [ `Initialize ];
    hooks : (string * Jsont.json) list option; (* Hook event to configuration *)
    unknown : Unknown.t;
  }
  (** Initialize request with optional hook configuration. *)

  type set_permission_mode = {
    subtype : [ `Set_permission_mode ];
    mode : Proto.Permissions.Mode.t;
    unknown : Unknown.t;
  }
  (** Request to change permission mode. *)

  type hook_callback = {
    subtype : [ `Hook_callback ];
    callback_id : string;
    input : Jsont.json;
    tool_use_id : string option;
    unknown : Unknown.t;
  }
  (** Hook callback request. *)

  type mcp_message = {
    subtype : [ `Mcp_message ];
    server_name : string;
    message : Jsont.json;
    unknown : Unknown.t;
  }
  (** MCP server message request. *)

  type set_model = {
    subtype : [ `Set_model ];
    model : string;
    unknown : Unknown.t;
  }
  (** Request to change the AI model. *)

  type get_server_info = { subtype : [ `Get_server_info ]; unknown : Unknown.t }
  (** Request to get server information. *)

  type t =
    | Interrupt of interrupt
    | Permission of permission
    | Initialize of initialize
    | Set_permission_mode of set_permission_mode
    | Hook_callback of hook_callback
    | Mcp_message of mcp_message
    | Set_model of set_model
    | Get_server_info of get_server_info
        (** The type of SDK control requests. *)

  val interrupt : ?unknown:Unknown.t -> unit -> t
  (** [interrupt ?unknown ()] creates an interrupt request. *)

  val permission :
    tool_name:string ->
    input:Jsont.json ->
    ?permission_suggestions:Proto.Permissions.Update.t list ->
    ?blocked_path:string ->
    ?unknown:Unknown.t ->
    unit ->
    t
  (** [permission ~tool_name ~input ?permission_suggestions ?blocked_path
       ?unknown ()] creates a permission request. *)

  val initialize :
    ?hooks:(string * Jsont.json) list -> ?unknown:Unknown.t -> unit -> t
  (** [initialize ?hooks ?unknown ()] creates an initialize request. *)

  val set_permission_mode :
    mode:Proto.Permissions.Mode.t -> ?unknown:Unknown.t -> unit -> t
  (** [set_permission_mode ~mode ?unknown] creates a permission mode change
      request. *)

  val hook_callback :
    callback_id:string ->
    input:Jsont.json ->
    ?tool_use_id:string ->
    ?unknown:Unknown.t ->
    unit ->
    t
  (** [hook_callback ~callback_id ~input ?tool_use_id ?unknown ()] creates a
      hook callback request. *)

  val mcp_message :
    server_name:string -> message:Jsont.json -> ?unknown:Unknown.t -> unit -> t
  (** [mcp_message ~server_name ~message ?unknown] creates an MCP message
      request. *)

  val set_model : model:string -> ?unknown:Unknown.t -> unit -> t
  (** [set_model ~model ?unknown] creates a model change request. *)

  val get_server_info : ?unknown:Unknown.t -> unit -> t
  (** [get_server_info ?unknown ()] creates a server info request. *)

  val jsont : t Jsont.t
  (** [jsont] is the jsont codec for requests. Use [Jsont.pp_value jsont ()] for
      pretty-printing. *)
end

(** {1 Response Types} *)

module Response : sig
  (** SDK control response types. *)

  module Error_code = Proto.Control.Response.Error_code
  (** Re-export Error_code from Proto for convenience. *)

  type error_detail = {
    code : int;  (** Error code for programmatic handling *)
    message : string;  (** Human-readable error message *)
    data : Jsont.json option;  (** Optional additional error data *)
  }
  (** Structured error detail similar to JSON-RPC.

      This allows programmatic error handling with numeric error codes and
      optional structured data for additional context. *)

  val error_detail :
    code:[< Error_code.t ] ->
    message:string ->
    ?data:Jsont.json ->
    unit ->
    error_detail
  (** [error_detail ~code ~message ?data ()] creates a structured error detail
      using typed error codes.

      Example:
      {[
        error_detail ~code:`Method_not_found ~message:"Hook callback not found"
          ()
      ]} *)

  val error_detail_jsont : error_detail Jsont.t
  (** [error_detail_jsont] is the Jsont codec for error details. *)

  type success = {
    subtype : [ `Success ];
    request_id : string;
    response : Jsont.json option;
    unknown : Unknown.t;
  }
  (** Successful response. *)

  type error = {
    subtype : [ `Error ];
    request_id : string;
    error : error_detail;
    unknown : Unknown.t;
  }
  (** Error response with structured error detail. *)

  type t =
    | Success of success
    | Error of error  (** The type of SDK control responses. *)

  val success :
    request_id:string -> ?response:Jsont.json -> ?unknown:Unknown.t -> unit -> t
  (** [success ~request_id ?response ?unknown ()] creates a success response. *)

  val error :
    request_id:string -> error:error_detail -> ?unknown:Unknown.t -> unit -> t
  (** [error ~request_id ~error ?unknown] creates an error response with
      structured error detail. *)

  val jsont : t Jsont.t
  (** [jsont] is the jsont codec for responses. Use [Jsont.pp_value jsont ()]
      for pretty-printing. *)
end

(** {1 Control Messages} *)

type control_request = {
  type_ : [ `Control_request ];
  request_id : string;
  request : Request.t;
  unknown : Unknown.t;
}
(** Control request message. *)

type control_response = {
  type_ : [ `Control_response ];
  response : Response.t;
  unknown : Unknown.t;
}
(** Control response message. *)

val control_request_jsont : control_request Jsont.t
(** [control_request_jsont] is the jsont codec for control request messages. *)

val control_response_jsont : control_response Jsont.t
(** [control_response_jsont] is the jsont codec for control response messages.
*)

type t =
  | Request of control_request
  | Response of control_response  (** The type of SDK control messages. *)

val create_request :
  request_id:string -> request:Request.t -> ?unknown:Unknown.t -> unit -> t
(** [create_request ~request_id ~request ?unknown ()] creates a control request
    message. *)

val create_response : response:Response.t -> ?unknown:Unknown.t -> unit -> t
(** [create_response ~response ?unknown ()] creates a control response message.
*)

val jsont : t Jsont.t
(** [jsont] is the jsont codec for control messages. Use
    [Jsont.pp_value jsont ()] for pretty-printing. *)

(** {1 Logging} *)

val log_request : Request.t -> unit
(** [log_request req] logs an SDK control request. *)

val log_response : Response.t -> unit
(** [log_response resp] logs an SDK control response. *)

(** {1 Server Information}

    Server information provides metadata about the Claude CLI server, including
    version, capabilities, available commands, and output styles.

    {2 Use Cases}

    - Feature detection: Check if specific capabilities are available
    - Version compatibility: Ensure minimum version requirements
    - Debugging: Log server information for troubleshooting
    - Dynamic adaptation: Adjust SDK behavior based on capabilities

    {2 Example}

    {[
      let info = Client.get_server_info client in
      Printf.printf "Claude CLI version: %s\n" (Server_info.version info);

      if List.mem "structured-output" (Server_info.capabilities info) then
        Printf.printf "Structured output is supported\n"
      else Printf.printf "Structured output not available\n"
    ]} *)

module Server_info : sig
  (** Server information and capabilities. *)

  type t = {
    version : string;  (** Server version string (e.g., "2.0.0") *)
    capabilities : string list;
        (** Available server capabilities (e.g., "hooks", "structured-output")
        *)
    commands : string list;  (** Available CLI commands *)
    output_styles : string list;
        (** Supported output formats (e.g., "json", "stream-json") *)
    unknown : Unknown.t;  (** Unknown fields for forward compatibility *)
  }
  (** Server metadata and capabilities.

      This information is useful for feature detection and debugging. *)

  val create :
    version:string ->
    capabilities:string list ->
    commands:string list ->
    output_styles:string list ->
    ?unknown:Unknown.t ->
    unit ->
    t
  (** [create ~version ~capabilities ~commands ~output_styles ?unknown ()]
      creates server info. *)

  val version : t -> string
  (** [version t] returns the server version. *)

  val capabilities : t -> string list
  (** [capabilities t] returns the server capabilities. *)

  val commands : t -> string list
  (** [commands t] returns available commands. *)

  val output_styles : t -> string list
  (** [output_styles t] returns available output styles. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns the unknown fields. *)

  val jsont : t Jsont.t
  (** [jsont] is the jsont codec for server info. Use [Jsont.pp_value jsont ()]
      for pretty-printing. *)
end
