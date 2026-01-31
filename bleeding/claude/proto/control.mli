(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Control protocol wire format for SDK communication.

    This module defines the wire format for the SDK control protocol used for
    bidirectional communication between the SDK and the Claude CLI. It handles
    JSON serialization and deserialization of control messages.

    The control protocol enables:
    - Permission requests for tool usage authorization
    - Hook callbacks for intercepting and modifying tool execution
    - Dynamic control for changing settings mid-conversation
    - Server introspection for querying capabilities *)

(** {1 Request Types} *)

module Request : sig
  (** SDK control request types. *)

  type permission_r = private {
    tool_name : string;
    input : Jsont.json;
    permission_suggestions : Permissions.Update.t list option;
    blocked_path : string option;
    unknown : Unknown.t;
  }

  type initialize_r = private {
    hooks : (string * Jsont.json) list option;
    unknown : Unknown.t;
  }

  type set_permission_mode_r = private {
    mode : Permissions.Mode.t;
    unknown : Unknown.t;
  }

  type hook_callback_r = private {
    callback_id : string;
    input : Jsont.json;
    tool_use_id : string option;
    unknown : Unknown.t;
  }

  type mcp_message_r = private {
    server_name : string;
    message : Jsont.json;
    unknown : Unknown.t;
  }

  type set_model_r = private { model : string; unknown : Unknown.t }

  type t =
    | Interrupt
    | Permission of permission_r
    | Initialize of initialize_r
    | Set_permission_mode of set_permission_mode_r
    | Hook_callback of hook_callback_r
    | Mcp_message of mcp_message_r
    | Set_model of set_model_r
    | Get_server_info
        (** The type of SDK control requests. Wire format uses "subtype" field:
            "interrupt", "canUseTool", "initialize", "setPermissionMode",
            "hookCallback", "mcpMessage", "setModel", "getServerInfo". *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for requests. *)

  val interrupt : unit -> t
  (** [interrupt ()] creates an interrupt request. *)

  val permission :
    tool_name:string ->
    input:Jsont.json ->
    ?permission_suggestions:Permissions.Update.t list ->
    ?blocked_path:string ->
    unit ->
    t
  (** [permission ~tool_name ~input ?permission_suggestions ?blocked_path ()]
      creates a permission request. *)

  val initialize : ?hooks:(string * Jsont.json) list -> unit -> t
  (** [initialize ?hooks ()] creates an initialize request. *)

  val set_permission_mode : mode:Permissions.Mode.t -> unit -> t
  (** [set_permission_mode ~mode ()] creates a permission mode change request.
  *)

  val hook_callback :
    callback_id:string -> input:Jsont.json -> ?tool_use_id:string -> unit -> t
  (** [hook_callback ~callback_id ~input ?tool_use_id ()] creates a hook
      callback request. *)

  val mcp_message : server_name:string -> message:Jsont.json -> unit -> t
  (** [mcp_message ~server_name ~message ()] creates an MCP message request. *)

  val set_model : model:string -> unit -> t
  (** [set_model ~model ()] creates a model change request. *)

  val get_server_info : unit -> t
  (** [get_server_info ()] creates a server info request. *)
end

(** {1 Response Types} *)

module Response : sig
  (** SDK control response types. *)

  (** Standard JSON-RPC 2.0 error codes.

      These codes follow the JSON-RPC 2.0 specification for structured error
      responses. Using the typed codes instead of raw integers improves code
      clarity and prevents typos. Polymorphic variants allow for easy extension.
  *)
  module Error_code : sig
    type t =
      [ `Parse_error  (** -32700: Invalid JSON received *)
      | `Invalid_request  (** -32600: The request object is invalid *)
      | `Method_not_found  (** -32601: The requested method does not exist *)
      | `Invalid_params  (** -32602: Invalid method parameters *)
      | `Internal_error  (** -32603: Internal server error *)
      | `Custom of int  (** Application-specific error codes *) ]

    val to_int : [< t ] -> int
    (** [to_int t] converts an error code to its integer representation. *)

    val of_int : int -> t
    (** [of_int n] converts an integer to an error code. Standard codes are
        mapped to their variants, others become [`Custom n]. *)
  end

  type error_detail = {
    code : int;  (** Error code for programmatic handling *)
    message : string;  (** Human-readable error message *)
    data : Jsont.json option;  (** Optional additional error data *)
  }
  (** Structured error detail similar to JSON-RPC. *)

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

  type success_r = private {
    request_id : string;
    response : Jsont.json option;
    unknown : Unknown.t;
  }

  type error_r = private {
    request_id : string;
    error : error_detail;
    unknown : Unknown.t;
  }

  type t =
    | Success of success_r
    | Error of error_r
        (** The type of SDK control responses. Wire format uses "subtype" field:
            "success", "error". *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for responses. *)

  val success : request_id:string -> ?response:Jsont.json -> unit -> t
  (** [success ~request_id ?response ()] creates a success response. *)

  val error : request_id:string -> error:error_detail -> unit -> t
  (** [error ~request_id ~error ()] creates an error response with structured
      error detail. *)
end

(** {1 Control Envelopes} *)

type request_envelope = {
  request_id : string;
  request : Request.t;
  unknown : Unknown.t;
}
(** Control request envelope. Wire format has "type": "control_request". *)

type response_envelope = { response : Response.t; unknown : Unknown.t }
(** Control response envelope. Wire format has "type": "control_response". *)

val request_envelope_jsont : request_envelope Jsont.t
(** [request_envelope_jsont] is the Jsont codec for request envelopes. *)

val response_envelope_jsont : response_envelope Jsont.t
(** [response_envelope_jsont] is the Jsont codec for response envelopes. *)

val create_request :
  request_id:string -> request:Request.t -> unit -> request_envelope
(** [create_request ~request_id ~request ()] creates a control request envelope.
*)

val create_response : response:Response.t -> unit -> response_envelope
(** [create_response ~response ()] creates a control response envelope. *)

(** {1 Server Information} *)

module Server_info : sig
  (** Server information and capabilities. *)

  type t
  (** Server metadata and capabilities. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for server info. *)

  val create :
    version:string ->
    capabilities:string list ->
    commands:string list ->
    output_styles:string list ->
    unit ->
    t
  (** [create ~version ~capabilities ~commands ~output_styles ()] creates server
      info. *)

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
end
