(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Messages exchanged with Claude. Opaque types.

    This module provides opaque message types that wrap the proto types but hide
    the unknown fields and wire format details from the public API. *)

val src : Logs.Src.t
(** The log source for message operations *)

(** {1 User Messages} *)

module User : sig
  (** Messages sent by the user. *)

  type t
  (** The type of user messages (opaque). *)

  val of_string : string -> t
  (** [of_string s] creates a user message with simple text content. *)

  val of_blocks : Content_block.t list -> t
  (** [of_blocks blocks] creates a user message with content blocks. *)

  val with_tool_result :
    tool_use_id:string -> content:Jsont.json -> ?is_error:bool -> unit -> t
  (** [with_tool_result ~tool_use_id ~content ?is_error ()] creates a user
      message containing a tool result. Content can be a string or array. *)

  val as_text : t -> string option
  (** [as_text t] returns the text content if the message is a simple string,
      None otherwise. *)

  val blocks : t -> Content_block.t list
  (** [blocks t] returns the content blocks, or a single text block if it's a
      string message. *)

  (** {1 Internal - for lib use only} *)

  val of_proto : Proto.Message.User.t -> t
  (** [of_proto proto] wraps a proto user message. *)

  val to_proto : t -> Proto.Message.User.t
  (** [to_proto t] extracts the proto user message. *)

  val incoming_jsont : t Jsont.t
  (** Internal codec for parsing incoming messages. *)

  val to_json : t -> Jsont.json
  (** Internal conversion to JSON for wire format. *)
end

(** {1 Assistant Messages} *)

module Assistant : sig
  (** Messages from Claude assistant. *)

  type error =
    [ `Authentication_failed  (** Authentication with Claude API failed *)
    | `Billing_error  (** Billing or account issue *)
    | `Rate_limit  (** Rate limit exceeded *)
    | `Invalid_request  (** Request was invalid *)
    | `Server_error  (** Internal server error *)
    | `Unknown  (** Unknown error type *) ]
  (** The type of assistant message errors based on Python SDK error types. *)

  type t
  (** The type of assistant messages (opaque). *)

  val content : t -> Content_block.t list
  (** [content t] returns the content blocks of the assistant message. *)

  val model : t -> string
  (** [model t] returns the model identifier. *)

  val error : t -> error option
  (** [error t] returns the optional error that occurred during message
      generation. *)

  (** {2 Convenience accessors} *)

  val text_blocks : t -> string list
  (** [text_blocks t] extracts all text content from the message. *)

  val tool_uses : t -> Content_block.Tool_use.t list
  (** [tool_uses t] extracts all tool use blocks from the message. *)

  val thinking_blocks : t -> Content_block.Thinking.t list
  (** [thinking_blocks t] extracts all thinking blocks from the message. *)

  val combined_text : t -> string
  (** [combined_text t] concatenates all text blocks into a single string. *)

  val has_tool_use : t -> bool
  (** [has_tool_use t] returns true if the message contains any tool use blocks.
  *)

  (** {1 Internal - for lib use only} *)

  val of_proto : Proto.Message.Assistant.t -> t
  (** [of_proto proto] wraps a proto assistant message. *)

  val to_proto : t -> Proto.Message.Assistant.t
  (** [to_proto t] extracts the proto assistant message. *)

  val incoming_jsont : t Jsont.t
  (** Internal codec for parsing incoming messages. *)

  val to_json : t -> Jsont.json
  (** Internal conversion to JSON for wire format. *)
end

(** {1 System Messages} *)

module System : sig
  (** System control and status messages. *)

  type t
  (** The type of system messages (opaque). *)

  val is_init : t -> bool
  (** [is_init t] returns true if the message is an init message. *)

  val is_error : t -> bool
  (** [is_error t] returns true if the message is an error message. *)

  val session_id : t -> string option
  (** [session_id t] returns session_id from Init, None otherwise. *)

  val model : t -> string option
  (** [model t] returns model from Init, None otherwise. *)

  val cwd : t -> string option
  (** [cwd t] returns cwd from Init, None otherwise. *)

  val error_message : t -> string option
  (** [error_message t] returns error from Error, None otherwise. *)

  (** {1 Internal - for lib use only} *)

  val of_proto : Proto.Message.System.t -> t
  (** [of_proto proto] wraps a proto system message. *)

  val to_proto : t -> Proto.Message.System.t
  (** [to_proto t] extracts the proto system message. *)

  val jsont : t Jsont.t
  (** Internal codec for wire format. *)

  val to_json : t -> Jsont.json
  (** Internal conversion to JSON for wire format. *)
end

(** {1 Result Messages} *)

module Result : sig
  (** Final result messages with metadata about the conversation. *)

  module Usage : sig
    (** Usage statistics for API calls. *)

    type t
    (** Type for usage statistics (opaque). *)

    val input_tokens : t -> int option
    (** [input_tokens t] returns the number of input tokens used. *)

    val output_tokens : t -> int option
    (** [output_tokens t] returns the number of output tokens generated. *)

    val total_tokens : t -> int option
    (** [total_tokens t] returns the total number of tokens. *)

    val cache_creation_input_tokens : t -> int option
    (** [cache_creation_input_tokens t] returns cache creation input tokens. *)

    val cache_read_input_tokens : t -> int option
    (** [cache_read_input_tokens t] returns cache read input tokens. *)

    (** {1 Internal - for lib use only} *)

    val of_proto : Proto.Message.Result.Usage.t -> t
    (** [of_proto proto] wraps a proto usage object. *)
  end

  type t
  (** The type of result messages (opaque). *)

  val duration_ms : t -> int
  (** [duration_ms t] returns the total duration in milliseconds. *)

  val duration_api_ms : t -> int
  (** [duration_api_ms t] returns the API duration in milliseconds. *)

  val is_error : t -> bool
  (** [is_error t] returns whether this result represents an error. *)

  val num_turns : t -> int
  (** [num_turns t] returns the number of conversation turns. *)

  val session_id : t -> string
  (** [session_id t] returns the session identifier. *)

  val total_cost_usd : t -> float option
  (** [total_cost_usd t] returns the optional total cost in USD. *)

  val usage : t -> Usage.t option
  (** [usage t] returns the optional usage statistics. *)

  val result_text : t -> string option
  (** [result_text t] returns the optional result string. *)

  val structured_output : t -> Jsont.json option
  (** [structured_output t] returns the optional structured JSON output. *)

  (** {1 Internal - for lib use only} *)

  val of_proto : Proto.Message.Result.t -> t
  (** [of_proto proto] wraps a proto result message. *)

  val to_proto : t -> Proto.Message.Result.t
  (** [to_proto t] extracts the proto result message. *)

  val jsont : t Jsont.t
  (** Internal codec for wire format. *)

  val to_json : t -> Jsont.json
  (** Internal conversion to JSON for wire format. *)
end

(** {1 Message Union Type} *)

type t =
  | User of User.t
  | Assistant of Assistant.t
  | System of System.t
  | Result of Result.t
      (** The type of messages, which can be user, assistant, system, or result.
      *)

val of_proto : Proto.Message.t -> t
(** [of_proto proto] converts a proto message to a lib message. *)

val to_proto : t -> Proto.Message.t
(** [to_proto t] converts a lib message to a proto message. *)

(** {1 Internal - wire format conversion} *)

val to_json : t -> Jsont.json
(** [to_json t] converts any message to its JSON wire format representation. *)

(** {1 Convenience Constructors} *)

val user_string : string -> t
(** [user_string s] creates a user message with text content. *)

val user_blocks : Content_block.t list -> t
(** [user_blocks blocks] creates a user message with content blocks. *)

(** {1 Message Analysis} *)

val is_user : t -> bool
(** [is_user t] returns true if the message is from a user. *)

val is_assistant : t -> bool
(** [is_assistant t] returns true if the message is from the assistant. *)

val is_system : t -> bool
(** [is_system t] returns true if the message is a system message. *)

val is_result : t -> bool
(** [is_result t] returns true if the message is a result message. *)

val is_error : t -> bool
(** [is_error t] returns true if the message represents an error. *)

val extract_text : t -> string option
(** [extract_text t] attempts to extract text content from any message type. *)

val extract_tool_uses : t -> Content_block.Tool_use.t list
(** [extract_tool_uses t] extracts tool use blocks from assistant messages. *)

val get_session_id : t -> string option
(** [get_session_id t] extracts the session ID from system or result messages.
*)

(** {1 Logging} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] pretty-prints any message. *)

val log_received : t -> unit
(** [log_received t] logs that a message was received. *)

val log_sending : t -> unit
(** [log_sending t] logs that a message is being sent. *)
