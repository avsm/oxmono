(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Messages exchanged with Claude wire format.

    This module defines the wire format types for messages that can be sent to
    and received from Claude, including user input, assistant responses, system
    messages, and result metadata. *)

(** {1 User Messages} *)

module User : sig
  (** Messages sent by the user. *)

  (** The content of a user message. *)
  type content =
    | String of string  (** Simple text message *)
    | Blocks of Content_block.t list
        (** Complex message with multiple content blocks *)

  type t
  (** The type of user messages. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for user messages. *)

  val incoming_jsont : t Jsont.t
  (** [incoming_jsont] is the codec for parsing incoming user messages from CLI.
      This parses the envelope format with "message" wrapper. *)

  val outgoing_jsont : t Jsont.t
  (** [outgoing_jsont] is the codec for encoding outgoing user messages to CLI.
      This produces the envelope format with "message" wrapper containing "role"
      and "content" fields. *)

  val create_string : string -> t
  (** [create_string s] creates a user message with simple text content. *)

  val create_blocks : Content_block.t list -> t
  (** [create_blocks blocks] creates a user message with content blocks. *)

  val create_with_tool_result :
    tool_use_id:string -> content:Jsont.json -> ?is_error:bool -> unit -> t
  (** [create_with_tool_result ~tool_use_id ~content ?is_error ()] creates a
      user message containing a tool result. Content can be a string or array.
  *)

  val content : t -> content
  (** [content t] returns the content of the user message. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns the unknown fields preserved from JSON. *)
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
  (** The type of assistant messages. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for assistant messages. *)

  val incoming_jsont : t Jsont.t
  (** [incoming_jsont] is the codec for parsing incoming assistant messages from
      CLI. This parses the envelope format with "message" wrapper. *)

  val create :
    content:Content_block.t list -> model:string -> ?error:error -> unit -> t
  (** [create ~content ~model ?error ()] creates an assistant message.
      @param content List of content blocks in the response
      @param model The model identifier used for the response
      @param error Optional error that occurred during message generation *)

  val content : t -> Content_block.t list
  (** [content t] returns the content blocks of the assistant message. *)

  val model : t -> string
  (** [model t] returns the model identifier. *)

  val error : t -> error option
  (** [error t] returns the optional error that occurred during message
      generation. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns the unknown fields preserved from JSON. *)
end

(** {1 System Messages} *)

module System : sig
  (** System control and status messages.

      System messages use a discriminated union on the "subtype" field:
      - "init": Session initialization with session_id, model, cwd
      - "error": Error messages with error string *)

  type init = {
    session_id : string option;
    model : string option;
    cwd : string option;
    unknown : Unknown.t;
  }
  (** Init message fields. *)

  type error = { error : string; unknown : Unknown.t }
  (** Error message fields. *)

  type t = Init of init | Error of error

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for system messages. *)

  (** {2 Constructors} *)

  val init : ?session_id:string -> ?model:string -> ?cwd:string -> unit -> t
  (** [init ?session_id ?model ?cwd ()] creates an init message. *)

  val error : error:string -> t
  (** [error ~error] creates an error message. *)

  (** {2 Accessors} *)

  val session_id : t -> string option
  (** [session_id t] returns session_id from Init, None otherwise. *)

  val model : t -> string option
  (** [model t] returns model from Init, None otherwise. *)

  val cwd : t -> string option
  (** [cwd t] returns cwd from Init, None otherwise. *)

  val error_msg : t -> string option
  (** [error_msg t] returns error from Error, None otherwise. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns the unknown fields. *)
end

(** {1 Result Messages} *)

module Result : sig
  (** Final result messages with metadata about the conversation. *)

  module Usage : sig
    (** Usage statistics for API calls. *)

    type t
    (** Type for usage statistics. *)

    val jsont : t Jsont.t
    (** [jsont] is the Jsont codec for usage statistics. *)

    val create :
      ?input_tokens:int ->
      ?output_tokens:int ->
      ?total_tokens:int ->
      ?cache_creation_input_tokens:int ->
      ?cache_read_input_tokens:int ->
      unit ->
      t
    (** [create ?input_tokens ?output_tokens ?total_tokens
         ?cache_creation_input_tokens ?cache_read_input_tokens ()] creates usage
        statistics. *)

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

    val unknown : t -> Unknown.t
    (** [unknown t] returns the unknown fields preserved from JSON. *)
  end

  type t
  (** The type of result messages. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for result messages. *)

  val create :
    subtype:string ->
    duration_ms:int ->
    duration_api_ms:int ->
    is_error:bool ->
    num_turns:int ->
    session_id:string ->
    ?total_cost_usd:float ->
    ?usage:Usage.t ->
    ?result:string ->
    ?structured_output:Jsont.json ->
    unit ->
    t
  (** [create ~subtype ~duration_ms ~duration_api_ms ~is_error ~num_turns
       ~session_id ?total_cost_usd ?usage ?result ?structured_output ()] creates
      a result message.
      @param subtype The subtype of the result
      @param duration_ms Total duration in milliseconds
      @param duration_api_ms API duration in milliseconds
      @param is_error Whether the result represents an error
      @param num_turns Number of conversation turns
      @param session_id Unique session identifier
      @param total_cost_usd Optional total cost in USD
      @param usage Optional usage statistics
      @param result Optional result string
      @param structured_output Optional structured JSON output from Claude *)

  val subtype : t -> string
  (** [subtype t] returns the subtype of the result. *)

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

  val result : t -> string option
  (** [result t] returns the optional result string. *)

  val structured_output : t -> Jsont.json option
  (** [structured_output t] returns the optional structured JSON output. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns the unknown fields preserved from JSON. *)
end

(** {1 Message Union Type} *)

type t =
  | User of User.t
  | Assistant of Assistant.t
  | System of System.t
  | Result of Result.t
      (** The type of messages, which can be user, assistant, system, or result.
      *)

val jsont : t Jsont.t
(** [jsont] is the Jsont codec for messages. *)
