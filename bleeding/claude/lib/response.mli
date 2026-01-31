(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** High-level response events from Claude.

    This module provides a unified interface for handling different types of
    responses from Claude. It converts low-level message and content block types
    into high-level response events that are easier to work with in application
    code. *)

(** {1 Response Event Types} *)

module Text : sig
  (** Text content from the assistant. *)

  type t
  (** The type of text response events (opaque). *)

  val content : t -> string
  (** [content t] returns the text content. *)

  val of_block : Content_block.Text.t -> t
  (** [of_block block] creates a text response from a content block. *)
end

module Tool_use : sig
  (** Tool invocation request from the assistant. *)

  type t
  (** The type of tool use response events (opaque). *)

  val id : t -> string
  (** [id t] returns the unique identifier of the tool use. *)

  val name : t -> string
  (** [name t] returns the name of the tool being invoked. *)

  val input : t -> Tool_input.t
  (** [input t] returns the input parameters for the tool. *)

  val of_block : Content_block.Tool_use.t -> t
  (** [of_block block] creates a tool use response from a content block. *)
end

module Thinking : sig
  (** Internal reasoning from the assistant. *)

  type t
  (** The type of thinking response events (opaque). *)

  val content : t -> string
  (** [content t] returns the thinking content. *)

  val signature : t -> string
  (** [signature t] returns the cryptographic signature. *)

  val of_block : Content_block.Thinking.t -> t
  (** [of_block block] creates a thinking response from a content block. *)
end

module Init : sig
  (** Session initialization event. *)

  type t
  (** The type of init response events (opaque). *)

  val session_id : t -> string option
  (** [session_id t] returns the optional session identifier. *)

  val model : t -> string option
  (** [model t] returns the optional model name. *)

  val cwd : t -> string option
  (** [cwd t] returns the optional current working directory. *)

  val of_system : Message.System.t -> t option
  (** [of_system sys] returns Some if system message is init, None if error. *)
end

module Error : sig
  (** Error events from system or assistant. *)

  type t
  (** The type of error response events (opaque). *)

  val message : t -> string
  (** [message t] returns the error message. *)

  val is_system_error : t -> bool
  (** [is_system_error t] returns true if this is a system error. *)

  val is_assistant_error : t -> bool
  (** [is_assistant_error t] returns true if this is an assistant error. *)

  val of_system : Message.System.t -> t option
  (** [of_system sys] returns Some if system message is error, None if init. *)

  val of_assistant : Message.Assistant.t -> t option
  (** [of_assistant msg] returns Some if assistant has error, None otherwise. *)
end

module Complete : sig
  (** Session completion event with final results. *)

  type t
  (** The type of completion response events (opaque). *)

  val duration_ms : t -> int
  (** [duration_ms t] returns the total duration in milliseconds. *)

  val num_turns : t -> int
  (** [num_turns t] returns the number of conversation turns. *)

  val session_id : t -> string
  (** [session_id t] returns the session identifier. *)

  val total_cost_usd : t -> float option
  (** [total_cost_usd t] returns the optional total cost in USD. *)

  val usage : t -> Message.Result.Usage.t option
  (** [usage t] returns the optional usage statistics. *)

  val result_text : t -> string option
  (** [result_text t] returns the optional result string. *)

  val structured_output : t -> Jsont.json option
  (** [structured_output t] returns the optional structured JSON output. *)

  val of_result : Message.Result.t -> t
  (** [of_result result] creates a completion response from a result message. *)
end

(** {1 Response Event Union Type} *)

(** The type of response events that can be received from Claude. *)
type t =
  | Text of Text.t  (** Text content from assistant *)
  | Tool_use of Tool_use.t  (** Tool invocation request *)
  | Tool_result of Content_block.Tool_result.t
      (** Tool result (pass-through) *)
  | Thinking of Thinking.t  (** Internal reasoning *)
  | Init of Init.t  (** Session initialization *)
  | Error of Error.t  (** Error event *)
  | Complete of Complete.t  (** Session completion *)

(** {1 Conversion} *)

val of_message : Message.t -> t list
(** [of_message msg] converts a message to response events. An assistant message
    may produce multiple events (one per content block). User messages produce
    empty lists since they are not responses. *)
