(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Content blocks for Claude messages wire format.

    This module defines the wire format types for content blocks that can appear
    in Claude messages, including text, tool use, tool results, and thinking
    blocks. *)

(** {1 Text Blocks} *)

module Text : sig
  (** Plain text content blocks. *)

  type t
  (** The type of text blocks. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for text blocks. Use [Jsont.Json.encode jsont]
      and [Jsont.Json.decode jsont] for serialization. Use
      [Jsont.pp_value jsont ()] for pretty-printing. *)

  val create : string -> t
  (** [create text] creates a new text block with the given text content. *)

  val text : t -> string
  (** [text t] returns the text content of the block. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns any unknown fields from JSON parsing. *)
end

(** {1 Tool Use Blocks} *)

module Tool_use : sig
  (** Tool invocation requests from the assistant. *)

  type t
  (** The type of tool use blocks. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for tool use blocks. Use
      [Jsont.Json.encode jsont] and [Jsont.Json.decode jsont] for serialization.
      Use [Jsont.pp_value jsont ()] for pretty-printing. *)

  val create : id:string -> name:string -> input:Jsont.json -> t
  (** [create ~id ~name ~input] creates a new tool use block.
      @param id Unique identifier for this tool invocation
      @param name Name of the tool to invoke
      @param input Parameters for the tool as raw JSON *)

  val id : t -> string
  (** [id t] returns the unique identifier of the tool use. *)

  val name : t -> string
  (** [name t] returns the name of the tool being invoked. *)

  val input : t -> Jsont.json
  (** [input t] returns the input parameters for the tool as raw JSON. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns any unknown fields from JSON parsing. *)
end

(** {1 Tool Result Blocks} *)

module Tool_result : sig
  (** Results from tool invocations. *)

  type t
  (** The type of tool result blocks. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for tool result blocks. Use
      [Jsont.Json.encode jsont] and [Jsont.Json.decode jsont] for serialization.
      Use [Jsont.pp_value jsont ()] for pretty-printing. *)

  val create :
    tool_use_id:string -> ?content:Jsont.json -> ?is_error:bool -> unit -> t
  (** [create ~tool_use_id ?content ?is_error ()] creates a new tool result
      block.
      @param tool_use_id The ID of the corresponding tool use block
      @param content
        Optional result content (can be string or array of content blocks)
      @param is_error Whether the tool execution resulted in an error *)

  val tool_use_id : t -> string
  (** [tool_use_id t] returns the ID of the corresponding tool use. *)

  val content : t -> Jsont.json option
  (** [content t] returns the optional result content as raw JSON. *)

  val is_error : t -> bool option
  (** [is_error t] returns whether this result represents an error. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns any unknown fields from JSON parsing. *)
end

(** {1 Thinking Blocks} *)

module Thinking : sig
  (** Assistant's internal reasoning blocks. *)

  type t
  (** The type of thinking blocks. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for thinking blocks. Use
      [Jsont.Json.encode jsont] and [Jsont.Json.decode jsont] for serialization.
      Use [Jsont.pp_value jsont ()] for pretty-printing. *)

  val create : thinking:string -> signature:string -> t
  (** [create ~thinking ~signature] creates a new thinking block.
      @param thinking The assistant's internal reasoning
      @param signature Cryptographic signature for verification *)

  val thinking : t -> string
  (** [thinking t] returns the thinking content. *)

  val signature : t -> string
  (** [signature t] returns the cryptographic signature. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns any unknown fields from JSON parsing. *)
end

(** {1 Content Block Union Type} *)

type t =
  | Text of Text.t
  | Tool_use of Tool_use.t
  | Tool_result of Tool_result.t
  | Thinking of Thinking.t
      (** The type of content blocks, which can be text, tool use, tool result,
          or thinking. *)

val jsont : t Jsont.t
(** [jsont] is the Jsont codec for content blocks. Use [Jsont.Json.encode jsont]
    and [Jsont.Json.decode jsont] for serialization. Use
    [Jsont.pp_value jsont ()] for pretty-printing. *)

val text : string -> t
(** [text s] creates a text content block. *)

val tool_use : id:string -> name:string -> input:Jsont.json -> t
(** [tool_use ~id ~name ~input] creates a tool use content block. *)

val tool_result :
  tool_use_id:string -> ?content:Jsont.json -> ?is_error:bool -> unit -> t
(** [tool_result ~tool_use_id ?content ?is_error ()] creates a tool result
    content block. Content can be a string or an array of content blocks. *)

val thinking : thinking:string -> signature:string -> t
(** [thinking ~thinking ~signature] creates a thinking content block. *)
