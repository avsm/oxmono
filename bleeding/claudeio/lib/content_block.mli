(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Content blocks in messages. Opaque types without wire concerns.

    This module provides opaque wrapper types around the proto content block
    types, hiding unknown fields and wire format details from the public API. *)

val src : Logs.Src.t
(** Log source for content block operations. *)

(** {1 Text Blocks} *)

module Text : sig
  (** Plain text content blocks. *)

  type t
  (** The type of text blocks (opaque). *)

  val text : t -> string
  (** [text t] returns the text content of the block. *)

  (** {1 Internal - for lib use only} *)

  val of_proto : Proto.Content_block.Text.t -> t
  (** [of_proto proto] wraps a proto text block. *)

  val to_proto : t -> Proto.Content_block.Text.t
  (** [to_proto t] extracts the proto text block. *)
end

(** {1 Tool Use Blocks} *)

module Tool_use : sig
  (** Tool invocation requests from the assistant. *)

  type t
  (** The type of tool use blocks (opaque). *)

  val id : t -> string
  (** [id t] returns the unique identifier of the tool use. *)

  val name : t -> string
  (** [name t] returns the name of the tool being invoked. *)

  val input : t -> Tool_input.t
  (** [input t] returns the input parameters for the tool. *)

  (** {1 Internal - for lib use only} *)

  val of_proto : Proto.Content_block.Tool_use.t -> t
  (** [of_proto proto] wraps a proto tool use block. *)

  val to_proto : t -> Proto.Content_block.Tool_use.t
  (** [to_proto t] extracts the proto tool use block. *)
end

(** {1 Tool Result Blocks} *)

module Tool_result : sig
  (** Results from tool invocations. *)

  type t
  (** The type of tool result blocks (opaque). *)

  val tool_use_id : t -> string
  (** [tool_use_id t] returns the ID of the corresponding tool use. *)

  val content : t -> Jsont.json option
  (** [content t] returns the optional result content as raw JSON. *)

  val is_error : t -> bool option
  (** [is_error t] returns whether this result represents an error. *)

  (** {1 Internal - for lib use only} *)

  val of_proto : Proto.Content_block.Tool_result.t -> t
  (** [of_proto proto] wraps a proto tool result block. *)

  val to_proto : t -> Proto.Content_block.Tool_result.t
  (** [to_proto t] extracts the proto tool result block. *)
end

(** {1 Thinking Blocks} *)

module Thinking : sig
  (** Assistant's internal reasoning blocks. *)

  type t
  (** The type of thinking blocks (opaque). *)

  val thinking : t -> string
  (** [thinking t] returns the thinking content. *)

  val signature : t -> string
  (** [signature t] returns the cryptographic signature. *)

  (** {1 Internal - for lib use only} *)

  val of_proto : Proto.Content_block.Thinking.t -> t
  (** [of_proto proto] wraps a proto thinking block. *)

  val to_proto : t -> Proto.Content_block.Thinking.t
  (** [to_proto t] extracts the proto thinking block. *)
end

(** {1 Content Block Union Type} *)

type t =
  | Text of Text.t
  | Tool_use of Tool_use.t
  | Tool_result of Tool_result.t
  | Thinking of Thinking.t
      (** The type of content blocks, which can be text, tool use, tool result,
          or thinking. *)

(** {1 Constructors} *)

val text : string -> t
(** [text s] creates a text content block. *)

val tool_use : id:string -> name:string -> input:Tool_input.t -> t
(** [tool_use ~id ~name ~input] creates a tool use content block. *)

val tool_result :
  tool_use_id:string -> ?content:Jsont.json -> ?is_error:bool -> unit -> t
(** [tool_result ~tool_use_id ?content ?is_error ()] creates a tool result
    content block. Content can be a string or array. *)

val thinking : thinking:string -> signature:string -> t
(** [thinking ~thinking ~signature] creates a thinking content block. *)

(** {1 Conversion} *)

val of_proto : Proto.Content_block.t -> t
(** [of_proto proto] converts a proto content block to a lib content block. *)

val to_proto : t -> Proto.Content_block.t
(** [to_proto t] converts a lib content block to a proto content block. *)

(** {1 Logging} *)

val log_received : t -> unit
(** [log_received t] logs that a content block was received. *)

val log_sending : t -> unit
(** [log_sending t] logs that a content block is being sent. *)
