(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Serialize - high-level serialization to buffers and event streams

    This module provides functions to convert YAML values to events and strings.
    Both {!Emitter.t}-based and function-based emission APIs are provided. *)

(** {1 Emitter.t-based API} *)

val emit_yaml_node : Emitter.t -> Yaml.t -> unit
(** Emit a YAML node to an emitter *)

val emit_yaml : Emitter.t -> Yaml.t -> unit
(** Emit a complete YAML document to an emitter (includes stream/document
    markers) *)

val emit_value_node : Emitter.t -> Value.t -> unit
(** Emit a Value node to an emitter *)

val emit_value : Emitter.t -> Value.t -> unit
(** Emit a complete Value document to an emitter (includes stream/document
    markers) *)

val emit_document : ?resolve_aliases:bool -> Emitter.t -> Document.t -> unit
(** Emit a document to an emitter

    @param resolve_aliases
      Whether to resolve aliases before emission (default true) *)

(** {1 Buffer-based API} *)

val value_to_buffer :
  ?config:Emitter.config -> ?buffer:Buffer.t -> Value.t -> Buffer.t
(** Serialize a Value to a buffer

    @param config Emitter configuration (default: {!Emitter.default_config})
    @param buffer Optional buffer to append to; creates new one if not provided
*)

val yaml_to_buffer :
  ?config:Emitter.config -> ?buffer:Buffer.t -> Yaml.t -> Buffer.t
(** Serialize a Yaml.t to a buffer *)

val documents_to_buffer :
  ?config:Emitter.config ->
  ?resolve_aliases:bool ->
  ?buffer:Buffer.t ->
  Document.t list ->
  Buffer.t
(** Serialize documents to a buffer

    @param resolve_aliases
      Whether to resolve aliases before emission (default true) *)

(** {1 String-based API} *)

val value_to_string : ?config:Emitter.config -> Value.t -> string
(** Serialize a Value to a string *)

val yaml_to_string : ?config:Emitter.config -> Yaml.t -> string
(** Serialize a Yaml.t to a string *)

val documents_to_string :
  ?config:Emitter.config -> ?resolve_aliases:bool -> Document.t list -> string
(** Serialize documents to a string *)

(** {1 Writer-based API}

    These functions write directly to a bytesrw [Bytes.Writer.t], enabling true
    streaming output without intermediate string allocation. *)

val value_to_writer :
  ?config:Emitter.config ->
  ?eod:bool ->
  Bytesrw.Bytes.Writer.t ->
  Value.t ->
  unit
(** Serialize a Value directly to a Bytes.Writer

    @param eod Whether to write end-of-data after serialization (default true)
*)

val yaml_to_writer :
  ?config:Emitter.config ->
  ?eod:bool ->
  Bytesrw.Bytes.Writer.t ->
  Yaml.t ->
  unit
(** Serialize a Yaml.t directly to a Bytes.Writer *)

val documents_to_writer :
  ?config:Emitter.config ->
  ?resolve_aliases:bool ->
  ?eod:bool ->
  Bytesrw.Bytes.Writer.t ->
  Document.t list ->
  unit
(** Serialize documents directly to a Bytes.Writer *)

(** {1 Function-based API}

    These functions accept an emit function [Event.t -> unit] instead of an
    {!Emitter.t}, allowing them to work with any event sink. *)

val emit_yaml_node_fn : emitter:(Event.t -> unit) -> Yaml.t -> unit
(** Emit a YAML node using an emitter function *)

val emit_yaml_fn :
  emitter:(Event.t -> unit) -> config:Emitter.config -> Yaml.t -> unit
(** Emit a complete YAML stream using an emitter function *)

val emit_value_node_fn :
  emitter:(Event.t -> unit) -> config:Emitter.config -> Value.t -> unit
(** Emit a Value node using an emitter function *)

val emit_value_fn :
  emitter:(Event.t -> unit) -> config:Emitter.config -> Value.t -> unit
(** Emit a complete Value stream using an emitter function *)

val emit_document_fn :
  ?resolve_aliases:bool -> emitter:(Event.t -> unit) -> Document.t -> unit
(** Emit a document using an emitter function *)

val emit_documents :
  emitter:(Event.t -> unit) ->
  config:Emitter.config ->
  ?resolve_aliases:bool ->
  Document.t list ->
  unit
(** Emit multiple documents using an emitter function *)
