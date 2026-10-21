(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Adapt DS4 agent events to Agentkit's common event types. *)

val stats : Ds4.Agent.stats -> Agentkit.Agent.stats
(** [stats value] converts DS4 accounting without losing a field. *)

val compaction : Ds4.Agent.compaction -> Agentkit.Agent.compaction
(** [compaction value] converts a DS4 compaction record. *)

val event : Ds4.Agent.event -> Agentkit.Agent.event
(** [event value] converts one DS4 event. *)

val tools : Agentkit.Agent.Tool.t list -> Ds4.Tool.t list
(** [tools] converts generic Agentkit tools to DS4's native tool protocol. *)

val messages :
  dialect:Dsml.dialect ->
  tools:Agentkit.Agent.Tool.t list ->
  Agentkit.Chat.message list ->
  Dsml.message list
(** [messages ~dialect ~tools history] preserves roles and tool associations,
    advertises [tools], and neutralises control tokens in message text. Image
    messages raise [Invalid_argument] unless their image list is empty. *)

val transcript : Agentkit.Chat.message list -> string
(** [transcript messages] renders structured [messages] in the DeepSeek V4
    protocol without tools or reasoning. *)

val reasoning :
  string option -> Dsml.thinking_mode * Dsml.reasoning_effort option
(** [reasoning effort] maps none, high or max to DS4's reasoning mode. Other
    effort names raise [Invalid_argument]. *)

val complete :
  Ds4.V4.engine ->
  ctx_size:int ->
  ?max_tokens:int ->
  unit ->
  Agentkit.Chat.complete
(** [complete engine ~ctx_size ()] generates one structured completion. Tool
    calls are returned for the caller to execute. [max_tokens] is the default
    when the request does not set one. Reasoning accepts none, high or max.
    Image messages are rejected. A truncated reply finishes with [Length] and
    has no executable calls. *)

module Agent : Agentkit.Agent.S with type t = Ds4.Agent.t
(** A DS4 agent exposed through Agentkit's common operations. *)

val models : dir:string -> unit -> Agentkit.Driver.model list
(** [models ~dir ()] lists DS4 targets and local GGUF files, marking downloaded
    ones. Local files use [local/FILE] names. *)

val model_path : dir:string -> string -> string
(** [model_path ~dir name] resolves [name] to a model file. ["auto"] selects the
    preferred downloaded model. Invalid paths raise [Failure] with the same
    diagnostics as [ds4.cli]. *)

val driver :
  models:(unit -> Agentkit.Driver.model list) ->
  create:(string -> Ds4.Agent.t) ->
  Agentkit.Driver.session Agentkit.Driver.t
(** [driver ~models ~create] registers DS4 as [ds4/MODEL]. [create model]
    constructs its DS4 tools and agent for [model], using native DSML codecs. *)
