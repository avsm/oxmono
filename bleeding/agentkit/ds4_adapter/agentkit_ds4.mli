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

val transcript : Agentkit.Chat.message list -> string
(** [transcript messages] renders the messages after a leading system message
    as one prompt, labelling each with its role and each tool result with its
    call id. *)

val complete :
  Ds4.V4.engine ->
  ctx_size:int ->
  ?max_tokens:int ->
  unit ->
  Agentkit.Chat.complete
(** [complete engine ~ctx_size ()] answers each request with a fresh DS4
    agent. The leading system message becomes its system prompt and the rest
    its {!transcript}. DS4 runs the request's tools itself through
    {!Agentkit.Agent.Tool.invoke}, so the response never has calls. Attach
    execution with {!Agentkit.Turn.bind}. The agent replies without reasoning.
    A reply stopped by the token ceiling finishes with [Length].
    [max_tokens] is the default when a request does not set one. *)

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
