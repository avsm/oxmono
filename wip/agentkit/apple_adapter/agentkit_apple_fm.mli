(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Apple Foundation Models agents for Agentkit. *)

module Tool : sig
  type t
  (** A text-result Foundation Models tool instrumented for Agentkit. *)

  val v :
    ?includes_schema_in_instructions:bool ->
    description:string ->
    'a Apple_fm.Codec.t ->
    ('a -> string) ->
    t
  (** [v ~description codec handler] creates a tool. Its calls and complete
      results appear in the enclosing agent's event stream. *)

  val name : t -> string
  (** [name tool] is the name in [tool]'s invocation codec. *)

  val invoke : ?on_event:(Agentkit.Agent.event -> unit) -> t -> string -> string
  (** [invoke ~on_event tool arguments] invokes [tool] with a JSON argument
      object. It is intended for tests and reports the same events as an agent.
  *)
end

module Agent : sig
  (** Cancelling an active exchange closes Apple's session. The agent cannot be
      used afterwards. *)

  include Agentkit.Agent.S

  val create :
    sw:Eio.Switch.t ->
    ?model:Apple_fm.Model.t ->
    ?instructions:string ->
    ?transcript:Apple_fm.Transcript.t ->
    ?options:Apple_fm.Generation.options ->
    ?context:Apple_fm.Context.t ->
    ?compact_at:int ->
    ?compact_tokens:int ->
    ?response_reserve:int ->
    Tool.t list ->
    t
  (** [create ~sw tools] creates a persistent Apple Foundation Models agent.

      [compact_at] is the percentage of the model context at which the agent
      compacts before an exchange. It defaults to zero, which disables automatic
      compaction. A nonzero value is from 10 through 95. [compact_tokens] bounds
      the summary and [response_reserve] reserves room for the next response. *)

  val compact :
    t -> on_event:(Agentkit.Agent.event -> unit) -> reason:string -> unit
  (** [compact agent ~on_event ~reason] replaces older transcript turns with a
      model-written summary and reports one [Compacted] event. *)

  val transcript : t -> Apple_fm.Transcript.t
  (** [transcript agent] is a serializable snapshot of its conversation. *)

  val replace_transcript : t -> Apple_fm.Transcript.t -> unit
  (** [replace_transcript agent transcript] replaces its conversation. *)
end

val driver :
  ?models:(unit -> Agentkit.Driver.model list) ->
  create:(string -> Agent.t) ->
  unit ->
  Agentkit.Driver.session Agentkit.Driver.t
(** [driver ~create ()] registers [apple/default]. [create model] builds its
    Apple tools and agent with native Foundation Models codecs. [models] may
    list additional configurations accepted by [create]. *)
