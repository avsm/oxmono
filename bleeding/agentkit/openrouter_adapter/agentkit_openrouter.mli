(** OpenRouter adapter for Agentkit. *)

module Tool : sig
  type t = Agentkit.Agent.Tool.t
  val to_openrouter : t -> Openrouter.Tool.t
end

module Agent : sig
  include Agentkit.Agent.S

  val create :
    client:Openrouter.t ->
    model:string ->
    ?system:string ->
    ?max_tokens:int ->
    unit ->
    t
  (** [create] starts an OpenRouter conversation. The client owns transport and
      credentials; the agent retains the message history for two-way turns. *)
end

val models : Openrouter.t -> unit -> Agentkit.Driver.model list
(** Fetch the currently advertised OpenRouter models. *)

val driver :
  models:(unit -> Agentkit.Driver.model list) ->
  create:(string -> Agent.t) ->
  unit -> Agentkit.Driver.session Agentkit.Driver.t
(** Register a driver under the [openrouter/] prefix. *)
