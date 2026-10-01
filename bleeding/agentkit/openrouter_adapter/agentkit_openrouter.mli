(** OpenRouter adapter for Agentkit. *)

module Tool : sig
  type t = Agentkit.Agent.Tool.t

  val to_openrouter : t -> Openrouter.Tool.t
end

val messages : Agentkit.Chat.message list -> Openrouter.Message.t list
(** [messages ms] is [ms] in OpenRouter's wire form. *)

val complete : Openrouter.t -> model:string -> Agentkit.Chat.complete
(** [complete client ~model] sends each request as one non-streaming chat
    completion and returns the first choice. Tools are offered one call at a
    time. A reasoning effort is sent as [reasoning_effort], which OpenRouter and
    vLLM accept. Raises [Failure] when the response has no choice. *)

module Agent : sig
  include Agentkit.Agent.S

  val create :
    client:Openrouter.t ->
    model:string ->
    ?system:string ->
    ?max_tokens:int ->
    ?tools:Agentkit.Agent.Tool.t list ->
    ?max_rounds:int ->
    unit ->
    t
  (** [create ~client ~model ()] starts a streamed OpenRouter conversation that
      keeps its own history. [send] runs tool calls through
      {!Agentkit.Agent.Tool.invoke}, reporting [Tool_call] then [Tool_result],
      and asks again until the model answers. After [max_rounds] model
      requests, 8 by default, it asks once more without tools. A reply stopped
      by [max_tokens] reports [Cut_off]. *)
end

val models : Openrouter.t -> unit -> Agentkit.Driver.model list
(** [models client ()] fetches the currently advertised OpenRouter models. *)

val driver :
  models:(unit -> Agentkit.Driver.model list) ->
  create:(string -> Agent.t) ->
  unit ->
  Agentkit.Driver.session Agentkit.Driver.t
(** [driver ~models ~create ()] registers a driver under the [openrouter/]
    prefix. *)
