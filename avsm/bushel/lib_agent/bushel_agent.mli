type t
(** Structured Agentkit tools over a Bushel knowledge corpus. *)

val create :
  corpus:Bushel_knowledge.t ->
  ?provider:Retrieval.Embedding.provider ->
  ?refresh:(unit -> unit) ->
  max_bytes:int ->
  unit ->
  t

val names : string list
val is_tool : string -> bool
val tools : Agentkit.Agent.Tool.t list
val system_prompt : string

val invoke :
  t -> check:(unit -> unit) -> string -> string -> (string, string) result
(** [invoke t ~check name arguments] refreshes the corpus and returns complete,
    bounded JSON. Semantic queries index at most eight batches and report
    coverage. [check] must enforce current authority and cancellation. Callers
    serialize invocations on a shared corpus and own audit logging. *)

val bind : t -> check:(unit -> unit) -> Agentkit.Agent.Tool.t list
(** [bind t ~check] attaches handlers for direct Agentkit use. *)
