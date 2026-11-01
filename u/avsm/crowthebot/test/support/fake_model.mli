(** Scripted models for tests. *)

val v :
  (Agentkit.Chat.message list ->
  Agentkit.Agent.Tool.t list ->
  string option * Agentkit.Agent.tool_call list) ->
  Agentkit.Chat.complete
(** [v f] answers each request with [f messages tools], which returns the reply
    text and the tool calls. The finish reason is unknown. *)
