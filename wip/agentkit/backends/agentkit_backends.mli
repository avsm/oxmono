(** A single registry containing the local and network model families. *)

val registry :
  ds4:Agentkit.Driver.session Agentkit.Driver.t ->
  apple:Agentkit.Driver.session Agentkit.Driver.t ->
  openrouter:Agentkit.Driver.session Agentkit.Driver.t ->
  Agentkit.Driver.session Agentkit.Driver.registry
(** [registry ~ds4 ~apple ~openrouter] combines the three providers. Choices
    are qualified as [ds4/MODEL], [apple/MODEL], and [openrouter/MODEL]. *)
