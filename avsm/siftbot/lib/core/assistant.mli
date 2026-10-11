(** Read-only, bounded Agentkit discussion. Model output never commits edits. *)

val discuss :
  ?people:Identity.person list ->
  Store.t ->
  complete:Agentkit.Chat.complete ->
  model:string ->
  selected:Siftbot_model.source option ->
  string ->
  string
(** [discuss db ~complete ~model ~selected question] supplies bounded evidence
    tools, records model/prompt/source revisions and tool outcomes, and returns
    the answer. Failed turns retain an audit record without review changes. *)

val configured :
  sw:Eio.Switch.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  Store.t ->
  Profile.t ->
  selected:Siftbot_model.source option ->
  string ->
  string
(** [configured ~sw ~fs db profile ~selected question] uses the configured
    OpenRouter model and SIFTBOT_OPENROUTER_API_KEY. Missing settings fail. *)

val completion : sw:Eio.Switch.t -> Profile.t -> string * Agentkit.Chat.complete
(** [completion ~sw profile] constructs the explicitly configured chat adapter. *)
