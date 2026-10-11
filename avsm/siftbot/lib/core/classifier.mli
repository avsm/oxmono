(** Bounded model suggestions, separated from confirmed review decisions. *)

val run :
  Store.t ->
  complete:Agentkit.Chat.complete ->
  model:string ->
  max_messages:int ->
  int
(** [run db ~complete ~model ~max_messages] classifies changed inputs using
    approved definitions and confirmed examples. Results are validated before
    committing, cached by input/model/prompt versions, and audited. Returns the
    number of newly classified messages. *)
