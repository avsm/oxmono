type example = {
  feedback : Siftbot_model.feedback;
  reasons : string list;
  score : float;
}
(** Relevant, explicit human feedback for classification. *)

type t

val prepare : ?provider:Embedding.provider -> Store.t -> t
(** [prepare db] indexes all active corrections. Optional embeddings are cached
    by provider identity, dimensions and source text. Failures are reported. *)

val select :
  ?chronological:bool -> t -> limit:int -> Siftbot_model.message -> example list
(** [select index ~limit message] retrieves relevant corrections from the same
    account. It excludes the message itself, its thread, duplicate message IDs
    and duplicate text. [chronological] additionally excludes later sources. *)

val trace : example list -> string
(** [trace examples] describes the corrections actually supplied in a prompt. *)
