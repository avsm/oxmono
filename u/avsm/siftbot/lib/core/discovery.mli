(** Reproducible baseline discovery. Suggestions require explicit approval. *)

val propose :
  before:string -> Siftbot_model.message list -> Siftbot_model.area list
(** [propose ~before messages] groups bounded, deduplicated conversation terms
    from messages strictly before the chronological cut. *)

val classify :
  Siftbot_model.area list ->
  Siftbot_model.message list ->
  Siftbot_model.assignment list
(** [classify areas messages] produces conservative header evidence and
    definition-term rankings for approved areas. Scores are not probabilities. *)

val run : Store.t -> before:string -> unit
