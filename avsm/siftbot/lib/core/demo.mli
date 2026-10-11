val messages : Siftbot_model.message list
(** Synthetic corpus requiring no credentials or inference. *)

val seed : Store.t -> unit
(** [seed db] installs fixtures once, preserving subsequent review decisions. *)
