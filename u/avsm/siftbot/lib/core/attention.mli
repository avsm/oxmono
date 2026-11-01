(** Conservative attention evidence. Inferred state is always a suggestion. *)

val infer :
  self_addresses:string list ->
  Siftbot_model.message list ->
  Siftbot_model.assignment list
(** [infer ~self_addresses messages] identifies explicit requests and subsequent
    admin replies by conversation and time. Empty identity configuration yields
    no guesses about who owes a reply. *)

val queue : Store.t -> now:string -> Siftbot_model.assignment list
(** [queue db ~now] returns visible attention predictions and confirmations,
    excluding active local snoozes and resolved/FYI items. *)
