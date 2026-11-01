val snapshot :
  Store.t ->
  query:string ->
  ?selected:Siftbot_model.source ->
  unit ->
  Siftbot_model.snapshot
(** [snapshot db ~query ?selected ()] returns compact message summaries and the
    selected body, defaulting to the first result. Display bodies are limited to
    256 KiB and marked truncated when shortened. The stored sources remain
    complete. Assignments are limited to the displayed messages. *)
