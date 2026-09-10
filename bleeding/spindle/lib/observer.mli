(** Jetstream and verified knot event ingestion with durable retry. *)

val run :
  engine:Engine.t ->
  network:Network.t ->
  jetstream:string ->
  health:Health.t ->
  policy:Operations.t ->
  unit
(** [run ~engine ~network ~jetstream] bootstraps PDS assignments and follows
    record and knot changes until cancelled. Cursors and pending events are
    stored together; successful dispatch deduplication survives restart. *)
