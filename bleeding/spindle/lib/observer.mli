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

val recover_once : engine:Engine.t -> network:Network.t -> unit
(** [recover_once ~engine ~network] attempts bounded batches of current-state
    recovery. Each repository checks its own catalog dependencies. Unrelated
    catalog retries and queued events do not prevent recovery. *)
