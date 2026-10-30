(** Pull requests and assignment changes from a Jetstream source. *)

val run :
  engine:Engine.t ->
  network:Network.t ->
  jetstream:string ->
  health:Health.t ->
  policy:Operations.t ->
  unit
(** [run ~engine ~network ~jetstream ~health ~policy] follows
    [org.tangled.repo.manifest] and [org.tangled.review.patch] records until
    cancelled. A manifest change drops the repository's cached acceptance. A
    new patch on an open pull request of a served repository starts a
    pipeline, blocked for approval when its author cannot push. Events and the
    stream cursor are stored together, and a dispatch is deduplicated across
    restarts. *)
