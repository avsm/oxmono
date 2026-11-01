(** Pull requests and assignment changes from a Jetstream source. *)

val run :
  engine:Engine.t ->
  network:Network.t ->
  hooks:Hooks.t ->
  jetstream:string ->
  health:Health.t ->
  policy:Operations.t ->
  unit
(** [run ~engine ~network ~hooks ~jetstream ~health ~policy] follows
    [org.tangled.repo.manifest], [org.tangled.review.patch] and
    [org.tangled.track.ticket] records until cancelled. A manifest change drops
    the repository's cached acceptance. A new patch on an open pull request of a
    served repository dispatches it as [opened] or [synchronize], and a change
    of its ticket's state as [closed], [merged] or [reopened]. Each also sends
    the matching [pull_request:*] webhook. A pipeline waits for approval when
    its author or sender cannot push. Events and the stream cursor are stored
    together, and a dispatch is deduplicated across restarts. *)
