(** Pull requests and assignment changes from a Jetstream source. *)

val wanted : Settings.t -> Jsont.json -> bool
(** [wanted settings event] holds for a Jetstream commit to a followed
    collection, and for an account event that makes an owner with accepted
    repositories inactive. Other events are not stored. *)

val groups : (string * string) list -> (string * string) list list
(** [groups ready] splits inbox items by the repository DID of their event,
    keeping their order within each repository. *)

val run :
  engine:Engine.t ->
  network:Network.t ->
  hooks:Hooks.t ->
  wipe:(string -> unit) ->
  jetstream:string ->
  health:Health.t ->
  policy:Operations.t ->
  unit
(** [run ~engine ~network ~hooks ~wipe ~jetstream ~health ~policy] follows
    [org.tangled.repo.manifest], [org.tangled.review.patch] and
    [org.tangled.track.ticket] records until cancelled.

    A manifest change drops the repository's cached acceptance, and one naming
    this spindle registers the repository. A deleted manifest whose knot reports
    the repository deleted calls [wipe] on it. An account that becomes inactive
    calls [wipe] on each repository it owns. Other network-wide events are not
    stored.

    A new patch on an open pull request of an accepted repository dispatches it
    as [opened] or [synchronize], and a change of its ticket's state as
    [closed], [merged] or [reopened]. Each also sends the matching
    [pull_request:*] webhook. A pipeline waits for approval when its author or
    sender cannot push.

    Events and the stream cursor are stored together, and a dispatch is
    deduplicated across restarts. A repository's events are handled in order,
    an event waiting behind an earlier one that is due for a retry. A failing
    event is retried with backoff and rejected after sixteen attempts, and a
    full job queue delays it without counting. *)
