(** Resolve imported papers and publish searchable metadata. *)

type result = Updated | Partial of string list | Unavailable of string | Superseded
type stats = {
  examined : int; updated : int; unavailable : int; superseded : int;
  partial : int;
}
val run :
  ?retry_errors:bool -> ?force:bool -> ?limit:int -> ?uid:Taposaur.Uid.t -> ?jobs:int ->
  ?progress:(Taposaur.Uid.t -> result -> unit) ->
  Taposaur_catalog.write Taposaur_catalog.t -> Taposaur_archive.t -> stats
(** [run catalog archive] resolves pending papers with the current paper
    resolver. Reads occur outside SQL transactions, with at most [jobs]
    concurrent papers (default 8, range 1..64). [limit] counts attempts,
    making limited runs resumable. Successful and partial resolutions also
    publish complete paper details. Older rows without details are backfilled.
    [force] refreshes completed attempts.
    Failed documents produce [Partial] with searchable metadata and diagnostics.
    [retry_errors] retries partial results and unavailable manifests. Missing or invalid manifests
    produce [Unavailable]. Replaced index snapshots produce [Superseded].
    Cancellation and exceptions from [progress] propagate. *)

type freshness = { checked : int; stale : int; next : string option }
val check :
  ?limit:int -> ?after:string -> Taposaur_catalog.write Taposaur_catalog.t ->
  Taposaur_archive.t -> freshness
(** [check catalog archive] checks up to [limit] source fingerprints in UID
    order. Changed, missing and previously unchecked sources are marked
    stale without discarding cached values. [next] resumes the scan.
    [limit] is 1..1000. A subsequent [run] refreshes stale papers. *)
