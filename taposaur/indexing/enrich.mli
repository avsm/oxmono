(** Resolve imported papers and publish searchable metadata. *)

type result = Updated | Unavailable of string | Superseded
type stats = {
  examined : int; updated : int; unavailable : int; superseded : int;
}
val run :
  ?force:bool -> ?limit:int -> ?uid:Taposaur.Uid.t -> ?jobs:int ->
  ?progress:(Taposaur.Uid.t -> result -> unit) ->
  Taposaur_catalog.write Taposaur_catalog.t -> Taposaur_archive.t -> stats
(** [run catalog archive] resolves pending papers with the current paper
    resolver. Reads occur outside SQL transactions, with at most [jobs]
    concurrent papers (default 8, range 1..64). [limit] counts attempts,
    making limited runs resumable. [force] refreshes completed attempts.
    Failed documents permit partial metadata. Missing or invalid manifests
    produce [Unavailable]. Replaced index snapshots produce [Superseded].
    Cancellation and exceptions from [progress] propagate. *)
