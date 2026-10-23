(** Archive validation and work-list coverage. *)

type coverage = Not_indexed | Tei | Held | No_artifacts
val coverage :
  'a Taposaur_catalog.t -> Taposaur.Uid.t -> coverage
(** [coverage catalog uid] describes imported holdings. [Not_indexed]
    does not imply that the archive lacks the paper. *)
val coverage_name : coverage -> string
(** [coverage_name status] gives its stable CLI spelling. *)

val validate :
  ?limit:int -> ?shard:string -> ?artifacts:bool ->
  Taposaur_archive.t -> int
(** [validate archive] checks up to [limit] index entries against their
    manifests (default 100). [shard] confines the sample to one shard.
    [artifacts] additionally opens every listed file and checks its size.
    Returns the checked count. Parse and I/O errors propagate. Missing
    papers, wrong shards and size mismatches raise [Failure]. *)
