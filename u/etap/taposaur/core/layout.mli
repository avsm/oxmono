(** Paths relative to a configured archive tree. *)

val paper : Uid.t -> string
(** [paper uid] is relative to [papers/]. Unsafe dot and empty DOI
    suffix segments raise [Invalid_argument]. *)
val flat_paper : Uid.t -> string
(** [flat_paper uid] is the legacy staging path without a DOI sub-shard. *)
val index : Uid.t -> string
(** [index uid] is relative to [index/], including [.jsonl]. *)
