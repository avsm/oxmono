(** Stream archive indexes into atomic catalogue publications. *)

type result = Unchanged | Loaded of int | Failed of string
val shard :
  ?force:bool -> Taposaur_catalog.write Taposaur_catalog.t ->
  Taposaur_archive.t -> shard:string -> result
(** [shard catalog archive ~shard] validates and imports one shard. Size
    and mtime skip unchanged sources unless [force] is set. Source changes
    during a read fail without replacing the old rows. Failures are
    recorded in the catalogue. Cancellation propagates. *)

type stats = { scanned : int; loaded : int; unchanged : int; failed : int }
val sync :
  ?force:bool -> ?limit:int -> ?prefix:string ->
  ?progress:(string -> result -> unit) ->
  Taposaur_catalog.write Taposaur_catalog.t -> Taposaur_archive.t -> stats
(** [sync catalog archive] inventories all index shards and imports changed
    shards matching [prefix]. [limit] bounds attempted imports, so repeated
    limited runs advance. Missing source shards retain their cached rows. *)
