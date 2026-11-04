type stamp = { size : int64; mtime : float }
val paths : ?after:string -> limit:int -> 'a Db.t -> string list
val unchanged : 'a Db.t -> shard:string -> stamp -> bool
val replace :
  Db.write Db.t -> shard:string -> stamp ->
  ((Taposaur.Index.t -> unit) -> unit) -> int
val record_error : Db.write Db.t -> shard:string -> string -> unit
val set_discovered : Db.write Db.t -> int -> unit
val errors : 'a Db.t -> (string * string) list

val upsert : Db.write Db.t -> shard:string -> stamp -> Taposaur.Index.t ->
  [ `Inserted | `Updated | `Unchanged ]
val retire : Db.write Db.t -> shard:string -> unit
