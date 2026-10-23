type candidate
val index : candidate -> Taposaur.Index.t
val pending :
  ?force:bool -> ?uid:Taposaur.Uid.t -> ?after:string ->
  limit:int -> version:int -> 'a Db.t -> candidate list
val publish :
  Db.write Db.t -> candidate -> version:int ->
  (Taposaur.Bibliography.t, string) result -> bool
val errors : 'a Db.t -> (Taposaur.Uid.t * string) list
