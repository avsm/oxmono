type candidate
val index : candidate -> Taposaur.Index.t
val source_stamp : candidate -> string option
val invalidate : Db.write Db.t -> candidate -> bool
type status = {
  source_stamp : string option; stale : bool; diagnostics : string list;
  error : string option;
}
val status : 'a Db.t -> Taposaur.Uid.t -> status option
val pending :
?force:bool -> ?retry_errors:bool -> ?need_details:bool ->
?uid:Taposaur.Uid.t -> ?after:string ->
limit:int -> version:int -> 'a Db.t -> candidate list
val publish :
  ?source_stamp:string -> ?detail_cache:string -> ?diagnostics:string list ->
Db.write Db.t -> candidate -> version:int ->
(Taposaur.Bibliography.t, string) result -> bool
val errors : 'a Db.t -> (Taposaur.Uid.t * string) list

type detail = {
  index : Taposaur.Index.t; json : string; saved_at : float;
  stale : bool; error : string option; diagnostics : string list;
}
val detail : 'a Db.t -> Taposaur.Uid.t -> detail option
val detail_count : 'a Db.t -> int
