type candidate
val index : candidate -> Taposaur.Index.t
val row_id : candidate -> int64
val source_stamp : candidate -> string option
val invalidate : Db.write Db.t -> candidate -> bool
type status = {
  source_stamp : string option; stale : bool; diagnostics : string list;
  error : string option;
}
val status : 'a Db.t -> Taposaur.Uid.t -> status option
val pending :
?force:bool -> ?retry_errors:bool -> ?need_details:bool ->
?uid:Taposaur.Uid.t -> ?shard:string -> ?after_row:int64 -> ?after:string ->
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

val fill_publisher : Db.write Db.t -> uid:Taposaur.Uid.t -> publisher:string ->
  update_detail:(Taposaur.Index.t -> string -> string) -> bool
(** [fill_publisher t ~uid ~publisher ~update_detail] atomically fills a
    missing publisher and fences concurrent resolver publications. Other fields,
    cache timestamps and freshness are preserved. [update_detail] runs inside
    the SQLite transaction and must be pure. Returns false for absent papers
    or existing publishers. The caller owns audit and evidence retention. *)
