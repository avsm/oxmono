type catalog = Db.read Db.t
type t
type row = {
  summary : Taposaur.Summary.t; stamp : string;
  stale : bool; error : string option; diagnostics : string list;
}
val with_read : Db.read Db.t -> (t -> 'a) -> 'a
val search : t -> Query.t -> Search.page
val find : t -> Taposaur.Uid.t -> row option
