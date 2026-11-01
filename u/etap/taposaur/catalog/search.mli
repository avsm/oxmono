type page = { papers : Taposaur.Summary.t list; next : string option }
val search : 'a Db.t -> Query.t -> page
val find : 'a Db.t -> Taposaur.Uid.t -> Taposaur.Summary.t option
val find_index : 'a Db.t -> Taposaur.Uid.t -> Taposaur.Index.t option
val search_db : Sqlite3.db -> Query.t -> page
val find_db : Sqlite3.db -> Taposaur.Uid.t -> Taposaur.Summary.t option
