type t = {
  papers : int; pdf : int; xml : int; tei : int; s2orc : int;
  loaded_shards : int; discovered_shards : int option;
  errors : int; last_import : float option;
  titles : int; abstracts : int; publishers : int;
  enriched : int; enrichment_errors : int;
}
val get : 'a Db.t -> t
