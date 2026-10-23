type read = Db.read
type write = Db.write
type 'access t = 'access Db.t
let open_read = Db.open_read
let open_write = Db.open_write
module Query = Query
module Shard = Shard
module Metadata = Metadata
type page = Search.page = {
  papers : Taposaur.Summary.t list;
  next : string option;
}
let search = Search.search
let find = Search.find
let find_index = Search.find_index
type stats = Statistics.t = {
  papers : int; pdf : int; xml : int; tei : int; s2orc : int;
  loaded_shards : int; discovered_shards : int option;
  errors : int; last_import : float option;
  titles : int; abstracts : int; publishers : int;
  enriched : int; enrichment_errors : int;
}
let stats = Statistics.get
