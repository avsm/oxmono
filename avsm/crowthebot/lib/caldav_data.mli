type collection = {
  href : string;
  title : string;
  properties : string;
  sync : bool;
}
(** Calendar mirror values without network or secret capabilities. *)

type change = { href : string; etag : string option; removed : bool }

type page = {
  token : string option;
  more : bool;
  inventory : bool;
  changes : change list;
}

type item = {
  href : string;
  etag : string option;
  raw : string;
  search : string;
  parsed : bool;
}
