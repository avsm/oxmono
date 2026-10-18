type kind = Node | Way | Relation

let kind_jsont =
  Jsont.enum [ ("node", Node); ("way", Way); ("relation", Relation) ]

let kind_name = function
  | Node -> "node"
  | Way -> "way"
  | Relation -> "relation"

type t = { kind : kind; id : int64 }

let v kind id =
  if id <= 0L then invalid_arg "OSM IDs must be positive";
  { kind; id }

let jsont =
  Jsont.Object.map (fun kind id -> Codec.checked (fun () -> v kind id))
  |> Jsont.Object.mem "type" kind_jsont ~enc:(fun r -> r.kind)
  |> Jsont.Object.mem "ref" Codec.integer ~enc:(fun r -> r.id)
  |> Jsont.Object.finish

let to_string r =
  let prefix = match r.kind with Node -> "N" | Way -> "W" | Relation -> "R" in
  prefix ^ Int64.to_string r.id

let compare a b =
  let c = Stdlib.compare a.kind b.kind in
  if c = 0 then Int64.compare a.id b.id else c
