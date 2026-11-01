type metadata = {
  version : int64 option;
  timestamp : string option;
  changeset : int64 option;
  uid : int64 option;
  user : string option;
}

type common = {
  id : int64;
  tags : Tags.t;
  metadata : metadata;
  raw : Jsont.json;
}

type node = { common : common; point : Point.t option }

type way = {
  common : common;
  nodes : int64 list option;
  geometry : Point.t option list option;
  center : Point.t option;
  bounds : Bounds.t option;
}

type member = {
  reference : Reference.t;
  role : string;
  point : Point.t option;
  geometry : Point.t option list option;
}

type relation = {
  common : common;
  members : member list option;
  center : Point.t option;
  bounds : Bounds.t option;
}

type count = {
  nodes : int64;
  ways : int64;
  relations : int64;
  areas : int64 option;
  total : int64;
  raw : Jsont.json;
}

type t =
  | Node of node
  | Way of way
  | Relation of relation
  | Area of common
  | Count of count
  | Other of Jsont.json

let point json =
  match
    ( Codec.optional "lat" Jsont.number json,
      Codec.optional "lon" Jsont.number json )
  with
  | None, None -> None
  | Some lat, Some lon -> Some (Codec.checked (fun () -> Point.v ~lat ~lon))
  | _ -> Codec.fail json "Latitude and longitude must occur together"

let geometry = Jsont.list (Jsont.option Point.jsont)

let common json =
  let id = Codec.required "id" Codec.id json in
  if id = 0L then Codec.fail json "OSM IDs must be positive";
  {
    id;
    tags = Codec.default "tags" Tags.jsont Tags.empty json;
    metadata =
      {
        version = Codec.optional "version" Codec.integer json;
        timestamp = Codec.optional "timestamp" Jsont.string json;
        changeset = Codec.optional "changeset" Codec.integer json;
        uid = Codec.optional "uid" Codec.integer json;
        user = Codec.optional "user" Jsont.string json;
      };
    raw = json;
  }

let member_jsont =
  let make kind id role lat lon geometry =
    let reference = Codec.checked (fun () -> Reference.v kind id) in
    let point =
      match (lat, lon) with
      | None, None -> None
      | Some lat, Some lon -> Some (Codec.checked (fun () -> Point.v ~lat ~lon))
      | _ ->
          Jsont.Error.msgf Jsont.Meta.none
            "Latitude and longitude must occur together"
    in
    { reference; role; point; geometry }
  in
  Jsont.Object.map make
  |> Jsont.Object.mem "type" Reference.kind_jsont ~enc:(fun m ->
         m.reference.kind)
  |> Jsont.Object.mem "ref" Codec.id ~enc:(fun m -> m.reference.id)
  |> Jsont.Object.mem "role" Jsont.string
       ~dec_absent:(fun () -> "")
       ~enc:(fun m -> m.role)
  |> Jsont.Object.opt_mem "lat" Jsont.number ~enc:(fun m ->
         Option.map (fun p -> p.Point.lat) m.point)
  |> Jsont.Object.opt_mem "lon" Jsont.number ~enc:(fun m ->
         Option.map (fun p -> p.Point.lon) m.point)
  |> Jsont.Object.opt_mem "geometry" geometry ~enc:(fun m -> m.geometry)
  |> Jsont.Object.finish

let decode json =
  match Codec.required "type" Jsont.string json with
  | "node" -> Node { common = common json; point = point json }
  | "way" ->
      Way
        {
          common = common json;
          nodes = Codec.optional "nodes" (Jsont.list Codec.id) json;
          geometry = Codec.optional "geometry" geometry json;
          center = Codec.optional "center" Point.jsont json;
          bounds = Codec.optional "bounds" Bounds.jsont json;
        }
  | "relation" ->
      Relation
        {
          common = common json;
          members = Codec.optional "members" (Jsont.list member_jsont) json;
          center = Codec.optional "center" Point.jsont json;
          bounds = Codec.optional "bounds" Bounds.jsont json;
        }
  | "area" -> Area (common json)
  | "count" ->
      let tags = Codec.required "tags" Jsont.json json in
      Count
        {
          nodes = Codec.required "nodes" Codec.integer tags;
          ways = Codec.required "ways" Codec.integer tags;
          relations = Codec.required "relations" Codec.integer tags;
          areas = Codec.optional "areas" Codec.integer tags;
          total = Codec.required "total" Codec.integer tags;
          raw = json;
        }
  | _ -> Other json

let raw = function
  | Node n -> n.common.raw
  | Way w -> w.common.raw
  | Relation r -> r.common.raw
  | Area c -> c.raw
  | Count c -> c.raw
  | Other j -> j

let jsont = Jsont.map Jsont.json ~dec:decode ~enc:raw

let reference = function
  | Node n -> Some (Reference.v Node n.common.id)
  | Way w -> Some (Reference.v Way w.common.id)
  | Relation r -> Some (Reference.v Relation r.common.id)
  | Area _ | Count _ | Other _ -> None

let tags = function
  | Node n -> n.common.tags
  | Way w -> w.common.tags
  | Relation r -> r.common.tags
  | Area c -> c.tags
  | Count _ | Other _ -> Tags.empty

let tag key element = Tags.find key (tags element)
