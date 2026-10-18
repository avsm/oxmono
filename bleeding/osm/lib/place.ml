type t = {
  place_id : int64;
  reference : Reference.t option;
  point : Point.t;
  display_name : string;
  address : Address.t option;
  bounds : Bounds.t option;
  category : string option;
  kind : string option;
  importance : float option;
  raw : Jsont.json;
}

let decode raw =
  let lat = Codec.required "lat" Codec.number_string raw in
  let lon = Codec.required "lon" Codec.number_string raw in
  let reference =
    match
      ( Codec.optional "osm_type" Reference.kind_jsont raw,
        Codec.optional "osm_id" Codec.integer raw )
    with
    | None, None -> None
    | Some kind, Some id -> Some (Codec.checked (fun () -> Reference.v kind id))
    | _ -> Codec.fail raw "osm_type and osm_id must occur together"
  in
  {
    place_id = Codec.required "place_id" Codec.integer raw;
    reference;
    point = Codec.checked (fun () -> Point.v ~lat ~lon);
    display_name = Codec.required "display_name" Jsont.string raw;
    address = Codec.optional "address" Address.jsont raw;
    bounds = Codec.optional "boundingbox" Bounds.nominatim_jsont raw;
    category = Codec.optional "category" Jsont.string raw;
    kind = Codec.optional "type" Jsont.string raw;
    importance = Codec.optional "importance" Jsont.number raw;
    raw;
  }

let jsont = Jsont.map Jsont.json ~dec:decode ~enc:(fun t -> t.raw)

let area t =
  match t.reference with
  | Some ({ kind = Reference.Way | Relation; _ } as reference) -> Some reference
  | _ -> None
