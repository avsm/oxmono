type t = {
  elements : Element.t list;
  generator : string option;
  timestamp : string option;
  copyright : string option;
  raw : Jsont.json;
  index : Element.t Jsont.String_map.t;
}

let decode raw =
  let elements = Codec.required "elements" (Jsont.list Element.jsont) raw in
  let osm3s = Codec.optional "osm3s" Jsont.json raw in
  let metadata key = Option.bind osm3s (Codec.optional key Jsont.string) in
  let index =
    List.fold_left
      (fun index e ->
        match Element.reference e with
        | None -> index
        | Some reference ->
            Jsont.String_map.add (Reference.to_string reference) e index)
      Jsont.String_map.empty elements
  in
  {
    elements;
    raw;
    index;
    generator = Codec.optional "generator" Jsont.string raw;
    timestamp = metadata "timestamp_osm_base";
    copyright = metadata "copyright";
  }

let jsont = Jsont.map Jsont.json ~dec:decode ~enc:(fun t -> t.raw)
let elements t = t.elements
let raw t = t.raw
let generator t = t.generator
let timestamp t = t.timestamp
let copyright t = t.copyright

let find reference t =
  Jsont.String_map.find_opt (Reference.to_string reference) t.index

let nodes t =
  List.filter_map (function Element.Node n -> Some n | _ -> None) t.elements

let ways t =
  List.filter_map (function Element.Way w -> Some w | _ -> None) t.elements

let relations t =
  List.filter_map
    (function Element.Relation r -> Some r | _ -> None)
    t.elements

let counts t =
  List.filter_map (function Element.Count c -> Some c | _ -> None) t.elements

type incomplete = Not_requested | Missing of Reference.t list

let resolve refs project t =
  let values, missing =
    List.fold_left
      (fun (values, missing) reference ->
        match Option.bind (find reference t) project with
        | Some value -> (value :: values, missing)
        | None -> (values, reference :: missing))
      ([], []) refs
  in
  if missing = [] then Ok (List.rev values)
  else Error (Missing (List.rev missing))

let way_nodes t (way : Element.way) =
  match way.nodes with
  | None -> Error Not_requested
  | Some ids ->
      resolve
        (List.map (Reference.v Node) ids)
        (function Element.Node n -> Some n | _ -> None)
        t

let relation_members t (relation : Element.relation) =
  match relation.members with
  | None -> Error Not_requested
  | Some members ->
      let refs = List.map (fun (m : Element.member) -> m.reference) members in
      Result.map (List.combine members) (resolve refs Option.some t)
