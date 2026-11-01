open Osm

let check name value = if not value then failwith name

let decode codec text =
  match Jsont_bytesrw.decode_string codec text with
  | Ok x -> x
  | Error e -> failwith e

let bad codec text =
  check ("reject " ^ text)
    (Result.is_error (Jsont_bytesrw.decode_string codec text))

let invalid f =
  match f () with
  | _ -> failwith "Expected Invalid_argument"
  | exception Invalid_argument _ -> ()

let roundtrip codec input =
  let value = decode codec input in
  let encoded =
    match Jsont_bytesrw.encode_string codec value with
    | Ok s -> s
    | Error e -> failwith e
  in
  check "lossless JSON roundtrip"
    (Jsont.Json.equal (decode Jsont.json input) (decode Jsont.json encoded))

let fixture =
  {|{
  "version":0.6,"generator":"Overpass API",
  "osm3s":{"timestamp_osm_base":"2026-10-17T00:00:00Z","copyright":"OSM"},
  "elements":[
    {"type":"node","id":1,"lat":52.2,"lon":0.1,"tags":{"name":"A","amenity":"cafe"},
     "version":3,"timestamp":"2026-01-01T00:00:00Z","changeset":123,"uid":4,"user":"mapper","future":true},
    {"type":"node","id":2,"lat":52.3,"lon":0.2},
    {"type":"way","id":1,"nodes":[1,2,1],"tags":{"highway":"path"},
     "geometry":[{"lat":52.2,"lon":0.1},null,{"lat":52.2,"lon":0.1}],
     "center":{"lat":52.25,"lon":0.15},
     "bounds":{"minlat":52.2,"minlon":0.1,"maxlat":52.3,"maxlon":0.2}},
    {"type":"relation","id":1,"members":[
      {"type":"way","ref":1,"role":"outer","geometry":[{"lat":52.2,"lon":0.1},null]},
      {"type":"node","ref":2,"role":"label","lat":52.3,"lon":0.2},
      {"type":"relation","ref":1,"role":"cycle"}]},
    {"type":"area","id":3600000001,"tags":{"name":"area"}},
    {"type":"custom","label":"kept"}
  ]
}|}

let test_models () =
  let result = decode Dataset.jsont fixture in
  roundtrip Dataset.jsont fixture;
  check "metadata"
    (Dataset.timestamp result = Some "2026-10-17T00:00:00Z"
    && Dataset.copyright result = Some "OSM");
  check "variant collections"
    (List.length (Dataset.nodes result) = 2
    && List.length (Dataset.ways result) = 1
    && List.length (Dataset.relations result) = 1);
  let reference = Reference.v Way 1L in
  check "ID namespaces"
    (match Dataset.find reference result with
    | Some (Element.Way _) -> true
    | _ -> false);
  let node = List.hd (Dataset.nodes result) in
  check "typed metadata" (node.common.metadata.version = Some 3L);
  check "tag access" (Tags.find "amenity" node.common.tags = Some "cafe");
  let way = List.hd (Dataset.ways result) in
  check "cropped geometry"
    (match way.geometry with Some [ _; None; _ ] -> true | _ -> false);
  check "way node order"
    (match Dataset.way_nodes result way with
    | Ok nodes ->
        List.map (fun (n : Element.node) -> n.common.id) nodes = [ 1L; 2L; 1L ]
    | Error _ -> false);
  let relation = List.hd (Dataset.relations result) in
  check "roles and cycle stay finite"
    (match Dataset.relation_members result relation with
    | Ok members ->
        List.map (fun (m, _) -> m.Element.role) members
        = [ "outer"; "label"; "cycle" ]
    | _ -> false);
  let sparse =
    decode Dataset.jsont
      {|{"elements":[{"type":"node","id":1},
       {"type":"way","id":2,"nodes":[8,9]},
       {"type":"way","id":3},{"type":"way","id":4,"nodes":[]}]}|}
  in
  let ways = Dataset.ways sparse in
  check "absent node coordinates" ((List.hd (Dataset.nodes sparse)).point = None);
  check "all missing references"
    (Dataset.way_nodes sparse (List.nth ways 0)
    = Error (Dataset.Missing [ Reference.v Node 8L; Reference.v Node 9L ]));
  check "not requested"
    (Dataset.way_nodes sparse (List.nth ways 1) = Error Dataset.Not_requested);
  check "empty requested" (Dataset.way_nodes sparse (List.nth ways 2) = Ok []);
  List.iter (bad Element.jsont)
    [
      {|{"type":"node","id":1.5}|};
      {|{"type":"node","id":-1}|};
      {|{"type":"node","id":9007199254740992}|};
      {|{"type":"node","id":1,"lat":91,"lon":0}|};
      {|{"type":"node","id":1,"lat":0}|};
      {|{"type":"way","id":1,"nodes":[0]}|};
      {|{"type":"relation","id":1,"members":[{"type":"bogus","ref":1}]}|};
    ];
  bad Bounds.nominatim_jsont {|["1","2","3"]|};
  bad Bounds.nominatim_jsont {|["2","1","3","4"]|};
  let crossing = Bounds.v ~south:(-10.) ~north:10. ~west:170. ~east:(-170.) in
  check "dateline crossing" (crossing.west > crossing.east);
  invalid (fun () -> Point.v ~lat:nan ~lon:0.);
  invalid (fun () -> Reference.v Node 0L)

let test_place () =
  let input =
    {|{"place_id":2,"osm_type":"relation","osm_id":42,
    "lat":"52.2","lon":"0.1","display_name":"Cambridge",
    "boundingbox":["52","53","0","1"],"category":"boundary",
    "type":"administrative","importance":0.7,
    "address":{"city":"Cambridge","country_code":"gb","neighbourhood":"Centre"},
    "licence":"OSM"}|}
  in
  let place = decode Place.jsont input in
  roundtrip Place.jsont input;
  check "typed bbox" (Option.map (fun b -> b.Bounds.west) place.bounds = Some 0.);
  check "area reference" (Place.area place = Some (Reference.v Relation 42L));
  let address = Option.get place.address in
  check "typed address" (address.city = Some "Cambridge");
  check "extensible address"
    (Address.find "neighbourhood" address = Some "Centre");
  let interpolated =
    decode Place.jsont
      {|{"place_id":3,"lat":"0","lon":"1","display_name":"postcode"}|}
  in
  check "optional reference" (interpolated.reference = None);
  bad Place.jsont
    {|{"place_id":1,"lat":"0","lon":"0","display_name":"x","osm_id":42}|};
  bad Place.jsont {|{"place_id":1,"lat":"91","lon":"0","display_name":"x"}|}

let count_response =
  {|{"elements":[{"type":"count","tags":{
  "nodes":"2","ways":"3","relations":"4","total":"9"}}]}|}

let test_queries () =
  let scope = Query.bbox (Bounds.v ~south:1. ~west:2. ~north:3. ~east:4.) in
  let selection =
    Query.select ~scope ~filters:[ Query.eq "amenity" "cafe" ] Nwr
  in
  let query = Query.elements ~geometry:Center ~limit:10 [ selection ] in
  check "generated QL"
    (Query.to_ql query
   = "[out:json][timeout:25];\n\
      (\n\
      nwr[\"amenity\"=\"cafe\"](1,2,3,4);\n\
      );\n\
      out body center 10;");
  let count = Query.count [ selection ] in
  let c = decode (Query.jsont count) count_response in
  check "typed counts"
    (c.nodes = 2L && c.ways = 3L && c.relations = 4L && c.total = 9L);
  roundtrip (Query.jsont count) count_response;
  bad (Query.jsont count) {|{"elements":[]}|};
  bad (Query.jsont count)
    {|{"elements":[{"type":"count","tags":{"nodes":"-1","ways":"0","relations":"0","total":"0"}}]}|};
  let q =
    Query.elements
      [
        Query.select ~scope:Query.global
          ~filters:[ Query.eq "name" "A\";out;\\B" ]
          Nodes;
      ]
  in
  check "QL escaping"
    (Query.to_ql q
   = "[out:json][timeout:25];\n\
      (\n\
      node[\"name\"=\"A\\\";out;\\\\B\"];\n\
      );\n\
      out body;");
  invalid (fun () -> Query.eq "name" "line\nbreak");
  invalid (fun () -> Query.elements []);
  invalid (fun () -> Query.around ~radius:infinity (Point.v ~lat:0. ~lon:0.));
  invalid (fun () -> Query.area (Reference.v Node 1L));
  let two_areas =
    Query.count
      [
        Query.select Nodes ~scope:(Query.area (Reference.v Relation 1L));
        Query.select Ways ~scope:(Query.area (Reference.v Way 2L));
      ]
  in
  check "separate area sets"
    (Query.to_ql two_areas
   = "[out:json][timeout:25];\n\
      relation(1);map_to_area->.area0;\n\
      way(2);map_to_area->.area1;\n\
      (\n\
      node(area.area0);\n\
      way(area.area1);\n\
      );\n\
      out count;")

let test_references () =
  let q =
    Query.references ~recurse:true
      [ Reference.v Way 1L; Reference.v Relation 2L ]
  in
  check "explicit reference query"
    (Query.to_ql q
   = "[out:json][timeout:25];\n\
      (\n\
      way(1);\n\
      relation(2);\n\
      );\n\
      (._; >>;);\n\
      out body;");
  invalid (fun () -> Query.references []);
  let result =
    decode Dataset.jsont
      {|{"elements":[{"type":"node","id":1,"tags":{"name":"first"}},
        {"type":"node","id":1,"tags":{"name":"last"}}]}|}
  in
  check "last duplicate indexed"
    (Option.bind
       (Dataset.find (Reference.v Node 1L) result)
       (Element.tag "name")
    = Some "last");
  check "all entries retained" (List.length (Dataset.elements result) = 2)

let test_http () =
  Eio_main.run @@ fun _ ->
  let calls = ref 0 in
  let http =
    Fetch_mock.client (fun req ->
        incr calls;
        let uri =
          Uri.of_string
            (Fetch.Middleware.Url.to_string req.Fetch.Middleware.url)
        in
        check "structured street"
          (Uri.get_query_param uri "street" = Some "1 High Street");
        check "no freeform term" (Uri.get_query_param uri "q" = None);
        check "viewbox order"
          (Uri.get_query_param uri "viewbox" = Some "2,3,4,1");
        check "bounded flag" (Uri.get_query_param uri "bounded" = Some "1");
        Fetch_mock.respond "[]" req)
  in
  let client = Osm.v http in
  let query =
    Nominatim.structured ~street:"1 High Street" ~city:"Cambridge" ()
  in
  let viewbox = Bounds.v ~south:1. ~west:2. ~north:3. ~east:4. in
  check "structured search"
    (Nominatim.search_query client ~viewbox ~bounded:true query = Ok []);
  invalid (fun () -> Nominatim.search_query client (Nominatim.structured ()));
  invalid (fun () -> Nominatim.search client ~bounded:true ~q:"x" ());
  check "invalid queries never reach HTTP" (!calls = 1);
  let client = Osm.v (Fetch_mock.client (Fetch_mock.respond count_response)) in
  let q =
    Query.count
      [
        Query.select Nodes
          ~scope:(Query.around ~radius:10. (Point.v ~lat:52.2 ~lon:0.1));
      ]
  in
  check "typed HTTP count"
    (match Overpass.run client q with
    | Ok count -> count.total = 9L
    | Error _ -> false)

let () =
  test_models ();
  test_place ();
  test_queries ();
  test_references ();
  test_http ();
  print_endline "Typed OSM model, query and traversal tests passed"
