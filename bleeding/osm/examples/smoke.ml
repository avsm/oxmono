open Osm

let ok = function
  | Ok value -> value
  | Error error -> failwith (Format.asprintf "%a" pp_error error)

let () =
  Eio_main.run @@ fun env ->
  Eio.Time.with_timeout_exn env#clock 45. @@ fun () ->
  let osm = Osm.v (Fetch_httpz.std env) in
  print_string (ok (Overpass.status osm));
  let places = ok (Nominatim.search osm ~q:"Cambridge, UK" ~limit:1 ()) in
  List.iter
    (fun (p : Place.t) ->
      Printf.printf "Search: %s (%g, %g)\n%!" p.display_name p.point.lat
        p.point.lon)
    places;
  let point = Point.v ~lat:52.2053 ~lon:0.1218 in
  let p = ok (Nominatim.reverse_at osm point) in
  Printf.printf "Reverse: %s\n%!" p.display_name;
  let selection = Query.select Nwr ~scope:(Query.around ~radius:50. point) in
  let result =
    ok
      (Overpass.run osm
         (Query.elements ~timeout:10 ~geometry:Full ~limit:3 [ selection ]))
  in
  List.iter
    (function
      | Element.Node n -> Printf.printf "Node %Ld\n" n.common.id
      | Element.Way w -> Printf.printf "Way %Ld\n" w.common.id
      | Element.Relation r -> Printf.printf "Relation %Ld\n" r.common.id
      | _ -> ())
    (Dataset.elements result);
  let count = ok (Overpass.run osm (Query.count ~timeout:10 [ selection ])) in
  Printf.printf "Nearby: %Ld nodes, %Ld ways, %Ld relations\n%!" count.nodes
    count.ways count.relations;
  match List.find_map Place.area places with
  | None -> failwith "Search returned no area candidate"
  | Some reference ->
      let cafes =
        Query.select Nwr ~scope:(Query.area reference)
          ~filters:[ Query.eq "amenity" "cafe" ]
      in
      let count = ok (Overpass.run osm (Query.count ~timeout:10 [ cafes ])) in
      Printf.printf "Cambridge area: %Ld cafes\n%!" count.total
