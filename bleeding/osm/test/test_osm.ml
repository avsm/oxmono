let check name value = if not value then failwith name

let ok = function
  | Ok x -> x
  | Error e -> failwith (Format.asprintf "%a" Osm.pp_error e)

let invalid f =
  match f () with
  | _ -> failwith "Expected Invalid_argument"
  | exception Invalid_argument _ -> ()

let place =
  {|{"place_id":123,"osm_type":"node","osm_id":9876543210,
"lat":"52.205","lon":"0.119","display_name":"Cambridge",
"boundingbox":["52.1","52.3","0.1","0.2"],
"address":{"city":"Cambridge"},"licence":"OSM attribution"}|}

let client ?(status = 200) body inspect =
  Fetch_mock.client (fun req ->
      inspect req;
      Fetch_mock.respond ~status body req)
  |> Osm.v

let uri req =
  Uri.of_string (Fetch.Middleware.Url.to_string req.Fetch.Middleware.url)

let param req key = Uri.get_query_param (uri req) key
let path req = Uri.path (uri req)

let run () =
  let t =
    client
      ("[" ^ place ^ "]")
      (fun req ->
        check "search method" (req.meth = `GET);
        check "default host" (Uri.host (uri req) = Some "marpe");
        check "default port" (Uri.port (uri req) = Some 8888);
        check "search path" (path req = "/nominatim/search");
        check "unicode/ampersand encoding"
          (param req "q" = Some "café & park + river");
        check "format" (param req "format" = Some "jsonv2");
        check "countries" (param req "countrycodes" = Some "gb,fr");
        check "language" (param req "accept-language" = Some "en"))
  in
  let results =
    ok
      (Osm.Nominatim.search t ~q:"café & park + river"
         ~countrycodes:[ "gb"; "fr" ] ~language:"en" ())
  in
  let p = List.hd results in
  check "place fields"
    (p.point.lat = 52.205 && p.point.lon = 0.119
    && p.reference = Some (Osm.Reference.v Node 9876543210L)
    && p.display_name = "Cambridge");
  check "raw fields retained"
    (Result.is_ok (Jsont.Json.decode (Jsont.mem "licence" Jsont.string) p.raw));
  let t =
    client place (fun req ->
        check "reverse path" (path req = "/nominatim/reverse");
        check "reverse lat"
          (Option.map float_of_string (param req "lat") = Some 52.205);
        check "zoom" (param req "zoom" = Some "16"))
  in
  ignore (ok (Osm.Nominatim.reverse t ~zoom:16 ~lat:52.205 ~lon:0.119 ()));
  let q = "[out:json];node[\"name\"=\"A&B+C\"](1,2,3,4);out;" in
  let t =
    client {|{"elements":[{"type":"node","id":1,"lat":2,"lon":3}]}|} (fun req ->
        check "overpass method" (req.meth = `POST);
        check "interpreter" (path req = "/overpass/api/interpreter");
        match req.body with
        | Fetch.String body ->
            let form = Uri.query_of_encoded body in
            check "QL preserved" (List.assoc "data" form = [ q ])
        | _ -> failwith "Expected form body")
  in
  check "elements"
    (List.length (Osm.Dataset.elements (ok (Osm.Overpass.query t q))) = 1);
  let text = "Connected as: 1\nRate limit: 2\n" in
  let t =
    client text (fun req ->
        check "status GET" (req.meth = `GET);
        check "status path" (path req = "/overpass/api/status"))
  in
  check "health text" (ok (Osm.Overpass.status t) = text);
  let t = client ~status:429 "slow down" ignore in
  check "HTTP error"
    (Osm.Overpass.status t
    = Error (Osm.Http_status { status = 429; body = "slow down" }));
  let t = client {|{"error":"Unable to geocode"}|} ignore in
  check "API error"
    (Osm.Nominatim.reverse t ~lat:0. ~lon:0. ()
    = Error (Osm.Api_error "Unable to geocode"));
  let t = client {|{"elements":[],"remark":"runtime error: timeout"}|} ignore in
  check "partial result rejected"
    (Osm.Overpass.query t q = Error (Osm.Api_error "runtime error: timeout"));
  List.iter
    (fun body ->
      let t = client body ignore in
      match Osm.Nominatim.search t ~q:"x" () with
      | Error (Osm.Invalid_response _) -> ()
      | _ -> failwith "Malformed response accepted")
    [
      "<html>error</html>";
      "{}";
      "[{}]";
      {|[{"place_id":1,"lat":"nan","lon":"0","display_name":"x"}]|};
    ];
  let t = client "[]" ignore in
  check "no matches" (ok (Osm.Nominatim.search t ~q:"x" ()) = []);
  let http =
    Fetch_mock.client (fun req ->
        check "custom path" (path req = "/custom/status");
        Fetch_mock.respond "ok" req)
  in
  let t = Osm.v ~overpass_url:"https://example.com/custom" http in
  check "override" (ok (Osm.Overpass.status t) = "ok");
  let t =
    Osm.v ~max_response:4 (Fetch_mock.client (Fetch_mock.respond "12345"))
  in
  check "bounded response"
    (Osm.Overpass.status t = Error (Osm.Response_too_large 4));
  let exact =
    Osm.v ~max_response:4 (Fetch_mock.client (Fetch_mock.respond "1234"))
  in
  check "exact response limit" (ok (Osm.Overpass.status exact) = "1234");
  invalid (fun () -> Osm.Nominatim.search t ~limit:41 ~q:"x" ());
  invalid (fun () -> Osm.Nominatim.search t ~q:" " ());
  invalid (fun () -> Osm.Nominatim.reverse t ~lat:nan ~lon:0. ());
  invalid (fun () -> Osm.Nominatim.reverse t ~lat:0. ~lon:181. ());
  invalid (fun () -> Osm.Nominatim.reverse t ~zoom:19 ~lat:0. ~lon:0. ());
  invalid (fun () -> Osm.Overpass.query_raw t "");
  invalid (fun () -> Osm.v ~nominatim_url:"file:///tmp" http);
  invalid (fun () -> Osm.v ~overpass_url:"http://host/?key=x" http);
  invalid (fun () -> Osm.v ~max_response:0 http);
  print_endline "OSM client tests passed"

let () = Eio_main.run (fun _ -> run ())
