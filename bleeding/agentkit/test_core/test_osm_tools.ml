let check name condition = if not condition then failwith name
let json s = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json s)

let field codec key json =
  Result.get_ok (Jsont.Json.decode (Jsont.mem key codec) json)

let place =
  {|{"place_id":1,"osm_type":"way","osm_id":42,
"lat":"52.2","lon":"0.1","display_name":"Cambridge address",
"address":{"city":"Cambridge","country":"United Kingdom"}}|}

let () =
  Eio_main.run @@ fun env ->
  let calls = ref 0 in
  let response = ref ("[" ^ place ^ "]") in
  let status = ref 200 in
  let fetch =
    Fetch_mock.client (fun req ->
        incr calls;
        check "GET only" (req.Fetch.Middleware.meth = `GET);
        let url = Fetch.Middleware.Url.to_string req.url in
        check "default service"
          (String.starts_with ~prefix:"http://marpe:8888/nominatim/" url);
        Fetch_mock.respond ~status:!status !response req)
  in
  let tools =
    Agentkit_osm.create ~fetch ~clock:(Eio.Stdenv.mono_clock env) ()
  in
  let search args = Agentkit_osm.invoke tools "address_search" args in
  let reverse args = Agentkit_osm.invoke tools "address_reverse" args in
  let result = Result.get_ok (search {|{"query":"Cambridge"}|}) |> json in
  let p = List.hd (field (Jsont.list Jsont.json) "results" result) in
  check "source link"
    (field Jsont.string "source_url" p = "https://www.openstreetmap.org/way/42");
  check "typed latitude" (field Jsont.number "latitude" p = 52.2);
  let before = !calls in
  List.iter
    (fun args -> check "invalid search" (Result.is_error (search args)))
    [
      {|{"query":""}|};
      {|{"query":"x","limit":1.5}|};
      {|{"query":"x","limit":6}|};
      {|{"query":"x","limit":0}|};
      {|{"query":"x","url":"http://other/"}|};
      {|{"query":"x","language":""}|};
      Printf.sprintf {|{"query":"%s"}|} (String.make 513 'x');
    ];
  List.iter
    (fun args -> check "invalid reverse" (Result.is_error (reverse args)))
    [
      {|{"latitude":91,"longitude":0}|};
      {|{"latitude":0,"longitude":181}|};
      {|{"latitude":1}|};
      {|{"latitude":"1","longitude":0}|};
    ];
  check "invalid arguments never requested" (!calls = before);
  response := place;
  check "reverse result"
    (Result.is_ok (reverse {|{"latitude":52.2,"longitude":0.1}|}));
  response := "[]";
  let empty = Result.get_ok (search {|{"query":"absent"}|}) |> json in
  check "true no match" (field (Jsont.list Jsont.json) "results" empty = []);
  status := 503;
  check "service failure distinct from no match"
    (search {|{"query":"x"}|} = Error "Address service returned HTTP 503.");
  status := 200;
  response := "not json";
  check "malformed service response"
    (Result.is_error (search {|{"query":"x"}|}));
  let tiny =
    Agentkit_osm.create ~max_result_bytes:256 ~fetch
      ~clock:(Eio.Stdenv.mono_clock env)
      ()
  in
  response := "[" ^ place ^ "]";
  check "output budget"
    (Result.is_error
       (Agentkit_osm.invoke tiny "address_search" {|{"query":"x"}|}));
  let bound = List.hd (Agentkit_osm.tools tools) in
  let result =
    Agentkit.Agent.Tool.invoke bound
      { id = "test"; name = "address_search"; arguments = {|{"query":"x"}|} }
    |> json
  in
  check "bound Agentkit tool"
    (List.length (field (Jsont.list Jsont.json) "results" result) = 1);
  let denied =
    Fetch_mock.client (fun req ->
        Fetch_mock.respond ~status:302
          ~headers:
            (Http.Header.of_list [ ("Location", "http://other/private") ])
          "" req)
  in
  let tool =
    Agentkit_osm.create ~fetch:denied ~clock:(Eio.Stdenv.mono_clock env) ()
  in
  check "cross-origin redirect denied"
    (Result.is_error
       (Agentkit_osm.invoke tool "address_search" {|{"query":"x"}|}));
  let cancelled =
    Agentkit_osm.create
      ~fetch:(Fetch_mock.client (fun _ -> raise (Eio.Cancel.Cancelled Exit)))
      ~clock:(Eio.Stdenv.mono_clock env)
      ()
  in
  check "cancellation propagates"
    (try
       ignore (Agentkit_osm.invoke cancelled "address_search" {|{"query":"x"}|});
       false
     with Eio.Cancel.Cancelled Exit -> true);
  print_endline
    "Agentkit address validation, limits, failures and binding passed"
