open Crowthebot

let check label b = if not b then failwith label
let json s = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json s)

let field key codec s =
  Result.get_ok (Jsont_bytesrw.decode_string (Jsont.mem key codec) s)

let bad f =
  try
    ignore (f ());
    false
  with Invalid_argument _ -> true

let admin = "@admin:example.org"
let friend = "@friend:example.org"
let room = "!room:example.org"
let now = 1788912000.

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let db = Sqlite3_eio.open_memory ~sw () in
  let store = Store.create ~now:(fun () -> now) db ~admin in
  Store.set_person store ~actor:admin ~user:friend ~role:Friend ~allowed:true;
  let state = Store.locations store in
  ignore
    (Location_store.attach state ~actor:admin ~room ~event:"$link"
       ~person:"Alice" ~connection:"home" ~user:"alice" ~device:"phone");
  let mode = ref "history"
  and requests = ref 0
  and on_request = ref (fun () -> ()) in
  let last_query = ref "" in
  let fetch =
    Fetch_mock.client (fun req ->
        incr requests;
        !on_request ();
        if req.meth = `GET then begin
          check "Recorder credentials present only on Recorder requests"
            (Http.Header.get req.headers "authorization" <> None);
          let uri =
            Uri.of_string (Fetch.Middleware.Url.path_and_query req.url)
          in
          check "selected tracker and bounded dates"
            (Uri.path uri = "/tracks/api/0/locations"
            && Uri.get_query_param uri "user" = Some "alice"
            && Uri.get_query_param uri "device" = Some "phone"
            && Uri.get_query_param uri "from" = Some "2026-09-08"
            && Uri.get_query_param uri "to" = Some "2026-09-10");
          if !mode = "date-redirect" then
            Fetch_mock.respond ~status:302
              ~headers:
                (Http.Header.of_list
                   [
                     ( "Location",
                       "/tracks/api/0/locations?user=alice&device=phone&from=2020-01-01&to=2026-09-10"
                     );
                   ])
              "" req
          else
            let points =
              List.init 26 (fun i ->
                  Printf.sprintf
                    {|{"lat":51,"lon":0,"tst":%.0f,"acc":5,"SSID":"Office Wi-Fi","BSSID":"02:00:00:00:00:01","conn":"w"}|}
                    (now -. float_of_int (i * 10)))
            in
            Fetch_mock.respond
              ("["
              ^ String.concat ","
                  ((List.hd points :: points)
                  @ [
                      {|{"lat":100,"lon":0,"tst":1788911999}|};
                      {|{"lat":51,"lon":0,"tst":1788912001}|};
                      {|{"lat":51,"lon":0,"tst":1}|};
                    ])
              ^ "]")
              req
        end
        else begin
          check "map uses POST and exact custom endpoint"
            (req.meth = `POST
            && Fetch.Middleware.Url.path_and_query req.url
               = "/custom/interpreter");
          check "Recorder credentials cannot reach Overpass"
            (Http.Header.get req.headers "authorization" = None);
          check "map client identifies itself"
            (Http.Header.get req.headers "user-agent" = Some "crowthebot/0.1");
          let body =
            match req.body with Fetch.String s -> s | _ -> assert false
          in
          last_query :=
            Option.get
              (Uri.get_query_param (Uri.of_string ("/?" ^ body)) "data");
          match !mode with
          | "redirect" ->
              Fetch_mock.respond ~status:302
                ~headers:
                  (Http.Header.of_list
                     [ ("Location", "https://other.example/interpreter") ])
                "" req
          | "partial" ->
              Fetch_mock.respond {|{"remark":"private failure","elements":[]}|}
                req
          | "flood" ->
              Fetch_mock.respond (String.make ((1024 * 1024) + 1) 'x') req
          | "invalid" -> Fetch_mock.respond "private failure" req
          | _ ->
              Fetch_mock.respond
                {|{"elements":[
          {"type":"node","id":1,"lat":51,"lon":0.001,"tags":{"name":"Cafe","amenity":"cafe","private":"hidden"}},
          {"type":"area","id":3600000123,"tags":{"name":"Town","boundary":"administrative","admin_level":"8"}},
          {"type":"way","id":2,"center":{"lat":51,"lon":0.002},"tags":{"addr:street":"High Street"}},
          {"type":"node","id":1,"lat":51,"lon":0.001,"tags":{"name":"Cafe","amenity":"cafe"}},
          {"type":"node","id":3,"lat":100,"lon":0,"tags":{"name":"Invalid"}}
        ]}|}
                req
        end)
  in
  let config =
    Result.get_ok
      (Owntracks_config.of_string ~client_id:"test"
         {|
[owntracks.recorder]
url="https://recorder.example/tracks"
user="operator"
password="recorder-secret"
[owntracks.overpass]
url="https://maps.example/custom/interpreter"
|})
  in
  let clock = Eio.Stdenv.mono_clock env in
  let source ?(config = config) () =
    Owntracks_source.initialize
      ~load:(fun _ -> config)
      ~fetch ~clock
      ~now:(fun () -> now)
      (json
         {|{"config_file":"/private/fixture.toml","user":"alice","device":"phone","allow_http":false,"lookback_days":7}|})
  in
  let invoke ?(actor = friend) ?config name args =
    let locations =
      Locations.create ~state
        ~sources:[ ("home", source ?config ()) ]
        ~default:(Some "home")
    in
    Locations.invoke
      (Locations.for_request locations ~actor ~room ~event:"$query")
      name args
  in
  let history offset =
    Printf.sprintf
      {|{"person":"Alice","from":"2026-09-08T23:55:50Z","to":"2026-09-09T00:00:00Z","offset":%d,"limit":20}|}
      offset
  in
  let first = Result.get_ok (invoke "location_history" (history 0)) in
  check "history first page, sorted, deduplicated and inclusive"
    (field "available" Jsont.int first = 26
    && field "next_offset" Jsont.int first
       = List.length (field "positions" (Jsont.list Jsont.json) first)
    && String.length first <= 3800
    && List.hd
         (field "positions"
            (Jsont.list (Jsont.mem "recorded_at" Jsont.string))
            first)
       = "2026-09-08T23:55:50Z");
  let last = Result.get_ok (invoke "location_history" (history 20)) in
  check "history remainder has final boundary"
    (List.length (field "positions" (Jsont.list Jsont.json) last) = 6
    && field "next_offset" (Jsont.option Jsont.int) last = None
    && contains last "2026-09-09T00:00:00Z");
  check "history exposes optional network context"
    (contains last {|"wifi_ssid":"Office Wi-Fi"|}
    && contains last {|"connection_type":"wifi"|});
  let rec collect offset acc =
    let page = Result.get_ok (invoke "location_history" (history offset)) in
    let timestamps =
      field "positions" (Jsont.list (Jsont.mem "recorded_at" Jsont.string)) page
    in
    let acc = acc @ timestamps in
    match field "next_offset" (Jsont.option Jsont.int) page with
    | None -> acc
    | Some next ->
        check "history cursor progresses with larger records" (next > offset);
        collect next acc
  in
  check "Wi-Fi fields preserve complete history pagination"
    (List.length (collect 0 []) = 26);
  check "history does not update cached point or memory"
    ((Option.get (Location_store.get state ~actor:admin ~person:"Alice")).point
     = None
    && Store.search_facts store ~actor:admin ~query:"" = []);
  let before = !requests in
  List.iter
    (fun args ->
      check "invalid interval rejected before network"
        (Result.is_error (invoke "location_history" args)))
    [
      {|{"person":"Alice","from":"2020-01-01T00:00:00Z","to":"2026-09-09T00:00:00Z"}|};
      {|{"person":"Alice","from":"2026-09-09T01:00:00Z","to":"2026-09-09T00:00:00Z"}|};
      {|{"person":"Alice","from":"2026-09-09T00:00:00Z","to":"2026-09-10T00:00:00Z"}|};
      {|{"person":"Alice","from":"yesterday","to":"now"}|};
    ];
  check "history denied to unknown sender"
    (Result.is_error
       (invoke ~actor:"@unknown:example.org" "location_history" (history 0))
    && !requests = before);
  mode := "date-redirect";
  check "redirect cannot widen history dates"
    (Result.is_error (invoke "location_history" (history 0))
    && !requests = before + 1);
  mode := "history";
  (on_request :=
     fun () ->
       Store.set_person store ~actor:admin ~user:friend ~role:Friend
         ~allowed:false);
  check "revocation during history request denied"
    (Result.is_error (invoke "location_history" (history 0)));
  (on_request := fun () -> ());
  Store.set_person store ~actor:admin ~user:friend ~role:Friend ~allowed:true;
  mode := "map";
  let query = {|{"latitude":51,"longitude":0,"limit":1}|} in
  let result = Result.get_ok (invoke "location_resolve" query) in
  check "map context preserves attribution and containing area source"
    (contains result "OpenStreetMap contributors"
    && contains result "https://www.openstreetmap.org/relation/123"
    && field "available" Jsont.int result = 3
    && field "next_offset" Jsont.int result = 1);
  check "bounded query contains administrative areas and nearby features"
    (contains !last_query "is_in(51.0000000,0.0000000)"
    && contains !last_query "around:500"
    && contains !last_query "out center tags 100");
  let result =
    Result.get_ok
      (invoke "location_resolve" {|{"latitude":51,"longitude":0,"offset":1}|})
  in
  check "nearby geometry uses centre distance and drops unrelated tags"
    (contains result "centre_distance_metres"
    && contains result "High Street"
    && (not (contains result "hidden"))
    && not (contains result "Invalid"));
  ignore
    (invoke "location_resolve"
       {|{"latitude":51,"longitude":0,"tag":"name","value":"Cafe\"];out;[\""}|});
  check "tag values quoted instead of executable QL"
    (contains !last_query {|["name"="Cafe\"];out;[\""]|});
  let before = !requests in
  List.iter
    (fun args ->
      check "map input rejected locally"
        (Result.is_error (invoke "location_resolve" args)))
    [
      {|{"latitude":91,"longitude":0}|};
      {|{"latitude":51,"longitude":0,"radius_metres":2001}|};
      {|{"latitude":51,"longitude":0,"tag":"name];out;"}|};
      {|{"latitude":51,"longitude":0,"value":"cafe"}|};
      {|{"latitude":51,"longitude":0,"query":"out;"}|};
    ];
  let disabled =
    {
      config with
      owntracks =
        {
          config.owntracks with
          overpass = { config.owntracks.overpass with enabled = false };
        };
    }
  in
  check "operator can disable maps"
    (Result.is_error (invoke ~config:disabled "location_resolve" query));
  check "local rejection has no network" (!requests = before);
  List.iter
    (fun mode_name ->
      mode := mode_name;
      let before = !requests in
      let result = invoke "location_resolve" query in
      check "map errors bounded, redacted and redirects not followed"
        (Result.is_error result
        && !requests = before + 1
        && not (contains (Result.get_error result) "private failure")))
    [ "redirect"; "partial"; "flood"; "invalid" ];
  List.iter
    (fun url ->
      check "unsafe map endpoints rejected at startup"
        (bad (fun () ->
             Overpass.create ~fetch ~clock
               ~config:{ Owntracks_config.default_overpass with url })))
    [
      "http://maps.example/query";
      "https://user:secret@maps.example/query";
      "https://maps.example/query?secret=x";
    ];
  print_endline
    "crowthebot: history pagination, map resolution and network authority \
     passed"
