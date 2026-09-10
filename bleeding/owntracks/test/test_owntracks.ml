module O = Owntracks

let get = function Ok value -> value | Error error -> Alcotest.fail error

let reject result =
  match result with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "unexpectedly accepted"

let location = {|{"_type":"location","lat":51.5,"lon":-0.1,"tst":1700000000}|}

let location_of message =
  match get message with
  | O.Message.Location loc -> loc
  | _ -> Alcotest.fail "expected location"

let test_messages () =
  List.iter
    (fun json ->
      let message = get (O.Message.of_string json) in
      let encoded = get (O.Message.to_string message) in
      let again = get (O.Message.of_string encoded) in
      Alcotest.(check string)
        "round trip" encoded
        (get (O.Message.to_string again)))
    [
      location;
      {|{"_type":"location","lat":51.5,"lon":-0.1,"tst":1700000000,"SSID":"Office Wi-Fi","BSSID":"02:00:00:00:00:01","conn":"w","created_at":1700000005}|};
      {|{"_type":"transition","tst":1,"acc":2,"wtst":0,"event":"enter"}|};
      {|{"_type":"waypoint","tst":1,"desc":"Home","lat":51,"lon":0,"rad":100}|};
      {|{"_type":"waypoint","tst":1,"desc":"Beacon","uuid":"abc","major":1,"minor":2}|};
      {|{"_type":"waypoints","waypoints":[{"_type":"waypoint","tst":1,"desc":"Home"}]}|};
      {|{"_type":"card","name":"Alice","tid":"ab","face":"aGVsbG8="}|};
      {|{"_type":"lwt","tst":1}|};
    ];
  let loc =
    location_of
      (O.Message.of_string
         {|{"lat":51.5,"lon":-0.1,"tst":1700000000,"_type":"location","unknown":{"a":[1,2]}}|})
  in
  Alcotest.(check (float 0.)) "tag after fields" 51.5 (O.Location.lat loc);
  Alcotest.(check (option string))
    "missing SSID stays absent" None (O.Location.ssid loc);
  let wifi =
    O.Location.v ~lat:51.5 ~lon:0. ~tst:1 ~created_at:2 ~ssid:"Office Wi-Fi"
      ~bssid:"02:00:00:00:00:01" ~conn:"w" ()
  in
  let encoded = get (O.Message.to_string (O.Message.Location wifi)) in
  let wifi = location_of (O.Message.of_string encoded) in
  Alcotest.(check (option string))
    "SSID survives codec" (Some "Office Wi-Fi") (O.Location.ssid wifi);
  Alcotest.(check (option string))
    "BSSID survives codec" (Some "02:00:00:00:00:01") (O.Location.bssid wifi);
  Alcotest.(check (option int))
    "report time distinct from fix" (Some 2)
    (O.Location.created_at wifi)

let test_invalid () =
  List.iter
    (fun json -> reject (O.Message.of_string json))
    [
      "";
      "{}";
      location ^ "null";
      {|{"_type":"configuration"}|};
      {|{"_type":"location","lat":null,"lon":0,"tst":1}|};
      {|{"_type":"location","lat":1e999,"lon":0,"tst":1}|};
      {|{"_type":"location","lat":0,"lon":0,"tst":1.5}|};
      {|{"_type":"location","lat":0,"lon":0,"tst":"1"}|};
      {|{"_type":"location","lat":0,"lon":0,"tst":9007199254740992}|};
      {|{"_type":"waypoints","tst":1,"lat":0,"lon":0,"rad":1,"desc":"x"}|};
    ];
  reject
    (Jsont_bytesrw.decode_string O.Location.jsont
       {|{"_type":"waypoint","lat":0,"lon":0,"tst":1}|});
  reject
    (O.Message.to_string
       (O.Message.Location (O.Location.v ~tst:1 ~lat:nan ~lon:0. ())))

let test_borrow () =
  let json =
    {| {"_type":"location","lat":51.5,"lon":-0.1,"tst":1,"topic":"spoof"} |}
  in
  let bytes = Bytes.of_string ("prefix" ^ json ^ "suffix") in
  let saved = Bytes.copy bytes in
  let payload = Mqttz.Slice.make ~off:6 ~len:(String.length json) bytes in
  let topic = "owntracks/alice/quote\"slash\\" in
  let envelope = get (O.Mqtt.of_mqtt ~topic ~payload) in
  Alcotest.(check bytes) "input untouched" saved bytes;
  Bytes.fill bytes 0 (Bytes.length bytes) 'x';
  let loc =
    match O.Mqtt.message envelope with
    | O.Message.Location l -> l
    | _ -> Alcotest.fail "expected location"
  in
  Alcotest.(check (option string))
    "transport topic wins" (Some topic) (O.Location.topic loc);
  Alcotest.(check (float 0.))
    "decoded record owns values" 51.5 (O.Location.lat loc);
  let encoded = get (O.Message.encode (O.Message.Location loc)) in
  ignore (location_of (O.Message.decode encoded));
  reject (O.Message.decode Mqttz.Slice.empty);
  let source = Bytes.of_string location in
  let local_ view =
    Mqttz.Slice.make_local source ~off:0 ~len:(Bytes.length source)
  in
  ignore (location_of (O.Message.decode view))

let test_topics () =
  Alcotest.(check (option (pair string string)))
    "topic"
    (Some ("alice", "phone"))
    (O.Mqtt.parse_topic "owntracks/alice/phone/waypoint");
  List.iter
    (fun topic ->
      Alcotest.(check bool)
        "invalid prefix or levels" true
        (O.Mqtt.parse_topic topic = None))
    [ "other/a/b"; "owntracks//b"; "owntracks/a/" ];
  Alcotest.(check string)
    "filter" "owntracks/alice/#"
    (O.Mqtt.user_topic "alice");
  List.iter
    (fun id ->
      match O.Mqtt.device_topic ~user:"alice" ~device:id with
      | _ -> Alcotest.fail "invalid device accepted"
      | exception Invalid_argument _ -> ())
    [ ""; "a/b"; "+"; "#"; "a\000b" ]

let test_geojson () =
  let make ?alt tst lon = O.Location.v ~tst ~lat:51. ~lon ?alt () in
  let point =
    O.Geojson.point_feature ~device_name:"Phone" (make ~alt:3. 1 2.)
  in
  let json = get (O.Geojson.to_string point) in
  let coordinates = Jsont.(mem "geometry" (mem "coordinates" (list number))) in
  Alcotest.(check (list (float 0.)))
    "longitude first" [ 2.; 51.; 3. ]
    (get (Jsont_bytesrw.decode_string coordinates json));
  let line =
    O.Geojson.linestring_feature ~device_name:"Phone" [ make 2 2.; make 1 1. ]
  in
  let json = get (O.Geojson.to_string line) in
  let coordinates =
    Jsont.(mem "geometry" (mem "coordinates" (list (list number))))
  in
  Alcotest.(check (list (list (float 0.))))
    "sorted line"
    [ [ 1.; 51. ]; [ 2.; 51. ] ]
    (get (Jsont_bytesrw.decode_string coordinates json));
  let collection =
    O.Geojson.collection [ point; O.Geojson.collection [ line ] ]
  in
  let json = get (O.Geojson.to_string collection) in
  let features =
    get (Jsont_bytesrw.decode_string Jsont.(mem "features" (list json)) json)
  in
  Alcotest.(check int) "flattened features" 2 (List.length features);
  List.iter
    (fun locs ->
      match O.Geojson.linestring_feature ~device_name:"bad" locs with
      | _ -> Alcotest.fail "invalid line accepted"
      | exception Invalid_argument _ -> ())
    [
      [];
      [ make 1 1. ];
      [
        O.Location.with_topic "a" (make 1 1.);
        O.Location.with_topic "b" (make 2 2.);
      ];
    ];
  reject
    (O.Geojson.to_string
       (O.Geojson.point_feature ~device_name:"bad" (make 1 nan)))

let test_recorder () =
  let read s = Bytesrw.Bytes.Reader.of_string ~slice_length:1 s in
  List.iter
    (fun json ->
      Alcotest.(check int)
        "location response" 1
        (List.length (get (O.Recorder.decode_locations (read json)))))
    [ "[" ^ location ^ "]"; "{\"data\":[" ^ location ^ "]}" ];
  List.iter
    (fun json ->
      Alcotest.(check (list string))
        "list response" [ "alice" ]
        (get (O.Recorder.decode_list (read json))))
    [ "[\"alice\"]"; "{\"results\":[\"alice\"]}" ];
  reject (O.Recorder.decode_locations (read "{\"data\":{}}"));
  reject (O.Recorder.decode_list (read "{\"error\":\"denied\"}"))

let test_config () =
  let parse = Owntracks_config.of_string ~client_id:"test" in
  let config = get (parse Owntracks_config.default_toml) in
  Alcotest.(check string)
    "default Overpass interpreter" "https://overpass-api.de/api/interpreter"
    config.owntracks.overpass.url;
  Alcotest.(check bool)
    "Overpass enabled" true config.owntracks.overpass.enabled;
  let custom =
    get
      (parse
         "[owntracks.overpass]\n\
          url='http://maps.example/query'\n\
          enabled=false\n\
          allow_http=true")
  in
  Alcotest.(check string)
    "custom Overpass interpreter" "http://maps.example/query"
    custom.owntracks.overpass.url;
  Alcotest.(check bool)
    "Overpass disabled" false custom.owntracks.overpass.enabled;
  Alcotest.(check bool)
    "explicit plain HTTP" true custom.owntracks.overpass.allow_http;
  Alcotest.(check string) "client default" "test" config.mqtt.client.client_id;
  let config =
    get
      (parse
         {|[mqtt]
tls=true
[owntracks]
default_device="phone"
[[owntracks.devices]]
id="phone"
name="My Phone"
|})
  in
  Alcotest.(check int) "TLS port" 8883 config.mqtt.port;
  Alcotest.(check string)
    "device name" "My Phone"
    (Owntracks_config.device_name config "phone");
  Alcotest.(check string)
    "resolve display name" "phone"
    (get (Owntracks_config.device_id config "My Phone"));
  Alcotest.(check string)
    "raw device ID" "unlisted"
    (get (Owntracks_config.device_id config "unlisted"));
  reject (Owntracks_config.device_id config "../other");
  let ambiguous =
    {
      config with
      owntracks =
        {
          config.owntracks with
          devices =
            [ { id = "a"; name = "Phone" }; { id = "b"; name = "Phone" } ];
        };
    }
  in
  reject (Owntracks_config.device_id ambiguous "Phone");
  List.iter
    (fun text -> reject (parse text))
    [
      "[pool]\nmax_connections=2";
      "[mqtt]\nhost=''";
      "[mqtt]\nversion='3'";
      "[owntracks]\ntopic='a/#/b'";
      "[owntracks]\nunknown=1";
      "[owntracks.overpass]\nunknown=1";
      "[owntracks.overpass]\nenabled='yes'";
      "[[owntracks.devices]]\n\
       id='x'\n\
       name='a'\n\
       [[owntracks.devices]]\n\
       id='x'\n\
       name='b'";
    ]

let test_encoding_allocation () =
  let message = get (O.Message.of_string location) in
  ignore (get (O.Message.encode message));
  let before = Gc.allocated_bytes () in
  for _ = 1 to 100 do
    ignore (Sys.opaque_identity (get (O.Message.encode message)))
  done;
  let allocated = Gc.allocated_bytes () -. before in
  Printf.printf "100 small location encodes: %.0f allocated bytes\n%!" allocated;
  (* A small message must not allocate the streaming writer's default 64 KiB. *)
  Alcotest.(check bool) "small-message scratch space" true (allocated < 3276800.);
  let face = String.make 10000 'a' in
  let card = O.Message.Card (O.Card.v ~tid:"ab" ~face ()) in
  let decoded = get (O.Message.decode (get (O.Message.encode card))) in
  match decoded with
  | O.Message.Card card ->
      Alcotest.(check (option string))
        "multiple buffer flushes" (Some face) (O.Card.face card)
  | _ -> Alcotest.fail "expected card"

let () =
  Alcotest.run "OwnTracks"
    [
      ( "codec",
        List.map
          (fun (name, test) -> Alcotest.test_case name `Quick test)
          [
            ("messages", test_messages);
            ("malformed values", test_invalid);
            ("borrowed payload", test_borrow);
            ("topics", test_topics);
            ("GeoJSON", test_geojson);
            ("Recorder", test_recorder);
            ("TOML", test_config);
            ("encoding allocation and chunking", test_encoding_allocation);
          ] );
    ]
