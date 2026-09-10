open Crowthebot

let check name value = if not value then failwith name
let admin = "@admin:example.org"
let alice = "@alice:example.org"
let bot = "@bot:example.org"
let room = "!room:example.org"
let json s = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json s)

let contains value needle =
  let rec loop i =
    i + String.length needle <= String.length value
    && (String.sub value i (String.length needle) = needle || loop (i + 1))
  in
  loop 0

let bad f =
  try
    ignore (f ());
    false
  with Invalid_argument _ -> true

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let current = ref 1788912000. in
  let db = Sqlite3_eio.open_memory ~sw () in
  let store = Store.create ~now:(fun () -> !current) db ~admin in
  Store.add_room store room;
  Store.set_person store ~actor:admin ~user:alice ~role:Friend ~allowed:true;
  Store.set_person store ~actor:admin ~user:bot ~role:Bot ~allowed:true;
  let mode = ref "normal" and reads = ref 0 and revoke = ref false in
  let clock = Eio.Stdenv.mono_clock env in
  let settings =
    json
      {|{"config_file":"/operator/owntracks.toml","user":"alice","device":"phone & watch","allow_http":false,"lookback_days":7}|}
  in
  let loads = ref 0 in
  let load path =
    incr loads;
    check "only operator config loaded" (path = "/operator/owntracks.toml");
    Result.get_ok
      (Owntracks_config.of_string ~client_id:"test"
         {|
[owntracks.recorder]
url="https://recorder.example/tracks"
user="operator"
password="fixture-password"
[mqtt]
password="unrelated-mqtt-secret"
|})
  in
  let fetch =
    Fetch_mock.client (fun req ->
        incr reads;
        check "Recorder GET only" (req.meth = `GET);
        check "Recorder initialized with Basic credentials"
          (Http.Header.get req.headers "authorization"
          = Some "Basic b3BlcmF0b3I6Zml4dHVyZS1wYXNzd29yZA==");
        let uri =
          Uri.of_string
            ("https://recorder.example"
            ^ Fetch.Middleware.Url.path_and_query req.url)
        in
        if !revoke then
          Store.set_person store ~actor:admin ~user:alice ~role:Friend
            ~allowed:false;
        if !mode = "redirect" then
          Fetch_mock.respond ~status:302
            ~headers:
              (Http.Header.of_list
                 [ ("Location", "https://elsewhere.example/api/0/list") ])
            "" req
        else if !mode = "subpath" then
          Fetch_mock.respond ~status:302
            ~headers:
              (Http.Header.of_list
                 [ ("Location", "/tracks/api/0/list/secret") ])
            "" req
        else if
          List.mem !mode [ "other-user"; "other-device"; "duplicate-user" ]
        then
          let query =
            match !mode with
            | "other-user" -> "user=bob&device=phone%20%26%20watch"
            | "other-device" -> "user=alice&device=other"
            | _ -> "user=alice&user=bob&device=phone%20%26%20watch"
          in
          Fetch_mock.respond ~status:302
            ~headers:
              (Http.Header.of_list
                 [ ("Location", "/tracks/api/0/locations?" ^ query) ])
            "" req
        else if !mode = "invalid" then
          Fetch_mock.respond {|{"data":"fixture-password"}|} req
        else if !mode = "exception" then invalid_arg "fixture-password"
        else if !mode = "flood" then
          Fetch_mock.respond ("[\"" ^ String.make (1024 * 1024) 'x' ^ "\"]") req
        else begin
          check "no Recorder discovery endpoints"
            (Uri.path uri = "/tracks/api/0/locations");
          let expected_from =
            if !mode = "wifi-change" then "2026-09-08" else "2026-09-02"
          in
          check "Recorder bounded query"
            (Uri.get_query_param uri "from" = Some expected_from
            && Uri.get_query_param uri "to" = Some "2026-09-10");
          check "Recorder device query encoded"
            (Uri.get_query_param uri "user" = Some "alice"
            && Uri.get_query_param uri "device" = Some "phone & watch");
          Fetch_mock.respond
            (if !mode = "empty" then "[]"
             else if !mode = "mobile" then
               {|[{"lat":52.2,"lon":0.2,"tst":1788911950,"created_at":1788911980,"conn":"m"}]|}
             else if !mode = "wifi-change" then
               {|[{"lat":52.2,"lon":0.2,"tst":1788911950,"created_at":1788911960,"SSID":"Home Wi-Fi","conn":"w"},{"lat":52.2,"lon":0.2,"tst":1788911950,"created_at":1788911970,"SSID":"Office Wi-Fi","conn":"w"}]|}
             else
               {|{"data":[{"lat":51.1,"lon":0.1,"tst":1788911900,"acc":8},{"lat":52.2,"lon":0.2,"tst":1788911950,"created_at":1788911970,"acc":5,"SSID":"Office Wi-Fi","BSSID":"02:00:00:00:00:01","conn":"w"},{"lat":51.3,"lon":0.2,"tst":1788911960,"created_at":1788911961},{"lat":100,"lon":0.2,"tst":1788911999},{"lat":1,"lon":2,"tst":1788999999}]}|})
            req
        end)
  in
  let source =
    Owntracks_source.initialize ~load ~fetch ~clock
      ~now:(fun () -> !current)
      settings
  in
  let state = Store.locations store in
  let locations =
    Locations.create ~state ~sources:[ ("home", source) ] ~default:(Some "home")
  in
  let access actor =
    Locations.for_request locations ~actor ~room ~event:"$attach"
  in
  let invoke actor name args = Locations.invoke (access actor) name args in
  check "named sources expose no config values"
    (invoke alice "location_sources" "{}"
    = Ok
        {|{"connections":[{"name":"home","default":true,"user":"alice","device":"phone & watch"}],"next_after":null}|}
    );
  check "users discovered"
    ( Result.get_ok (invoke alice "location_devices" "{}") |> fun s ->
      contains s "alice" );
  check "devices discovered"
    ( Result.get_ok (invoke alice "location_devices" {|{"user":"alice"}|})
    |> fun s -> contains s "phone" );
  check "discovery is restricted to local selection" (!reads = 0);
  check "other users cannot be discovered"
    (Result.is_error (invoke alice "location_devices" {|{"user":"bob"}|}));
  List.iter
    (fun args ->
      check "tool arguments cannot broaden tracker permissions"
        (Result.is_error (invoke alice "location_attach" args)))
    [
      {|{"person":"Alice","user":"bob","device":"phone & watch"}|};
      {|{"person":"Alice","user":"alice","device":"other"}|};
    ];
  check "denied attachment has no effects"
    (!reads = 0 && Location_store.get state ~actor:admin ~person:"Alice" = None);
  let many =
    Locations.create ~state ~default:None
      ~sources:
        (List.init 37 (fun i ->
             (Printf.sprintf "%02d-%s" i (String.make 60 'x'), source)))
  in
  let page_codec =
    Jsont.Object.map (fun users next -> (users, next))
    |> Jsont.Object.mem "connections"
         (Jsont.list (Jsont.mem "name" Jsont.string))
    |> Jsont.Object.mem "next_after" (Jsont.option Jsont.string)
    |> Jsont.Object.finish
  in
  let rec all pages after acc =
    check "pagination progresses" (pages < 10);
    let args = "{\"after\":\"" ^ after ^ "\"}" in
    let result =
      Result.get_ok
        (Locations.invoke
           (Locations.for_request many ~actor:alice ~room ~event:"")
           "location_sources" args)
    in
    check "pages fit tool output budget" (String.length result <= 3800);
    let users, next =
      Result.get_ok (Jsont_bytesrw.decode_string page_codec result)
    in
    match next with
    | None -> acc @ users
    | Some next -> all (pages + 1) next (acc @ users)
  in
  let users = all 0 "" [] in
  check "pagination retains every authorized connection once"
    (List.length users = 37
    && List.length (List.sort_uniq String.compare users) = 37);
  mode := "normal";
  let attached =
    Result.get_ok
      (invoke alice "location_attach" {|{"person":"Alice","connection":"home"}|})
  in
  check "latest valid position chosen"
    (contains attached "52.2"
    && contains attached "recorded_at"
    && contains attached "accuracy_metres");
  check "latest report carries Wi-Fi context even with an older GPS fix"
    (contains attached {|"wifi_ssid":"Office Wi-Fi"|}
    && contains attached {|"wifi_bssid":"02:00:00:00:00:01"|}
    && contains attached {|"connection_type":"wifi"|}
    && contains attached {|"reported_at":"2026-09-08T23:59:30Z"|});
  check "secrets and endpoint absent from result"
    ((not (contains attached "fixture-password"))
    && not (contains attached "recorder.example"));
  check "state has provenance"
    (match Location_store.get state ~actor:admin ~person:"Alice" with
    | Some link ->
        link.actor = alice && link.room = room && link.event = "$attach"
    | None -> false);
  let reopened = Store.create ~now:(fun () -> !current) db ~admin in
  check "reported position survives reopening"
    (match
       Location_store.get (Store.locations reopened) ~actor:admin
         ~person:"Alice"
     with
    | Some { point = Some p; _ } ->
        p.latitude = 52.2
        && p.ssid = Some "Office Wi-Fi"
        && p.bssid = Some "02:00:00:00:00:01"
        && p.reported_at = Some 1788911970.
    | _ -> false);
  check "locations separate from memory"
    (Store.search_facts store ~actor:admin ~query:"" = []);
  for i = 1 to 30 do
    ignore
      (Location_store.attach state ~actor:admin ~room ~event:"$old"
         ~person:(Printf.sprintf "A-hidden-%02d" i)
         ~connection:"home" ~user:"bob" ~device:"other")
  done;
  check "cached reads cannot reveal previously allowed trackers"
    (Result.is_error
       (invoke alice "location_get" {|{"person":"A-hidden-01","refresh":false}|}));
  let visible = Result.get_ok (invoke alice "location_list" "{}") in
  check "list skips hidden pages without losing allowed links"
    (contains visible "Alice"
    && (not (contains visible "A-hidden"))
    && not (contains visible "bob"));
  for i = 1 to 30 do
    ignore
      (Location_store.detach state ~actor:admin
         ~person:(Printf.sprintf "A-hidden-%02d" i))
  done;
  mode := "empty";
  let cached =
    Result.get_ok (invoke admin "location_get" {|{"person":"Alice"}|})
  in
  check "empty poll retains timestamped last fix" (contains cached "52.2");
  let prior_reads = !reads in
  ignore (invoke admin "location_get" {|{"person":"Alice","refresh":false}|});
  check "cached read has no network access" (!reads = prior_reads);
  List.iter
    (fun scenario ->
      mode := scenario;
      let result =
        Result.get_ok (invoke admin "location_get" {|{"person":"Alice"}|})
      in
      check "Recorder failures are redacted and cached fix labelled"
        (contains result "refresh_error"
        && contains result "cached_link"
        && not (contains result "fixture-password")))
    [ "invalid"; "exception"; "flood" ];
  mode := "redirect";
  let prior_reads = !reads in
  check "redirect cannot reach foreign endpoint"
    (bad (fun () -> Owntracks_source.latest source) && !reads = prior_reads + 1);
  mode := "subpath";
  let prior_reads = !reads in
  check "Recorder scope excludes endpoint subpaths"
    (bad (fun () -> Owntracks_source.latest source) && !reads = prior_reads + 1);
  List.iter
    (fun scenario ->
      mode := scenario;
      let before = !reads in
      check "redirect cannot change user/device selection"
        (bad (fun () -> Owntracks_source.latest source) && !reads = before + 1))
    [ "other-user"; "other-device"; "duplicate-user" ];
  mode := "normal";
  let prior_reads = !reads in
  List.iter
    (fun actor ->
      check "unauthorized discovery denied"
        (Result.is_error (invoke actor "location_devices" "{}"));
      check "unauthorized location read denied"
        (Result.is_error (invoke actor "location_get" {|{"person":"Alice"}|}));
      check "unauthorized erase denied"
        (Result.is_error
           (invoke actor "location_detach" {|{"person":"Alice"}|})))
    [ bot; "@unknown:example.org" ];
  check "unauthorized tools never reach Recorder" (!reads = prior_reads);
  revoke := true;
  check "revocation during lookup denied"
    (Result.is_error (invoke alice "location_get" {|{"person":"Alice"}|}));
  revoke := false;
  Store.set_person store ~actor:admin ~user:alice ~role:Friend ~allowed:true;
  let models = ref 0 in
  let config = Config.default ~admin ~homeserver:"https://matrix.example.org" in
  let model =
    Openrouter.of_fetch ~base_url:"https://model.example/v1"
      (Fetch_mock.client (fun req ->
           incr models;
           let encoded =
             match req.body with Fetch.String s -> s | _ -> assert false
           in
           check "model context has no configuration secret"
             ((not (contains encoded "fixture-password"))
             && (not (contains encoded "recorder.example"))
             && (not (contains encoded "/operator/"))
             && not (contains encoded "unrelated-mqtt-secret"));
           let body =
             if !models = 1 then
               {|{"id":"1","model":"test","created":1,"object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"tool_calls":[{"id":"loc","type":"function","function":{"name":"location_get","arguments":"{\"person\":\"Alice\"}"}}]}}]}|}
             else begin
               check "location result reaches model" (contains encoded "52.2");
               {|{"id":"2","model":"test","created":1,"object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"Alice last reported a position at 2026-09-08T23:59:10Z."}}]}|}
             end
           in
           Fetch_mock.respond
             ~headers:
               (Http.Header.of_list [ ("Content-Type", "application/json") ])
             body req))
  in
  let engine =
    Engine.create ~config ~store ~self:"@crow:example.org" ~plugins:[]
      ~now:(fun () -> !current)
      ~complete:(fun messages tools ->
        check "location tools available alongside memory and cron"
          (List.length tools = 15);
        App.complete env config model messages tools)
    |> fun engine -> Engine.with_locations engine locations
  in
  let replies = ref [] in
  Engine.handle engine
    ~send:(fun s -> replies := s :: !replies)
    Engine.
      {
        room;
        sender = alice;
        id = "$question";
        body = "!crow Where was Alice last seen?";
      };
  check "model tool round trip" (!models = 2 && List.length !replies = 1);
  let logs =
    Store.tool_uses store ~day:(Store.today store) ~after:0 ~through:max_int
      ~limit:100
  in
  check "location audited without coordinates or secrets"
    (match logs with
    | [ log ] ->
        log.tool = "location_get" && log.actor = alice && log.status = "ok"
        && log.arguments = "[location content omitted]"
        && log.result = "[location content omitted]"
    | _ -> false);
  mode := "mobile";
  let mobile =
    Result.get_ok (invoke alice "location_get" {|{"person":"Alice"}|})
  in
  check "newer report clears unavailable Wi-Fi instead of retaining an old SSID"
    (contains mobile {|"wifi_ssid":null|}
    && contains mobile {|"wifi_bssid":null|}
    && contains mobile {|"connection_type":"mobile"|});
  mode := "normal";
  let stale =
    Result.get_ok (invoke alice "location_get" {|{"person":"Alice"}|})
  in
  check "older Wi-Fi report cannot replace newer cached mobile report"
    (contains stale {|"wifi_ssid":null|}
    && contains stale {|"connection_type":"mobile"|});
  mode := "wifi-change";
  let history =
    Owntracks_source.history source ~from:1788911900. ~until:1788912000.
  in
  check "Wi-Fi changes at the same GPS fix survive history deduplication"
    (List.map (fun (p : Location_store.point) -> p.ssid) history
    = [ Some "Home Wi-Fi"; Some "Office Wi-Fi" ]);
  mode := "normal";
  let old_link =
    Option.get (Location_store.get state ~actor:admin ~person:"Alice")
  in
  check "friend can erase shared link"
    (invoke alice "location_detach" {|{"person":"Alice"}|}
    = Ok {|{"erased":true}|});
  check "erased state gone"
    (Location_store.get state ~actor:admin ~person:"Alice" = None);
  check "in-flight poll cannot recreate an erased link"
    (bad (fun () ->
         Location_store.update state ~actor:admin old_link old_link.point));
  ignore (Store.create ~now:(fun () -> !current) db ~admin);
  check "typed schema reopens"
    (Location_store.list state ~actor:admin ~after:"" = []);
  check "runtime never reloads the configuration capability" (!loads = 1);
  check "legacy unrestricted entries fail before loading credentials"
    (bad (fun () ->
         Owntracks_source.initialize ~load ~fetch ~clock
           ~now:(fun () -> !current)
           (json
              {|{"url":"https://recorder.example","password":"legacy-secret"}|}))
    && !loads = 1);
  let config_path =
    Eio.Path.(Eio.Stdenv.cwd env / "owntracks-linked-fixture.toml")
  in
  let native =
    let path = Eio.Path.native_exn config_path in
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  in
  let write password =
    Eio.Path.save ~create:(`Or_truncate 0o600) config_path
      ("[owntracks.recorder]\n\
        url=\"https://recorder.example/tracks\"\n\
        user=\"operator\"\n\
        password=\"" ^ password ^ "\"\n")
  in
  write "fixture-password";
  let encoded_path =
    Result.get_ok (Jsont_bytesrw.encode_string Jsont.string native)
  in
  let linked =
    json
      (Printf.sprintf
         {|{"config_file":%s,"user":"alice","device":"phone","allow_http":false,"lookback_days":7}|}
         encoded_path)
  in
  let auths = ref [] in
  let transport =
    Fetch_mock.client (fun request ->
        auths := Http.Header.get request.headers "authorization" :: !auths;
        let uri =
          Uri.of_string (Fetch.Middleware.Url.path_and_query request.url)
        in
        check "reference reload keeps the allowed tracker fixed"
          (Uri.get_query_param uri "user" = Some "alice"
          && Uri.get_query_param uri "device" = Some "phone");
        Fetch_mock.respond "[]" request)
  in
  let initialize () =
    Owntracks_source.initialize ~load:Owntracks_source.load_config
      ~fetch:transport ~clock
      ~now:(fun () -> !current)
      linked
  in
  let first = initialize () in
  ignore (Owntracks_source.latest first);
  write "rotated-password";
  ignore (Owntracks_source.latest first);
  ignore (Owntracks_source.latest (initialize ()));
  check "rotation in the original file is picked up only on reinitialization"
    (match !auths with
    | [ fresh; retained; original ] ->
        fresh <> None && fresh <> original && original = retained
        && original = Some "Basic b3BlcmF0b3I6Zml4dHVyZS1wYXNzd29yZA=="
    | _ -> false);
  Eio.Path.save ~create:(`Or_truncate 0o600) config_path
    "[owntracks.recorder]\npassword = \"secret-in-invalid-TOML\n";
  let error =
    try
      ignore (initialize ());
      ""
    with Invalid_argument message -> message
  in
  check "TOML parse errors never expose secret values"
    (error <> "" && not (contains error "secret-in-invalid-TOML"));
  Eio.Path.unlink config_path;
  let legacy = Sqlite3_eio.open_memory ~sw () in
  Sqlite3.Rc.check
    (Sqlite3_eio.exec legacy
       {|
CREATE TABLE tool_schemas(name TEXT PRIMARY KEY,version INTEGER NOT NULL);
INSERT INTO tool_schemas VALUES('locations',1);
CREATE TABLE locations_people(person TEXT PRIMARY KEY,connection TEXT NOT NULL,user TEXT NOT NULL,
 device TEXT NOT NULL,actor TEXT NOT NULL,room TEXT NOT NULL,event TEXT NOT NULL,
 attached_at TEXT NOT NULL,latitude REAL,longitude REAL,accuracy REAL,recorded_at REAL,checked_at TEXT);
INSERT INTO locations_people VALUES('Legacy','home','alice','phone','@admin:example.org','!room:example.org','$old',
 '2026-09-08T23:59:10Z',52.2,0.2,5,1788911950,'2026-09-08T23:59:10Z');
|});
  let migrated = Store.create legacy ~admin in
  check "location migration preserves old fixes with absent Wi-Fi"
    (match
       Location_store.get (Store.locations migrated) ~actor:admin
         ~person:"Legacy"
     with
    | Some { point = Some p; _ } ->
        p.latitude = 52.2 && p.ssid = None && p.bssid = None
        && p.reported_at = None
    | _ -> false);
  ignore (Store.create legacy ~admin);
  print_endline
    "crowthebot: OwnTracks locations, capability scope, authority and audit \
     passed"
