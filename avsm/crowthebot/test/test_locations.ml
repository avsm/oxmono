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
      {|{"url":"https://recorder.example/tracks","username":"operator","password":"fixture-password","allow_http":false,"lookback_days":7}|}
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
        else if !mode = "invalid" then
          Fetch_mock.respond {|{"data":"fixture-password"}|} req
        else if !mode = "exception" then invalid_arg "fixture-password"
        else if !mode = "flood" then
          Fetch_mock.respond ("[\"" ^ String.make (1024 * 1024) 'x' ^ "\"]") req
        else if Uri.path uri = "/tracks/api/0/list" then
          Fetch_mock.respond
            (if !mode = "many" then
               Result.get_ok
                 (Jsont_bytesrw.encode_string (Jsont.list Jsont.string)
                    (List.init 37 (fun i ->
                         Printf.sprintf "%02d-%s" i (String.make 240 'x'))))
             else if Uri.get_query_param uri "user" = None then {|["alice"]|}
             else {|{"results":["phone"]}|})
            req
        else begin
          check "Recorder bounded query"
            (Uri.get_query_param uri "from" = Some "2026-09-02"
            && Uri.get_query_param uri "to" = Some "2026-09-10");
          check "Recorder device query encoded"
            (Uri.get_query_param uri "user" = Some "alice"
            && Uri.get_query_param uri "device" = Some "phone & watch");
          Fetch_mock.respond
            (if !mode = "empty" then "[]"
             else
               {|{"data":[{"lat":51.1,"lon":0.1,"tst":1788911900,"acc":8},{"lat":52.2,"lon":0.2,"tst":1788911950,"acc":5},{"lat":100,"lon":0.2,"tst":1788911999},{"lat":1,"lon":2,"tst":1788999999}]}|})
            req
        end)
  in
  let source =
    Owntracks_source.initialize ~fetch ~clock ~now:(fun () -> !current) settings
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
    = Ok {|{"connections":[{"name":"home","default":true}],"next_after":null}|}
    );
  check "users discovered"
    ( Result.get_ok (invoke alice "location_devices" "{}") |> fun s ->
      contains s "alice" );
  check "devices discovered"
    ( Result.get_ok (invoke alice "location_devices" {|{"user":"alice"}|})
    |> fun s -> contains s "phone" );
  mode := "many";
  let page_codec =
    Jsont.Object.map (fun users next -> (users, next))
    |> Jsont.Object.mem "users" (Jsont.list Jsont.string)
    |> Jsont.Object.mem "next_after" (Jsont.option Jsont.string)
    |> Jsont.Object.finish
  in
  let rec all pages after acc =
    check "pagination progresses" (pages < 10);
    let args = "{\"after\":\"" ^ after ^ "\"}" in
    let result = Result.get_ok (invoke alice "location_devices" args) in
    check "pages fit tool output budget" (String.length result <= 3800);
    let users, next =
      Result.get_ok (Jsont_bytesrw.decode_string page_codec result)
    in
    match next with
    | None -> acc @ users
    | Some next -> all (pages + 1) next (acc @ users)
  in
  let users = all 0 "" [] in
  check "discovery pagination retains every user once"
    (List.length users = 37
    && List.length (List.sort_uniq String.compare users) = 37);
  mode := "normal";
  let attached =
    Result.get_ok
      (invoke alice "location_attach"
         {|{"person":"Alice","user":"alice","device":"phone & watch"}|})
  in
  check "latest valid position chosen"
    (contains attached "52.2"
    && contains attached "recorded_at"
    && contains attached "accuracy_metres");
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
    | Some { point = Some p; _ } -> p.latitude = 52.2
    | _ -> false);
  check "locations separate from memory"
    (Store.search_facts store ~actor:admin ~query:"" = []);
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
    (bad (fun () -> Owntracks_source.users source) && !reads = prior_reads + 1);
  mode := "subpath";
  let prior_reads = !reads in
  check "Recorder scope excludes endpoint subpaths"
    (bad (fun () -> Owntracks_source.users source) && !reads = prior_reads + 1);
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
             && not (contains encoded "recorder.example"));
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
          (List.length tools = 13);
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
  print_endline
    "crowthebot: OwnTracks locations, capability scope, authority and audit \
     passed"
