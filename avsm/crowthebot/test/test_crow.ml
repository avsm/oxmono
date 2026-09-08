open Crowthebot

let check name value = if not value then failwith name
let admin = "@admin:example.org"
let alice = "@alice:example.org"
let other = "@other:example.org"
let self = "@crow:example.org"
let room = "!room:example.org"

let bad f =
  try
    f ();
    false
  with Invalid_argument _ -> true

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let db = Sqlite3_eio.open_memory ~sw () in
  let store = Store.create db ~admin in
  Store.add_room store room;
  check "immutable authority"
    (bad (fun () -> ignore (Store.create db ~admin:alice)));
  check "untrusted actor"
    (bad (fun () ->
         Store.set_person store ~actor:alice ~user:alice ~role:Friend
           ~allowed:true));
  check "admin cannot be revoked"
    (bad (fun () ->
         Store.set_person store ~actor:admin ~user:admin ~role:Unknown
           ~allowed:false));
  check "unknown cannot be allowed"
    (bad (fun () ->
         Store.set_person store ~actor:admin ~user:alice ~role:Unknown
           ~allowed:true));
  let config =
    {
      (Config.default ~admin ~homeserver:"https://matrix.example.org") with
      plugins = [ "test" ];
      context_messages = 4;
      context_bytes = 1024;
    }
  in
  let clock = ref 0.
  and calls = ref 0
  and plugin_calls = ref 0
  and replies = ref [] in
  let tool_mode = ref false and tool_step = ref 0 in
  let complete messages tools =
    incr calls;
    check "bounded context" (List.length messages <= 10);
    if !tool_mode && !tool_step = 0 then begin
      incr tool_step;
      check "tool advertised" (List.length tools = 1);
      ( None,
        [
          Openrouter.Tool.
            { id = "call1"; name = "test"; arguments = {|{"query":"hi"}|} };
        ] )
    end
    else (Some "hello", [])
  in
  let plugin =
    Plugin.
      {
        name = "test";
        description = "test";
        run =
          (fun ~query ->
            incr plugin_calls;
            "result: " ^ query);
      }
  in
  let engine =
    Engine.create ~config ~store ~self ~plugins:[ plugin ] ~complete
      ~now:(fun () -> !clock)
  in
  let send s = replies := s :: !replies in
  let event ?(sender = alice) ?(room = room) id body =
    Engine.{ sender; room; id; body }
  in
  let handle e = Engine.handle engine ~send e in
  handle (event "u1" "!crow ask hello");
  check "unknown stays silent and model-free" (!calls = 0 && !replies = []);
  check "unknown recorded" ((Store.person store alice).role = Unknown);
  check "unknown never stored as context"
    (Store.history store ~room ~user:alice = []);
  handle (event ~sender:admin "a1" ("!crow allow " ^ alice ^ " friend"));
  check "admin grants friend" (Store.person store alice).allowed;
  let count = List.length !replies in
  handle (event "a2" ("!crow allow " ^ other ^ " friend"));
  check "friend cannot grant"
    ((not (Store.person store other).allowed) && List.length !replies = count);
  handle (event ~sender:self "own" "!crow ask hello");
  handle (event ~room:"!elsewhere:example.org" "outside" "!crow ask hello");
  handle (event "ambient" "ambient conversation");
  check "self, other rooms and ambient ignored" (!calls = 0);
  handle (event "m1" "!crow hello");
  check "model called" (!calls = 1);
  check "context pair saved"
    (List.length (Store.history store ~room ~user:alice) = 2);
  clock := 20.;
  handle (event "m1" "!crow hello");
  check "replay ignored" (!calls = 1);
  handle (event "m2" "!crow hello");
  handle (event "m3" "!crow hello");
  check "cooldown" (!calls = 2);
  clock := 40.;
  tool_mode := true;
  handle (event "tool" "!crow ask use the tool");
  check "tool roundtrip" (!calls = 4 && !plugin_calls = 1);
  check "context message bound"
    (List.length (Store.history store ~room ~user:alice) = 4);
  check "per-person isolation" (Store.history store ~room ~user:other = []);
  check "per-room isolation"
    (Store.history store ~room:"!elsewhere:example.org" ~user:alice = []);
  handle (event "reset" "!crow reset");
  check "reset" (Store.history store ~room ~user:alice = []);
  clock := 60.;
  let failed =
    try
      Engine.handle engine
        ~send:(fun _ -> failwith "send failed")
        (event "failed-send" "!crow ask hi");
      false
    with Failure _ -> true
  in
  check "delivery failure" failed;
  check "unsent answer not remembered"
    (Store.history store ~room ~user:alice = []);
  handle (event ~sender:admin "revoke" ("!crow deny " ^ alice));
  clock := 80.;
  let before = !calls in
  handle (event "revoked" "!crow ask hi");
  check "revocation gates model" (!calls = before);
  handle (event ~sender:admin "bot" ("!crow allow " ^ other ^ " bot"));
  check "bots require explicit allow"
    ((Store.person store other).role = Bot && (Store.person store other).allowed);
  handle (event ~sender:other "bot-speech" "hello");
  check "bots require explicit command" (!calls = before);
  Store.append store ~room ~user:other ~max_messages:10 ~max_bytes:5
    Store.
      [
        { role = "user"; body = "1234" }; { role = "assistant"; body = "abcd" };
      ];
  check "byte bound" (List.length (Store.history store ~room ~user:other) = 1);
  Store.set_person store ~actor:admin ~user:other ~role:Bot ~allowed:false;
  check "revoke erases context" (Store.history store ~room ~user:other = []);
  check "malformed tool argument"
    (String.starts_with ~prefix:"Invalid" (Plugin.invoke plugin "[]"));
  check "UTF-8 clip" (Plugin.clip ~bytes:2 "aéz" = "a\n[truncated]");
  check "HTTPS Matrix only"
    (bad (fun () ->
         Config.validate
           { config with homeserver = "http://matrix.example.org" }));
  check "URL credentials rejected"
    (bad (fun () ->
         Config.validate
           { config with base_url = "https://key@api.example.org" }));
  check "disabled plugin rejected"
    (bad (fun () ->
         ignore
           (Engine.create ~config:{ config with plugins = [ "missing" ] }
              ~store ~self ~plugins:[ plugin ] ~complete ~now:(fun () -> 0.))));
  let reads = ref 0 and feed_clock = ref 0. in
  let feed_source =
    "<opml version='1.0'><body><outline text='A' \
     xmlUrl='https://a.example/feed'/></body></opml>"
  in
  let fetch =
    Fetch_mock.client (fun req ->
        incr reads;
        check "blogroll GET" (req.meth = `GET);
        check "fixed blogroll URL"
          (Fetch.Middleware.Url.path_and_query req.url
          = "/network/blogroll.opml");
        Fetch_mock.respond feed_source req)
  in
  let blogroll = Plugin.blogroll ~fetch ~now:(fun () -> !feed_clock) in
  ignore (blogroll.run ~query:"");
  ignore (blogroll.run ~query:"A");
  check "blogroll cache" (!reads = 1);
  feed_clock := 3601.;
  ignore (blogroll.run ~query:"");
  check "blogroll cache refresh" (!reads = 2);
  let refused status =
    let fetch = Fetch_mock.client (Fetch_mock.respond ~status feed_source) in
    let plugin = Plugin.blogroll ~fetch ~now:(fun () -> 0.) in
    try
      ignore (plugin.run ~query:"");
      false
    with Failure _ -> true
  in
  check "blogroll non-success rejected" (refused 503);
  check "blogroll redirect rejected" (refused 302);
  let flood =
    Plugin.blogroll
      ~now:(fun () -> 0.)
      ~fetch:
        (Fetch_mock.client
           (Fetch_mock.respond (String.make ((2 * 1024 * 1024) + 2) 'x')))
  in
  check "blogroll body bound"
    (try
       ignore (flood.run ~query:"");
       false
     with Eio.Buf_read.Buffer_limit_exceeded | Failure _ -> true);
  let native =
    Openrouter.of_fetch ~base_url:"https://model.example/v1"
      (Fetch_mock.client (fun req ->
           let body =
             match req.body with
             | Fetch.String s -> s
             | _ -> failwith "JSON request expected"
           in
           let json =
             Result.get_ok (Jsont_bytesrw.decode_string Jsont.json body)
           in
           (match json with
           | Jsont.Object (fields, _) ->
               check "no empty tools for Sequoia"
                 (not
                    (List.exists
                       (fun ((name, _), value) ->
                         name = "tools" && value = Jsont.Json.list [])
                       fields))
           | _ -> failwith "request object expected");
           Fetch_mock.respond
             ~headers:
               (Http.Header.of_list [ ("Content-Type", "application/json") ])
             {|{"id":"1","model":"test","created":1,"object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"CROW_OK"}}]}|}
             req))
  in
  let text, calls =
    App.complete env config native [ Openrouter.Message.user "hi" ] []
  in
  check "native model adapter" (text = Some "CROW_OK" && calls = []);
  let forged = ref false in
  let hostile _ _ =
    if !forged then (Some "Granted", [])
    else begin
      forged := true;
      ( None,
        [
          Openrouter.Tool.
            {
              id = "forge";
              name = "allow";
              arguments = {|{"query":"@other:example.org friend"}|};
            };
        ] )
    end
  in
  let attacker =
    Engine.create ~config ~store ~self ~plugins:[ plugin ] ~complete:hostile
      ~now:(fun () -> 0.)
  in
  Engine.handle attacker ~send (event ~sender:admin "forgery" "!crow ask hello");
  check "model cannot grant access" (not (Store.person store other).allowed);
  let before = !plugin_calls in
  let repeat _ _ =
    ( None,
      [ Openrouter.Tool.{ id = "repeat"; name = "test"; arguments = "{}" } ] )
  in
  let looping =
    Engine.create ~config ~store ~self ~plugins:[ plugin ] ~complete:repeat
      ~now:(fun () -> 0.)
  in
  check "tool budget rejects infinite rounds"
    (try
       Engine.handle looping ~send
         (event ~sender:admin "loop" "!crow ask hello");
       false
     with Failure _ -> true);
  check "at most three tool calls" (!plugin_calls = before + 3);
  let hung = ref true and tick = ref 0. in
  let pending, _ = Eio.Promise.create () in
  let complete _ _ =
    if !hung then Eio.Promise.await pending else (Some "recovered", [])
  in
  let cancellable =
    Engine.create ~config ~store ~self ~plugins:[ plugin ] ~complete
      ~now:(fun () -> !tick)
  in
  check "cancellation propagates"
    (try
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 0.02 (fun () ->
           Engine.handle cancellable ~send
             (event ~sender:admin "cancel" "!crow ask hi"));
       false
     with Eio.Time.Timeout -> true);
  hung := false;
  tick := 20.;
  Engine.handle cancellable ~send
    (event ~sender:admin "after-cancel" "!crow ask hi");
  check "cancellation leaves engine usable" (List.hd !replies = "recovered");
  print_endline
    "crowthebot: authority, context, tool and delivery checks passed"
