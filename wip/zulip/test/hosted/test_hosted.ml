open Zulip_eio

exception Callback_failure

let ok = function
  | Ok x -> x
  | Error e -> Alcotest.fail (Error.error_to_string e)

let json s =
  match Jsont_bytesrw.decode_string Jsont.json s with
  | Ok j -> j
  | Error e -> failwith e

let headers = Http.Header.of_list [ ("content-type", "application/json") ]
let success = {|{"result":"success","msg":""}|}

let body (request : Fetch.Middleware.request) =
  match request.body with
  | Fetch.Empty -> ""
  | Fetch.String s -> s
  | Fetch.Stream { flow; _ } ->
      Eio.Buf_read.(take_all (of_flow ~max_size:65536 flow))

let fields request = Httpz_media.Urlencoded.decode (body request)

let make env ?(site = "https://zulip.test") ?user_agent handler =
  let auth =
    Auth.create ~site ~email:"bot@zulip.test" ~api_key:"test-key" |> ok
  in
  let transport =
    Transport.of_fetch ~clock:env#clock (Fetch_mock.client handler)
  in
  Client.create ~transport ~auth ?user_agent ()

let with_client handler f =
  Eio_mock.Backend.run_full (fun env -> f env (make env handler |> ok))

let test_identity_and_json () =
  Eio_mock.Backend.run_full @@ fun env ->
  let handler (request : Fetch.Middleware.request) =
    Alcotest.(check string)
      "normalized API suffix" "/api/v1/test"
      (Fetch.Middleware.Url.path_and_query request.url);
    Alcotest.(check (option string))
      "custom identity" (Some "my-bot/1")
      (Http.Header.get request.headers "user-agent");
    Alcotest.(check (list (pair string string)))
      "single JSON serialization"
      [ ("text", "λ &+"); ("null", "null"); ("array", "[1,true]") ]
      (fields request);
    Fetch_mock.respond ~headers success request
  in
  let client =
    make env ~site:"https://zulip.test/api" ~user_agent:"my-bot/1" handler |> ok
  in
  Client.request_json client ~method_:`POST ~path:"test"
    ~params:
      [
        ("text", Some (Jsont.Json.string "λ &+"));
        ("absent", None);
        ("null", Some (json "null"));
        ("array", Some (json "[1,true]"));
      ]
    ()
  |> ok |> ignore;
  List.iter
    (fun user_agent ->
      Alcotest.(check bool)
        "invalid identity rejected" true
        (Result.is_error (make env ~user_agent handler)))
    [ ""; "x\r\ny"; "x\000y" ]

let test_registration_snapshot () =
  with_client
    (fun request ->
      let params = fields request in
      List.iter
        (fun (key, value) ->
          Alcotest.(check (option string))
            key (Some value)
            (List.assoc_opt key params))
        [
          ("apply_markdown", "true");
          ("client_gravatar", "true");
          ("include_subscribers", "true");
          ("slim_presence", "false");
          ("presence_history_limit_days", "7");
          ("idle_queue_timeout", "90");
          ("fetch_event_types", {|["realm_user","user_settings"]|});
          ( "client_capabilities",
            {|{"notification_settings_null":true,"empty_topic_name":true}|} );
        ];
      Fetch_mock.respond ~headers
        {|{"result":"success","queue_id":"q","last_event_id":-1,"realm_users":[{"user_id":1,"email":"b@test","full_name":"Renamed","is_bot":true}],"realm_non_active_users":[{"user_id":2,"email":"i@test","full_name":"Inactive"}],"user_settings":{"twenty_four_hour_time":true},"muted_users":[],"future_snapshot":{"x":1}}|}
        request)
    (fun _ client ->
      let options : Event_queue.registration_options =
        {
          apply_markdown = true;
          client_gravatar = true;
          include_subscribers = `All;
          slim_presence = false;
          presence_history_limit_days = Some 7;
          idle_queue_timeout = Some (Event_queue.Seconds 90);
          fetch_event_types =
            Some [ Zulip.Event_type.Realm_user; User_settings ];
          client_capabilities = [ ("empty_topic_name", true) ];
        }
      in
      let state =
        Event_queue.register client ~options () |> ok |> Event_queue.state
      in
      let users = Initial_state.users state |> ok |> Option.get in
      Alcotest.(check bool)
        "registration active default" true
        (Zulip.User.is_active (List.hd users));
      let inactive = Initial_state.inactive_users state |> ok |> Option.get in
      Alcotest.(check bool)
        "inactive family" false
        (Zulip.User.is_active (List.hd inactive));
      Alcotest.(check (option bool))
        "typed setting" (Some true)
        (Initial_state.setting state "twenty_four_hour_time" Jsont.bool |> ok);
      Alcotest.(check bool)
        "missing subscriptions distinct" true
        (Initial_state.subscriptions state |> ok = None);
      Alcotest.(check bool)
        "present empty muted users" true
        (Initial_state.muted_users state |> ok = Some []);
      Alcotest.(check bool)
        "extension snapshot retained" true
        (Option.is_some (Initial_state.field state "future_snapshot")))

let test_default_registration () =
  with_client
    (fun request ->
      Alcotest.(check (option string))
        "fetch default left to server" None
        (List.assoc_opt "fetch_event_types" (fields request));
      Fetch_mock.respond ~headers
        {|{"result":"success","queue_id":"q","last_event_id":-1}|} request)
    (fun _ client -> ignore (Event_queue.register client () |> ok))

let test_partial_registration () =
  with_client
    (fun request ->
      let params = fields request in
      Alcotest.(check (option string))
        "partial subscribers" (Some "partial")
        (List.assoc_opt "include_subscribers" params);
      Alcotest.(check (option string))
        "mobile idle timeout" (Some "\"mobile\"")
        (List.assoc_opt "idle_queue_timeout" params);
      Alcotest.(check (option string))
        "required default capability"
        (Some {|{"notification_settings_null":true}|})
        (List.assoc_opt "client_capabilities" params);
      Fetch_mock.respond ~headers
        {|{"result":"success","queue_id":"q","last_event_id":-1}|} request)
    (fun _ client ->
      let options =
        {
          Event_queue.default_registration with
          include_subscribers = `Partial;
          idle_queue_timeout = Some Mobile;
        }
      in
      ignore (Event_queue.register client ~options () |> ok))

let test_malformed_message_collector () =
  let deleted = ref false in
  with_client
    (fun (request : Fetch.Middleware.request) ->
      if request.meth = `DELETE then (
        deleted := true;
        Fetch_mock.respond ~headers success request)
      else if
        Fetch.Middleware.Url.path_and_query request.url = "/api/v1/register"
      then
        Fetch_mock.respond ~headers
          {|{"result":"success","queue_id":"q","last_event_id":-1}|} request
      else
        Fetch_mock.respond ~headers
          {|{"result":"success","events":[{"id":0,"type":"message","message":{}}]}|}
          request)
    (fun _ client ->
      match
        Event_queue.iter_messages client (fun _ ->
            Alcotest.fail "malformed message reached callback")
      with
      | Error (Error.Json _) -> ()
      | _ ->
          Alcotest.fail
            "collector failed to return structured malformed-message error");
  Alcotest.(check bool) "malformed collector unregisters" true !deleted

let test_collector () =
  Eio_mock.Backend.run_full @@ fun env ->
  let attempts = ref 0
  and registered_at = ref None
  and deleted = ref 0
  and captured = ref None in
  let started = Eio.Time.now env#clock in
  let handler (request : Fetch.Middleware.request) =
    let target = Fetch.Middleware.Url.path_and_query request.url in
    if request.meth = `DELETE then (
      incr deleted;
      Fetch_mock.respond ~headers success request)
    else if target = "/api/v1/register" then (
      incr attempts;
      if !attempts = 1 then
        Fetch_mock.respond ~status:429
          ~headers:(Http.Header.add headers "retry-after" "7")
          {|{"result":"error","code":"RATE_LIMIT_HIT","msg":"wait"}|} request
      else (
        registered_at := Some (Eio.Time.now env#clock);
        Fetch_mock.respond ~headers
          {|{"result":"success","queue_id":"q","last_event_id":-1}|} request))
    else
      Fetch_mock.respond ~headers
        {|{"result":"success","events":[{"id":0,"type":"heartbeat"},{"id":1,"type":"future"},{"id":2,"type":"future"}]}|}
        request
  in
  let client = make env handler |> ok in
  let received = ref [] in
  Event_queue.iter client
    ~on_registered:(fun q -> captured := Some q)
    (fun event ->
      received := Zulip.Id.Event.to_int event.id :: !received;
      Event_queue.Stop)
  |> ok;
  Alcotest.(check (list int))
    "heartbeat skipped and stop respected" [ 1 ] !received;
  Alcotest.(check int)
    "only accepted events acknowledged" 1
    (Event_queue.last_event_id (Option.get !captured));
  Alcotest.(check bool)
    "server retry-after honored" true
    (Option.get !registered_at -. started >= 7.);
  Alcotest.(check int) "queue removed on exit" 1 !deleted

let test_callback_failure_cleanup () =
  let deleted = ref false and queue = ref None in
  with_client
    (fun (request : Fetch.Middleware.request) ->
      if request.meth = `DELETE then (
        deleted := true;
        Fetch_mock.respond ~headers success request)
      else if
        Fetch.Middleware.Url.path_and_query request.url = "/api/v1/register"
      then
        Fetch_mock.respond ~headers
          {|{"result":"success","queue_id":"q","last_event_id":-1}|} request
      else
        Fetch_mock.respond ~headers
          {|{"result":"success","events":[{"id":4,"type":"future"}]}|} request)
    (fun _ client ->
      (try
         ignore
           (Event_queue.iter client
              ~on_registered:(fun q -> queue := Some q)
              (fun _ -> raise Callback_failure));
         Alcotest.fail "callback swallowed"
       with
      | Callback_failure -> ()
      | ex -> raise ex);
      Alcotest.(check int)
        "rejected callback not acknowledged" (-1)
        (Event_queue.last_event_id (Option.get !queue)));
  Alcotest.(check bool) "callback failure removes queue" true !deleted

let test_identity_events () =
  with_client (Fetch_mock.respond ~headers success) @@ fun env client ->
  Eio.Switch.run @@ fun sw ->
  let id = Zulip.Id.User.of_int 1 in
  let identity : Zulip_bot.Context.identity =
    { user_id = id; email = "b@test"; full_name = "Old" }
  in
  let context = Zulip_bot.Context.v ~sw ~client ~identity ~clock:env#clock () in
  let state =
    Initial_state.of_json
      (json
         {|{"realm_users":[{"user_id":1,"email":"b@test","full_name":"New","is_bot":true},{"user_id":2,"email":"human@test","full_name":"Human","is_bot":false}]}|})
    |> ok
  in
  Zulip_bot.Context.apply_initial_state context state |> ok;
  Alcotest.(check string)
    "snapshot renamed identity" "New"
    (Zulip_bot.Context.identity context).full_name;
  Alcotest.(check bool) "known bot" true (Zulip_bot.Context.is_bot context id);
  let event =
    Zulip.Event.create ~id:(Zulip.Id.Event.of_int 0) ~type_:Realm_user
      ~data:
        (json
           {|{"op":"update","person":{"user_id":1,"full_name":"Newest","new_email":"renamed@test","is_bot":false}}|})
    |> Result.get_ok
  in
  Zulip_bot.Context.observe_event context event;
  Alcotest.(check string)
    "new_email event field updates identity" "renamed@test"
    (Zulip_bot.Context.identity context).email;
  let malformed =
    Zulip.Event.create ~id:(Zulip.Id.Event.of_int 0) ~type_:Realm_user
      ~data:(json {|{"op":"add","person":{"user_id":1,"full_name":"Invalid"}}|})
    |> Result.get_ok
  in
  Zulip_bot.Context.observe_event context malformed;
  Alcotest.(check string)
    "event renamed identity" "Newest"
    (Zulip_bot.Context.identity context).full_name;
  Alcotest.(check bool)
    "classification updated" false
    (Zulip_bot.Context.is_bot context id)

let test_location_refresh () =
  with_client (Fetch_mock.respond ~headers success) @@ fun env client ->
  Eio.Switch.run @@ fun sw ->
  let identity : Zulip_bot.Context.identity =
    { user_id = Zulip.Id.User.of_int 1; email = "b@test"; full_name = "Bot" }
  in
  let context = Zulip_bot.Context.v ~sw ~client ~identity ~clock:env#clock () in
  let destination topic =
    Zulip.Message.Channel
      { channel_id = Zulip.Id.Channel.of_int 1; channel_name = "test"; topic }
  in
  let remember id topic =
    let message =
      Jsont.Json.decode Zulip.Message.jsont
        (json
           (Printf.sprintf
              {|{"id":%d,"sender_id":2,"sender_email":"u@test","sender_full_name":"User","timestamp":0,"content":"text","content_type":"text/x-markdown","type":"stream","stream_id":1,"display_recipient":"test","subject":%S}|}
              id topic))
      |> Result.get_ok
    in
    ignore (Zulip_bot.Event.of_message context message)
  in
  let find () =
    let event =
      Jsont.Json.decode Zulip.Event.jsont
        (json
           {|{"id":1,"type":"reaction","op":"add","message_id":1,"user_id":2,"emoji_name":"smile","emoji_code":"1f600","reaction_type":"unicode_emoji"}|})
      |> Result.get_ok
    in
    match Zulip_bot.Event.of_zulip context event with
    | Some (Zulip_bot.Event.Reaction reaction) ->
        Some (Zulip_bot.Room.destination reaction.envelope.room)
    | _ -> None
  in
  remember 1 "old";
  remember 1 "moved";
  for id = 2 to 1024 do
    remember id "other"
  done;
  Alcotest.(check bool)
    "old queue entry cannot evict refreshed location" true
    (find () = Some (destination "moved"));
  remember 1025 "new";
  Alcotest.(check bool)
    "oldest current entry is eventually evicted" true
    (find () = None)

let test_malformed_snapshots_and_storage () =
  List.iter
    (fun text ->
      let state = Initial_state.of_json (json text) |> ok in
      match Initial_state.settings state with
      | Error (Error.Json _) -> ()
      | _ -> Alcotest.fail "malformed settings were treated as absent")
    [
      {|{"user_settings":null}|};
      {|{"user_settings":3}|};
      {|{"user_settings":[]}|};
    ];
  with_client
    (fun request ->
      Fetch_mock.respond ~headers
        {|{"result":"success","storage":{"x":"one","x":"two"}}|} request)
    (fun _ client ->
      match Bot_storage.get client () with
      | Error (Error.Json _) -> ()
      | _ -> Alcotest.fail "duplicate storage keys accepted")

let test_invalid_queue_cursor () =
  List.iter
    (fun cursor ->
      with_client
        (fun request ->
          Fetch_mock.respond ~headers
            (Printf.sprintf
               {|{"result":"success","queue_id":"q","last_event_id":%s}|} cursor)
            request)
        (fun _ client ->
          match Event_queue.register client () with
          | Error (Error.Json _) -> ()
          | _ -> Alcotest.fail "invalid event cursor accepted"))
    [ "-2"; "0.5"; "9007199254740992" ]

let () =
  Alcotest.run "Hosted bot foundation"
    [
      ( "hosted",
        List.map
          (fun (n, f) -> Alcotest.test_case n `Quick f)
          [
            ("client identity and JSON parameters", test_identity_and_json);
            ("registration options and state", test_registration_snapshot);
            ( "malformed snapshots and storage",
              test_malformed_snapshots_and_storage );
            ("invalid queue cursor", test_invalid_queue_cursor);
            ("default registration", test_default_registration);
            ("partial subscribers and mobile idle", test_partial_registration);
            ("malformed message collector", test_malformed_message_collector);
            ("collector acceptance and Retry-After", test_collector);
            ("callback cleanup", test_callback_failure_cleanup);
            ("identity cache", test_identity_events);
            ("refreshed location eviction", test_location_refresh);
          ] );
    ]
