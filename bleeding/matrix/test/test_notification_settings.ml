module Settings = Matrix_client.Notification_settings
module Client = Matrix_client.Client
module Push = Matrix_proto.Push
module Id = Matrix_proto.Id

let user = Id.User_id.of_string_exn "@alice:example.org"
let room = Id.Room_id.of_string_exn "!room:example.org"

let session =
  {
    Client.user_id = user;
    access_token = "token";
    device_id = Id.Device_id.of_string_exn "DEVICE";
    refresh_token = None;
  }

let mock_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 4096 'n')
  end

type request = { meth : string; url : string; body : string option }

let request_body (request : Fetch.Middleware.request) =
  match request.body with
  | Fetch.Empty -> None
  | Fetch.String body -> Some body
  | Fetch.Stream _ -> Some "<stream>"

let client ?(respond = fun request -> Fetch_mock.respond "{}" request) log =
  let fetch =
    Fetch_mock.client (fun request ->
        log :=
          !log
          @ [
              {
                meth = Http.Method.to_string request.Fetch.Middleware.meth;
                url = Fetch.Middleware.Url.to_string request.url;
                body = request_body request;
              };
            ];
        respond request)
  in
  Client.create
    ~config:
      (Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ())
    ~fetch
    ~random:(Matrix_client.Random.of_env mock_env)
  |> fun client -> Client.with_session client session

let check_mode label expected actual =
  let show = function
    | Settings.All_messages -> "all"
    | Settings.Mentions_and_keywords_only -> "mentions"
    | Settings.Mute -> "mute"
  in
  Alcotest.(check string) label (show expected) (show actual)

let check_mode_opt label expected actual =
  match (expected, actual) with
  | None, None -> ()
  | Some expected, Some actual -> check_mode label expected actual
  | _ -> Alcotest.fail label

let room_condition =
  Push.Condition.Event_match
    { key = "room_id"; pattern = Id.Room_id.to_string room }

let custom_room ?(enabled = true) actions =
  Push.Rule.v ~enabled ~rule_id:(Push.Rule_id.room room) actions

let custom_override ?(id = "!room:example.org") ?(enabled = true) actions =
  Push.Rule.v ~enabled ~conditions:[ room_condition ]
    ~rule_id:(Push.Rule_id.override id) actions

let custom_underride ?(id = "custom-under") actions =
  Push.Rule.v ~conditions:[ room_condition ]
    ~rule_id:(Push.Rule_id.underride id)
    actions

let notify =
  [ Push.Action.Notify; Push.Action.Set_tweak (Push.Tweak.Sound "default") ]

let run test () = Eio_mock.Backend.run test

let test_mode_projection () =
  let log = ref [] in
  let defaults = Push.default_ruleset ~user_id:user in
  let settings = Settings.create ~ruleset:defaults (client log) in
  check_mode_opt "no custom mode" None
    (Settings.user_defined_room_mode settings room);
  List.iter
    (fun (encrypted, one_to_one) ->
      check_mode "enabled default notifies" Settings.All_messages
        (Settings.default_room_mode settings ~encrypted ~one_to_one))
    [ (false, false); (false, true); (true, false); (true, true) ];
  let ruleset =
    {
      defaults with
      room = [ custom_room notify ];
      override = custom_override [] :: defaults.override;
      underride = custom_underride [] :: defaults.underride;
    }
  in
  let settings = Settings.create ~ruleset (client log) in
  check_mode_opt "mute override wins" (Some Settings.Mute)
    (Settings.user_defined_room_mode settings room);
  Alcotest.(check (list string))
    "room IDs deduplicate" [ "!room:example.org" ]
    (Settings.rooms_with_user_defined_rules settings);
  Alcotest.(check int) "pure projection made no request" 0 (List.length !log)

let test_set_room_mode_orders_and_is_idempotent () =
  let log = ref [] in
  let old_room = custom_room [] in
  let old_under = custom_underride [] in
  let ruleset =
    { Push.Ruleset.empty with room = [ old_room ]; underride = [ old_under ] }
  in
  let settings = Settings.create ~ruleset (client log) in
  let publications = ref 0 in
  ignore (Settings.subscribe settings (fun _ -> incr publications));
  Result.get_ok (Settings.set_room_mode settings room Settings.Mute);
  check_mode_opt "new local mode" (Some Settings.Mute)
    (Settings.user_defined_room_mode settings room);
  Alcotest.(check int) "one publication" 1 !publications;
  (match !log with
  | [ put; delete_room; delete_under ] ->
      Alcotest.(check string) "put first" "PUT" put.meth;
      Alcotest.(check string)
        "canonical mute URL"
        "https://hs.example/_matrix/client/v3/pushrules/global/override/!room:example.org"
        put.url;
      Alcotest.(check (option string))
        "canonical mute body"
        (Some
           {|{"actions":[],"conditions":[{"kind":"event_match","key":"room_id","pattern":"!room:example.org"}]}|})
        put.body;
      Alcotest.(check string) "delete room second" "DELETE" delete_room.meth;
      Alcotest.(check bool)
        "room rule deleted" true
        (String.ends_with ~suffix:"/room/!room:example.org" delete_room.url);
      Alcotest.(check bool)
        "underride deleted last" true
        (String.ends_with ~suffix:"/underride/custom-under" delete_under.url)
  | requests ->
      Alcotest.failf "expected three ordered requests, got %d"
        (List.length requests));
  Result.get_ok (Settings.set_room_mode settings room Settings.Mute);
  Alcotest.(check int) "same mode makes no request" 3 (List.length !log);
  Alcotest.(check int) "same mode makes no publication" 1 !publications

let test_failed_batch_does_not_publish () =
  let log = ref [] in
  let calls = ref 0 in
  let respond request =
    incr calls;
    if !calls = 2 then
      Fetch_mock.respond ~status:500 {|{"errcode":"M_UNKNOWN","error":"boom"}|}
        request
    else Fetch_mock.respond "{}" request
  in
  let ruleset =
    {
      Push.Ruleset.empty with
      room = [ custom_room notify ];
      underride = [ custom_underride [] ];
    }
  in
  let settings = Settings.create ~ruleset (client ~respond log) in
  let publications = ref 0 in
  ignore (Settings.subscribe settings (fun _ -> incr publications));
  Alcotest.(check bool)
    "second request fails" true
    (Result.is_error (Settings.set_room_mode settings room Settings.Mute));
  Alcotest.(check int) "stopped at failure" 2 (List.length !log);
  Alcotest.(check int) "no publication" 0 !publications;
  Alcotest.(check bool)
    "local rules unchanged" true
    (Push.Ruleset.equal ruleset (Settings.ruleset settings));
  check_mode_opt "old mode remains" (Some Settings.All_messages)
    (Settings.user_defined_room_mode settings room)

let keyword ?(enabled = true) id pattern =
  Push.Rule.v ~enabled ~rule_id:(Push.Rule_id.content id) ~pattern notify

let test_keyword_lifecycle () =
  let log = ref [] in
  let one = keyword ~enabled:false "first" "ocaml" in
  let two = keyword ~enabled:false "second" "ocaml" in
  let settings =
    Settings.create
      ~ruleset:{ Push.Ruleset.empty with content = [ one; two ] }
      (client log)
  in
  Result.get_ok (Settings.add_keyword settings "ocaml");
  Alcotest.(check (list string))
    "enabled keyword is unique" [ "ocaml" ]
    (Settings.enabled_keywords settings);
  Result.get_ok (Settings.add_keyword settings "ocaml");
  Alcotest.(check int) "enabled keyword add is a no-op" 1 (List.length !log);
  Result.get_ok (Settings.remove_keyword settings "ocaml");
  Alcotest.(check int) "enable then delete both" 3 (List.length !log);
  Alcotest.(check (list string))
    "all removed" []
    (Settings.enabled_keywords settings);
  let urls = List.map (fun request -> request.url) !log in
  Alcotest.(check (list string))
    "keyword request order"
    [
      "https://hs.example/_matrix/client/v3/pushrules/global/content/first/enabled";
      "https://hs.example/_matrix/client/v3/pushrules/global/content/first";
      "https://hs.example/_matrix/client/v3/pushrules/global/content/second";
    ]
    urls

let test_new_keyword_actions () =
  let log = ref [] in
  let settings = Settings.create (client log) in
  Result.get_ok (Settings.add_keyword settings "tea");
  match !log with
  | [ request ] ->
      Alcotest.(check string)
        "keyword URL"
        "https://hs.example/_matrix/client/v3/pushrules/global/content/tea"
        request.url;
      Alcotest.(check (option string))
        "keyword body includes sound"
        (Some
           {|{"actions":["notify",{"set_tweak":"sound","value":"default"}],"pattern":"tea"}|})
        request.body
  | requests ->
      Alcotest.failf "expected one keyword request, got %d"
        (List.length requests)

let test_default_mode_updates_room_and_poll () =
  let log = ref [] in
  let room_rule =
    Push.Rule.v ~default:true ~enabled:false
      ~rule_id:(Push.Rule_id.underride ".m.rule.encrypted")
      []
  in
  let poll_rule =
    Push.Rule.v ~default:true ~enabled:false
      ~rule_id:(Push.Rule_id.underride ".m.rule.poll_start")
      []
  in
  let settings =
    Settings.create
      ~ruleset:{ Push.Ruleset.empty with underride = [ room_rule; poll_rule ] }
      (client log)
  in
  Result.get_ok
    (Settings.set_default_room_mode settings ~encrypted:true ~one_to_one:false
       Settings.All_messages);
  check_mode "default now all" Settings.All_messages
    (Settings.default_room_mode settings ~encrypted:true ~one_to_one:false);
  Alcotest.(check (list string))
    "actions then enable for room and poll"
    [ "PUT"; "PUT"; "PUT"; "PUT" ]
    (List.map (fun request -> request.meth) !log);
  Alcotest.(check (list bool))
    "endpoint suffix order" [ true; true; true; true ]
    (List.map2
       (fun suffix request -> String.ends_with ~suffix request.url)
       [
         "/.m.rule.encrypted/actions";
         "/.m.rule.encrypted/enabled";
         "/.m.rule.poll_start/actions";
         "/.m.rule.poll_start/enabled";
       ]
       !log)

let test_unmute_deletes_or_creates () =
  let log = ref [] in
  let defaults = Push.default_ruleset ~user_id:user in
  let settings =
    Settings.create
      ~ruleset:
        { defaults with override = custom_override [] :: defaults.override }
      (client log)
  in
  Result.get_ok
    (Settings.unmute_room settings room ~encrypted:false ~one_to_one:false);
  check_mode_opt "mute removed" None
    (Settings.user_defined_room_mode settings room);
  Alcotest.(check int) "one delete" 1 (List.length !log);
  let log = ref [] in
  let settings = Settings.create ~ruleset:defaults (client log) in
  Result.get_ok
    (Settings.unmute_room settings room ~encrypted:false ~one_to_one:false);
  check_mode_opt "explicitly all" (Some Settings.All_messages)
    (Settings.user_defined_room_mode settings room);
  Alcotest.(check int) "one create" 1 (List.length !log)

let () =
  Alcotest.run "notification settings"
    [
      ( "rules",
        [
          Alcotest.test_case "mode projection" `Quick (run test_mode_projection);
          Alcotest.test_case "ordered and idempotent room mutation" `Quick
            (run test_set_room_mode_orders_and_is_idempotent);
          Alcotest.test_case "failed batch stays local-atomic" `Quick
            (run test_failed_batch_does_not_publish);
          Alcotest.test_case "keyword lifecycle" `Quick
            (run test_keyword_lifecycle);
          Alcotest.test_case "new keyword actions" `Quick
            (run test_new_keyword_actions);
          Alcotest.test_case "default room and poll rules" `Quick
            (run test_default_mode_updates_room_and_poll);
          Alcotest.test_case "unmute" `Quick
            (run test_unmute_deletes_or_creates);
        ] );
    ]
