open Zulip_eio

let ok = function
  | Ok value -> value
  | Error error -> Alcotest.fail (Error.error_to_string error)

let headers = Http.Header.of_list [ ("content-type", "application/json") ]
let success = {|{"result":"success","msg":""}|}

let read_body (request : Fetch.Middleware.request) =
  match request.body with
  | Fetch.Empty -> ""
  | Fetch.String body -> body
  | Fetch.Stream { flow; _ } ->
      Eio.Buf_read.(take_all (of_flow ~max_size:65536 flow))

let with_client handler run =
  Eio_mock.Backend.run_full @@ fun env ->
  let auth =
    Auth.create ~site:"https://zulip.test" ~email:"bot@zulip.test"
      ~api_key:"key"
    |> ok
  in
  let transport =
    Transport.of_fetch ~clock:env#clock (Fetch_mock.client handler)
  in
  let client = Client.create ~transport ~auth () |> ok in
  run client

let form request = Httpz_media.Urlencoded.decode (read_body request)

let decode codec json =
  match Jsont_bytesrw.decode_string codec json with
  | Ok value -> value
  | Error message -> Alcotest.fail message

let test_latest_response_fixtures () =
  let group =
    decode User_group.jsont
      {|{"id":3,"name":"team","description":"Team","members":[4],"direct_subgroup_ids":[8],"is_system_group":false,"deactivated":false,"creator_id":4,"date_created":1717484476,"can_mention_group":{"direct_members":[4,5],"direct_subgroups":[8]}}|}
  in
  (match group.can_mention_group with
  | User_group.Direct { members; subgroups } ->
      Alcotest.(check (list int))
        "direct group users" [ 4; 5 ]
        (List.map Zulip.Id.User.to_int members);
      Alcotest.(check (list int))
        "direct subgroups" [ 8 ]
        (List.map Zulip.Id.User_group.to_int subgroups)
  | Group _ -> Alcotest.fail "group setting object decoded as an ID");
  let presence =
    decode Presence.user_presence_jsont
      {|{"active_timestamp":1656958520,"idle_timestamp":1656958530,"website":{"status":"active","timestamp":1656958520},"aggregated":{"status":"active","timestamp":1656958520}}|}
  in
  Alcotest.(check (option (float 0.)))
    "active timestamp" (Some 1656958520.) presence.active_timestamp;
  Alcotest.(check int)
    "legacy clients retained" 2
    (List.length presence.clients);
  let emoji =
    decode Server.emoji_jsont
      {|{"id":"1","name":"green_tick","source_url":"/emoji/1.png","still_url":null,"deactivated":false,"author_id":null}|}
  in
  Alcotest.(check string) "emoji name comes from value" "green_tick" emoji.name;
  Alcotest.(check (option string)) "nullable still image" None emoji.still_url;
  let field =
    decode Server.profile_field_jsont
      {|{"id":4,"type":3,"order":4,"name":"Editor","hint":"Favorite","field_data":"{\"0\":{\"text\":\"Vim\",\"order\":\"1\"}}","display_in_profile_summary":true,"required":true,"editable_by_user":true,"use_for_user_matching":false}|}
  in
  Alcotest.(check bool) "profile field required" true field.required

let test_modern_topic_mute () =
  with_client
    (fun request ->
      Alcotest.(check string)
        "modern user-topic endpoint" "/api/v1/user_topics"
        (Fetch.Middleware.Url.path_and_query request.url);
      let params = form request in
      Alcotest.(check string)
        "mute visibility policy" "1"
        (List.assoc "visibility_policy" params);
      Fetch_mock.respond ~headers success request)
    (fun client ->
      Channels.set_topic_mute client
        ~channel_id:(Zulip.Id.Channel.of_int 7)
        ~topic:"release" ~op:Channels.Mute
      |> ok)

let test_typed_family_responses () =
  with_client
    (fun request ->
      match Fetch.Middleware.Url.path_and_query request.url with
      | "/api/v1/realm/presence" ->
          Fetch_mock.respond ~headers
            {|{"result":"success","msg":"","server_timestamp":1656958539.6,"presences":{"iago@zulip.test":{"website":{"client":"website","pushable":false,"status":"active","timestamp":1656958485},"aggregated":{"client":"website","status":"active","timestamp":1656958485}}}}|}
            request
      | "/api/v1/realm/emoji" ->
          Fetch_mock.respond ~headers
            {|{"result":"success","msg":"","emoji":{"1":{"id":"1","name":"green_tick","source_url":"/emoji/1.png","still_url":null,"deactivated":false,"author_id":null}}}|}
            request
      | "/api/v1/server_settings" ->
          Fetch_mock.respond ~headers
            {|{"result":"success","msg":"","zulip_version":"12.2","zulip_feature_level":500,"zulip_merge_base":"12.2","push_notifications_enabled":false,"is_incompatible":false,"email_auth_enabled":true,"require_email_format_usernames":true,"realm_uri":"https://zulip.test","realm_url":"https://zulip.test","realm_name":"Test","realm_icon":"/icon.png","realm_description":"<p>Test</p>","realm_web_public_access_enabled":false,"authentication_methods":{"password":true,"dev":false,"email":true,"ldap":false,"remoteuser":false,"github":false,"azuread":false,"gitlab":false,"apple":false,"google":false,"saml":false,"openid connect":false,"discord":false},"external_authentication_methods":[{"name":"google","display_name":"Google","display_icon":null,"login_url":"/login","signup_url":"/signup"}]}|}
            request
      | path -> Alcotest.failf "unexpected fixture request %s" path)
    (fun client ->
      let presences = Presence.get_all client |> ok in
      (match presences with
      | [ (email, presence) ] ->
          Alcotest.(check string)
            "realm presence keyed by email" "iago@zulip.test" email;
          Alcotest.(check int)
            "legacy presence clients" 2
            (List.length presence.Presence.clients)
      | _ -> Alcotest.fail "unexpected realm presence map");
      let emoji = Server.get_emoji client |> ok in
      (match emoji with
      | [ emoji ] ->
          Alcotest.(check string) "emoji value name" "green_tick" emoji.name
      | _ -> Alcotest.fail "unexpected emoji map");
      let settings = Server.get_settings client |> ok in
      Alcotest.(check string)
        "canonical realm URL" "https://zulip.test" settings.realm_url;
      Alcotest.(check bool)
        "lowercase auth keys" true settings.authentication_methods.password;
      match settings.external_authentication_methods with
      | [ method_ ] ->
          Alcotest.(check (option string))
            "nullable auth icon" None method_.display_icon
      | _ -> Alcotest.fail "unexpected external auth methods")

let test_user_group_latest_requests () =
  let calls = ref [] in
  with_client
    (fun request ->
      let path = Fetch.Middleware.Url.path_and_query request.url in
      calls := (request.meth, path, form request) :: !calls;
      if path = "/api/v1/user_groups/create" then
        Fetch_mock.respond ~headers
          {|{"result":"success","msg":"","group_id":23}|} request
      else Fetch_mock.respond ~headers success request)
    (fun client ->
      let id =
        User_group.create client ~name:"team" ~description:"Team"
          ~members:[ Zulip.Id.User.of_int 4 ]
          ~can_mention_group:(User_group.Group (Zulip.Id.User_group.of_int 11))
          ()
        |> ok
      in
      Alcotest.(check int) "created group ID" 23 (Zulip.Id.User_group.to_int id);
      User_group.update client ~group_id:id
        ~can_mention_group:
          {
            User_group.new_ =
              Direct
                {
                  members = [ Zulip.Id.User.of_int 4 ];
                  subgroups = [ Zulip.Id.User_group.of_int 8 ];
                };
            old = Some (Group (Zulip.Id.User_group.of_int 11));
          }
        ()
      |> ok;
      User_group.delete client ~group_id:id |> ok);
  match List.rev !calls with
  | [ (`POST, _, create); (`PATCH, _, update); (`POST, deactivate, _) ] ->
      Alcotest.(check string)
        "create group setting JSON" "11"
        (List.assoc "can_mention_group" create);
      Alcotest.(check string)
        "update group setting JSON"
        {|{"new":{"direct_members":[4],"direct_subgroups":[8]},"old":11}|}
        (List.assoc "can_mention_group" update);
      Alcotest.(check string)
        "deactivate path" "/api/v1/user_groups/23/deactivate" deactivate
  | _ -> Alcotest.fail "unexpected user-group request sequence"

let test_sends () =
  let calls = ref [] in
  with_client
    (fun request ->
      calls := form request :: !calls;
      Fetch_mock.respond ~headers
        {|{"result":"success","msg":"","id":55,"automatic_new_visibility_policy":2}|}
        request)
    (fun client ->
      let id =
        Messages.send_channel_id client
          ~channel_id:(Zulip.Id.Channel.of_int 7)
          ~topic:"λ & +" ~content:"hello &= world" ()
        |> ok
      in
      Alcotest.(check int) "sent ID" 55 (Zulip.Id.Message.to_int id);
      ignore
        (Messages.send_direct client
           ~recipients:[ Zulip.Id.User.of_int 2; Zulip.Id.User.of_int 3 ]
           ~content:"group" ()
        |> ok));
  match List.rev !calls with
  | [ channel; direct ] ->
      Alcotest.(check string)
        "numeric channel destination" "7" (List.assoc "to" channel);
      Alcotest.(check string)
        "topic form roundtrip" "λ & +"
        (List.assoc "topic" channel);
      Alcotest.(check string)
        "content form roundtrip" "hello &= world"
        (List.assoc "content" channel);
      Alcotest.(check string)
        "typed direct recipients" "[2,3]" (List.assoc "to" direct)
  | _ -> Alcotest.fail "expected two sends"

let test_storage_delete () =
  with_client
    (fun request ->
      Alcotest.(check string)
        "storage path" "/api/v1/bot_storage"
        (Fetch.Middleware.Url.path_and_query request.url);
      Alcotest.(check bool) "DELETE" true (request.meth = `DELETE);
      Alcotest.(check string)
        "JSON keys" {|["a&b","λ"]|}
        (List.assoc "keys" (form request));
      Fetch_mock.respond ~headers success request)
    (fun client -> Bot_storage.remove client ~keys:[ "a&b"; "λ" ] () |> ok)

let test_typing_and_subscription () =
  let calls = ref [] in
  with_client
    (fun request ->
      calls :=
        (Fetch.Middleware.Url.path_and_query request.url, form request)
        :: !calls;
      Fetch_mock.respond ~headers success request)
    (fun client ->
      Typing.set_dm client ~op:Typing.Start
        ~user_ids:[ Zulip.Id.User.of_int 8; Zulip.Id.User.of_int 9 ]
      |> ok;
      Channels.subscribe client
        ~subscriptions:
          [
            {
              Channels.name = "a&b";
              color = Some "#fff";
              description = Some "λ";
            };
          ]
        ~principals:(`User_ids [ Zulip.Id.User.of_int 8 ])
        ()
      |> ok |> ignore);
  let calls = List.rev !calls in
  let typing = List.assoc "/api/v1/typing" calls in
  Alcotest.(check string) "typing recipients" "[8,9]" (List.assoc "to" typing);
  let subscriptions = List.assoc "/api/v1/users/me/subscriptions" calls in
  Alcotest.(check string)
    "nested subscription JSON"
    {|[{"name":"a&b","color":"#fff","description":"λ"}]|}
    (List.assoc "subscriptions" subscriptions);
  Alcotest.(check string)
    "typed principals" "[8]"
    (List.assoc "principals" subscriptions)

let test_local_validation () =
  let called = ref false in
  with_client
    (fun request ->
      called := true;
      Fetch_mock.respond ~headers success request)
    (fun client ->
      match Messages.edit client ~message_id:(Zulip.Id.Message.of_int 1) () with
      | Error (Error.Invalid_request _) -> ()
      | _ -> Alcotest.fail "empty edit was not rejected");
  Alcotest.(check bool) "validation happens before request" false !called

let test_narrow_match_response () =
  with_client
    (Fetch_mock.respond ~headers
       {|{"result":"success","msg":"","messages":{"12":{"match_content":"<p>matched</p>","match_subject":""},"15":{"match_content":"","match_subject":"matched topic"}}}|})
    (fun client ->
      let matches =
        Messages.check_messages_match_narrow client
          ~message_ids:
            [ Zulip.Id.Message.of_int 12; Zulip.Id.Message.of_int 15 ]
          ~narrow:[ Zulip.Narrow.has `Link ]
        |> ok
      in
      match matches with
      | [ first; second ] ->
          Alcotest.(check int)
            "map key becomes ID" 12
            (Zulip.Id.Message.to_int first.Messages.message_id);
          Alcotest.(check string)
            "content match" "<p>matched</p>" first.match_content;
          Alcotest.(check string)
            "topic match" "matched topic" second.match_topic
      | _ -> Alcotest.fail "unexpected narrow-match response")

let test_channel_create () =
  with_client
    (fun request ->
      Alcotest.(check string)
        "dedicated create endpoint" "/api/v1/channels/create"
        (Fetch.Middleware.Url.path_and_query request.url);
      let params = Httpz_media.Urlencoded.decode (read_body request) in
      Alcotest.(check string)
        "channel name" "new & channel" (List.assoc "name" params);
      Alcotest.(check string)
        "channel description" "created directly"
        (List.assoc "description" params);
      Fetch_mock.respond ~headers {|{"result":"success","msg":"","id":71}|}
        request)
    (fun client ->
      let id =
        Channels.create_simple client ~name:"new & channel"
          ~description:"created directly" ()
        |> ok
      in
      Alcotest.(check int) "returned channel ID" 71 (Zulip.Id.Channel.to_int id))

let () =
  Alcotest.run "Zulip endpoint encoding"
    [
      ( "endpoints",
        [
          Alcotest.test_case "channel and direct sends" `Quick test_sends;
          Alcotest.test_case "storage deletion" `Quick test_storage_delete;
          Alcotest.test_case "typing and subscriptions" `Quick
            test_typing_and_subscription;
          Alcotest.test_case "local validation" `Quick test_local_validation;
          Alcotest.test_case "narrow match map response" `Quick
            test_narrow_match_response;
          Alcotest.test_case "channel creation API" `Quick test_channel_create;
          Alcotest.test_case "Zulip 12.2 response fixtures" `Quick
            test_latest_response_fixtures;
          Alcotest.test_case "modern topic mute API" `Quick
            test_modern_topic_mute;
          Alcotest.test_case "typed family response maps" `Quick
            test_typed_family_responses;
          Alcotest.test_case "Zulip 12.2 user-group requests" `Quick
            test_user_group_latest_requests;
        ] );
    ]
