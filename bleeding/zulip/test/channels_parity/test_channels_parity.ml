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

let form request = Httpz_media.Urlencoded.decode (read_body request)

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

let decode codec json =
  match Jsont_bytesrw.decode_string codec json with
  | Ok value -> value
  | Error message -> Alcotest.fail message

let has_member name = function
  | Jsont.Object (members, _) ->
      List.exists
        (fun ((member_name, _), _) -> String.equal name member_name)
        members
  | _ -> false

let test_channel_schema_and_extensions () =
  let channel =
    decode Zulip.Channel.jsont
      {|{"stream_id":7,"name":"design","description":"Design","rendered_description":"<p>Design</p>","date_created":1710000000,"creator_id":4,"invite_only":true,"is_web_public":false,"history_public_to_subscribers":false,"is_default":true,"message_retention_days":null,"first_message_id":12,"stream_post_policy":1,"is_archived":false,"topics_policy":"disable_empty_topic","folder_id":3,"is_recently_active":true,"is_announcement_only":false,"subscriber_count":6,"stream_weekly_traffic":14,"can_add_subscribers_group":{"direct_members":[4],"direct_subgroups":[9]},"can_send_message_group":11,"future_channel_field":{"enabled":true}}|}
  in
  Alcotest.(check (option int))
    "creator" (Some 4)
    (Option.map Zulip.Id.User.to_int (Zulip.Channel.creator_id channel));
  Alcotest.(check (option int))
    "folder" (Some 3)
    (Option.map Zulip.Id.Channel_folder.to_int
       (Option.join (Zulip.Channel.folder_id channel)));
  Alcotest.(check bool) "archived" false (Zulip.Channel.is_archived channel);
  Alcotest.(check bool)
    "unknown field retained" true
    (has_member "future_channel_field" (Zulip.Channel.extensions channel));
  match Zulip.Channel.can_add_subscribers_group channel with
  | Some (Zulip.Group_setting.Direct { members; subgroups = [ subgroup ] })
    when Zulip.Id.User_group.to_int subgroup = 9 ->
      Alcotest.(check (list int))
        "direct permission members" [ 4 ]
        (List.map Zulip.Id.User.to_int members)
  | _ -> Alcotest.fail "direct channel permission was not decoded"

let test_dedicated_create_and_group_settings () =
  with_client
    (fun request ->
      Alcotest.(check string)
        "dedicated path" "/api/v1/channels/create"
        (Fetch.Middleware.Url.path_and_query request.url);
      let params = form request in
      Alcotest.(check string)
        "subscriber IDs" "[4,5]"
        (List.assoc "subscribers" params);
      Alcotest.(check string)
        "topics policy is a JSON string" {|"empty_topic_only"|}
        (List.assoc "topics_policy" params);
      Alcotest.(check string)
        "realm-default retention is a JSON string" {|"realm_default"|}
        (List.assoc "message_retention_days" params);
      Alcotest.(check string)
        "direct permission" {|{"direct_members":[4],"direct_subgroups":[12]}|}
        (List.assoc "can_send_message_group" params);
      Fetch_mock.respond ~headers {|{"result":"success","msg":"","id":71}|}
        request)
    (fun client ->
      let direct =
        Zulip.Group_setting.Direct
          {
            members = [ Zulip.Id.User.of_int 4 ];
            subgroups = [ Zulip.Id.User_group.of_int 12 ];
          }
      in
      let id =
        Channels.create client
          {
            name = "new channel";
            description = Some "description";
            subscribers = [ Zulip.Id.User.of_int 4; Zulip.Id.User.of_int 5 ];
            announce = Some true;
            invite_only = Some true;
            is_web_public = None;
            is_default_stream = Some false;
            history_public_to_subscribers = Some false;
            message_retention_days = Some Channels.Realm_default;
            folder_id = Some (Zulip.Id.Channel_folder.of_int 3);
            topics_policy = Some Zulip.Channel.Topics_policy.Empty_topic_only;
            can_add_subscribers_group = None;
            can_create_topic_group = None;
            can_delete_any_message_group = None;
            can_delete_own_message_group = None;
            can_remove_subscribers_group = None;
            can_administer_channel_group = None;
            can_move_messages_out_of_channel_group = None;
            can_move_messages_within_channel_group = None;
            can_send_message_group = Some direct;
            can_subscribe_group = None;
            can_resolve_topics_group = None;
          }
        |> ok
      in
      Alcotest.(check int) "created ID" 71 (Zulip.Id.Channel.to_int id))

let test_typed_subscription_results () =
  with_client
    (fun request ->
      Alcotest.(check bool) "PATCH" true (request.meth = `PATCH);
      Fetch_mock.respond ~headers
        {|{"result":"success","msg":"","subscribed":{"bot@example.com":["new"]},"already_subscribed":{},"not_removed":["absent"],"removed":["old"],"future":1}|}
        request)
    (fun client ->
      let result =
        Channels.update_subscriptions client
          ~add:[ { name = "new"; color = Some "#fff"; description = None } ]
          ~remove:[ "old" ] ()
        |> ok
      in
      Alcotest.(check (list string)) "removed" [ "old" ] result.removed;
      Alcotest.(check (list string))
        "not removed" [ "absent" ] result.not_removed;
      Alcotest.(check bool)
        "raw extension accessible" true
        (has_member "future" result.raw))

let test_retention_request_values () =
  let expected = ref [ {|"realm_default"|}; {|"forever"|}; "30" ] in
  with_client
    (fun request ->
      let value = List.assoc "message_retention_days" (form request) in
      match !expected with
      | next :: rest ->
          expected := rest;
          Alcotest.(check string) "retention wire value" next value;
          Fetch_mock.respond ~headers success request
      | [] -> Alcotest.fail "unexpected retention update")
    (fun client ->
      let channel_id = Zulip.Id.Channel.of_int 7 in
      Channels.update client ~channel_id
        ~message_retention_days:Channels.Realm_default ()
      |> ok;
      Channels.update client ~channel_id
        ~message_retention_days:Channels.Forever ()
      |> ok;
      Channels.update client ~channel_id
        ~message_retention_days:(Channels.Days 30) ()
      |> ok;
      match
        Channels.update client ~channel_id
          ~message_retention_days:(Channels.Days 0) ()
      with
      | Error (Error.Invalid_request _) -> ()
      | _ -> Alcotest.fail "nonpositive retention days were accepted");
  Alcotest.(check int)
    "all retention updates observed" 0 (List.length !expected)

let test_topics_policy_update_value () =
  with_client
    (fun request ->
      Alcotest.(check string)
        "updated topics policy is a raw string" "disable_empty_topic"
        (List.assoc "topics_policy" (form request));
      Fetch_mock.respond ~headers success request)
    (fun client ->
      Channels.update client
        ~channel_id:(Zulip.Id.Channel.of_int 7)
        ~topics_policy:Zulip.Channel.Topics_policy.Disable_empty_topic ()
      |> ok)

let test_topic_visibility_and_individual_property () =
  let calls = ref [] in
  with_client
    (fun request ->
      calls :=
        (Fetch.Middleware.Url.path_and_query request.url, form request)
        :: !calls;
      Fetch_mock.respond ~headers success request)
    (fun client ->
      Channels.set_topic_visibility client
        ~channel_id:(Zulip.Id.Channel.of_int 7)
        ~topic:"release" ~visibility_policy:Zulip.Topic_visibility.Followed
      |> ok;
      Channels.update_subscription_property client
        ~channel_id:(Zulip.Id.Channel.of_int 7)
        (Channels.Desktop_notifications false)
      |> ok);
  let calls = List.rev !calls in
  let topic = List.assoc "/api/v1/user_topics" calls in
  Alcotest.(check string)
    "followed policy" "3"
    (List.assoc "visibility_policy" topic);
  let property = List.assoc "/api/v1/users/me/subscriptions/7" calls in
  Alcotest.(check string)
    "individual property" "desktop_notifications"
    (List.assoc "property" property);
  Alcotest.(check string) "JSON boolean" "false" (List.assoc "value" property)

let test_topic_query_option () =
  with_client
    (fun request ->
      let target = Fetch.Middleware.Url.path_and_query request.url in
      let query =
        String.sub target
          (String.index target '?' + 1)
          (String.length target - String.index target '?' - 1)
        |> Httpz_media.Urlencoded.decode
      in
      Alcotest.(check string)
        "empty-topic compatibility" "true"
        (List.assoc "allow_empty_topic_name" query);
      Fetch_mock.respond ~headers {|{"result":"success","msg":"","topics":[]}|}
        request)
    (fun client ->
      let topics =
        Channels.get_topics client
          ~channel_id:(Zulip.Id.Channel.of_int 7)
          ~allow_empty_topic_name:true ()
        |> ok
      in
      Alcotest.(check int) "empty topics" 0 (List.length topics))

let test_delete_topic_completion () =
  let channel_id = Zulip.Id.Channel.of_int 7 in
  let check_completion json expected =
    let requests = ref 0 in
    with_client
      (fun request ->
        incr requests;
        Alcotest.(check string)
          "delete-topic path" "/api/v1/streams/7/delete_topic"
          (Fetch.Middleware.Url.path_and_query request.url);
        Alcotest.(check bool) "POST" true (request.meth = `POST);
        Alcotest.(check string)
          "topic name" "release"
          (List.assoc "topic_name" (form request));
        Fetch_mock.respond ~headers json request)
      (fun client ->
        let actual =
          Channels.delete_topic client ~channel_id ~topic:"release" |> ok
        in
        Alcotest.(check bool) "completion state" true (actual = expected));
    Alcotest.(check int) "single request" 1 !requests
  in
  check_completion {|{"result":"success","msg":"","complete":true}|} `Complete;
  check_completion {|{"result":"success","msg":"","complete":false}|}
    `Incomplete

let test_delete_topic_requires_completion () =
  let check_error json =
    with_client (Fetch_mock.respond ~headers json) (fun client ->
        match
          Channels.delete_topic client
            ~channel_id:(Zulip.Id.Channel.of_int 7)
            ~topic:"release"
        with
        | Error (Error.Json _) -> ()
        | Error error ->
            Alcotest.failf "unexpected error: %s" (Error.error_to_string error)
        | Ok _ -> Alcotest.fail "invalid completion response was accepted")
  in
  check_error {|{"result":"success","msg":""}|};
  check_error {|{"result":"success","msg":"","complete":"yes"}|}

let test_group_controls_and_supported_subgroup_check () =
  let paths = ref [] in
  with_client
    (fun request ->
      let path = Fetch.Middleware.Url.path_and_query request.url in
      paths := path :: !paths;
      if String.starts_with ~prefix:"/api/v1/user_groups/2/subgroups" path then
        Fetch_mock.respond ~headers
          {|{"result":"success","msg":"","subgroups":[3,4]}|} request
      else Fetch_mock.respond ~headers success request)
    (fun client ->
      User_group.update_members client
        ~group_id:(Zulip.Id.User_group.of_int 2)
        ~add:[ Zulip.Id.User.of_int 5 ]
        ~add_subgroups:[ Zulip.Id.User_group.of_int 3 ]
        ()
      |> ok;
      Alcotest.(check bool)
        "transitive subgroup" true
        (User_group.is_subgroup client
           ~group_id:(Zulip.Id.User_group.of_int 2)
           ~subgroup_id:(Zulip.Id.User_group.of_int 4)
           ~direct_subgroup_only:false ()
        |> ok));
  Alcotest.(check bool)
    "no fabricated subgroup-by-ID route" false
    (List.exists
       (fun path ->
         String.starts_with ~prefix:"/api/v1/user_groups/2/subgroups/4" path)
       !paths)

let test_channel_folders_and_empty_updates () =
  let called = ref false in
  with_client
    (fun request ->
      called := true;
      Fetch_mock.respond ~headers success request)
    (fun client ->
      (match
         Channel_folders.update client
           ~folder_id:(Zulip.Id.Channel_folder.of_int 1)
           ()
       with
      | Error (Error.Invalid_request _) -> ()
      | _ -> Alcotest.fail "empty folder update was accepted");
      match Channels.update_subscriptions client () with
      | Error (Error.Invalid_request _) -> ()
      | _ -> Alcotest.fail "empty subscription update was accepted");
  Alcotest.(check bool) "validation before transport" false !called

let test_typed_settings_update_and_snapshot () =
  with_client
    (fun request ->
      Alcotest.(check string)
        "settings endpoint" "/api/v1/settings"
        (Fetch.Middleware.Url.path_and_query request.url);
      let params = form request in
      Alcotest.(check string)
        "notification boolean" "true"
        (List.assoc "enable_stream_push_notifications" params);
      Alcotest.(check string)
        "closed enum" "4"
        (List.assoc "automatically_follow_topics_policy" params);
      Alcotest.(check string)
        "target users"
        {|{"user_ids":[4],"group_ids":[9],"skip_if_already_edited":true}|}
        (List.assoc "target_users" params);
      Fetch_mock.respond ~headers
        {|{"result":"success","msg":"","ignored_parameters_unsupported":["future_setting"],"future":true}|}
        request)
    (fun client ->
      let result =
        Settings.update client
          ~target_users:
            {
              user_ids = [ Zulip.Id.User.of_int 4 ];
              group_ids = [ Zulip.Id.User_group.of_int 9 ];
              skip_if_already_edited = Some true;
            }
          [
            Settings.Set (Settings.Enable_stream_push_notifications, true);
            Settings.Set (Settings.Automatically_follow_topics_policy, `Never);
            Settings.Set (Settings.Web_home_view, `Inbox);
          ]
        |> ok
      in
      Alcotest.(check (list string))
        "ignored settings" [ "future_setting" ]
        result.ignored_parameters_unsupported;
      Alcotest.(check bool)
        "raw settings result" true
        (has_member "future" result.raw));
  let state =
    Initial_state.of_json
      (decode Jsont.json
         {|{"user_settings":{"enable_stream_push_notifications":true,"color_scheme":2}}|})
    |> ok
  in
  Alcotest.(check (option bool))
    "same key reads registration state" (Some true)
    (Settings.get state Settings.Enable_stream_push_notifications |> ok);
  Alcotest.(check bool)
    "typed enum reads registration state" true
    (Settings.get state Settings.Color_scheme |> ok = Some `Dark)

let () =
  Alcotest.run "channel and group parity"
    [
      ( "channels",
        [
          Alcotest.test_case "complete schema and extensions" `Quick
            test_channel_schema_and_extensions;
          Alcotest.test_case "dedicated create" `Quick
            test_dedicated_create_and_group_settings;
          Alcotest.test_case "retention request values" `Quick
            test_retention_request_values;
          Alcotest.test_case "topics policy request value" `Quick
            test_topics_policy_update_value;
          Alcotest.test_case "typed subscription results" `Quick
            test_typed_subscription_results;
          Alcotest.test_case "topic and subscription settings" `Quick
            test_topic_visibility_and_individual_property;
          Alcotest.test_case "topic query options" `Quick
            test_topic_query_option;
          Alcotest.test_case "topic deletion completion" `Quick
            test_delete_topic_completion;
          Alcotest.test_case "topic deletion response validation" `Quick
            test_delete_topic_requires_completion;
          Alcotest.test_case "group controls" `Quick
            test_group_controls_and_supported_subgroup_check;
          Alcotest.test_case "local validation" `Quick
            test_channel_folders_and_empty_updates;
          Alcotest.test_case "typed settings" `Quick
            test_typed_settings_update_and_snapshot;
        ] );
    ]
