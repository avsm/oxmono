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

let decode codec text =
  match Jsont_bytesrw.decode_string codec text with
  | Ok value -> value
  | Error message -> Alcotest.fail message

let decode_event = decode Zulip.Event.jsont

let json_member name = function
  | Jsont.Object (members, _) ->
      List.find_map
        (fun ((key, _), value) -> if key = name then Some value else None)
        members
  | _ -> None

let test_user_profile_and_extensions () =
  let user =
    decode Zulip.User.jsont
      {|{"user_id":4,"email":"person@example.com","full_name":"Person","delivery_email":null,"is_active":true,"is_admin":false,"is_owner":false,"is_guest":false,"is_bot":false,"bot_type":null,"bot_owner_id":null,"avatar_url":null,"avatar_version":2,"is_imported_stub":true,"is_deleted":false,"profile_data":{"7":{"value":"[9]"},"2":{"value":"Hello","rendered_value":"<p>Hello</p>"}},"future_user_field":{"x":1}}|}
  in
  Alcotest.(check bool) "imported stub" true (Zulip.User.is_imported_stub user);
  (match Zulip.User.profile_data user with
  | Some [ (a, first); (b, second) ]
    when Zulip.Id.Profile_field.to_int a = 7
         && Zulip.Id.Profile_field.to_int b = 2 ->
      Alcotest.(check string) "profile value" "[9]" first.value;
      Alcotest.(check (option string))
        "rendered profile value" (Some "<p>Hello</p>") second.rendered_value
  | _ -> Alcotest.fail "profile data map was not decoded");
  Alcotest.(check bool)
    "future user field retained" true
    (Option.is_some (json_member "future_user_field" (Zulip.User.raw user)))

let test_user_requests () =
  let calls = ref [] in
  with_client
    (fun request ->
      let target = Fetch.Middleware.Url.path_and_query request.url in
      calls := (request.meth, target, form request) :: !calls;
      if request.meth = `POST && target = "/api/v1/users" then
        Fetch_mock.respond ~headers
          {|{"result":"success","msg":"","user_id":42,"future":"kept"}|} request
      else Fetch_mock.respond ~headers success request)
    (fun client ->
      let created =
        Users.create_detailed client ~email:"new@example.com" ~password:"pw"
          ~full_name:"New User"
        |> ok
      in
      Alcotest.(check int)
        "created user ID" 42
        (Zulip.Id.User.to_int created.user_id);
      Users.update client ~user_id:(Zulip.Id.User.of_int 42)
        ~new_email:"renamed@example.com"
        ~profile_data:
          [
            {
              Users.field_id = Zulip.Id.Profile_field.of_int 2;
              value = Users.Text "bio";
            };
            {
              Users.field_id = Zulip.Id.Profile_field.of_int 7;
              value = Users.Users [ Zulip.Id.User.of_int 9 ];
            };
            {
              Users.field_id = Zulip.Id.Profile_field.of_int 8;
              value = Users.Remove;
            };
          ]
        ()
      |> ok;
      Users.deactivate client ~user_id:(Zulip.Id.User.of_int 42)
        ~actions:
          {
            Users.delete_profile = Some true;
            delete_public_channel_messages = Some false;
            delete_private_channel_messages = None;
            delete_direct_messages = Some true;
          }
        ~notification_comment:"Farewell" ()
      |> ok);
  match List.rev !calls with
  | [
   (`POST, "/api/v1/users", create);
   (`PATCH, _, update);
   (`DELETE, _, deactivate);
  ] ->
      Alcotest.(check string)
        "create name" "New User"
        (List.assoc "full_name" create);
      Alcotest.(check string)
        "typed profile update"
        {|[{"id":2,"value":"bio"},{"id":7,"value":[9]},{"id":8,"value":null}]|}
        (List.assoc "profile_data" update);
      Alcotest.(check string)
        "new email" "renamed@example.com"
        (List.assoc "new_email" update);
      Alcotest.(check string)
        "deactivation actions"
        {|{"delete_profile":true,"delete_public_channel_messages":false,"delete_direct_messages":true}|}
        (List.assoc "actions" deactivate);
      Alcotest.(check string)
        "deactivation comment" "Farewell"
        (List.assoc "deactivation_notification_comment" deactivate)
  | _ -> Alcotest.fail "unexpected user request sequence"

let test_status_and_attachments () =
  let status =
    decode Users.user_status_jsont
      {|{"away":false,"status_text":"heads down","emoji_name":"hammer","emoji_code":"1f528","reaction_type":"unicode_emoji","future":1}|}
  in
  Alcotest.(check (option string))
    "status text" (Some "heads down") status.status_text;
  (match status.emoji with
  | Some emoji -> Alcotest.(check string) "emoji code" "1f528" emoji.emoji_code
  | None -> Alcotest.fail "status emoji missing");
  (match
     Jsont_bytesrw.decode_string Users.user_status_jsont
       {|{"emoji_name":"hammer","emoji_code":"1f528"}|}
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "partial status emoji was accepted");
  let response =
    decode Attachments.page_jsont
      {|{"result":"success","msg":"","attachments":[{"id":1,"name":"a.txt","path_id":"2/a.txt","size":12,"create_time":1720000000,"message_ids":[10,11],"scan":"clean"}],"upload_space_used":12}|}
  in
  match response.attachments with
  | [ attachment ] ->
      Alcotest.(check int)
        "attachment timestamp seconds" 1720000000 attachment.create_time;
      Alcotest.(check (list int))
        "attachment message IDs" [ 10; 11 ]
        (List.map Zulip.Id.Message.to_int attachment.message_ids);
      Alcotest.(check bool)
        "attachment extension" true
        (Option.is_some (json_member "scan" attachment.raw))
  | _ -> Alcotest.fail "attachment response not decoded"

let test_presence_delta_and_validation () =
  let delta =
    decode Presence.update_response_jsont
      {|{"result":"success","msg":"","presence_last_update_id":1001,"server_timestamp":1656958539.5,"presences":{"10":{"active_timestamp":1656958520,"idle_timestamp":1656958530}},"future":true}|}
  in
  Alcotest.(check (option int))
    "presence update cursor" (Some 1001) delta.presence_last_update_id;
  (match delta.presences with
  | Some [ ("10", presence) ] ->
      Alcotest.(check (option (float 0.)))
        "modern active timestamp" (Some 1656958520.) presence.active_timestamp
  | _ -> Alcotest.fail "modern presence delta not decoded");
  let called = ref false in
  with_client
    (fun request ->
      called := true;
      Fetch_mock.respond ~headers success request)
    (fun client ->
      match Presence.update client ~status:Presence.Offline () with
      | Error (Error.Invalid_request _) -> ()
      | _ -> Alcotest.fail "offline presence update was accepted");
  Alcotest.(check bool) "invalid update made no request" false !called

let test_presence_request_options () =
  with_client
    (fun request ->
      let params = form request in
      Alcotest.(check string)
        "presence cursor" "40"
        (List.assoc "last_update_id" params);
      Alcotest.(check string)
        "presence history" "7"
        (List.assoc "history_limit_days" params);
      Alcotest.(check string)
        "slim presence" "true"
        (List.assoc "slim_presence" params);
      Fetch_mock.respond ~headers
        {|{"result":"success","msg":"","presence_last_update_id":40}|} request)
    (fun client ->
      let response =
        Presence.update client ~status:Presence.Active ~last_update_id:40
          ~history_limit_days:7 ~ping_only:true ~slim_presence:true ()
        |> ok
      in
      Alcotest.(check bool)
        "ping response omits map" true
        (Option.is_none response.presences))

let test_server_metadata () =
  let linkifier =
    decode Server.linkifier_jsont
      {|{"id":4,"pattern":"#(?P<id>[0-9]+)","url_template":"https://example/{id}","example_input":"#42","reverse_template":"#{id}","alternative_url_templates":["https://alt/{id}"],"future":true}|}
  in
  Alcotest.(check (option string))
    "reverse example" (Some "#42") linkifier.example_input;
  Alcotest.(check bool)
    "linkifier extension" true
    (Option.is_some (json_member "future" linkifier.extensions));
  let field =
    decode Server.profile_field_jsont
      {|{"id":4,"type":3,"order":4,"name":"Editor","hint":"Favorite","field_data":"{\"0\":{\"text\":\"Vim\"}}","display_in_profile_summary":true,"required":true,"editable_by_user":false,"use_for_user_matching":true,"future":"kept"}|}
  in
  Alcotest.(check string)
    "profile field data remains JSON text" {|{"0":{"text":"Vim"}}|}
    field.field_data;
  Alcotest.(check bool)
    "profile field extension" true
    (Option.is_some (json_member "future" field.extensions))

let test_server_and_typing_requests () =
  let calls = ref [] in
  with_client
    (fun request ->
      calls :=
        ( request.meth,
          Fetch.Middleware.Url.path_and_query request.url,
          form request )
        :: !calls;
      if request.meth = `POST then
        Fetch_mock.respond ~headers {|{"result":"success","msg":"","id":5}|}
          request
      else Fetch_mock.respond ~headers success request)
    (fun client ->
      ignore
        (Server.add_linkifier client ~pattern:"BUG-(?P<id>[0-9]+)"
           ~url_template:"https://tracker/{id}"
           ~example_input:(Server.Set "BUG-1") ~reverse_template:Server.Clear
           ~alternative_url_templates:[ "https://mirror/{id}" ] ()
        |> ok);
      Server.reorder_linkifiers client
        ~ordered_linkifier_ids:(List.map Zulip.Id.Linkifier.of_int [ 5; 2 ])
      |> ok;
      Typing.set_edit client ~op:Typing.Start
        ~message_id:(Zulip.Id.Message.of_int 99)
      |> ok);
  match List.rev !calls with
  | [ (`POST, _, add); (`PATCH, _, reorder); (`POST, edit_path, edit) ] ->
      Alcotest.(check string)
        "reverse clear uses empty wire value" ""
        (List.assoc "reverse_template" add);
      Alcotest.(check string)
        "alternative templates JSON" {|["https://mirror/{id}"]|}
        (List.assoc "alternative_url_templates" add);
      Alcotest.(check string)
        "ordered linkifier IDs" "[5,2]"
        (List.assoc "ordered_linkifier_ids" reorder);
      Alcotest.(check string)
        "edit typing path" "/api/v1/messages/99/typing" edit_path;
      Alcotest.(check string) "edit typing op" "start" (List.assoc "op" edit)
  | _ -> Alcotest.fail "unexpected server/typing request sequence"

let test_storage_absent_and_empty_keys () =
  let paths = ref [] in
  with_client
    (fun request ->
      paths := Fetch.Middleware.Url.path_and_query request.url :: !paths;
      Fetch_mock.respond ~headers {|{"result":"success","msg":"","storage":{}}|}
        request)
    (fun client ->
      ignore (Bot_storage.get client () |> ok);
      ignore (Bot_storage.get client ~keys:[] () |> ok));
  match List.rev !paths with
  | [ "/api/v1/bot_storage"; filtered ] ->
      Alcotest.(check bool)
        "explicit empty key filter encoded" true
        (String.starts_with ~prefix:"/api/v1/bot_storage?keys=" filtered)
  | _ -> Alcotest.fail "storage key omission was not preserved"

let payload kind text =
  let json = decode Jsont.json text in
  match Zulip.Event_payload.decode kind json with
  | Ok payload -> payload
  | Error message -> Alcotest.fail (Zulip.Event_payload.error_to_string message)

let test_core_event_payloads () =
  let open Zulip.Event_payload in
  let message =
    decode_event
      {|{"id":1,"type":"message","message":{"id":10,"sender_id":3,"sender_email":"a@example.test","sender_full_name":"A","timestamp":1,"content":"hello","content_type":"text/html","type":"stream","stream_id":4,"display_recipient":"general","subject":"topic"},"flags":["mentioned"],"future_event":true}|}
  in
  (match Zulip.Event_payload.of_event message with
  | Ok (Message event) ->
      Alcotest.(check int)
        "message ID" 10
        (Zulip.Id.Message.to_int (Zulip.Message.id event.message));
      Alcotest.(check (list string))
        "event flags" [ "mentioned" ]
        (List.map Zulip.Message_flag.to_string event.flags);
      Alcotest.(check bool)
        "full event payload retained" true
        (Option.is_some (json_member "future_event" event.raw))
  | Ok _ -> Alcotest.fail "message event decoded as another family"
  | Error message -> Alcotest.fail (Zulip.Event_payload.error_to_string message));
  (match
     payload Zulip.Event_type.Update_message
       {|{"message_id":10,"message_ids":[10,11],"user_id":3,"edit_timestamp":8,"content":"new","stream_id":4,"subject":"new topic"}|}
   with
  | Message_edit event ->
      Alcotest.(check (list int))
        "edited IDs" [ 10; 11 ]
        (List.map Zulip.Id.Message.to_int event.message_ids)
  | _ -> Alcotest.fail "edit event decoded as another family");
  (match
     payload Zulip.Event_type.Delete_message
       {|{"message_type":"private","message_id":37}|}
   with
  | Message_delete { message_ids = [ id ]; message_type = Some `Direct; _ } ->
      Alcotest.(check int) "deleted ID" 37 (Zulip.Id.Message.to_int id)
  | _ -> Alcotest.fail "delete event decoded incorrectly");
  (match
     payload Zulip.Event_type.Reaction
       {|{"op":"add","message_id":10,"user_id":3,"emoji_name":"wave","emoji_code":"1f44b","reaction_type":"unicode_emoji"}|}
   with
  | Reaction event ->
      Alcotest.(check string)
        "reaction op" "add"
        (Zulip.Event_payload.change_to_string event.op)
  | _ -> Alcotest.fail "reaction event decoded incorrectly");
  match
    payload Zulip.Event_type.Update_message_flags
      {|{"op":"remove","flag":"starred","messages":[10],"all":false}|}
  with
  | Message_flags event ->
      Alcotest.(check string)
        "flag" "starred"
        (Zulip.Message_flag.to_string event.flag)
  | _ -> Alcotest.fail "flags event decoded incorrectly"

let test_account_and_realm_event_payloads () =
  let open Zulip.Event_payload in
  (match
     payload Zulip.Event_type.Realm_user
       {|{"op":"add","person":{"user_id":38,"email":"foo@example.test","full_name":"Foo","is_active":true,"is_bot":false}}|}
   with
  | Realm_user { added_user = Some user; _ } ->
      Alcotest.(check string) "added user" "Foo" (Zulip.User.full_name user)
  | _ -> Alcotest.fail "realm-user add was not typed");
  (match
     payload Zulip.Event_type.Stream
       {|{"op":"create","streams":[{"name":"private","stream_id":12,"invite_only":true}]}|}
   with
  | Channel { channel_ids = [ id ]; channels = [ channel ]; _ } ->
      Alcotest.(check int) "created channel ID" 12 (Zulip.Id.Channel.to_int id);
      Alcotest.(check string)
        "created channel name" "private"
        (Zulip.Channel.name channel)
  | _ -> Alcotest.fail "channel create was not typed");
  (match
     payload Zulip.Event_type.Subscription
       {|{"op":"peer_add","stream_ids":[9],"user_ids":[12]}|}
   with
  | Subscription { channel_ids = [ channel_id ]; user_ids = [ user_id ]; _ } ->
      Alcotest.(check int) "peer channel" 9 (Zulip.Id.Channel.to_int channel_id);
      Alcotest.(check int) "peer user" 12 (Zulip.Id.User.to_int user_id)
  | _ -> Alcotest.fail "subscription peer add was not typed");
  (match
     payload Zulip.Event_type.User_status
       {|{"user_id":10,"status_text":"out","emoji_name":"car","emoji_code":"1f697","reaction_type":"unicode_emoji"}|}
   with
  | User_status event ->
      Alcotest.(check (option string))
        "event status" (Some "out") event.status_text
  | _ -> Alcotest.fail "status event was not typed");
  (match
     payload Zulip.Event_type.User_topic
       {|{"stream_id":1,"topic_name":"topic","last_updated":1594825442,"visibility_policy":1}|}
   with
  | Topic event -> Alcotest.(check string) "topic name" "topic" event.topic_name
  | _ -> Alcotest.fail "topic event was not typed");
  (match
     payload Zulip.Event_type.User_group
       {|{"op":"add_members","group_id":2,"user_ids":[10]}|}
   with
  | User_group { group_id; user_ids = [ user_id ]; _ }
    when Zulip.Id.User_group.to_int group_id = 2 ->
      Alcotest.(check int) "group member" 10 (Zulip.Id.User.to_int user_id)
  | _ -> Alcotest.fail "group event was not typed");
  match
    payload Zulip.Event_type.Presence
      {|{"user_id":10,"server_timestamp":20.5,"presence":{"active_timestamp":19,"idle_timestamp":20}}|}
  with
  | Presence event ->
      Alcotest.(check (option (float 0.)))
        "presence active" (Some 19.) event.active_timestamp
  | _ -> Alcotest.fail "presence event was not typed"

let test_unknown_and_malformed_events () =
  let open Zulip.Event_payload in
  let unknown =
    decode_event {|{"id":4,"type":"future_event","future":{"value":1}}|}
  in
  (match Zulip.Event_payload.of_event unknown with
  | Ok (Unknown { event_type = Zulip.Event_type.Other "future_event"; raw }) ->
      Alcotest.(check bool)
        "unknown raw retained" true
        (Option.is_some (json_member "future" raw))
  | _ -> Alcotest.fail "future event was not preserved as unknown");
  (match
     Zulip.Event_payload.decode (Zulip.Event_type.Other "future_scalar")
       (Jsont.Json.string "kept")
   with
  | Ok (Unknown { raw = Jsont.String ("kept", _); _ }) -> ()
  | _ -> Alcotest.fail "non-object future payload was not preserved");
  let malformed =
    decode_event {|{"id":5,"type":"delete_message","message_id":"bad"}|}
  in
  match Zulip.Event_payload.of_event malformed with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "malformed known event was treated as unknown"

let () =
  Alcotest.run "Accounts and metadata parity"
    [
      ( "codecs",
        [
          Alcotest.test_case "user profile and extensions" `Quick
            test_user_profile_and_extensions;
          Alcotest.test_case "status and attachments" `Quick
            test_status_and_attachments;
          Alcotest.test_case "presence delta" `Quick
            test_presence_delta_and_validation;
          Alcotest.test_case "server metadata" `Quick test_server_metadata;
          Alcotest.test_case "core event payloads" `Quick
            test_core_event_payloads;
          Alcotest.test_case "account and realm event payloads" `Quick
            test_account_and_realm_event_payloads;
          Alcotest.test_case "unknown versus malformed events" `Quick
            test_unknown_and_malformed_events;
        ] );
      ( "requests",
        [
          Alcotest.test_case "users" `Quick test_user_requests;
          Alcotest.test_case "presence options" `Quick
            test_presence_request_options;
          Alcotest.test_case "server and edit typing" `Quick
            test_server_and_typing_requests;
          Alcotest.test_case "storage omitted and empty keys" `Quick
            test_storage_absent_and_empty_keys;
        ] );
    ]
