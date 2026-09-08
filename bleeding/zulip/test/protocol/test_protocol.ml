let ok = function Ok value -> value | Error message -> Alcotest.fail message

let json_member name = function
  | Jsont.Object (members, _) ->
      Option.map snd (Jsont.Json.find_mem name members)
  | _ -> None

let test_ids () =
  Alcotest.check_raises "constructor rejects negative IDs"
    (Invalid_argument "Zulip ID must be an exact nonnegative JSON integer")
    (fun () -> ignore (Zulip.Id.User.of_int (-1)));
  Alcotest.check_raises "constructor rejects inexact JSON IDs"
    (Invalid_argument "Zulip ID must be an exact nonnegative JSON integer")
    (fun () -> ignore (Zulip.Id.User.of_int 9_007_199_254_740_992));
  (match Jsont_bytesrw.decode_string Zulip.Id.Event.jsont "-1" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "event ID codec accepted a negative value");
  (match
     Jsont_bytesrw.decode_string Zulip.Id.Event.jsont "9007199254740992"
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "event ID codec accepted an inexact JSON integer");
  (match Jsont_bytesrw.decode_string Zulip.Id.User.jsont "1.5" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "user ID codec rounded a fractional value");
  let id = Zulip.Id.Message.of_int 9_007_199_254_740_991 in
  let encoded = ok (Jsont_bytesrw.encode_string Zulip.Id.Message.jsont id) in
  let decoded =
    ok (Jsont_bytesrw.decode_string Zulip.Id.Message.jsont encoded)
  in
  Alcotest.(check int)
    "large ID roundtrip"
    (Zulip.Id.Message.to_int id)
    (Zulip.Id.Message.to_int decoded)

let channel_fixture =
  {|{"id":123,"sender_id":42,"sender_email":"a@example.com","sender_full_name":"Alice","timestamp":1710000000.25,"content":"<p>hello</p>","content_type":"text/html","type":"stream","stream_id":7,"display_recipient":"general","subject":"test","flags":["read"],"future":{"nested":true}}|}

let test_channel_message () =
  let message =
    ok (Jsont_bytesrw.decode_string Zulip.Message.jsont channel_fixture)
  in
  Alcotest.(check int) "message ID" 123 (Zulip.Id.Message.to_int message.id);
  (match message.destination with
  | Zulip.Message.Channel { channel_id; channel_name; topic } ->
      Alcotest.(check int) "channel ID" 7 (Zulip.Id.Channel.to_int channel_id);
      Alcotest.(check string) "channel" "general" channel_name;
      Alcotest.(check string) "topic" "test" topic
  | Direct _ -> Alcotest.fail "decoded channel message as a DM");
  Alcotest.(check bool)
    "raw contains known member" true
    (Option.is_some (json_member "id" message.raw));
  Alcotest.(check bool)
    "raw preserves extension" true
    (Option.is_some (json_member "future" message.raw));
  let encoded = ok (Jsont_bytesrw.encode_string Zulip.Message.jsont message) in
  let raw = ok (Jsont_bytesrw.decode_string Jsont.json encoded) in
  (match raw with
  | Jsont.Object (members, _) ->
      let names = List.map (fun ((name, _), _) -> name) members in
      let count name = List.length (List.filter (String.equal name) names) in
      Alcotest.(check int) "known field emitted once" 1 (count "id");
      Alcotest.(check int) "extension emitted once" 1 (count "future")
  | _ -> Alcotest.fail "encoded message is not an object");
  ignore (ok (Jsont_bytesrw.decode_string Zulip.Message.jsont encoded))

let test_direct_message () =
  let fixture =
    {|{"id":9,"sender_id":2,"sender_email":"a@example.com","sender_full_name":"Alice","timestamp":1,"content":"hi","content_type":"text/html","type":"private","recipient_id":18,"display_recipient":[{"id":2,"email":"a@example.com"},{"id":3,"email":"b@example.com"}],"flags":[]}|}
  in
  let message = ok (Jsont_bytesrw.decode_string Zulip.Message.jsont fixture) in
  let encoded = ok (Jsont_bytesrw.encode_string Zulip.Message.jsont message) in
  let roundtrip = ok (Jsont_bytesrw.decode_string Jsont.json encoded) in
  Alcotest.(check bool)
    "participant metadata survives encoding" true
    (json_member "display_recipient" roundtrip
    = json_member "display_recipient" message.raw);
  match message.destination with
  | Zulip.Message.Direct { recipient_id; participants } ->
      Alcotest.(check int)
        "recipient ID" 18
        (Zulip.Id.Recipient.to_int recipient_id);
      Alcotest.(check (list int))
        "all DM participants" [ 2; 3 ]
        (List.map Zulip.Id.User.to_int participants)
  | Channel _ -> Alcotest.fail "decoded DM as a channel message"

let test_user_nulls () =
  let fixture =
    {|{"user_id":4,"email":"person@example.com","full_name":"Person","delivery_email":null,"is_active":true,"is_admin":false,"is_owner":false,"is_guest":false,"is_bot":false,"bot_type":null,"bot_owner_id":null,"avatar_url":null,"avatar_version":null}|}
  in
  let user = ok (Jsont_bytesrw.decode_string Zulip.User.jsont fixture) in
  Alcotest.(check int)
    "user ID" 4
    (Zulip.Id.User.to_int (Zulip.User.user_id user));
  Alcotest.(check (option int))
    "null owner" None
    (Option.map Zulip.Id.User.to_int (Zulip.User.bot_owner_id user));
  Alcotest.(check (option string))
    "null avatar" None
    (Zulip.User.avatar_url user);
  Alcotest.(check (option string))
    "null delivery email" None
    (Zulip.User.delivery_email user);
  Alcotest.(check bool)
    "removed billing flag defaults false" false
    (Zulip.User.is_billing_admin user)

let test_invalid_message () =
  let negative =
    String.concat ""
      [
        {|{"id":1,"sender_id":-2,"sender_email":"a@b","sender_full_name":"A","timestamp":1,"content":"x","content_type":"text/html","type":"stream","stream_id":7,"display_recipient":"g","subject":"t","flags":[]}|};
      ]
  in
  match Jsont_bytesrw.decode_string Zulip.Message.jsont negative with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "message accepted a negative sender ID"

let test_future_values () =
  let flag =
    ok (Jsont_bytesrw.decode_string Zulip.Message_flag.jsont {|"future_flag"|})
  in
  Alcotest.(check string)
    "future flag spelling" "future_flag"
    (Zulip.Message_flag.to_string flag);
  let policy =
    ok
      (Jsont_bytesrw.decode_string Zulip.Channel.Topics_policy.jsont
         {|"future_policy"|})
  in
  Alcotest.(check string)
    "future topic policy" "future_policy"
    (Zulip.Channel.Topics_policy.to_string policy);
  let visibility =
    ok (Jsont_bytesrw.decode_string Zulip.Topic_visibility.jsont "17")
  in
  Alcotest.(check int)
    "future visibility" 17
    (Zulip.Topic_visibility.to_int visibility);
  let role = ok (Jsont_bytesrw.decode_string Zulip.User.Role.jsont "700") in
  Alcotest.(check int) "future user role" 700 (Zulip.User.Role.to_int role);
  List.iter
    (fun text ->
      match Jsont_bytesrw.decode_string Zulip.Id.User_group.jsont text with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "invalid group ID was accepted")
    [ "-1"; "2.5"; "9007199254740992" ];
  (match Jsont_bytesrw.decode_string Zulip.Topic_visibility.jsont "1.5" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "fractional policy was rounded");
  (match
     Jsont_bytesrw.encode_string Zulip.Topic_visibility.jsont (Other max_int)
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "inexact policy encoded successfully");
  let filter = Zulip.Narrow.topic "release" in
  Alcotest.(check string)
    "double negation restores filter"
    (ok (Jsont_bytesrw.encode_string Zulip.Narrow.jsont filter))
    (ok
       (Jsont_bytesrw.encode_string Zulip.Narrow.jsont
          (Zulip.Narrow.not_ (Zulip.Narrow.not_ filter))))

let test_event_errors () =
  let decode kind text =
    Zulip.Event_payload.decode kind
      (ok (Jsont_bytesrw.decode_string Jsont.json text))
  in
  (match
     decode Zulip.Event_type.Reaction
       {|{"op":"create","message_id":1,"user_id":2,"emoji_name":"smile"}|}
   with
  | Error error ->
      Alcotest.(check (list string)) "operation path" [ "op" ] error.path;
      Alcotest.(check bool)
        "error keeps event family" true
        (error.event_type = Zulip.Event_type.Reaction)
  | Ok _ -> Alcotest.fail "reaction accepted create operation");
  (match
     decode Zulip.Event_type.Realm_user
       {|{"op":"update","person":{"user_id":"bad"}}|}
   with
  | Error error ->
      Alcotest.(check (list string))
        "nested field path" [ "person"; "user_id" ] error.path;
      Alcotest.(check bool)
        "structured JSON cause" true
        (Option.is_some error.cause)
  | Ok _ -> Alcotest.fail "malformed nested user ID accepted");
  match
    Zulip.Event.create ~id:(Zulip.Id.Event.of_int 0) ~type_:Heartbeat
      ~data:(ok (Jsont_bytesrw.decode_string Jsont.json {|{"id":9}|}))
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "reserved envelope field accepted"

let () =
  Alcotest.run "Zulip protocol"
    [
      ( "codecs",
        [
          Alcotest.test_case "typed IDs" `Quick test_ids;
          Alcotest.test_case "future policies and flags" `Quick
            test_future_values;
          Alcotest.test_case "structured event errors" `Quick test_event_errors;
          Alcotest.test_case "channel message and extensions" `Quick
            test_channel_message;
          Alcotest.test_case "group DM participants" `Quick test_direct_message;
          Alcotest.test_case "nullable user fields" `Quick test_user_nulls;
          Alcotest.test_case "invalid message" `Quick test_invalid_message;
        ] );
    ]
