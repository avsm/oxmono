(** Response codecs must accept the explicit [null] representation used by
    deployed homeservers for optional values. These tests deliberately drive the
    public endpoint functions, rather than reaching into private codecs, so
    request-only and persisted-store shapes cannot be loosened by accident. *)

module Client = Matrix_client.Client
module Devices = Matrix_client.Devices
module Directory = Matrix_client.Directory
module Notifications = Matrix_client.Notifications
module Presence = Matrix_client.Presence
module Profile = Matrix_client.Profile
module Search = Matrix_client.Search
module Id = Matrix_proto.Id

let mock_env =
  object
    method secure_random =
      Eio.Flow.string_source (String.init 4096 (fun i -> Char.chr (i land 255)))
  end

let json body = Fetch_mock.respond body
let mock body = Fetch_mock.client (fun req -> json body req)

let client_of fetch =
  let config =
    Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ()
  in
  Client.create ~config ~fetch ~random:(Matrix_client.Random.of_env mock_env)

let test_session : Client.session =
  {
    user_id = Result.get_ok (Id.User_id.of_string "@alice:example.org");
    access_token = "syt_secret_token";
    device_id = Result.get_ok (Id.Device_id.of_string "TESTDEVICE");
    refresh_token = None;
  }

let logged_in fetch = Client.with_session (client_of fetch) test_session
let run f () = Eio_mock.Backend.run f

let ok = function
  | Ok value -> value
  | Error error ->
      Alcotest.failf "expected a decoded response, got %s"
        (Matrix_client.Error.to_string error)

let uid value = Result.get_ok (Id.User_id.of_string value)
let did value = Result.get_ok (Id.Device_id.of_string value)

let test_devices () =
  let device =
    ok
      (Devices.get_device
         (logged_in
            (mock
               {|{"device_id":"OTHER","display_name":null,"last_seen_ip":null,"last_seen_ts":null}|}))
         ~device_id:(did "OTHER"))
  in
  Alcotest.(check (option string)) "display_name null" None device.display_name;
  Alcotest.(check (option string)) "last_seen_ip null" None device.last_seen_ip;
  Alcotest.(check bool) "last_seen_ts null" true (device.last_seen_ts = None)

let test_profile () =
  let profile =
    ok
      (Profile.get_profile
         (logged_in (mock {|{"displayname":null,"avatar_url":null}|}))
         ~user_id:(uid "@alice:example.org"))
  in
  Alcotest.(check (option string)) "displayname null" None profile.displayname;
  Alcotest.(check bool) "avatar_url null" true (profile.avatar_url = None)

let test_presence () =
  let presence =
    ok
      (Presence.get_presence
         (logged_in (mock {|{"presence":"offline","status_msg":null}|}))
         ~user_id:(uid "@alice:example.org"))
  in
  Alcotest.(check (option string)) "status_msg null" None presence.status_msg

let event_json =
  {|{"event_id":"$e1:example.org","sender":"@bob:example.org",
     "origin_server_ts":1234,"type":"m.room.message",
     "content":{"body":"hello","msgtype":"m.text"}}|}

let test_notifications () =
  let body =
    Printf.sprintf
      {|{"notifications":[{"actions":[],"event":%s,"profile_tag":null,
         "read":false,"room_id":"!r:example.org","ts":1234}],
         "next_token":null}|}
      event_json
  in
  let notifications = ok (Notifications.get (logged_in (mock body)) ()) in
  let notification = List.hd notifications.chunk in
  Alcotest.(check (option string))
    "profile_tag null" None notification.profile_tag;
  Alcotest.(check (option string))
    "next_token null" None notifications.next_token

let test_search () =
  let body =
    Printf.sprintf
      {|{"search_categories":{"room_events":{
         "highlights":[],"next_batch":null,
         "results":[{"rank":1,"result":%s,
           "context":{"start":null,"end":null,
             "profile_info":{"@bob:example.org":
               {"displayname":null,"avatar_url":null}},
             "events_before":[],"events_after":[]}}]}}}|}
      event_json
  in
  let result =
    ok
      (Search.room_events
         (logged_in (mock body))
         ~criteria:(Search.v "hello") ())
  in
  Alcotest.(check (option string)) "next_batch null" None result.next_batch;
  let hit = List.hd result.results in
  match hit.context with
  | None -> Alcotest.fail "expected search context"
  | Some context ->
      Alcotest.(check (option string)) "context start null" None context.start;
      Alcotest.(check (option string)) "context end null" None context.end_;
      let profile =
        match List.assoc_opt (uid "@bob:example.org") context.profile_info with
        | Some profile -> profile
        | None -> Alcotest.fail "expected profile information"
      in
      Alcotest.(check (option string))
        "profile displayname null" None profile.displayname;
      Alcotest.(check (option string))
        "profile avatar_url null" None profile.avatar_url

let test_directory () =
  let body =
    {|{"chunk":[{"room_id":"!r:example.org","name":null,"topic":null,
       "avatar_url":null,"canonical_alias":null,"num_joined_members":0,
       "room_type":null,"room_version":null,"guest_can_join":false,
       "world_readable":false,"encryption":null}],
       "next_batch":null,"prev_batch":null}|}
  in
  let rooms = ok (Directory.get_public_rooms (logged_in (mock body)) ()) in
  Alcotest.(check (option string)) "next_batch null" None rooms.page.next_batch;
  Alcotest.(check (option string)) "prev_batch null" None rooms.page.prev_batch;
  let room = List.hd rooms.page.chunk in
  Alcotest.(check (option string)) "name null" None room.name;
  Alcotest.(check (option string)) "topic null" None room.topic;
  Alcotest.(check bool) "avatar_url null" true (room.avatar_url = None);
  Alcotest.(check bool) "canonical_alias null" true (room.canonical_alias = None);
  Alcotest.(check (option string)) "room_type null" None room.room_type;
  Alcotest.(check (option string)) "room_version null" None room.room_version;
  Alcotest.(check (option string)) "encryption null" None room.encryption

let () =
  Alcotest.run "nullable response codecs"
    [
      ( "responses",
        [
          Alcotest.test_case "devices" `Quick (run test_devices);
          Alcotest.test_case "profile" `Quick (run test_profile);
          Alcotest.test_case "presence" `Quick (run test_presence);
          Alcotest.test_case "notifications" `Quick (run test_notifications);
          Alcotest.test_case "search" `Quick (run test_search);
          Alcotest.test_case "directory" `Quick (run test_directory);
        ] );
    ]
