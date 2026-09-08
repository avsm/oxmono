(** Tests for {!Matrix_proto.Sliding_sync} and {!Matrix_client.Sliding_sync},
    simplified sliding sync (MSC4186), against a mock homeserver.

    The request assertions compare the bytes that actually leave
    {!Matrix_client.Client} against fixtures written from ruma's
    [sync::sync_events::v5] member names, so a rename on either side shows up
    here; the response fixtures are hand-written from the same source. The
    harness is copied from [test/test_matrix_client.ml] rather than shared, so
    that neither test constrains the other. *)

module Client = Matrix_client.Client
module Error = Matrix_client.Error
module Ss = Matrix_client.Sliding_sync
module Ss_eio = Matrix_eio.Sliding_sync
module Wire = Matrix_proto.Sliding_sync
module Request = Wire.Request
module Response = Wire.Response
module Required_state = Wire.Required_state
module Base = Matrix_client.Base_client
module Thread_subscriptions = Matrix_client.Thread_subscriptions
module Store = Matrix_client.Store
module Id = Matrix_proto.Id

let mock_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 4096 '\007')
  end

type recorded = { url : string; meth : string; body : string option }

let body_of_request (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> None
  | Fetch.String s -> Some s
  | Fetch.Stream _ -> Some "<stream>"

let mock handler =
  let log = ref [] in
  let client =
    Fetch_mock.client (fun (req : Fetch.Middleware.request) ->
        log :=
          {
            url = Fetch.Middleware.Url.to_string req.url;
            meth = Http.Method.to_string req.meth;
            body = body_of_request req;
          }
          :: !log;
        handler req)
  in
  (log, client)

let requests log = List.rev !log

let one_request log =
  match requests log with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected one request, got %d" (List.length rs)

let req_state ty key =
  Required_state.v ~state_key:key (Matrix_proto.Event.Event_type.of_string ty)

let uid s = Result.get_ok (Id.User_id.of_string s)
let did s = Result.get_ok (Id.Device_id.of_string s)
let rid s = Result.get_ok (Id.Room_id.of_string s)

let test_session : Client.session =
  {
    user_id = uid "@alice:example.org";
    access_token = "syt_token";
    device_id = did "TESTDEVICE";
    refresh_token = None;
  }

let homeserver = Uriz.of_string_exn "https://hs.example"

let client_of fetch =
  let config = Client.config ~homeserver () in
  Client.with_session
    (Client.create ~config ~fetch
       ~random:(Matrix_client.Random.of_env mock_env))
    test_session

(* The loop lives in [matrix-chat.eio], so its tests need a real backend and a
   client bound to a switch. The mock fetch keeps them off the network. *)
let eio_client ~sw ~env fetch =
  let t = Matrix_eio.Client.create ~sw ~env ~homeserver ~fetch () in
  Matrix_eio.Client.with_session t test_session

let with_loop f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> f ~env ~sw

let unexpected_error e =
  Alcotest.failf "unexpected: %a" Matrix_eio.Error.pp_err e

let run f () = Eio_mock.Backend.run f
let check_string = Alcotest.(check string)

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  n = 0 || go 0

let json body = Fetch_mock.respond body

let sent_body r =
  match r.body with
  | Some b -> b
  | None -> Alcotest.fail "expected a request body"

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "expected Ok, got %s" (Error.to_string e)

let err = function
  | Ok _ -> Alcotest.fail "expected Error, got Ok"
  | Error e -> e

let test_empty_request_is_empty_object () =
  (* Every member is omitted when empty, so a request that asks for nothing
     is [{}] and not [{"lists":{},...}]. *)
  let log, fetch = mock (json {|{"pos":"p1"}|}) in
  let t = client_of fetch in
  ignore (ok (Ss.sync_once t (Request.v ())));
  check_string "body" "{}" (sent_body (one_request log))

let full_request () =
  Request.v ~conn_id:"room-list" ()
  |> Request.add_list ~name:"all"
       ~ranges:[ (0, 19); (30, 39) ]
       ~required_state:
         [
           req_state "m.room.name" "";
           req_state "m.room.member" Required_state.lazy_members;
         ]
       ~timeline_limit:1
       ~filters:
         {
           Request.is_dm = Some false;
           is_encrypted = None;
           is_invite = Some false;
           room_types = [ "m.space" ];
           not_room_types = [ "m.space" ];
         }
  |> Request.subscribe_room ~room_id:(rid "!room:example.org")
       ~required_state:[ req_state "m.room.topic" "" ]
       ~timeline_limit:20
  |> Request.enable_e2ee
  |> Request.enable_to_device ~limit:100 ~since:"td1"
  |> Request.enable_account_data ~lists:[ "all" ]
  |> Request.enable_receipts
       ~rooms:[ Request.All_subscribed; Request.Room (rid "!x:example.org") ]
  |> Request.enable_typing ~lists:[]

(* [Jsont] emits object members in the order the codec declares them, and
   map members in key order. *)
let full_request_json =
  {|{"conn_id":"room-list","lists":{"all":{"ranges":[[0,19],[30,39]],|}
  ^ {|"required_state":[["m.room.name",""],["m.room.member","$LAZY"]],|}
  ^ {|"timeline_limit":1,"filters":{"is_dm":false,"is_invite":false,|}
  ^ {|"room_types":["m.space"],"not_room_types":["m.space"]}}},|}
  ^ {|"room_subscriptions":{"!room:example.org":|}
  ^ {|{"required_state":[["m.room.topic",""]],"timeline_limit":20}},|}
  ^ {|"extensions":{"to_device":{"enabled":true,"limit":100,"since":"td1"},|}
  ^ {|"e2ee":{"enabled":true},"account_data":{"enabled":true,|}
  ^ {|"lists":["all"]},"receipts":{"enabled":true,|}
  ^ {|"rooms":["*","!x:example.org"]},|}
  ^ {|"typing":{"enabled":true,"lists":[]}}}|}

let test_full_request_matches_ruma () =
  let log, fetch = mock (json {|{"pos":"p1"}|}) in
  let t = client_of fetch in
  ignore (ok (Ss.sync_once t (full_request ())));
  check_string "body" full_request_json (sent_body (one_request log))

let test_request_roundtrips () =
  (* The codec decodes what it encodes, so a server-side implementation
     reading these bytes sees the same request. *)
  let encoded = ok (Client.Http.encode_body Request.jsont (full_request ())) in
  let decoded = ok (Client.Http.decode_response Request.jsont encoded) in
  check_string "re-encoded" encoded
    (ok (Client.Http.encode_body Request.jsont decoded))

let test_builder_replaces_in_place () =
  (* Re-adding a list or subscription must replace it, not duplicate it, so
     that the encoding is stable. *)
  let r =
    Request.v ()
    |> Request.add_list ~name:"a" ~timeline_limit:1
    |> Request.add_list ~name:"b" ~timeline_limit:2
    |> Request.add_list ~name:"a" ~timeline_limit:3
    |> Request.subscribe_room ~room_id:(rid "!r:example.org") ~timeline_limit:1
    |> Request.subscribe_room ~room_id:(rid "!r:example.org") ~timeline_limit:9
  in
  Alcotest.(check int) "one entry per list" 2 (List.length r.lists);
  Alcotest.(check int) "one entry per room" 1 (List.length r.room_subscriptions);
  check_string "body"
    ({|{"lists":{"a":{"ranges":[[0,19]],"timeline_limit":3},|}
   ^ {|"b":{"ranges":[[0,19]],"timeline_limit":2}},|}
   ^ {|"room_subscriptions":{"!r:example.org":{"timeline_limit":9}}}|})
    (ok (Client.Http.encode_body Request.jsont r))

let test_request_profiles_extension () =
  let request =
    Request.v ()
    |> Request.enable_profiles ~fields:[ "displayname"; "m.status" ]
  in
  check_string "profiles request"
    {|{"extensions":{"org.matrix.msc4262.profiles":{"enabled":true,"fields":["displayname","m.status"]}}}|}
    (ok (Client.Http.encode_body Request.jsont request))

let test_request_unknown_extension_roundtrip () =
  let input =
    {|{"extensions":{"org.example.future":{"enabled":true,"nested":{"answer":42},"values":[1,"two",null]}}}|}
  in
  let request = ok (Client.Http.decode_response Request.jsont input) in
  check_string "unknown extension roundtrip" input
    (ok (Client.Http.encode_body Request.jsont request))

let test_request_unknown_extension_keeps_outer_object () =
  let input = {|{"extensions":{"org.example.future":{"enabled":true}}}|} in
  let request = ok (Client.Http.decode_response Request.jsont input) in
  check_string "unknown-only extensions" input
    (ok (Client.Http.encode_body Request.jsont request))

let test_request_known_and_unknown_extensions_coexist () =
  let input =
    {|{"extensions":{"e2ee":{"enabled":true},"org.example.future":{"nested":{"answer":42}}}}|}
  in
  let request = ok (Client.Http.decode_response Request.jsont input) in
  check_string "known and unknown extensions" input
    (ok (Client.Http.encode_body Request.jsont request));
  (* A manually supplied collision must not duplicate or override a known
     member on the wire. *)
  let request =
    {
      request with
      extensions =
        {
          request.extensions with
          other = ("e2ee", Jsont.Json.object' []) :: request.extensions.other;
        };
    }
  in
  check_string "known extension wins"
    {|{"extensions":{"e2ee":{"enabled":true},"org.example.future":{"nested":{"answer":42}}}}|}
    (ok (Client.Http.encode_body Request.jsont request))

let test_request_thread_subscriptions_extension () =
  let request = Request.v () |> Request.enable_thread_subscriptions ~limit:10 in
  check_string "thread subscriptions request"
    {|{"extensions":{"io.element.msc4308.thread_subscriptions":{"enabled":true,"limit":10}}}|}
    (ok (Client.Http.encode_body Request.jsont request));
  (match Request.enable_thread_subscriptions ~limit:(-1) (Request.v ()) with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "negative thread-subscription limit was accepted");
  if Sys.int_size > 53 then
    match
      Request.enable_thread_subscriptions
        ~limit:(Int64.to_int 9007199254740992L)
        (Request.v ())
    with
    | exception Invalid_argument _ -> ()
    | _ -> Alcotest.fail "unsafe thread-subscription limit was accepted"

let test_path_and_query () =
  let log, fetch = mock (json {|{"pos":"p2"}|}) in
  let t = client_of fetch in
  ignore
    (ok
       (Ss.sync_once t ~pos:"p1" ~timeout_ms:5000 ~set_presence:`Unavailable
          (Request.v ())));
  let r = one_request log in
  check_string "method" "POST" r.meth;
  check_string "url"
    ("https://hs.example"
   ^ "/_matrix/client/unstable/org.matrix.simplified_msc3575/sync"
   ^ "?pos=p1&set_presence=unavailable&timeout=5000")
    r.url

let test_query_without_pos () =
  (* No [pos] starts a new session; the member is omitted, not sent empty. *)
  let log, fetch = mock (json {|{"pos":"p1"}|}) in
  let t = client_of fetch in
  ignore (ok (Ss.sync_once t (Request.v ())));
  check_string "url"
    ("https://hs.example"
   ^ "/_matrix/client/unstable/org.matrix.simplified_msc3575/sync"
   ^ "?timeout=30000")
    (one_request log).url

let availability body =
  let log, fetch = mock (json body) in
  let available = Ss.is_available (client_of fetch) in
  (log, available)

let test_native_availability () =
  let log, available =
    availability
      {|{"versions":["v1.17"],"unstable_features":{"org.matrix.simplified_msc3575":true}}|}
  in
  Alcotest.(check bool) "enabled flag" true (ok available);
  let request = one_request log in
  check_string "method" "GET" request.meth;
  check_string "versions URL" "https://hs.example/_matrix/client/versions"
    request.url;
  let _, disabled =
    availability
      {|{"versions":["v1.17"],"unstable_features":{"org.matrix.simplified_msc3575":false}}|}
  in
  Alcotest.(check bool) "false flag" false (ok disabled);
  let _, absent = availability {|{"versions":["v1.17"]}|} in
  Alcotest.(check bool) "absent flag" false (ok absent)

let test_native_availability_error () =
  let _, fetch =
    mock (fun req -> Fetch_mock.respond ~status:503 "temporarily down" req)
  in
  match Ss.is_available (client_of fetch) with
  | Error (Error.Http_error { status = 503; _ }) -> ()
  | Error e -> Alcotest.failf "expected HTTP 503, got %s" (Error.to_string e)
  | Ok available ->
      Alcotest.failf "expected discovery error, got availability %b" available

let response_fixture =
  {|{
    "pos": "s58_224_0_13_10_1_1_16_0_1",
    "txn_id": "t123",
    "lists": { "all_rooms": { "count": 42 } },
    "rooms": {
      "!quiet:example.org": {
        "name": "Quiet room",
        "bump_stamp": 3,
        "required_state": [
          { "type": "m.room.name", "state_key": "", "sender": "@bob:example.org",
            "origin_server_ts": 1000, "event_id": "$s1",
            "content": { "name": "Quiet room" } }
        ]
      },
      "!busy:example.org": {
        "name": "Busy room",
        "avatar": "mxc://example.org/abc",
        "initial": true,
        "is_dm": true,
        "notification_count": 7,
        "highlight_count": 2,
        "joined_count": 3,
        "invited_count": 1,
        "num_live": 1,
        "limited": true,
        "prev_batch": "t9-1_0",
        "bump_stamp": 9,
        "heroes": [
          { "user_id": "@bob:example.org", "displayname": "Bob",
            "avatar_url": "mxc://example.org/bob" },
          { "user_id": "@carol:example.org" }
        ],
        "timeline": [
          { "type": "m.room.message", "sender": "@bob:example.org",
            "origin_server_ts": 2000, "event_id": "$e1",
            "content": { "msgtype": "m.text", "body": "hello" } }
        ],
        "required_state": [
          { "type": "m.room.member", "state_key": "@bob:example.org",
            "sender": "@bob:example.org", "origin_server_ts": 900,
            "event_id": "$m1", "content": { "membership": "join" } }
        ]
      },
      "!invited:example.org": {
        "avatar": null,
        "invite_state": [
          { "type": "m.room.member", "state_key": "@alice:example.org",
            "sender": "@bob:example.org",
            "content": { "membership": "invite" } }
        ]
      }
    },
    "extensions": {
      "to_device": {
        "next_batch": "td-42",
        "events": [
          { "type": "m.room_key", "sender": "@bob:example.org",
            "content": { "algorithm": "m.megolm.v1.aes-sha2" } }
        ]
      },
      "e2ee": {
        "device_lists": { "changed": ["@bob:example.org"], "left": [] },
        "device_one_time_keys_count": { "signed_curve25519": 50 },
        "device_unused_fallback_key_types": ["signed_curve25519"]
      },
      "account_data": {
        "global": [ { "type": "m.direct", "content": {} } ],
        "rooms": {
          "!busy:example.org": [ { "type": "m.tag", "content": {} } ]
        }
      },
      "receipts": {
        "rooms": { "!busy:example.org": { "type": "m.receipt", "content": {} } }
      },
      "typing": {
        "rooms": { "!busy:example.org": { "type": "m.typing", "content": {} } }
      }
    }
  }|}

let decode_fixture () =
  ok (Client.Http.decode_response Response.jsont response_fixture)

let room r name =
  match List.assoc_opt (rid name) r.Response.rooms with
  | Some room -> room
  | None -> Alcotest.failf "no room %s in response" name

let test_response_top_level () =
  let r = decode_fixture () in
  check_string "pos" "s58_224_0_13_10_1_1_16_0_1" r.pos;
  Alcotest.(check (option string)) "txn_id" (Some "t123") r.txn_id;
  Alcotest.(check (list (pair string int)))
    "lists"
    [ ("all_rooms", 42) ]
    (List.map (fun (n, (l : Response.list_response)) -> (n, l.count)) r.lists);
  Alcotest.(check int) "rooms" 3 (List.length r.rooms)

let test_response_room () =
  let r = decode_fixture () in
  let b = room r "!busy:example.org" in
  Alcotest.(check (option string)) "name" (Some "Busy room") b.name;
  Alcotest.(check bool)
    "avatar set" true
    (b.avatar = Response.Set "mxc://example.org/abc");
  Alcotest.(check (option bool)) "initial" (Some true) b.initial;
  Alcotest.(check (option bool)) "is_dm" (Some true) b.is_dm;
  (* The unread counts are flattened into the room object, not nested. *)
  Alcotest.(check (option int))
    "notification_count" (Some 7) b.notification_count;
  Alcotest.(check (option int)) "highlight_count" (Some 2) b.highlight_count;
  Alcotest.(check (option int)) "joined_count" (Some 3) b.joined_count;
  Alcotest.(check (option int)) "invited_count" (Some 1) b.invited_count;
  Alcotest.(check (option int)) "num_live" (Some 1) b.num_live;
  Alcotest.(check (option int)) "bump_stamp" (Some 9) b.bump_stamp;
  Alcotest.(check bool) "limited" true b.limited;
  Alcotest.(check (option string)) "prev_batch" (Some "t9-1_0") b.prev_batch;
  Alcotest.(check int) "timeline" 1 (List.length b.timeline);
  Alcotest.(check int) "required_state" 1 (List.length b.required_state);
  Alcotest.(check bool) "not an invite" true (b.invite_state = None);
  match b.heroes with
  | Some [ h1; h2 ] ->
      check_string "hero 1" "@bob:example.org" (Id.User_id.to_string h1.user_id);
      Alcotest.(check (option string)) "hero 1 name" (Some "Bob") h1.displayname;
      Alcotest.(check (option string)) "hero 2 name" None h2.displayname
  | _ -> Alcotest.fail "expected two heroes"

let test_response_avatar_tri_state () =
  (* Absent, [null] and a string are three different things. *)
  let r = decode_fixture () in
  Alcotest.(check bool)
    "absent means unchanged" true
    ((room r "!quiet:example.org").avatar = Response.Unchanged);
  Alcotest.(check bool)
    "null means removed" true
    ((room r "!invited:example.org").avatar = Response.Removed)

let test_response_invite_state () =
  let r = decode_fixture () in
  match (room r "!invited:example.org").invite_state with
  | Some [ event ] ->
      check_string "sender" "@bob:example.org"
        (Id.User_id.to_string event.sender);
      check_string "event type" "m.room.member"
        (Matrix_proto.Event.Event_type.to_string event.type_);
      check_string "state key" "@alice:example.org" event.state_key
  | _ -> Alcotest.fail "expected one stripped state event"

let test_response_extensions () =
  let r = decode_fixture () in
  let x = r.extensions in
  (match x.to_device with
  | Some td ->
      check_string "next_batch" "td-42" td.next_batch;
      Alcotest.(check int) "to-device events" 1 (List.length td.events)
  | None -> Alcotest.fail "expected a to-device extension");
  Alcotest.(check (option string))
    "to_device_next_batch" (Some "td-42")
    (Response.to_device_next_batch r);
  Alcotest.(check (list string))
    "device_lists.changed" [ "@bob:example.org" ]
    (List.map Id.User_id.to_string x.e2ee.device_lists.changed);
  Alcotest.(check (list (pair string int)))
    "otk counts"
    [ ("signed_curve25519", 50) ]
    x.e2ee.device_one_time_keys_count;
  Alcotest.(check (option (list string)))
    "fallback keys" (Some [ "signed_curve25519" ])
    x.e2ee.device_unused_fallback_key_types;
  Alcotest.(check int)
    "global account data" 1
    (List.length x.account_data.global);
  Alcotest.(check int) "room account data" 1 (List.length x.account_data.rooms);
  Alcotest.(check int) "receipts" 1 (List.length x.receipts.rooms);
  Alcotest.(check int) "typing" 1 (List.length x.typing.rooms)

let test_response_unknown_extension_roundtrip () =
  let input =
    {|{"pos":"p","extensions":{"org.example.future":{"nested":{"answer":42},"values":[1,"two",null]}}}|}
  in
  let response = ok (Client.Http.decode_response Response.jsont input) in
  check_string "unknown response extension roundtrip" input
    (ok (Client.Http.encode_body Response.jsont response))

let test_response_known_and_unknown_extensions_coexist () =
  let input =
    {|{"pos":"p","extensions":{"e2ee":{"device_one_time_keys_count":{"signed_curve25519":2}},"org.example.future":{"nested":{"answer":42}}}}|}
  in
  let response = ok (Client.Http.decode_response Response.jsont input) in
  Alcotest.(check int)
    "unknown response extension count" 1
    (List.length response.extensions.other);
  check_string "known and unknown response extensions"
    {|{"pos":"p","extensions":{"e2ee":{"device_lists":{"changed":[],"left":[]},"device_one_time_keys_count":{"signed_curve25519":2}},"org.example.future":{"nested":{"answer":42}}}}|}
    (ok (Client.Http.encode_body Response.jsont response))

let test_response_minimal () =
  (* Only [pos] is required. *)
  let r = ok (Client.Http.decode_response Response.jsont {|{"pos":"p"}|}) in
  check_string "pos" "p" r.pos;
  Alcotest.(check int) "no rooms" 0 (List.length r.rooms);
  Alcotest.(check bool) "no to-device" true (r.extensions.to_device = None)

let test_response_rejects_bad_room_id () =
  (* A key that is not a room id is a protocol violation, not something to
     silently drop. *)
  match
    Client.Http.decode_response Response.jsont
      {|{"pos":"p","rooms":{"nope":{}}}|}
  with
  | Error (Error.Json_error _) -> ()
  | Error e -> Alcotest.failf "wrong error: %s" (Error.to_string e)
  | Ok _ -> Alcotest.fail "expected a decode failure"

let test_response_profiles () =
  let response_body =
    {|{"pos":"p","extensions":{"org.matrix.msc4262.profiles":{"users":{"@alice:example.org":{"updated":{"displayname":"Alice","avatar_url":null}},"@bob:example.org":null}}}}|}
  in
  let response =
    ok (Client.Http.decode_response Response.jsont response_body)
  in
  match response.extensions.profiles.users with
  | [ (alice, Response.Updated fields); (bob, Response.Dropped) ] ->
      check_string "first user" "@alice:example.org"
        (Id.User_id.to_string alice);
      check_string "second user" "@bob:example.org" (Id.User_id.to_string bob);
      Alcotest.(check int) "updated fields" 2 (List.length fields);
      Alcotest.(check bool)
        "null deletion retained in patch" true
        (List.exists
           (fun (name, value) ->
             name = "avatar_url"
             && match value with Jsont.Null _ -> true | _ -> false)
           fields)
  | _ -> Alcotest.fail "expected one updated and one dropped profile"

let test_response_profiles_rejects_malformed () =
  let decode body = Client.Http.decode_response Response.jsont body in
  let bad_user =
    decode
      {|{"pos":"p","extensions":{"org.matrix.msc4262.profiles":{"users":{"not-a-user":null}}}}|}
  in
  let bad_value =
    decode
      {|{"pos":"p","extensions":{"org.matrix.msc4262.profiles":{"users":{"@alice:example.org":42}}}}|}
  in
  let malformed_update =
    decode
      {|{"pos":"p","extensions":{"org.matrix.msc4262.profiles":{"users":{"@alice:example.org":{"updated":42}}}}}|}
  in
  let is_json_error = function Error.Json_error _ -> true | _ -> false in
  Alcotest.(check bool)
    "bad user id" true
    (match bad_user with Error e -> is_json_error e | Ok _ -> false);
  Alcotest.(check bool)
    "bad profile value" true
    (match bad_value with Error e -> is_json_error e | Ok _ -> false);
  Alcotest.(check bool)
    "malformed update tag" true
    (match malformed_update with Error e -> is_json_error e | Ok _ -> false)

let test_response_thread_subscriptions () =
  let response =
    ok
      (Client.Http.decode_response Response.jsont
         {|{"pos":"p","extensions":{"io.element.msc4308.thread_subscriptions":{"subscribed":{"!a:example.org":{"$one:example.org":{"automatic":true,"bump_stamp":42}}},"unsubscribed":{"!b:example.org":{"$two:example.org":{"bump_stamp":7}}},"prev_batch":"older"}}}|})
  in
  match response.extensions.thread_subscriptions with
  | {
   subscribed = [ (room_a, [ (root_a, subscription) ]) ];
   unsubscribed = [ (room_b, [ (root_b, unsubscription) ]) ];
   prev_batch = Some "older";
  } ->
      check_string "subscribed room" "!a:example.org"
        (Id.Room_id.to_string room_a);
      check_string "subscribed root" "$one:example.org"
        (Id.Event_id.to_string root_a);
      Alcotest.(check bool) "automatic" true subscription.automatic;
      Alcotest.(check int64) "subscription stamp" 42L subscription.bump_stamp;
      check_string "unsubscribed room" "!b:example.org"
        (Id.Room_id.to_string room_b);
      check_string "unsubscribed root" "$two:example.org"
        (Id.Event_id.to_string root_b);
      Alcotest.(check int64) "unsubscription stamp" 7L unsubscription.bump_stamp
  | _ -> Alcotest.fail "unexpected thread-subscriptions extension"

let test_apply_thread_subscriptions_extension () =
  let response =
    ok
      (Client.Http.decode_response Response.jsont
         {|{"pos":"new","extensions":{"io.element.msc4308.thread_subscriptions":{"subscribed":{"!a:example.org":{"$one:example.org":{"automatic":true,"bump_stamp":42}}},"unsubscribed":{"!b:example.org":{"$two:example.org":{"bump_stamp":7}}},"prev_batch":"older"}}}|})
  in
  let store = Store.memory () in
  ignore
    (ok
       (Thread_subscriptions.apply_sliding_extension store
          ~previous_pos:(Some "before") response.extensions.thread_subscriptions));
  (match Thread_subscriptions.subscriptions store with
  | Ok
      [
        ( room_a,
          root_a,
          { status = Thread_subscriptions.Automatic; bump_stamp = Some 42L } );
        ( room_b,
          root_b,
          { status = Thread_subscriptions.Unsubscribed; bump_stamp = Some 7L }
        );
      ] ->
      check_string "stored subscribed room" "!a:example.org"
        (Id.Room_id.to_string room_a);
      check_string "stored subscribed root" "$one:example.org"
        (Id.Event_id.to_string root_a);
      check_string "stored unsubscribed room" "!b:example.org"
        (Id.Room_id.to_string room_b);
      check_string "stored unsubscribed root" "$two:example.org"
        (Id.Event_id.to_string root_b)
  | Ok _ -> Alcotest.fail "sliding thread subscriptions were not stored"
  | Error error ->
      Alcotest.failf "stored sliding subscriptions: %s" (Error.to_string error));
  (match Thread_subscriptions.catchup_tokens store with
  | Ok [ { from_ = "older"; to_ = Some "before" } ] -> ()
  | Ok _ -> Alcotest.fail "sliding catch-up range was not stored"
  | Error error ->
      Alcotest.failf "stored sliding catch-up range: %s" (Error.to_string error));
  let raw_slot =
    Store.Slot.v ~name:"thread_subscriptions" Matrix_proto.Json.Codec.json
  in
  let legacy =
    ok
      (Client.Http.decode_response Matrix_proto.Json.Codec.json
         {|{"format_version":"1","entries":[{"room_id":"!a:example.org","thread_root":"$one:example.org","subscription":{"status":"automatic","bump_stamp":"42"}}],"catchup_tokens":[]}|})
  in
  ignore (ok (Store.Slot.set store raw_slot legacy));
  match Thread_subscriptions.subscriptions store with
  | Ok [ (_, _, { status = Automatic; bump_stamp = Some 42L }) ] -> ()
  | Ok _ -> Alcotest.fail "legacy numeric subscription stamp was not restored"
  | Error error ->
      Alcotest.failf "legacy numeric subscription stamp: %s"
        (Error.to_string error)

let test_response_thread_subscriptions_rejects_malformed () =
  let decode extension =
    Client.Http.decode_response Response.jsont
      (Printf.sprintf
         {|{"pos":"p","extensions":{"io.element.msc4308.thread_subscriptions":%s}}|}
         extension)
  in
  let is_error = function Error _ -> true | Ok _ -> false in
  Alcotest.(check bool)
    "invalid room id" true
    (is_error
       (decode
          {|{"subscribed":{"not-a-room":{"$root:example.org":{"automatic":false,"bump_stamp":1}}}}|}));
  Alcotest.(check bool)
    "invalid event id" true
    (is_error
       (decode
          {|{"subscribed":{"!room:example.org":{"not-an-event":{"automatic":false,"bump_stamp":1}}}}|}));
  Alcotest.(check bool)
    "negative stamp" true
    (is_error
       (decode
          {|{"unsubscribed":{"!room:example.org":{"$root:example.org":{"bump_stamp":-1}}}}|}));
  Alcotest.(check bool)
    "unsafe stamp" true
    (is_error
       (decode
          {|{"unsubscribed":{"!room:example.org":{"$root:example.org":{"bump_stamp":9007199254740992}}}}|}));
  Alcotest.(check bool)
    "numeric-string stamp" true
    (is_error
       (decode
          {|{"unsubscribed":{"!room:example.org":{"$root:example.org":{"bump_stamp":"1"}}}}|}))

let test_own_profile_projection_updates_and_deduplicates () =
  Eio_main.run @@ fun _env ->
  let alice = uid "@alice:example.org" in
  let observer = Ss_eio.Own_profile.create ~user_id:alice in
  let seen = ref [] in
  let isolated = ref 0 in
  let bad_subscription =
    Ss_eio.Own_profile.subscribe observer (fun _ ->
        incr isolated;
        if !isolated > 0 then failwith "subscriber failure")
  in
  let subscription =
    Ss_eio.Own_profile.subscribe observer (fun profile ->
        seen := profile :: !seen)
  in
  let response body = ok (Client.Http.decode_response Response.jsont body) in
  let updated name avatar extra =
    response
      (Printf.sprintf
         {|{"pos":"p","extensions":{"org.matrix.msc4262.profiles":{"users":{"@alice:example.org":{"updated":{"displayname":%s,"avatar_url":%s,"m.custom":%s}}}}}}|}
         (match name with None -> "null" | Some s -> Printf.sprintf "%S" s)
         (match avatar with None -> "null" | Some s -> Printf.sprintf "%S" s)
         extra)
  in
  let state, _ =
    Base.apply_sliding
      (Base.create ~user_id:alice ())
      (updated (Some "Alice") (Some "mxc://example.org/avatar") "true")
  in
  Ss_eio.Own_profile.refresh observer state;
  let deleted, _ = Base.apply_sliding state (updated None None "null") in
  Ss_eio.Own_profile.refresh observer deleted;
  (match Ss_eio.Own_profile.current observer with
  | Some { Matrix_client.Profile.displayname = None; avatar_url = None; _ } ->
      ()
  | Some _ -> Alcotest.fail "null profile fields were not deleted"
  | None -> Alcotest.fail "null patch dropped the whole profile");
  Ss_eio.Own_profile.refresh observer state;
  let dropped =
    response
      {|{"pos":"drop","extensions":{"org.matrix.msc4262.profiles":{"users":{"@alice:example.org":null}}}}|}
  in
  let dropped, _ = Base.apply_sliding state dropped in
  Ss_eio.Own_profile.refresh observer dropped;
  let after_drop = Ss_eio.Own_profile.current observer in
  Alcotest.(check bool) "drop clears profile" true (Option.is_none after_drop);
  Alcotest.(check int)
    "initial, patches, drop and no-op only" 5 (List.length !seen);
  Alcotest.(check int) "listener failures are isolated" 5 !isolated;
  Ss_eio.Own_profile.unsubscribe observer bad_subscription;
  Ss_eio.Own_profile.unsubscribe observer subscription;
  Ss_eio.Own_profile.unsubscribe observer subscription;
  Ss_eio.Own_profile.refresh observer state;
  Alcotest.(check int) "unsubscribe is leak-free" 5 (List.length !seen);
  let cancellation_observed = ref false in
  Eio.Cancel.sub (fun cancel ->
      let armed = ref false in
      let cancellation_subscription =
        Ss_eio.Own_profile.subscribe observer (fun _ ->
            if !armed then begin
              Eio.Cancel.cancel cancel (Failure "profile cancellation");
              Eio.Cancel.check cancel
            end)
      in
      armed := true;
      (try Ss_eio.Own_profile.refresh observer deleted
       with Eio.Cancel.Cancelled _ -> cancellation_observed := true);
      Ss_eio.Own_profile.unsubscribe observer cancellation_subscription);
  Alcotest.(check bool)
    "listener cancellation propagates" true !cancellation_observed

let test_profile_service_updates_are_durable_and_deduplicated () =
  Eio_main.run @@ fun _env ->
  let alice = uid "@alice:example.org" in
  let bob = uid "@bob:example.org" in
  let store = Store.memory () in
  Store.replace_profiles store
    [ (bob, [ ("displayname", Jsont.Json.string "Bob") ]) ];
  let service = Matrix_eio.Sync_service.of_store ~store ~user_id:alice () in
  let notifications = ref [] in
  Matrix_eio.Sync_service.on_profile_change service (fun state changes ->
      (* Hooks run after the common snapshot has been flushed, and see the
         same deterministic profile ordering as the service state. *)
      notifications :=
        ( Store.profiles store,
          List.map
            (fun (change : Matrix_client.Base_client.profile_change) ->
              Id.User_id.to_string change.changed_user_id)
            changes,
          Matrix_client.Base_client.profiles state )
        :: !notifications);
  let apply fields =
    Matrix_eio.Sync_service.apply_profile_updates service
      { Response.users = [ (alice, Response.Updated fields) ] }
  in
  ignore
    (apply
       [
         ("displayname", Jsont.Json.string "Alice");
         ("m.custom", Jsont.Json.string "one");
       ]);
  ignore
    (apply
       [
         ("displayname", Jsont.Json.string "Alice");
         ("m.custom", Jsont.Json.string "one");
       ]);
  ignore (apply [ ("displayname", Jsont.Json.null ()) ]);
  ignore
    (Matrix_eio.Sync_service.apply_profile_updates service
       { Response.users = [ (alice, Response.Dropped) ] });
  ignore
    (Matrix_eio.Sync_service.apply_profile_updates service
       { Response.users = [ (alice, Response.Dropped) ] });
  Alcotest.(check int) "no-op does not notify" 3 (List.length !notifications);
  Alcotest.(check bool)
    "Alice is dropped" true
    (Matrix_client.Base_client.find_profile
       (Matrix_eio.Sync_service.state service)
       alice
    = None);
  Alcotest.(check bool)
    "unrelated Bob is preserved" true
    (Matrix_client.Base_client.find_profile
       (Matrix_eio.Sync_service.state service)
       bob
    <> None);
  List.iter
    (fun (profiles, _, state_profiles) ->
      Alcotest.(check bool)
        "hook sees persisted common snapshot" true
        (profiles = state_profiles))
    !notifications;
  Alcotest.(check (list (list string)))
    "notifications have deterministic users"
    [
      [ "@alice:example.org" ];
      [ "@alice:example.org" ];
      [ "@alice:example.org" ];
    ]
    (List.rev_map (fun (_, users, _) -> users) !notifications)

(* Answer each request from a script, in order. *)
let scripted responses =
  let remaining = ref responses in
  mock (fun req ->
      match !remaining with
      | [] -> Alcotest.fail "the loop made more requests than scripted"
      | body :: rest ->
          remaining := rest;
          Fetch_mock.respond body req)

let controller_subscriptions controller =
  let request : Request.t = Ss_eio.Controller.request controller in
  List.map
    (fun (room_id, (settings : Request.room_subscription)) ->
      (Id.Room_id.to_string room_id, settings.timeline_limit))
    request.room_subscriptions

let test_controller_subscription_mutations () =
  let room_a = rid "!a:example.org" in
  let room_b = rid "!b:example.org" in
  let room_c = rid "!c:example.org" in
  let controller =
    Ss_eio.Controller.create
      (Request.v ()
      |> Request.subscribe_room ~room_id:room_c ~timeline_limit:30
      |> Request.subscribe_room ~room_id:room_a ~timeline_limit:10)
  in
  let subscriptions = Alcotest.(list (pair string int)) in
  Alcotest.check subscriptions "initial subscriptions are deterministic"
    [ ("!a:example.org", 10); ("!c:example.org", 30) ]
    (controller_subscriptions controller);
  Ss_eio.Controller.add_room_subscriptions ~room_ids:[ room_b ]
    ~timeline_limit:20 controller;
  Alcotest.check subscriptions "add preserves the other rooms"
    [ ("!a:example.org", 10); ("!b:example.org", 20); ("!c:example.org", 30) ]
    (controller_subscriptions controller);
  Ss_eio.Controller.remove_room_subscriptions ~room_ids:[ room_a; room_c ]
    controller;
  Alcotest.check subscriptions "remove drops only named rooms"
    [ ("!b:example.org", 20) ]
    (controller_subscriptions controller);
  Ss_eio.Controller.set_room_subscriptions ~room_ids:[ room_c; room_a; room_c ]
    ~timeline_limit:42 controller;
  Alcotest.check subscriptions "set deduplicates and sorts"
    [ ("!a:example.org", 42); ("!c:example.org", 42) ]
    (controller_subscriptions controller);
  Ss_eio.Controller.reset_and_add_room_subscriptions ~room_ids:[] controller;
  Alcotest.check subscriptions "reset can empty the set" []
    (controller_subscriptions controller)

let decoded_sent_request recorded =
  ok (Client.Http.decode_response Request.jsont (sent_body recorded))

let test_controller_cancels_changed_long_polls () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let starts = Array.init 4 (fun _ -> Eio.Promise.create ()) in
  let blocks = Array.init 4 (fun _ -> fst (Eio.Promise.create ())) in
  let calls = ref 0 in
  let log, fetch =
    mock (fun req ->
        let call = !calls in
        incr calls;
        if call < Array.length starts then begin
          Eio.Promise.resolve (snd starts.(call)) ();
          Eio.Promise.await blocks.(call);
          assert false
        end
        else Fetch_mock.respond {|{"pos":"done"}|} req)
  in
  let room_0 = rid "!r0:example.org" in
  let room_1 = rid "!r1:example.org" in
  let controller =
    Ss_eio.Controller.create
      (Request.v () |> Request.subscribe_room ~room_id:room_0)
  in
  let responses = ref 0 in
  let errors = ref 0 in
  let done_, done_r = Eio.Promise.create () in
  Ss_eio.sync_forever_controlled ~sw ~clock:(Eio.Stdenv.clock env)
    (eio_client ~sw ~env fetch)
    ~callbacks:
      {
        on_response =
          (fun _ ->
            incr responses;
            Eio.Promise.resolve done_r ();
            Matrix_eio.Sync.Stop);
        on_error =
          (fun _ ->
            incr errors;
            if not (Eio.Promise.is_resolved done_) then
              Eio.Promise.resolve done_r ();
            Matrix_eio.Sync.Stop);
      }
    controller;
  Eio.Promise.await (fst starts.(0));
  (* An identical delta must leave the current request alone. *)
  Ss_eio.Controller.set_room_subscriptions ~room_ids:[ room_0 ] controller;
  for _ = 1 to 4 do
    Eio.Fiber.yield ()
  done;
  Alcotest.(check int) "an exact no-op does not cancel" 1 !calls;
  (* Resetting is deliberately stronger than setting, as in the Rust API. *)
  Ss_eio.Controller.reset_and_add_room_subscriptions ~room_ids:[ room_0 ]
    controller;
  Eio.Promise.await (fst starts.(1));
  Ss_eio.Controller.set_room_subscriptions ~room_ids:[ room_1 ] controller;
  Eio.Promise.await (fst starts.(2));
  Ss_eio.Controller.set_room_subscriptions ~room_ids:[ room_1 ]
    ~timeline_limit:42 controller;
  Eio.Promise.await (fst starts.(3));
  Ss_eio.Controller.set_room_subscriptions ~room_ids:[] controller;
  Eio.Promise.await done_;
  Alcotest.(check int) "only the final response is delivered" 1 !responses;
  Alcotest.(check int) "internal restarts are not errors" 0 !errors;
  let sent = requests log in
  Alcotest.(check int) "one request per effective change" 5 (List.length sent);
  let summaries =
    List.map
      (fun recorded ->
        let request = decoded_sent_request recorded in
        List.map
          (fun (room_id, (settings : Request.room_subscription)) ->
            (Id.Room_id.to_string room_id, settings.timeline_limit))
          request.room_subscriptions)
      sent
  in
  Alcotest.check
    Alcotest.(list (list (pair string int)))
    "every restart carries the current subscriptions"
    [
      [ ("!r0:example.org", 10) ];
      [ ("!r0:example.org", 10) ];
      [ ("!r1:example.org", 10) ];
      [ ("!r1:example.org", 42) ];
      [];
    ]
    summaries;
  List.iter
    (fun recorded ->
      Alcotest.(check bool)
        "a cancelled poll does not advance pos" false
        (contains ~sub:"pos=" recorded.url))
    sent

let test_controller_can_defer_cancellation () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let first_started, first_started_r = Eio.Promise.create () in
  let release_first, release_first_r = Eio.Promise.create () in
  let calls = ref 0 in
  let log, fetch =
    mock (fun req ->
        incr calls;
        match !calls with
        | 1 ->
            Eio.Promise.resolve first_started_r ();
            Eio.Promise.await release_first;
            Fetch_mock.respond {|{"pos":"p1"}|} req
        | _ -> Fetch_mock.respond {|{"pos":"p2"}|} req)
  in
  let room_0 = rid "!r0:example.org" in
  let room_1 = rid "!r1:example.org" in
  let controller =
    Ss_eio.Controller.create
      (Request.v () |> Request.subscribe_room ~room_id:room_0)
  in
  let responses = ref 0 in
  let done_, done_r = Eio.Promise.create () in
  Ss_eio.sync_forever_controlled ~sw ~clock:(Eio.Stdenv.clock env)
    (eio_client ~sw ~env fetch)
    ~callbacks:
      {
        on_response =
          (fun _ ->
            incr responses;
            if !responses = 2 then begin
              Eio.Promise.resolve done_r ();
              Matrix_eio.Sync.Stop
            end
            else Matrix_eio.Sync.Continue);
        on_error = unexpected_error;
      }
    controller;
  Eio.Promise.await first_started;
  Ss_eio.Controller.add_room_subscriptions ~cancel_in_flight_request:false
    ~room_ids:[ room_1 ] controller;
  for _ = 1 to 4 do
    Eio.Fiber.yield ()
  done;
  Alcotest.(check int) "false leaves the first request running" 1 !calls;
  Eio.Promise.resolve release_first_r ();
  Eio.Promise.await done_;
  let sent = requests log in
  Alcotest.(check int) "both responses are accepted" 2 !responses;
  Alcotest.(check int) "two requests" 2 (List.length sent);
  let second = List.nth sent 1 in
  Alcotest.(check bool)
    "the accepted position advances" true
    (contains ~sub:"pos=p1" second.url);
  Alcotest.check
    Alcotest.(list (pair string int))
    "the next natural poll carries the deferred update"
    [ ("!r0:example.org", 10); ("!r1:example.org", 10) ]
    (let request = decoded_sent_request second in
     List.map
       (fun (room_id, (settings : Request.room_subscription)) ->
         (Id.Room_id.to_string room_id, settings.timeline_limit))
       request.room_subscriptions)

let test_default_presence_change_cancels_long_poll () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let first_started, first_started_r = Eio.Promise.create () in
  let blocks = Array.init 1 (fun _ -> fst (Eio.Promise.create ())) in
  let replacement_started, replacement_started_r = Eio.Promise.create () in
  let calls = ref 0 in
  let log, fetch =
    mock (fun req ->
        let call = !calls in
        incr calls;
        if call = 0 then begin
          Eio.Promise.resolve first_started_r ();
          (* Presence changes cancel this request before it can answer. *)
          Eio.Promise.await blocks.(0);
          assert false
        end
        else begin
          Eio.Promise.resolve replacement_started_r ();
          Fetch_mock.respond {|{"pos":"same"}|} req
        end)
  in
  let client = eio_client ~sw ~env fetch in
  Client.set_sync_presence (Matrix_eio.Client.base client) `Offline;
  let controller = Ss_eio.Controller.create (Request.v ()) in
  let responses = ref 0 in
  let done_, done_r = Eio.Promise.create () in
  Ss_eio.sync_forever_controlled ~sw ~clock:(Eio.Stdenv.clock env) client
    ~initial_pos:"before"
    ~callbacks:
      {
        on_response =
          (fun _ ->
            incr responses;
            Eio.Promise.resolve done_r ();
            Matrix_eio.Sync.Stop);
        on_error = unexpected_error;
      }
    controller;
  Eio.Promise.await first_started;
  (* This is an effective change and must wake the live default-presence
     poll, but setting the same value again must not create another wake. *)
  Client.set_sync_presence (Matrix_eio.Client.base client) `Offline;
  for _ = 1 to 4 do
    Eio.Fiber.yield ()
  done;
  Alcotest.(check int) "a no-op presence update does not cancel" 1 !calls;
  Client.set_sync_presence (Matrix_eio.Client.base client) `Unavailable;
  for _ = 1 to 4 do
    Eio.Fiber.yield ()
  done;
  Alcotest.(check int) "effective presence change restarts" 2 !calls;
  Eio.Promise.await replacement_started;
  Eio.Promise.await done_;
  Alcotest.(check int) "only replacement response is delivered" 1 !responses;
  let sent = requests log in
  Alcotest.(check int) "presence change restarts once" 2 (List.length sent);
  Alcotest.(check bool)
    "initial request uses the previous presence" true
    (contains ~sub:"set_presence=offline" (List.nth sent 0).url);
  Alcotest.(check bool)
    "replacement uses latest presence and same position" true
    (contains ~sub:"set_presence=unavailable" (List.nth sent 1).url
    && contains ~sub:"pos=before" (List.nth sent 1).url);
  Alcotest.(check bool)
    "replacement does not advance position" true
    (contains ~sub:"pos=before" (List.nth sent 1).url);
  (* The listener is removed when the loop terminates. *)
  Client.set_sync_presence (Matrix_eio.Client.base client) `Online;
  for _ = 1 to 4 do
    Eio.Fiber.yield ()
  done;
  Alcotest.(check int) "terminated loop has no presence listener" 2 !calls

let test_explicit_presence_ignores_client_changes () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let first_started, first_started_r = Eio.Promise.create () in
  let release_first, release_first_r = Eio.Promise.create () in
  let calls = ref 0 in
  let log, fetch =
    mock (fun req ->
        incr calls;
        if !calls = 1 then begin
          Eio.Promise.resolve first_started_r ();
          Eio.Promise.await release_first;
          Fetch_mock.respond {|{"pos":"fixed"}|} req
        end
        else Fetch_mock.respond {|{"pos":"unexpected"}|} req)
  in
  let client = eio_client ~sw ~env fetch in
  Client.set_sync_presence (Matrix_eio.Client.base client) `Offline;
  let done_, done_r = Eio.Promise.create () in
  Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env) client
    ~set_presence:`Unavailable
    ~callbacks:
      {
        on_response =
          (fun _ ->
            Eio.Promise.resolve done_r ();
            Matrix_eio.Sync.Stop);
        on_error = unexpected_error;
      }
    (Request.v ());
  Eio.Promise.await first_started;
  Client.set_sync_presence (Matrix_eio.Client.base client) `Unavailable;
  for _ = 1 to 4 do
    Eio.Fiber.yield ()
  done;
  Alcotest.(check int) "explicit presence does not cancel" 1 !calls;
  Eio.Promise.resolve release_first_r ();
  Eio.Promise.await done_;
  Alcotest.(check int) "explicit presence makes one request" 1 !calls;
  Alcotest.(check bool)
    "explicit presence is sent on the request" true
    (contains ~sub:"set_presence=unavailable" (one_request log).url)

let test_default_presence_change_wakes_retry_sleep () =
  with_loop @@ fun ~env ~sw ->
  let failed, failed_r = Eio.Promise.create () in
  let done_, done_r = Eio.Promise.create () in
  let calls = ref 0 in
  let log, fetch =
    mock (fun request ->
        incr calls;
        match !calls with
        | 1 ->
            Fetch_mock.respond ~status:500
              {|{"errcode":"M_UNKNOWN","error":"retry"}|} request
        | 2 -> Fetch_mock.respond {|{"pos":"after-presence-retry"}|} request
        | n -> Alcotest.failf "unexpected request %d" n)
  in
  let client = eio_client ~sw ~env fetch in
  Client.set_sync_presence (Matrix_eio.Client.base client) `Offline;
  Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env) client
    ~callbacks:
      {
        on_response =
          (fun _ ->
            Eio.Promise.resolve done_r ();
            Matrix_eio.Sync.Stop);
        on_error =
          (fun _ ->
            Eio.Promise.resolve failed_r ();
            Matrix_eio.Sync.Retry_after 60.);
      }
    (Request.v ());
  Eio.Promise.await failed;
  (* The update is deliberately made immediately after the error callback. It
     may race with installation of the retry timer, which is why the loop's
     generation check is part of this test. *)
  Client.set_sync_presence (Matrix_eio.Client.base client) `Unavailable;
  Eio.Promise.await done_;
  Alcotest.(check int) "presence wakes a retry" 2 !calls;
  Alcotest.(check int)
    "presence restart makes two requests" 2
    (List.length (requests log));
  Alcotest.(check bool)
    "replacement sends latest presence" true
    (contains ~sub:"set_presence=unavailable" (List.nth (requests log) 1).url)

let test_subscription_changes_wake_retry_sleep_including_empty () =
  with_loop @@ fun ~env ~sw ->
  let failed_one, failed_one_r = Eio.Promise.create () in
  let failed_two, failed_two_r = Eio.Promise.create () in
  let done_, done_r = Eio.Promise.create () in
  let calls = ref 0 in
  let room = rid "!retry:example.org" in
  let log, fetch =
    mock (fun request ->
        incr calls;
        match !calls with
        | 1 ->
            Fetch_mock.respond ~status:500
              {|{"errcode":"M_UNKNOWN","error":"retry-one"}|} request
        | 2 ->
            Fetch_mock.respond ~status:500
              {|{"errcode":"M_UNKNOWN","error":"retry-two"}|} request
        | 3 -> Fetch_mock.respond {|{"pos":"after-empty-retry"}|} request
        | n -> Alcotest.failf "unexpected request %d" n)
  in
  let controller = Ss_eio.Controller.create (Request.v ()) in
  Ss_eio.sync_forever_controlled ~sw ~clock:(Eio.Stdenv.clock env)
    (eio_client ~sw ~env fetch)
    ~callbacks:
      {
        on_response =
          (fun _ ->
            Eio.Promise.resolve done_r ();
            Matrix_eio.Sync.Stop);
        on_error =
          (fun _ ->
            match !calls with
            | 1 ->
                Eio.Promise.resolve failed_one_r ();
                Matrix_eio.Sync.Retry_after 60.
            | 2 ->
                Eio.Promise.resolve failed_two_r ();
                Matrix_eio.Sync.Retry_after 60.
            | n -> Alcotest.failf "unexpected error on request %d" n);
      }
    controller;
  Eio.Promise.await failed_one;
  Ss_eio.Controller.add_room_subscriptions ~room_ids:[ room ] controller;
  Eio.Promise.await failed_two;
  Ss_eio.Controller.remove_room_subscriptions ~room_ids:[ room ] controller;
  Eio.Promise.await done_;
  let sent = requests log in
  Alcotest.(check int) "each effective update wakes the retry" 3 !calls;
  Alcotest.(check int)
    "initial request is empty" 0
    (List.length (decoded_sent_request (List.nth sent 0)).room_subscriptions);
  Alcotest.(check int)
    "middle request has the subscription" 1
    (List.length (decoded_sent_request (List.nth sent 1)).room_subscriptions);
  Alcotest.(check int)
    "final request is empty again" 0
    (List.length (decoded_sent_request (List.nth sent 2)).room_subscriptions)

let test_retry_sleep_honours_noop_and_opt_out () =
  with_loop @@ fun ~env ~sw ->
  let failed, failed_r = Eio.Promise.create () in
  let done_, done_r = Eio.Promise.create () in
  let room_a = rid "!a:example.org" in
  let room_b = rid "!b:example.org" in
  let room_c = rid "!c:example.org" in
  let calls = ref 0 in
  let log, fetch =
    mock (fun request ->
        incr calls;
        match !calls with
        | 1 ->
            Fetch_mock.respond ~status:500
              {|{"errcode":"M_UNKNOWN","error":"retry"}|} request
        | 2 -> Fetch_mock.respond {|{"pos":"after-deferred-change"}|} request
        | n -> Alcotest.failf "unexpected request %d" n)
  in
  let controller =
    Ss_eio.Controller.create
      (Request.v () |> Request.subscribe_room ~room_id:room_a)
  in
  Ss_eio.sync_forever_controlled ~sw ~clock:(Eio.Stdenv.clock env)
    (eio_client ~sw ~env fetch)
    ~callbacks:
      {
        on_response =
          (fun _ ->
            Eio.Promise.resolve done_r ();
            Matrix_eio.Sync.Stop);
        on_error =
          (fun _ ->
            Eio.Promise.resolve failed_r ();
            Matrix_eio.Sync.Retry_after 60.);
      }
    controller;
  Eio.Promise.await failed;
  (* Same settings are a no-op. The effective update explicitly opts out of
     cancellation and must therefore wait for the normal backoff. *)
  Ss_eio.Controller.set_room_subscriptions ~room_ids:[ room_a ] controller;
  Ss_eio.Controller.add_room_subscriptions ~cancel_in_flight_request:false
    ~room_ids:[ room_b ] controller;
  for _ = 1 to 4 do
    Eio.Fiber.yield ()
  done;
  Alcotest.(check int) "no-op and opt-out do not wake retry" 1 !calls;
  Ss_eio.Controller.add_room_subscriptions ~room_ids:[ room_c ] controller;
  Eio.Promise.await done_;
  let request = decoded_sent_request (List.nth (requests log) 1) in
  Alcotest.(check int)
    "deferred and waking updates are carried" 3
    (List.length request.room_subscriptions)

let test_parent_cancellation_interrupts_retry_sleep () =
  Eio_main.run @@ fun env ->
  let cancelled = ref false in
  (try
     Eio.Switch.run @@ fun sw ->
     let failed, failed_r = Eio.Promise.create () in
     let calls = ref 0 in
     let _, fetch =
       mock (fun request ->
           incr calls;
           Fetch_mock.respond ~status:500
             {|{"errcode":"M_UNKNOWN","error":"retry"}|} request)
     in
     Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
       (eio_client ~sw ~env fetch)
       ~callbacks:
         {
           on_response = (fun _ -> Alcotest.fail "unexpected response");
           on_error =
             (fun _ ->
               Eio.Promise.resolve failed_r ();
               Matrix_eio.Sync.Retry_after 60.);
         }
       (Request.v ());
     Eio.Promise.await failed;
     Eio.Switch.fail sw Exit
   with Exit -> cancelled := true);
  Alcotest.(check bool)
    "parent cancellation exits the retry sleep" true !cancelled

let test_loop_threads_pos_and_to_device_since () =
  let log, fetch =
    scripted
      [
        {|{"pos":"p1","extensions":{"to_device":{"next_batch":"td-1"}}}|};
        {|{"pos":"p2","extensions":{"to_device":{"next_batch":"td-2"}}}|};
      ]
  in
  let seen = ref [] in
  let request =
    Request.v ~conn_id:"c" ()
    |> Request.add_list ~name:"all" ~timeline_limit:1
    |> Request.enable_to_device
  in
  with_loop (fun ~env ~sw ->
      Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~sw ~env fetch)
        ~set_presence:`Online
        ~callbacks:
          {
            on_response =
              (fun (r : Response.t) ->
                seen := r.pos :: !seen;
                if List.length !seen >= 2 then Matrix_eio.Sync.Stop
                else Matrix_eio.Sync.Continue);
            on_error = unexpected_error;
          }
        request);
  Alcotest.(check (list string)) "responses" [ "p1"; "p2" ] (List.rev !seen);
  match requests log with
  | [ first; second ] ->
      (* [pos] is absent on the first request and carries the previous
         response's value on the second. *)
      Alcotest.(check bool)
        "first request has no pos" false
        (contains ~sub:"pos=" first.url);
      Alcotest.(check bool)
        "second request carries pos=p1" true
        (contains ~sub:"pos=p1" second.url);
      Alcotest.(check bool)
        "online presence is omitted on first request" false
        (contains ~sub:"set_presence=online" first.url);
      Alcotest.(check bool)
        "online presence is omitted on every request" false
        (contains ~sub:"set_presence=online" second.url);
      (* [extensions.to_device.since] is threaded from [next_batch]. *)
      let decode b = ok (Client.Http.decode_response Request.jsont b) in
      Alcotest.(check (option string))
        "no since initially" None
        (decode (sent_body first)).extensions.to_device.since;
      Alcotest.(check (option string))
        "since from next_batch" (Some "td-1")
        (decode (sent_body second)).extensions.to_device.since;
      (* No sticky parameters: the whole request goes out every time. *)
      Alcotest.(check int)
        "lists resent" 1
        (List.length (decode (sent_body second)).lists);
      Alcotest.(check (option string))
        "conn_id resent" (Some "c") (decode (sent_body second)).conn_id;
      (* And no transaction id unless asked for. *)
      Alcotest.(check (option string))
        "no txn_id" None (decode (sent_body second)).txn_id
  | rs -> Alcotest.failf "expected two requests, got %d" (List.length rs)

let test_loop_persists_thread_subscriptions_before_callback () =
  let _, fetch =
    scripted
      [
        {|{"pos":"after","extensions":{"io.element.msc4308.thread_subscriptions":{"subscribed":{"!room:example.org":{"$thread:example.org":{"automatic":false,"bump_stamp":12}}},"prev_batch":"older"}}}|};
      ]
  in
  let store = Store.memory () in
  let room_id = rid "!room:example.org" in
  let thread_root =
    Result.get_ok (Id.Event_id.of_string "$thread:example.org")
  in
  let observed = ref false in
  with_loop (fun ~env ~sw ->
      Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~sw ~env fetch)
        ~initial_pos:"before" ~thread_subscription_store:store
        ~callbacks:
          {
            on_response =
              (fun _ ->
                observed :=
                  Thread_subscriptions.find_stored store ~room_id ~thread_root
                  = Ok
                      (Some
                         {
                           status = Thread_subscriptions.Manual;
                           bump_stamp = Some 12L;
                         })
                  && Thread_subscriptions.catchup_tokens store
                     = Ok [ { from_ = "older"; to_ = Some "before" } ];
                Matrix_eio.Sync.Stop);
            on_error = unexpected_error;
          }
        (Request.v () |> Request.enable_thread_subscriptions ~limit:10));
  Alcotest.(check bool)
    "store is updated before response callback" true !observed

let test_loop_persists_sliding_state_before_callback () =
  let log, fetch = scripted [ {|{"pos":"after"}|} ] in
  let store = Store.memory () in
  let observed = ref false in
  with_loop (fun ~env ~sw ->
      Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~sw ~env fetch)
        ~state_store:store
        ~callbacks:
          {
            on_response =
              (fun _ ->
                observed :=
                  Base.sliding_pos
                    (Base.of_store store ~user_id:test_session.user_id ())
                  = Some "after";
                Matrix_eio.Sync.Stop);
            on_error = unexpected_error;
          }
        (Request.v ()));
  Alcotest.(check bool) "state is persisted before callback" true !observed;
  Alcotest.(check bool)
    "state remains after loop" true
    (Base.sliding_pos (Base.of_store store ~user_id:test_session.user_id ())
    = Some "after");
  Alcotest.(check int) "one request" 1 (List.length (requests log))

let test_loop_publishes_own_profile_after_save () =
  let log, fetch =
    scripted
      [
        {|{"pos":"profile-pos","extensions":{"org.matrix.msc4262.profiles":{"users":{"@alice:example.org":{"updated":{"displayname":"Alice","avatar_url":"mxc://example.org/a"}}}}}}|};
      ]
  in
  let store = Store.memory () in
  let observer = Ss_eio.Own_profile.create ~user_id:test_session.user_id in
  let initial = ref true in
  let current_at_callback = ref None in
  let subscription = Ss_eio.Own_profile.subscribe observer (fun _ -> ()) in
  Ss_eio.Own_profile.unsubscribe observer subscription;
  with_loop (fun ~env ~sw ->
      Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~sw ~env fetch)
        ~state_store:store ~own_profile:observer
        ~callbacks:
          {
            on_response =
              (fun _ ->
                initial := false;
                current_at_callback := Ss_eio.Own_profile.current observer;
                Matrix_eio.Sync.Stop);
            on_error = unexpected_error;
          }
        (Request.v ()));
  Alcotest.(check bool) "callback ran" false !initial;
  (match !current_at_callback with
  | Some
      {
        Matrix_client.Profile.displayname = Some "Alice";
        avatar_url = Some _;
        _;
      } ->
      ()
  | Some _ -> Alcotest.fail "own profile was not typed"
  | None -> Alcotest.fail "own profile was not published");
  Alcotest.(check (option string))
    "profile response was persisted" (Some "profile-pos")
    (Base.sliding_pos (Base.of_store store ~user_id:test_session.user_id ()));
  Alcotest.(check int) "one request" 1 (List.length (requests log))

let test_loop_profile_bridge_preserves_unrelated_profiles () =
  let log, fetch =
    scripted
      [
        {|{"pos":"profile-pos","extensions":{"org.matrix.msc4262.profiles":{"users":{"@alice:example.org":{"updated":{"displayname":"Alice","m.custom":"one"}}}}}}|};
      ]
  in
  let common_store = Store.memory () in
  let alice = test_session.user_id in
  let bob = uid "@bob:example.org" in
  Store.replace_profiles common_store
    [ (bob, [ ("displayname", Jsont.Json.string "Bob") ]) ];
  let profile_service =
    Matrix_eio.Sync_service.of_store ~store:common_store ~user_id:alice ()
  in
  let callback_saw_persisted = ref false in
  with_loop (fun ~env ~sw ->
      Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~sw ~env fetch)
        ~state_store:common_store ~profile_service
        ~callbacks:
          {
            on_response =
              (fun _ ->
                callback_saw_persisted :=
                  List.exists
                    (fun (user_id, fields) ->
                      Id.User_id.equal user_id alice
                      && List.assoc_opt "displayname" fields
                         = Some (Jsont.Json.string "Alice"))
                    (Store.profiles common_store)
                  && List.exists
                       (fun (user_id, fields) ->
                         Id.User_id.equal user_id bob
                         && List.assoc_opt "displayname" fields
                            = Some (Jsont.Json.string "Bob"))
                       (Store.profiles common_store);
                Matrix_eio.Sync.Stop);
            on_error = unexpected_error;
          }
        (Request.v ()));
  Alcotest.(check bool)
    "profile snapshot is committed before response" true !callback_saw_persisted;
  Alcotest.(check bool)
    "Bob remains in the common snapshot" true
    (List.exists
       (fun (user_id, _) -> Id.User_id.equal user_id bob)
       (Store.profiles common_store));
  Alcotest.(check int) "one request" 1 (List.length (requests log))

let test_loop_profile_bridge_failure_rolls_back_without_callbacks () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let path = Filename.temp_file "matrix-profile-bridge-failure-" ".d" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  let dir = Eio.Path.(Eio.Stdenv.fs env / path) in
  let common_store = Store.on_disk ~dir in
  let alice = test_session.user_id in
  let bob = uid "@bob:example.org" in
  let previous_profiles =
    [ (bob, [ ("displayname", Jsont.Json.string "Bob") ]) ]
  in
  Store.replace_profiles common_store previous_profiles;
  (match Store.flush common_store with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "seed profile flush: %s" (Error.to_string error));
  let profile_service =
    Matrix_eio.Sync_service.of_store ~store:common_store ~user_id:alice ()
  in
  let state_store = common_store in
  let observer = Ss_eio.Own_profile.create ~user_id:alice in
  let publications = ref 0 in
  let subscription =
    Ss_eio.Own_profile.subscribe observer (fun _ -> incr publications)
  in
  (* [Store.flush] leaves its lock and base snapshot in the directory. Remove
       these known files so the parent directory can be taken away and the
       next profile flush has a deterministic I/O failure. *)
  Sys.remove (Filename.concat path ".profile.lock");
  Sys.remove (Filename.concat path "base_state.json");
  Unix.rmdir path;
  let _log, fetch =
    scripted
      [
        {|{"pos":"not-committed","extensions":{"org.matrix.msc4262.profiles":{"users":{"@alice:example.org":{"updated":{"displayname":"Alice"}}}}}}|};
      ]
  in
  let responses = ref 0 in
  let errors = ref 0 in
  let done_, done_r = Eio.Promise.create () in
  Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
    (eio_client ~sw ~env fetch)
    ~state_store ~profile_service ~own_profile:observer
    ~callbacks:
      {
        on_response =
          (fun _ ->
            incr responses;
            Matrix_eio.Sync.Stop);
        on_error =
          (fun _ ->
            incr errors;
            Eio.Promise.resolve done_r ();
            Matrix_eio.Sync.Stop);
      }
    (Request.v ());
  Eio.Promise.await done_;
  Eio.Switch.on_release sw (fun () ->
      Ss_eio.Own_profile.unsubscribe observer subscription);
  Alcotest.(check int) "profile persistence error is reported" 1 !errors;
  Alcotest.(check int) "failed profile is not published" 0 !responses;
  Alcotest.(check int) "only initial own-profile publication" 1 !publications;
  Alcotest.(check bool)
    "common profile snapshot is rolled back" true
    (Store.profiles common_store = previous_profiles);
  Alcotest.(check bool)
    "sliding state was not accepted" true
    (Store.Slot.find state_store
       (Store.Slot.v ~name:"sliding_sync_state" Matrix_proto.Json.Codec.json)
    = Ok None)

let test_loop_expired_position_clears_profile_after_save () =
  let log, fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:400
          {|{"errcode":"M_UNKNOWN_POS","error":"unknown pos"}|} req)
  in
  let store = Store.memory () in
  let alice = test_session.user_id in
  let observer = Ss_eio.Own_profile.create ~user_id:alice in
  let response =
    ok
      (Client.Http.decode_response Response.jsont
         {|{"pos":"old","extensions":{"org.matrix.msc4262.profiles":{"users":{"@alice:example.org":{"updated":{"displayname":"Alice"}}}}}}|})
  in
  let state, _ = Base.apply_sliding (Base.create ~user_id:alice ()) response in
  Base.persist store state;
  (* The sliding slot is independent from the common profile snapshot. Seed
     that snapshot explicitly so expiry exercises the profile bridge. *)
  Store.replace_profiles store
    [ (alice, [ ("displayname", Jsont.Json.string "Alice") ]) ];
  let profile_service =
    Matrix_eio.Sync_service.of_store ~store ~user_id:alice ()
  in
  let seen = ref [] in
  let subscription =
    Ss_eio.Own_profile.subscribe observer (fun profile ->
        seen := profile :: !seen)
  in
  with_loop (fun ~env ~sw ->
      Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~sw ~env fetch)
        ~state_store:store ~profile_service ~own_profile:observer
        ~callbacks:
          {
            on_response = (fun _ -> Alcotest.fail "unexpected response");
            on_error =
              (fun error ->
                Alcotest.(check bool)
                  "expired" true
                  (Ss_eio.is_expired_pos error);
                Matrix_eio.Sync.Stop);
          }
        (Request.v ()));
  Alcotest.(check bool)
    "expired state is empty" true
    (Base.sliding_pos (Base.of_store store ~user_id:alice ()) = None);
  Alcotest.(check bool)
    "expired profile remains in common state" true
    (Option.is_some (Ss_eio.Own_profile.current observer));
  Alcotest.(check bool)
    "expired profile remains in common snapshot" true
    (Store.profiles store <> []);
  Alcotest.(check int)
    "initial and durable profile publications" 2 (List.length !seen);
  Ss_eio.Own_profile.unsubscribe observer subscription;
  Alcotest.(check int) "one request" 1 (List.length (requests log))

let test_loop_failed_state_save_retries_profile_idempotently () =
  with_loop (fun ~env ~sw ->
      let path = Filename.temp_file "matrix-sliding-profile-save-" ".d" in
      Sys.remove path;
      Unix.mkdir path 0o700;
      let dir = Eio.Path.(Eio.Stdenv.fs env / path) in
      let store = Store.on_disk ~dir in
      let alice = uid "@alice:example.org" in
      let common_store = store in
      let profile_service =
        Matrix_eio.Sync_service.of_store ~store:common_store ~user_id:alice ()
      in
      let profile_changes = ref 0 in
      Matrix_eio.Sync_service.on_profile_change profile_service (fun _ _ ->
          incr profile_changes);
      (* Make the persistence boundary fail after the request has been
         accepted. The in-memory store remains usable, but its directory is
         gone, so the canonical service cannot flush the new snapshot. *)
      Unix.rmdir path;
      let _log, fetch =
        scripted
          [
            {|{"pos":"not-saved","extensions":{"org.matrix.msc4262.profiles":{"users":{"@alice:example.org":{"updated":{"displayname":"Alice"}}}}}}|};
            {|{"pos":"saved-after-retry","extensions":{"org.matrix.msc4262.profiles":{"users":{"@alice:example.org":{"updated":{"displayname":"Alice"}}}}}}|};
          ]
      in
      let observer = Ss_eio.Own_profile.create ~user_id:test_session.user_id in
      let publications = ref 0 in
      let subscription =
        Ss_eio.Own_profile.subscribe observer (fun _ -> incr publications)
      in
      let responses = ref 0 in
      let errors = ref 0 in
      let done_, done_r = Eio.Promise.create () in
      Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~sw ~env fetch)
        ~state_store:store ~profile_service ~own_profile:observer
        ~callbacks:
          {
            on_response =
              (fun _ ->
                incr responses;
                Eio.Promise.resolve done_r ();
                Matrix_eio.Sync.Stop);
            on_error =
              (fun _ ->
                incr errors;
                Unix.mkdir path 0o700;
                Matrix_eio.Sync.Continue);
          }
        (Request.v ());
      Eio.Switch.on_release sw (fun () ->
          Ss_eio.Own_profile.unsubscribe observer subscription);
      Eio.Promise.await done_;
      Alcotest.(check int) "one save error" 1 !errors;
      Alcotest.(check int) "one response callback after retry" 1 !responses;
      Alcotest.(check int)
        "profile change is not republished on retry" 1 !profile_changes;
      Alcotest.(check int) "initial None and durable profile" 2 !publications;
      Alcotest.(check bool)
        "own profile publishes only after the retry saves" true
        (Option.is_some (Ss_eio.Own_profile.current observer));
      Alcotest.(check bool)
        "canonical profile snapshot commits with retry" true
        (Store.profiles common_store
        = [ (alice, [ ("displayname", Jsont.Json.string "Alice") ]) ]);
      Alcotest.(check (option string))
        "retry accepts the second position" (Some "saved-after-retry")
        (Base.sliding_pos (Base.of_store store ~user_id:alice ())))

let test_loop_uses_stored_position_and_since () =
  let log, fetch = scripted [ {|{"pos":"next"}|} ] in
  let store = Store.memory () in
  let stored, _ =
    Base.apply_sliding
      (Base.create ~user_id:test_session.user_id ())
      (decode_fixture ())
  in
  Base.persist store stored;
  with_loop (fun ~env ~sw ->
      Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~sw ~env fetch)
        ~initial_pos:"caller-pos" ~state_store:store
        ~callbacks:
          {
            on_response = (fun _ -> Matrix_eio.Sync.Stop);
            on_error = unexpected_error;
          }
        (Request.v () |> Request.enable_to_device));
  let request = one_request log in
  Alcotest.(check bool)
    "stored position wins" true
    (contains ~sub:"pos=s58_224_0_13_10_1_1_16_0_1" request.url);
  let body =
    ok (Client.Http.decode_response Request.jsont (sent_body request))
  in
  Alcotest.(check (option string))
    "stored since seeds request" (Some "td-42") body.extensions.to_device.since

let test_loop_persisted_empty_clears_caller_cursors () =
  let log, fetch = scripted [ {|{"pos":"fresh"}|} ] in
  let store = Store.memory () in
  let legacy_slot =
    Store.Slot.v ~name:"sliding_sync_state" Matrix_proto.Json.Codec.json
  in
  ignore
    (ok
       (Store.Slot.set store legacy_slot
          (Jsont.Json.object'
             [
               Jsont.Json.mem
                 (Jsont.Json.name "format_version")
                 (Jsont.Json.int 1);
             ])));
  with_loop (fun ~env ~sw ->
      Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~sw ~env fetch)
        ~initial_pos:"stale-pos" ~state_store:store
        ~callbacks:
          {
            on_response = (fun _ -> Matrix_eio.Sync.Stop);
            on_error = unexpected_error;
          }
        (Request.v () |> Request.enable_to_device ~since:"stale-since"));
  let request = one_request log in
  Alcotest.(check bool)
    "persisted empty state clears initial pos" false
    (contains ~sub:"pos=" request.url);
  let body =
    ok (Client.Http.decode_response Request.jsont (sent_body request))
  in
  Alcotest.(check (option string))
    "persisted empty state clears initial since" None
    body.extensions.to_device.since

let test_loop_ignores_unsolicited_to_device_cursor () =
  let _log, fetch =
    scripted
      [
        {|{"pos":"after","extensions":{"to_device":{"next_batch":"unsolicited"}}}|};
      ]
  in
  let store = Store.memory () in
  with_loop (fun ~env ~sw ->
      Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~sw ~env fetch)
        ~state_store:store
        ~callbacks:
          {
            on_response = (fun _ -> Matrix_eio.Sync.Stop);
            on_error = unexpected_error;
          }
        (Request.v ()));
  Alcotest.(check (option string))
    "disabled extension cannot advance stored since" None
    (Base.sliding_to_device_since
       (Base.of_store store ~user_id:test_session.user_id ()))

let test_loop_reports_malformed_persisted_state_before_io () =
  let log, fetch = scripted [] in
  let store = Store.memory () in
  let raw_slot =
    Store.Slot.v ~name:"sliding_sync_state" Matrix_proto.Json.Codec.json
  in
  ignore (ok (Store.Slot.set store raw_slot (Jsont.Json.string "bad")));
  let observed = ref false in
  with_loop (fun ~env ~sw ->
      Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~sw ~env fetch)
        ~state_store:store
        ~callbacks:
          {
            on_response = (fun _ -> Alcotest.fail "unexpected response");
            on_error =
              (fun _ ->
                observed := true;
                Matrix_eio.Sync.Stop);
          }
        (Request.v ()));
  Alcotest.(check bool) "load error reaches on_error" true !observed;
  Alcotest.(check int)
    "load error stops before HTTP" 0
    (List.length (requests log))

let test_loop_sends_txn_id_when_asked () =
  let log, fetch = scripted [ {|{"pos":"p1"}|} ] in
  with_loop (fun ~env ~sw ->
      Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~sw ~env fetch)
        ~txn_id:true
        ~callbacks:
          {
            on_response = (fun _ -> Matrix_eio.Sync.Stop);
            on_error = unexpected_error;
          }
        (Request.v ()));
  let sent =
    ok (Client.Http.decode_response Request.jsont (sent_body (one_request log)))
  in
  match sent.txn_id with
  | Some id when String.length id > 1 && id.[0] = 'm' -> ()
  | Some id -> Alcotest.failf "unexpected txn_id %S" id
  | None -> Alcotest.fail "expected a txn_id"

let test_loop_stops_on_error () =
  let _, fetch = mock (fun req -> Fetch_mock.respond ~status:500 "boom" req) in
  let errors = ref 0 in
  with_loop (fun ~env ~sw ->
      Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~sw ~env fetch)
        ~callbacks:
          {
            on_response = (fun _ -> Alcotest.fail "unexpected response");
            on_error =
              (fun _ ->
                incr errors;
                Matrix_eio.Sync.Stop);
          }
        (Request.v ()));
  Alcotest.(check int) "one error" 1 !errors

let test_loop_resets_pos_on_expired_session () =
  (* [M_UNKNOWN_POS] means the server forgot the session: the next request
     must start a fresh one and stop resending subscriptions. *)
  (* The middle reply is a 4xx so that [Client] reads it as an error. *)
  let log, fetch =
    let n = ref 0 in
    mock (fun req ->
        incr n;
        match !n with
        | 1 -> Fetch_mock.respond {|{"pos":"p1"}|} req
        | 2 ->
            Fetch_mock.respond ~status:400
              {|{"errcode":"M_UNKNOWN_POS","error":"unknown pos"}|} req
        | _ -> Fetch_mock.respond {|{"pos":"p9"}|} req)
  in
  let responses = ref 0 in
  let request =
    Request.v () |> Request.subscribe_room ~room_id:(rid "!r:example.org")
  in
  with_loop (fun ~env ~sw ->
      Ss_eio.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~sw ~env fetch)
        ~callbacks:
          {
            on_response =
              (fun _ ->
                incr responses;
                if !responses >= 2 then Matrix_eio.Sync.Stop
                else Matrix_eio.Sync.Continue);
            on_error =
              (fun e ->
                Alcotest.(check bool) "expired" true (Ss_eio.is_expired_pos e);
                Matrix_eio.Sync.Continue);
          }
        request);
  match requests log with
  | [ first; second; third ] ->
      Alcotest.(check bool)
        "second replays pos=p1" true
        (contains ~sub:"pos=p1" second.url);
      Alcotest.(check bool)
        "third starts a new session" false
        (contains ~sub:"pos=" third.url);
      let decode b = ok (Client.Http.decode_response Request.jsont b) in
      Alcotest.(check int)
        "subscription sent at first" 1
        (List.length (decode (sent_body first)).room_subscriptions);
      Alcotest.(check int)
        "subscriptions cleared after expiry" 0
        (List.length (decode (sent_body third)).room_subscriptions)
  | rs -> Alcotest.failf "expected three requests, got %d" (List.length rs)

let test_404_is_unsupported () =
  let _, fetch = mock (fun req -> Fetch_mock.respond ~status:404 "" req) in
  let t = client_of fetch in
  let e = err (Ss.sync_once t (Request.v ())) in
  Alcotest.(check bool) "recognised" true (Ss.is_unsupported e);
  match e with
  | Error.Matrix_error m ->
      check_string "errcode" "M_UNRECOGNIZED"
        (Error.errcode_to_string m.errcode);
      Alcotest.(check bool)
        "names the endpoint" true
        (contains ~sub:Ss.path m.error)
  | e -> Alcotest.failf "expected a Matrix error, got %s" (Error.to_string e)

let test_m_unrecognized_is_unsupported () =
  (* Synapse answers an unknown unstable endpoint with a Matrix error rather
     than a bare 404; both must land on the same value. *)
  let _, fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:404
          {|{"errcode":"M_UNRECOGNIZED","error":"Unrecognized request"}|} req)
  in
  let t = client_of fetch in
  let e = err (Ss.sync_once t (Request.v ())) in
  Alcotest.(check bool) "recognised" true (Ss.is_unsupported e)

let test_other_errors_are_left_alone () =
  let _, fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:403
          {|{"errcode":"M_FORBIDDEN","error":"nope"}|} req)
  in
  let t = client_of fetch in
  let e = err (Ss.sync_once t (Request.v ())) in
  Alcotest.(check bool)
    "not mistaken for unsupported" false (Ss.is_unsupported e)

let () =
  let case name f = Alcotest.test_case name `Quick f in
  Alcotest.run "sliding_sync"
    [
      ( "request",
        [
          case "empty request is {}" (run test_empty_request_is_empty_object);
          case "field names match ruma v5" (run test_full_request_matches_ruma);
          case "codec round-trips" (run test_request_roundtrips);
          case "builder replaces in place" (run test_builder_replaces_in_place);
          case "profiles extension" (run test_request_profiles_extension);
          case "unknown extension round-trips"
            (run test_request_unknown_extension_roundtrip);
          case "unknown extension keeps outer object"
            (run test_request_unknown_extension_keeps_outer_object);
          case "known and unknown extensions coexist"
            (run test_request_known_and_unknown_extensions_coexist);
          case "thread subscriptions extension"
            (run test_request_thread_subscriptions_extension);
        ] );
      ( "endpoint",
        [
          case "native capability discovery" (run test_native_availability);
          case "capability discovery errors"
            (run test_native_availability_error);
          case "path and query" (run test_path_and_query);
          case "no pos starts a session" (run test_query_without_pos);
        ] );
      ( "response",
        [
          case "top level" (run test_response_top_level);
          case "room" (run test_response_room);
          case "avatar is tri-state" (run test_response_avatar_tri_state);
          case "invite state" (run test_response_invite_state);
          case "extensions" (run test_response_extensions);
          case "unknown extension round-trips"
            (run test_response_unknown_extension_roundtrip);
          case "known and unknown extensions coexist"
            (run test_response_known_and_unknown_extensions_coexist);
          case "only pos is required" (run test_response_minimal);
          case "rejects a bad room id" (run test_response_rejects_bad_room_id);
          case "profiles" (run test_response_profiles);
          case "profiles reject malformed values"
            (run test_response_profiles_rejects_malformed);
          case "thread subscriptions" (run test_response_thread_subscriptions);
          case "thread subscriptions apply"
            (run test_apply_thread_subscriptions_extension);
          case "thread subscriptions reject malformed values"
            (run test_response_thread_subscriptions_rejects_malformed);
        ] );
      ( "profiles",
        [
          case "own profile projection updates and deduplicates"
            (run test_own_profile_projection_updates_and_deduplicates);
          case "profile service updates are durable and deduplicated"
            (run test_profile_service_updates_are_durable_and_deduplicated);
        ] );
      ( "loop",
        [
          case "controller subscription mutations"
            (run test_controller_subscription_mutations);
          case "subscription changes cancel long polls"
            test_controller_cancels_changed_long_polls;
          case "subscription cancellation can be deferred"
            test_controller_can_defer_cancellation;
          case "default presence changes cancel long polls"
            test_default_presence_change_cancels_long_poll;
          case "explicit presence ignores client changes"
            test_explicit_presence_ignores_client_changes;
          case "default presence wakes retry sleep"
            test_default_presence_change_wakes_retry_sleep;
          case "subscription changes wake retry sleep, including empty"
            test_subscription_changes_wake_retry_sleep_including_empty;
          case "retry sleep honours no-op and opt-out"
            test_retry_sleep_honours_noop_and_opt_out;
          case "parent cancellation interrupts retry sleep"
            test_parent_cancellation_interrupts_retry_sleep;
          case "threads pos and to-device since"
            test_loop_threads_pos_and_to_device_since;
          case "persists thread subscriptions before callback"
            test_loop_persists_thread_subscriptions_before_callback;
          case "persists sliding state before callback"
            test_loop_persists_sliding_state_before_callback;
          case "publishes own profile after save"
            test_loop_publishes_own_profile_after_save;
          case "profile bridge preserves unrelated profiles"
            test_loop_profile_bridge_preserves_unrelated_profiles;
          case "profile bridge rolls back failed persistence"
            test_loop_profile_bridge_failure_rolls_back_without_callbacks;
          case "expired position clears own profile after save"
            test_loop_expired_position_clears_profile_after_save;
          case "state-save retry applies profiles idempotently"
            test_loop_failed_state_save_retries_profile_idempotently;
          case "uses stored position and since"
            test_loop_uses_stored_position_and_since;
          case "persisted empty state clears caller cursors"
            test_loop_persisted_empty_clears_caller_cursors;
          case "ignores unsolicited to-device cursor"
            test_loop_ignores_unsolicited_to_device_cursor;
          case "reports malformed persisted state before I/O"
            test_loop_reports_malformed_persisted_state_before_io;
          case "sends a txn_id when asked" test_loop_sends_txn_id_when_asked;
          case "stops when on_error says so" test_loop_stops_on_error;
          case "resets pos on an expired session"
            test_loop_resets_pos_on_expired_session;
        ] );
      ( "unsupported homeserver",
        [
          case "404" (run test_404_is_unsupported);
          case "M_UNRECOGNIZED" (run test_m_unrecognized_is_unsupported);
          case "other errors are left alone"
            (run test_other_errors_are_left_alone);
        ] );
    ]
