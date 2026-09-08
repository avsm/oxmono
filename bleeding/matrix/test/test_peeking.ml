(** Hermetic tests for the historical initial-sync and event-stream APIs. *)

module Client = Matrix_client.Client
module Peeking = Matrix_client.Peeking
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Error = Matrix_client.Error

let mock_env =
  object
    method secure_random =
      Eio.Flow.string_source (String.init 4096 (fun i -> Char.chr (i land 255)))
  end

type recorded = { meth : string; url : string; body : string option }

let body_of_request (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> None
  | Fetch.String s -> Some s
  | Fetch.Stream _ -> Some "<stream>"

let mock handler =
  let log = ref [] in
  let fetch =
    Fetch_mock.client (fun req ->
        log :=
          {
            meth = Http.Method.to_string req.meth;
            url = Fetch.Middleware.Url.to_string req.url;
            body = body_of_request req;
          }
          :: !log;
        handler req)
  in
  (log, fetch)

let client_of fetch =
  let config =
    Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ()
  in
  Client.with_session
    (Client.create ~config ~fetch
       ~random:(Matrix_client.Random.of_env mock_env))
    {
      user_id = Result.get_ok (Id.User_id.of_string "@alice:example.org");
      access_token = "syt_secret_token";
      device_id = Result.get_ok (Id.Device_id.of_string "TESTDEVICE");
      refresh_token = None;
    }

let rid s = Result.get_ok (Id.Room_id.of_string s)
let run f () = Eio_mock.Backend.run f

let raw_event =
  {|{"event_id":"$event:example.org","room_id":"!r:example.org","sender":"@bob:example.org","origin_server_ts":1,"type":"m.room.message","content":{"body":"hi"}}|}

let response_events =
  "{\"chunk\":[" ^ raw_event ^ "],\"start\":\"s\",\"end\":\"e\"}"

let ok = function
  | Ok value -> value
  | Error error ->
      Alcotest.failf "expected success, got %s" (Error.to_string error)

let encoded label codec value =
  match Jsont.Json.encode codec value with
  | Ok json -> json
  | Error error -> Alcotest.failf "%s: %s" label error

let has_member name = function
  | Jsont.Object (members, _) ->
      Option.is_some (Jsont.Json.find_mem name members)
  | _ -> Alcotest.fail "encoded response was not an object"

let request log =
  match List.rev !log with
  | [ request ] -> request
  | requests ->
      Alcotest.failf "expected one request, got %d" (List.length requests)

let check_request log ~url =
  let request = request log in
  Alcotest.(check string) "method" "GET" request.meth;
  Alcotest.(check string) "url" url request.url;
  Alcotest.(check (option string)) "no request body" None request.body

let test_initial_sync () =
  let body =
    "{\"room_id\":\"!r:example.org\",\"membership\":\"join\",\"visibility\":\"public\",\"account_data\":[{\"type\":\"m.tag\",\"content\":{\"tags\":{}}}],\"messages\":{\"chunk\":["
    ^ raw_event ^ "],\"start\":\"a\",\"end\":\"b\"},\"state\":[" ^ raw_event
    ^ "]}"
  in
  let log, fetch = mock (Fetch_mock.respond body) in
  let response =
    ok (Peeking.initial_sync (client_of fetch) ~room_id:(rid "!r:example.org"))
  in
  check_request log
    ~url:"https://hs.example/_matrix/client/v3/rooms/!r:example.org/initialSync";
  Alcotest.(check string)
    "room id" "!r:example.org"
    (Id.Room_id.to_string response.room_id);
  Alcotest.(check bool)
    "join membership" true
    (response.membership = Some Peeking.Join);
  Alcotest.(check bool)
    "public visibility" true
    (response.visibility = Some Peeking.Public);
  Alcotest.(check int) "account data" 1 (List.length response.account_data);
  Alcotest.(check int)
    "messages" 1
    (List.length (Option.get response.messages).chunk);
  Alcotest.(check int) "state" 1 (List.length response.state)

let test_initial_sync_optional () =
  let log, fetch =
    mock
      (Fetch_mock.respond
         {|{"room_id":"!r:example.org","messages":{"chunk":[],"end":"e"}}|})
  in
  let response =
    ok (Peeking.initial_sync (client_of fetch) ~room_id:(rid "!r:example.org"))
  in
  check_request log
    ~url:"https://hs.example/_matrix/client/v3/rooms/!r:example.org/initialSync";
  Alcotest.(check (option string))
    "membership absent" None
    (Option.map
       (function
         | Peeking.Invite -> "invite"
         | Join -> "join"
         | Leave -> "leave"
         | Ban -> "ban"
         | Knock -> "knock")
       response.membership);
  Alcotest.(check (option string))
    "visibility absent" None
    (Option.map
       (function Peeking.Private -> "private" | Public -> "public")
       response.visibility);
  Alcotest.(check int)
    "account data absent defaults empty" 0
    (List.length response.account_data);
  Alcotest.(check int)
    "state absent defaults empty" 0
    (List.length response.state);
  let messages = Option.get response.messages in
  Alcotest.(check int) "empty messages" 0 (List.length messages.chunk);
  Alcotest.(check (option string)) "message start absent" None messages.start;
  let encoded =
    encoded "encode optional initialSync response"
      Peeking.initial_sync_response_jsont response
  in
  Alcotest.(check bool)
    "empty account data omitted" false
    (has_member "account_data" encoded);
  Alcotest.(check bool) "empty state omitted" false (has_member "state" encoded);
  let _log, fetch =
    mock
      (Fetch_mock.respond {|{"room_id":"!r:example.org","membership":"knock"}|})
  in
  let response =
    ok (Peeking.initial_sync (client_of fetch) ~room_id:(rid "!r:example.org"))
  in
  Alcotest.(check bool)
    "knock membership" true
    (response.membership = Some Peeking.Knock)

let test_initial_sync_escapes_room_id () =
  let log, fetch =
    mock
      (Fetch_mock.respond
         {|{"room_id":"!r/name:example.org","membership":"leave","visibility":"private"}|})
  in
  ignore
    (ok
       (Peeking.initial_sync (client_of fetch)
          ~room_id:(rid "!r/name:example.org")));
  check_request log
    ~url:
      "https://hs.example/_matrix/client/v3/rooms/!r%2Fname:example.org/initialSync"

let test_events_queries_and_escaping () =
  let log, fetch = mock (Fetch_mock.respond response_events) in
  ignore
    (ok (Peeking.events (client_of fetch) ~from:"a/b?c&d=e#f" ~timeout:250 ()));
  check_request log
    ~url:
      "https://hs.example/_matrix/client/v3/events?from=a/b?c%26d%3De%23f&timeout=250";
  let log, fetch = mock (Fetch_mock.respond response_events) in
  ignore
    (ok
       (Peeking.peek_events (client_of fetch)
          ~room_id:(rid "!r/name:example.org")
          ~from:"s t" ~timeout:0 ()));
  check_request log
    ~url:
      "https://hs.example/_matrix/client/v3/events?room_id=!r/name:example.org&from=s%20t&timeout=0"

let test_events_no_query () =
  let log, fetch = mock (Fetch_mock.respond response_events) in
  let response = ok (Peeking.events (client_of fetch) ()) in
  check_request log ~url:"https://hs.example/_matrix/client/v3/events";
  Alcotest.(check (option string)) "start" (Some "s") response.start;
  Alcotest.(check (option string)) "end" (Some "e") response.end_

let test_events_defaults () =
  let log, fetch = mock (Fetch_mock.respond {|{}|}) in
  let response = ok (Peeking.events (client_of fetch) ()) in
  check_request log ~url:"https://hs.example/_matrix/client/v3/events";
  Alcotest.(check int) "chunk defaults empty" 0 (List.length response.chunk);
  Alcotest.(check (option string)) "start absent" None response.start;
  Alcotest.(check (option string)) "end absent" None response.end_;
  let encoded =
    encoded "encode empty events response" Peeking.events_response_jsont
      response
  in
  Alcotest.(check bool) "empty chunk omitted" false (has_member "chunk" encoded)

let test_events_presence_chunk () =
  let log, fetch =
    mock
      (Fetch_mock.respond
         {|{"chunk":[{"type":"m.presence","content":{"user_id":"@bob:example.org"}}]}|})
  in
  let response = ok (Peeking.events (client_of fetch) ()) in
  check_request log ~url:"https://hs.example/_matrix/client/v3/events";
  Alcotest.(check int) "presence chunk retained" 1 (List.length response.chunk)

let test_rejects_bad_response () =
  let log, fetch =
    mock
      (Fetch_mock.respond
         {|{"room_id":"!r:example.org","membership":"unknown","visibility":"public"}|})
  in
  (match
     Peeking.initial_sync (client_of fetch) ~room_id:(rid "!r:example.org")
   with
  | Ok _ -> Alcotest.fail "invalid membership was accepted"
  | Error _ -> ());
  let _log, fetch =
    mock
      (Fetch_mock.respond
         {|{"room_id":"!r:example.org","messages":{"chunk":[{"event_id":"$event:example.org","sender":"@bob:example.org","origin_server_ts":1,"type":"m.room.message","content":{}}],"end":"e"}}|})
  in
  (match
     Peeking.initial_sync (client_of fetch) ~room_id:(rid "!r:example.org")
   with
  | Ok _ -> Alcotest.fail "message event without room_id was accepted"
  | Error _ -> ());
  let _log, fetch =
    mock
      (Fetch_mock.respond
         {|{"room_id":"!r:example.org","messages":{"chunk":[]}}|})
  in
  match
    Peeking.initial_sync (client_of fetch) ~room_id:(rid "!r:example.org")
  with
  | Ok _ -> Alcotest.fail "missing message end was accepted"
  | Error _ -> ()

let test_error_and_timeout_validation () =
  let log, fetch =
    mock
      (Fetch_mock.respond ~status:403
         {|{"errcode":"M_FORBIDDEN","error":"not visible"}|})
  in
  (match Peeking.events (client_of fetch) () with
  | Error (Error.Matrix_error { errcode = Error.M_FORBIDDEN; _ }) -> ()
  | Error error ->
      Alcotest.failf "wrong Matrix error: %s" (Error.to_string error)
  | Ok _ -> Alcotest.fail "403 response was accepted");
  ignore (request log);
  let log, fetch = mock (Fetch_mock.respond response_events) in
  (match
     Peeking.peek_events (client_of fetch) ~room_id:(rid "!r:example.org")
       ~timeout:(-1) ()
   with
  | exception Invalid_argument _ -> ()
  | Error error ->
      Alcotest.failf "negative timeout returned %s" (Error.to_string error)
  | Ok _ -> Alcotest.fail "negative timeout was accepted");
  Alcotest.(check int) "negative timeout made no request" 0 (List.length !log)

let () =
  Alcotest.run "matrix peeking"
    [
      ( "initial_sync",
        [
          Alcotest.test_case "typed response" `Quick (run test_initial_sync);
          Alcotest.test_case "optional members" `Quick
            (run test_initial_sync_optional);
          Alcotest.test_case "room id escaping" `Quick
            (run test_initial_sync_escapes_room_id);
          Alcotest.test_case "invalid response" `Quick
            (run test_rejects_bad_response);
        ] );
      ( "events",
        [
          Alcotest.test_case "queries and escaping" `Quick
            (run test_events_queries_and_escaping);
          Alcotest.test_case "without query" `Quick (run test_events_no_query);
          Alcotest.test_case "optional tokens and default chunk" `Quick
            (run test_events_defaults);
          Alcotest.test_case "presence chunk is raw" `Quick
            (run test_events_presence_chunk);
          Alcotest.test_case "error and timeout validation" `Quick
            (run test_error_and_timeout_validation);
        ] );
    ]
