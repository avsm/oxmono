module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Client = Matrix_client.Client
module Rooms = Matrix_client.Rooms
module Ui = Matrix_ui

let user = Id.User_id.of_string_exn "@alice:example.org"
let room = Id.Room_id.of_string_exn "!room:example.org"
let beacon_id = Id.Event_id.of_string_exn "$beacon-info:example.org"
let state_id = Id.Event_id.of_string_exn "$state:example.org"

let raw value =
  match Jsont_bytesrw.decode_string Event.Raw_event.jsont value with
  | Ok event -> event
  | Error error -> Alcotest.fail error

let json value =
  match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json value with
  | Ok json -> json
  | Error error -> Alcotest.fail error

let session =
  {
    Client.user_id = user;
    access_token = "token";
    device_id = Id.Device_id.of_string_exn "DEVICE";
    refresh_token = None;
  }

let mock_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 4096 'r')
  end

type request = { meth : string; url : string; body : string option }

let client handler log =
  let fetch =
    Fetch_mock.client (fun req ->
        let body =
          match req.Fetch.Middleware.body with
          | Fetch.String value -> Some value
          | Fetch.Empty -> None
          | Fetch.Stream _ -> Some "<stream>"
        in
        log :=
          {
            meth = Http.Method.to_string req.meth;
            url = Fetch.Middleware.Url.to_string req.url;
            body;
          }
          :: !log;
        handler req)
  in
  Client.create
    ~config:
      (Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ())
    ~fetch
    ~random:(Matrix_client.Random.of_env mock_env)
  |> fun client -> Client.with_session client session

let response body request = Fetch_mock.respond body request
let run f () = Eio_mock.Backend.run f

let room_state ?(live = true) ?(timestamp = 1000L) ?(timeout = 10_000L) () =
  Printf.sprintf
    {|{"event_id":"%s","sender":"@alice:example.org","origin_server_ts":1000,"type":"org.matrix.msc3672.beacon_info","state_key":"@alice:example.org","content":{"description":"desc","live":%s,"timeout":%Ld,"org.matrix.msc3488.ts":%Ld,"org.matrix.msc3488.asset":{"type":"m.self"}}}|}
    (Id.Event_id.to_string beacon_id)
    (if live then "true" else "false")
    timeout timestamp

let test_start_exact () =
  let log = ref [] in
  let client =
    client
      (fun request -> response {|{"event_id":"$new:example.org"}|} request)
      log
  in
  let result =
    Rooms.start_live_location_share client ~room_id:room ~duration_millis:3000L
      ~description:"desc"
      ~timestamp:(Event.Timestamp.of_ms 1234L)
      ()
  in
  Alcotest.(check bool) "start succeeded" true (Result.is_ok result);
  match List.rev !log with
  | [ request ] ->
      Alcotest.(check string) "method" "PUT" request.meth;
      Alcotest.(check string)
        "url"
        "https://hs.example/_matrix/client/v3/rooms/!room:example.org/state/org.matrix.msc3672.beacon_info/@alice:example.org"
        request.url;
      Alcotest.(check (option string))
        "body"
        (Some
           {|{"description":"desc","live":true,"timeout":3000,"org.matrix.msc3488.ts":1234,"org.matrix.msc3488.asset":{"type":"m.self"}}|})
        request.body
  | requests ->
      Alcotest.failf "expected one start request, got %d" (List.length requests)

let test_start_rejects_invalid_duration () =
  let log = ref [] in
  let client =
    client
      (fun request -> response {|{"event_id":"$new:example.org"}|} request)
      log
  in
  (match
     Rooms.start_live_location_share client ~room_id:room ~duration_millis:0L ()
   with
  | Error (Matrix_client.Error.Json_error _) -> ()
  | Error error ->
      Alcotest.failf "wrong duration error: %s"
        (Matrix_client.Error.to_string error)
  | Ok _ -> Alcotest.fail "zero-duration share was started");
  Alcotest.(check int) "no request" 0 (List.length !log)

let test_stop_preserves_and_send_reference () =
  let log = ref [] in
  let client =
    client
      (fun request ->
        let path = Fetch.Middleware.Url.to_string request.url in
        if String.ends_with ~suffix:"/state" path then
          response ("[" ^ room_state () ^ "]") request
        else response {|{"event_id":"$sent:example.org"}|} request)
      log
  in
  ignore (Rooms.stop_live_location_share client ~room_id:room ());
  let stopped_body =
    match List.rev !log with
    | [ _; request ] -> request.body
    | requests ->
        Alcotest.failf "expected state lookup and stop, got %d requests"
          (List.length requests)
  in
  Alcotest.(check (option string))
    "stop preserves fields"
    (Some
       {|{"description":"desc","live":false,"timeout":10000,"org.matrix.msc3488.ts":1000,"org.matrix.msc3488.asset":{"type":"m.self"}}|})
    stopped_body;
  log := [];
  ignore
    (Rooms.send_location_beacon client ~room_id:room ~geo_uri:"geo:1,2"
       ~timestamp:(Event.Timestamp.of_ms 2000L)
       ~now:(fun () -> Event.Timestamp.of_ms 3000L)
       ());
  match List.rev !log with
  | [ _; request ] ->
      Alcotest.(check (option string))
        "beacon body"
        (Some
           {|{"org.matrix.msc3488.location":{"uri":"geo:1,2"},"org.matrix.msc3488.ts":2000,"m.relates_to":{"rel_type":"m.reference","event_id":"$beacon-info:example.org"}}|})
        request.body
  | requests ->
      Alcotest.failf "expected state lookup and beacon, got %d requests"
        (List.length requests)

let test_stop_missing_stopped_redacted_and_expired () =
  let check_stop_error state =
    let log = ref [] in
    let client =
      client (fun request -> response ("[" ^ state ^ "]") request) log
    in
    match Rooms.stop_live_location_share client ~room_id:room () with
    | Error (Matrix_client.Error.Json_error message) ->
        Alcotest.(check bool)
          "non-empty local error" true
          (String.length message > 0)
    | Error error ->
        Alcotest.failf "wrong stop error: %s"
          (Matrix_client.Error.to_string error)
    | Ok _ -> Alcotest.fail "invalid share was stopped"
  in
  check_stop_error "";
  check_stop_error (room_state ~live:false ());
  check_stop_error
    {|{"event_id":"$state:example.org","sender":"@alice:example.org","origin_server_ts":1000,"type":"org.matrix.msc3672.beacon_info","state_key":"@alice:example.org","content":{},"unsigned":{"redacted_because":{}}}|};
  let log = ref [] in
  let client =
    client
      (fun request ->
        response
          ("[" ^ room_state ~timestamp:1000L ~timeout:1L () ^ "]")
          request)
      log
  in
  (match
     Rooms.send_location_beacon client ~room_id:room ~geo_uri:"geo:1,2"
       ~now:(fun () -> Event.Timestamp.of_ms 5000L)
       ()
   with
  | Error (Matrix_client.Error.Json_error _) -> ()
  | Error error ->
      Alcotest.failf "wrong expiry error: %s"
        (Matrix_client.Error.to_string error)
  | Ok _ -> Alcotest.fail "expired share sent a beacon");
  Alcotest.(check int) "only state lookup" 1 (List.length !log)

let state_with_info ~event_id ~origin content =
  let store = Matrix_client.Store.memory () in
  let info =
    Matrix_client.Store.empty_room_info ~room_id:room ~membership:Joined
  in
  let state_event =
    {
      Matrix_client.Store.event_type = Event.Event_type.Beacon_info;
      state_key = Id.User_id.to_string user;
      content;
      sender = Some user;
      event_id = Some event_id;
      origin_server_ts = Some (Event.Timestamp.of_ms origin);
    }
  in
  Matrix_client.Store.set_room store
    { info with state_events = [ state_event ] };
  Matrix_client.Base_client.of_store store ~user_id:user ()

let room_change state timeline =
  {
    Matrix_client.Base_client.changed_room_id = room;
    info = Option.get (Matrix_client.Base_client.find_room state room);
    previous = None;
    timeline;
    decrypted = [];
    undecrypted = [];
    state_events = [];
    ephemeral = [];
    room_account_data = [];
    limited = false;
    unread = Matrix_client.Read_state.zero_counts;
  }

let beacon ~id ~timestamp ~geo_uri =
  raw
    (Printf.sprintf
       {|{"event_id":"%s","sender":"@alice:example.org","origin_server_ts":%Ld,"type":"org.matrix.msc3672.beacon","content":{"org.matrix.msc3488.location":{"uri":"%s"},"org.matrix.msc3488.ts":%Ld,"m.relates_to":{"rel_type":"m.reference","event_id":"$state:example.org"}}}|}
       id timestamp geo_uri timestamp)

let active_shares observer =
  Ui.Observable.List.snapshot (Ui.Live_locations.shares observer)

let test_observer_lifecycle () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let cache = Ui.Event_cache.create () in
  let active_content =
    json
      {|{"description":"desc","live":true,"timeout":10000,"org.matrix.msc3488.ts":1000,"org.matrix.msc3488.asset":{"type":"m.self"}}|}
  in
  let state = state_with_info ~event_id:state_id ~origin:1000L active_content in
  Ui.Event_cache.apply_room_change cache
    (room_change state
       [ beacon ~id:"$old:example.org" ~timestamp:2000L ~geo_uri:"geo:1,2" ]);
  let now = ref 3000L in
  let observer =
    Ui.Live_locations.create ~sw ~event_cache:cache ~state ~room_id:room
      ~now:(fun () -> !now)
      ()
  in
  Alcotest.(check int) "initial share" 1 (Array.length (active_shares observer));
  Ui.Event_cache.apply_room_change cache
    (room_change state
       [ beacon ~id:"$new:example.org" ~timestamp:3000L ~geo_uri:"geo:3,4" ]);
  Eio.Fiber.yield ();
  let current = (active_shares observer).(0) in
  let latest = Option.get current.last_location in
  Alcotest.(check string)
    "automatic newest location" "geo:3,4" latest.location.uri;
  Alcotest.(check int64)
    "newest timestamp" 3000L
    (Event.Timestamp.to_ms latest.timestamp);
  let redaction =
    raw
      {|{"event_id":"$redaction:example.org","sender":"@alice:example.org","origin_server_ts":4000,"type":"m.room.redaction","redacts":"$new:example.org","content":{}}|}
  in
  Ui.Event_cache.apply_room_change cache (room_change state [ redaction ]);
  Eio.Fiber.yield ();
  let fallback = Option.get (active_shares observer).(0).last_location in
  Alcotest.(check int64)
    "redaction falls back to prior beacon" 2000L
    (Event.Timestamp.to_ms fallback.timestamp);
  Ui.Live_locations.close observer;
  Ui.Event_cache.apply_room_change cache
    (room_change state
       [
         beacon ~id:"$after-close:example.org" ~timestamp:5000L
           ~geo_uri:"geo:5,6";
       ]);
  Eio.Fiber.yield ();
  let after_close = Option.get (active_shares observer).(0).last_location in
  Alcotest.(check int64)
    "close unsubscribes" 2000L
    (Event.Timestamp.to_ms after_close.timestamp);
  let replacement_id = Id.Event_id.of_string_exn "$replacement:example.org" in
  let replacement_content =
    json
      {|{"description":"replacement","live":true,"timeout":10000,"org.matrix.msc3488.ts":3000,"org.matrix.msc3488.asset":{"type":"m.self"}}|}
  in
  let replacement =
    state_with_info ~event_id:replacement_id ~origin:3000L replacement_content
  in
  Ui.Live_locations.refresh_state observer replacement;
  let replaced = (active_shares observer).(0) in
  Alcotest.(check string)
    "replacement id" "$replacement:example.org"
    (Id.Event_id.to_string replaced.beacon_id);
  Alcotest.(check bool)
    "old beacons do not cross sessions" false
    (Option.is_some replaced.last_location);
  now := 14_000L;
  Ui.Live_locations.refresh_time observer;
  Alcotest.(check int) "expired share" 0 (Array.length (active_shares observer));
  let expired_reload =
    Ui.Live_locations.create ~event_cache:cache ~state:replacement ~room_id:room
      ~now:(fun () -> !now)
      ()
  in
  Alcotest.(check int)
    "expired share is not resurrected" 0
    (Array.length (active_shares expired_reload));
  let stopped_content =
    json
      {|{"description":"replacement","live":false,"timeout":10000,"org.matrix.msc3488.ts":3000,"org.matrix.msc3488.asset":{"type":"m.self"}}|}
  in
  let stopped =
    state_with_info ~event_id:replacement_id ~origin:3000L stopped_content
  in
  now := 4000L;
  Ui.Live_locations.refresh_state observer stopped;
  Alcotest.(check int) "stopped share" 0 (Array.length (active_shares observer));
  let stopped_reload =
    Ui.Live_locations.create ~event_cache:cache ~state:stopped ~room_id:room
      ~now:(fun () -> !now)
      ()
  in
  Alcotest.(check int)
    "stopped share is not resurrected" 0
    (Array.length (active_shares stopped_reload));
  Ui.Live_locations.close expired_reload;
  Ui.Live_locations.close stopped_reload;
  Ui.Live_locations.close observer

let () =
  Alcotest.run "live locations"
    [
      ( "client",
        [
          Alcotest.test_case "start exact" `Quick (run test_start_exact);
          Alcotest.test_case "invalid duration" `Quick
            (run test_start_rejects_invalid_duration);
          Alcotest.test_case "stop preserves/send reference" `Quick
            (run test_stop_preserves_and_send_reference);
          Alcotest.test_case "local errors and expiry" `Quick
            (run test_stop_missing_stopped_redacted_and_expired);
        ] );
      ( "ui",
        [
          Alcotest.test_case "observer lifecycle" `Quick test_observer_lifecycle;
        ] );
    ]
