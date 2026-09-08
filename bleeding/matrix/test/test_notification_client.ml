module Notification = Matrix_ui.Notification_client
module Event_cache = Matrix_ui.Event_cache
module Base_client = Matrix_client.Base_client
module Client = Matrix_client.Client
module Push = Matrix_proto.Push
module Event = Matrix_proto.Event
module Id = Matrix_proto.Id

let user = Id.User_id.of_string_exn "@alice:example.org"
let sender = Id.User_id.of_string_exn "@bob:example.org"
let room = Id.Room_id.of_string_exn "!room:example.org"
let event_id = Id.Event_id.of_string_exn "$event:example.org"

let mock_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 4096 'p')
  end

let client handler =
  Client.create
    ~config:
      (Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ())
    ~fetch:(Fetch_mock.client handler)
    ~random:(Matrix_client.Random.of_env mock_env)

let message ?(id = "$event:example.org") ?(type_ = "m.room.message")
    ?(content = {|{"msgtype":"m.text","body":"hello"}|}) ?unsigned () =
  Printf.sprintf
    {|{"event_id":"%s","sender":"@bob:example.org","origin_server_ts":1000,"type":"%s","content":%s%s}|}
    id type_ content
    (match unsigned with
    | None -> ""
    | Some value -> Printf.sprintf {|,"unsigned":%s|} value)

let context event =
  Printf.sprintf
    {|{"event":%s,"events_before":[],"events_after":[],"start":null,"end":null,"state":[]}|}
    event

let raw json =
  match Jsont_bytesrw.decode_string Event.Raw_event.jsont json with
  | Ok event -> event
  | Error error -> Alcotest.fail error

let room_rule actions = Push.Rule.v ~rule_id:(Push.Rule_id.room room) actions

let notifying_rules =
  {
    Push.Ruleset.empty with
    room =
      [
        room_rule
          [
            Push.Action.Notify;
            Push.Action.Set_tweak (Push.Tweak.Sound "default");
          ];
      ];
  }

let silent_rules = { Push.Ruleset.empty with room = [ room_rule [] ] }
let state ruleset = Base_client.create ~user_id:user ~ruleset ()

let notification_client ?decrypt ~handler state_ref cache =
  Notification.create ~client:(client handler) ~cache
    ~state:(fun () -> !state_ref)
    ?decrypt ()

let expect_event = function
  | Ok (Notification.Event event) -> event
  | Ok _ -> Alcotest.fail "expected a notification event"
  | Error error ->
      Alcotest.failf "notification failed: %s"
        (Matrix_client.Error.to_string error)

let test_private_dedup_and_sync_handoff () =
  let requests = ref 0 in
  let wire = message () in
  let handler request =
    incr requests;
    Fetch_mock.respond (context wire) request
  in
  let cache = Event_cache.create () in
  let state_ref = ref (state notifying_rules) in
  let client = notification_client ~handler state_ref cache in
  let first = expect_event (Notification.fetch client room event_id) in
  let second = expect_event (Notification.fetch client room event_id) in
  Alcotest.(check int) "one context request" 1 !requests;
  Alcotest.(check bool)
    "private record reused" true
    (first.event == second.event);
  let synced = raw wire in
  Event_cache.prepend cache room ~events:[ synced ] ~prev_batch:None;
  let shared = Array.get (Event_cache.snapshot cache room) 0 in
  let third = expect_event (Notification.fetch client room event_id) in
  Alcotest.(check int) "sync handoff makes no request" 1 !requests;
  Alcotest.(check bool) "shared cache wins" true (third.event == shared)

let test_dynamic_rules_filter () =
  let requests = ref 0 in
  let handler request =
    incr requests;
    Fetch_mock.respond (context (message ())) request
  in
  let cache = Event_cache.create () in
  let state_ref = ref (state notifying_rules) in
  let client = notification_client ~handler state_ref cache in
  ignore (expect_event (Notification.fetch client room event_id));
  state_ref := Base_client.with_ruleset !state_ref silent_rules;
  (match Notification.fetch client room event_id with
  | Ok (Notification.Event_filtered_out _) -> ()
  | _ -> Alcotest.fail "updated silent rules did not filter the event");
  Alcotest.(check int) "private event reused" 1 !requests

let test_redacted_event () =
  let handler request =
    Fetch_mock.respond
      (context (message ~unsigned:{|{"redacted_because":{}}|} ()))
      request
  in
  let state_ref = ref (state notifying_rules) in
  let client = notification_client ~handler state_ref (Event_cache.create ()) in
  match Notification.fetch client room event_id with
  | Ok (Notification.Event_redacted _) -> ()
  | _ -> Alcotest.fail "redacted event was not classified"

let test_not_found_and_network_error () =
  let state_ref = ref (state notifying_rules) in
  let missing request =
    Fetch_mock.respond ~status:404
      {|{"errcode":"M_NOT_FOUND","error":"missing"}|} request
  in
  let client =
    notification_client ~handler:missing state_ref (Event_cache.create ())
  in
  (match Notification.fetch client room event_id with
  | Ok Notification.Event_not_found -> ()
  | _ -> Alcotest.fail "M_NOT_FOUND was not classified");
  let failed request =
    Fetch_mock.respond ~status:500 {|{"errcode":"M_UNKNOWN","error":"boom"}|}
      request
  in
  let client =
    notification_client ~handler:failed state_ref (Event_cache.create ())
  in
  match Notification.fetch client room event_id with
  | Error
      (Matrix_client.Error.Matrix_error
         { errcode = Matrix_client.Error.M_UNKNOWN; _ }) ->
      ()
  | _ -> Alcotest.fail "non-404 error was not preserved"

let test_encrypted_decryption () =
  let encrypted = message ~type_:"m.room.encrypted" ~content:"{}" () in
  let handler request = Fetch_mock.respond (context encrypted) request in
  let called = ref 0 in
  let decrypt room_id wire =
    incr called;
    Alcotest.(check string)
      "decrypt room" "!room:example.org"
      (Id.Room_id.to_string room_id);
    Ok
      {
        wire with
        Event.Raw_event.type_ = Event.Event_type.Room_message;
        content =
          Jsont.Json.object'
            [
              Jsont.Json.mem
                (Jsont.Json.name "msgtype")
                (Jsont.Json.string "m.text");
              Jsont.Json.mem (Jsont.Json.name "body")
                (Jsont.Json.string "decrypted");
            ];
      }
  in
  let state_ref = ref (state notifying_rules) in
  let client =
    notification_client ~decrypt ~handler state_ref (Event_cache.create ())
  in
  let result = expect_event (Notification.fetch client room event_id) in
  Alcotest.(check int) "decrypted once" 1 !called;
  Alcotest.(check bool)
    "effective event is clear" true
    (Event.Event_type.equal (Event_cache.effective result.event).type_
       Event.Event_type.Room_message);
  let no_decrypt =
    notification_client ~handler state_ref (Event_cache.create ())
  in
  match Notification.fetch no_decrypt room event_id with
  | Ok (Notification.Unable_to_decrypt { error = None; _ }) -> ()
  | _ -> Alcotest.fail "missing decryptor was not explicit"

let test_mismatched_context_target () =
  let handler request =
    Fetch_mock.respond (context (message ~id:"$other:example.org" ())) request
  in
  let state_ref = ref (state notifying_rules) in
  let client = notification_client ~handler state_ref (Event_cache.create ()) in
  match Notification.fetch client room event_id with
  | Error (Matrix_client.Error.Json_error _) -> ()
  | _ -> Alcotest.fail "mismatched context target was accepted"

let run test () = Eio_mock.Backend.run test

let () =
  Alcotest.run "notification client"
    [
      ( "fetch",
        [
          Alcotest.test_case "private dedup and sync handoff" `Quick
            (run test_private_dedup_and_sync_handoff);
          Alcotest.test_case "dynamic rules" `Quick
            (run test_dynamic_rules_filter);
          Alcotest.test_case "redacted" `Quick (run test_redacted_event);
          Alcotest.test_case "not found and network" `Quick
            (run test_not_found_and_network_error);
          Alcotest.test_case "encrypted" `Quick (run test_encrypted_decryption);
          Alcotest.test_case "mismatched target" `Quick
            (run test_mismatched_context_target);
        ] );
    ]
