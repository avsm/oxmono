(** Tests for the base client: {!Matrix_client.Base_client},
    {!Matrix_client.Push_evaluator}, {!Matrix_client.Read_state},
    {!Matrix_client.Store}, {!Matrix_client.Send_queue} and
    {!Matrix_client.Timeline}.

    The sync and send-queue tests run against a [fetch.mock] homeserver under
    [Eio_mock.Backend.run], so the assertions are on the request that actually
    leaves the library and on the state the library folds out of the reply. The
    store round trip needs a real filesystem and runs under [Eio_main.run]
    against a temporary directory. *)

module Client = Matrix_client.Client
module Error = Matrix_client.Error
module Push_evaluator = Matrix_client.Push_evaluator
module Read_state = Matrix_client.Read_state
module Rooms = Matrix_client.Rooms
module Send_queue = Matrix_client.Send_queue
module Media_store = Matrix_client.Media_store
module Media = Matrix_client.Media
module Store = Matrix_client.Store
module Sqlite = Matrix_ui_sqlite
module Sync = Matrix_client.Sync
module Sync_service = Matrix_client.Base_client
module Timeline = Matrix_client.Timeline
module Attachment = Matrix_client.Encrypted_attachment
module Id = Matrix_proto.Id
module Sliding_sync = Matrix_proto.Sliding_sync
module Push = Matrix_proto.Push
module Event = Matrix_proto.Event
module Timestamp = Matrix_proto.Event.Timestamp
module Encryption = Matrix_client.Encryption
module Ck = Matrix_client.Crypto_key

let mock_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 4096 'r')
  end

type recorded = { meth : string; url : string; body : string option }

let body_of_request (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> None
  | Fetch.String s -> Some s
  | Fetch.Stream _ -> Some "<stream>"

let record log (req : Fetch.Middleware.request) =
  log :=
    {
      meth = Http.Method.to_string req.meth;
      url = Fetch.Middleware.Url.to_string req.url;
      body = body_of_request req;
    }
    :: !log

(* A client answering a scripted sequence of handlers, one per request. *)
let mock_seq handlers =
  let log = ref [] in
  let remaining = ref handlers in
  let client =
    Fetch_mock.client (fun req ->
        record log req;
        match !remaining with
        | [] -> Alcotest.fail "more requests than scripted responses"
        | h :: rest ->
            remaining := rest;
            h req)
  in
  (log, client)

let client_of fetch =
  let config =
    Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ()
  in
  Client.create ~config ~fetch ~random:(Matrix_client.Random.of_env mock_env)

let uid s = Result.get_ok (Id.User_id.of_string s)
let rid s = Result.get_ok (Id.Room_id.of_string s)
let eid s = Result.get_ok (Id.Event_id.of_string s)
let requests log = List.rev !log
let run f () = Eio_mock.Backend.run f
let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let alice = uid "@alice:example.org"

(* A substring test; the tree has no Astring dependency. *)
let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.sub haystack i n = needle || go (i + 1))
  in
  n = 0 || go 0

let replace_once haystack ~needle ~replacement =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec find offset =
    if offset + needle_length > haystack_length then None
    else if String.sub haystack offset needle_length = needle then Some offset
    else find (offset + 1)
  in
  match find 0 with
  | None -> Alcotest.failf "fixture does not contain %S" needle
  | Some offset ->
      String.sub haystack 0 offset
      ^ replacement
      ^ String.sub haystack (offset + needle_length)
          (haystack_length - offset - needle_length)

let json_of s =
  match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json s with
  | Ok j -> j
  | Error e -> Alcotest.failf "bad fixture json: %s" e

let event_of s =
  match Jsont_bytesrw.decode_string Event.Raw_event.jsont s with
  | Ok e -> e
  | Error e -> Alcotest.failf "bad fixture event: %s" e

let ctx ?(display_name = "Alice") ?(member_count = 5) ?power_levels () =
  Push_evaluator.Context.v ~user_id:alice ~room_id:(rid "!r:example.org")
    ~display_name ~member_count ?power_levels ()

let message ?(sender = "@bob:example.org") body =
  event_of
    (Printf.sprintf
       {|{"type":"m.room.message","event_id":"$m:example.org","sender":"%s",
          "origin_server_ts":1000,
          "content":{"msgtype":"m.text","body":%s}}|}
       sender (Printf.sprintf "%S" body))

let rule ?enabled ?pattern ?conditions ~rule_id actions =
  Push.Rule.v ?enabled ?pattern ?conditions
    ~rule_id:(Push.Rule_id.override rule_id)
    actions

let notify_rules rules = { Push.Ruleset.empty with override = rules }

(* An [event_match] on [content.body] matches on word boundaries; on any
   other key it matches the whole value. *)
let matches ~key ~pattern body =
  let rules =
    notify_rules
      [
        rule ~rule_id:"m"
          ~conditions:[ Push.Condition.Event_match { key; pattern } ]
          [ Push.Action.Notify ];
      ]
  in
  (Push_evaluator.notification_for_event rules (ctx ()) (message body)).notify

let test_glob_word_matching () =
  let m pattern value = matches ~key:"content.body" ~pattern value in
  check_bool "whole word" true (m "alice" "hello alice there");
  check_bool "case insensitive" true (m "ALICE" "hello alice");
  check_bool "not a substring" false (m "lic" "hello alice");
  check_bool "at the end" true (m "alice" "hello alice");
  check_bool "punctuation is a boundary" true (m "alice" "hello, alice!");
  check_bool "underscore is a word char" false (m "alice" "hello_alice");
  check_bool "star glob" true (m "ali*" "hello alicia there");
  check_bool "question glob" true (m "al?ce" "say alice now");
  check_bool "question glob is one char" false (m "al?ce" "say aliice now");
  let g pattern = matches ~key:"type" ~pattern "hi" in
  check_bool "exact" true (g "m.room.message");
  check_bool "no substring" false (g "room");
  check_bool "trailing star" true (g "m.room.*")

let test_event_match_on_body () =
  let n body = matches ~key:"content.body" ~pattern:"ping" body in
  check_bool "word match" true (n "please ping me");
  check_bool "substring is not a match" false (n "pinguin");
  check_bool "absent" false (n "nothing here")

let test_contains_display_name () =
  let rules =
    notify_rules
      [
        rule ~rule_id:"dn"
          ~conditions:[ Push.Condition.Contains_display_name ]
          [
            Push.Action.Notify;
            Push.Action.Set_tweak (Push.Tweak.Highlight true);
          ];
      ]
  in
  let n body =
    Push_evaluator.notification_for_event rules (ctx ()) (message body)
  in
  check_bool "mentions us" true (n "hey Alice, look").highlight;
  check_bool "does not mention us" false (n "hey Bob, look").highlight

let condition_of s =
  match Jsont_bytesrw.decode_string Push.Condition.jsont s with
  | Ok c -> c
  | Error e -> Alcotest.failf "bad fixture condition: %s" e

let test_room_member_count () =
  let dm_rule condition =
    notify_rules
      [ rule ~rule_id:"dm" ~conditions:[ condition ] [ Push.Action.Notify ] ]
  in
  let n is count =
    (Push_evaluator.notification_for_event
       (dm_rule
          (condition_of
             (Printf.sprintf {|{"kind":"room_member_count","is":%S}|} is)))
       (ctx ~member_count:count ())
       (message "hi"))
      .notify
  in
  check_bool "<=2 with 2" true (n "<=2" 2);
  check_bool "<=2 with 3" false (n "<=2" 3);
  check_bool "bare 2 means ==2" true (n "2" 2);
  check_bool "==2 with 1" false (n "==2" 1);
  check_bool ">1 with 2" true (n ">1" 2);
  check_bool ">=5 with 5" true (n ">=5" 5);
  check_bool "<5 with 5" false (n "<5" 5);
  (* An [is] the specification does not define is kept unmodelled, and an
     unmodelled condition applies to nothing. *)
  check_bool "junk never matches" false (n "many" 2)

let test_sender_notification_permission () =
  let rules =
    notify_rules
      [
        rule ~rule_id:"atroom"
          ~conditions:
            [ Push.Condition.Sender_notification_permission { key = "room" } ]
          [ Push.Action.Notify ];
      ]
  in
  let n pl =
    (Push_evaluator.notification_for_event rules (ctx ?power_levels:pl ())
       (message "@room hello"))
      .notify
  in
  check_bool "no power levels: never applies" false (n None);
  check_bool "default users_default 0 is below 50" false
    (n (Some Push_evaluator.Power_levels.default));
  check_bool "sender at 50 reaches the default" true
    (n
       (Some
          {
            Push_evaluator.Power_levels.users = [ (uid "@bob:example.org", 50) ];
            users_default = 0;
            notifications = [];
          }));
  check_bool "explicit notifications.room raises the bar" false
    (n
       (Some
          {
            Push_evaluator.Power_levels.users = [ (uid "@bob:example.org", 50) ];
            users_default = 0;
            notifications = [ ("room", 100) ];
          }))

let test_event_property_conditions () =
  let mention =
    event_of
      {|{"type":"m.room.message","event_id":"$m:example.org",
         "sender":"@bob:example.org","origin_server_ts":1000,
         "content":{"msgtype":"m.text","body":"hi",
                    "m.mentions":{"user_ids":["@alice:example.org"]}}}|}
  in
  let edit =
    event_of
      {|{"type":"m.room.message","event_id":"$e:example.org",
         "sender":"@bob:example.org","origin_server_ts":1000,
         "content":{"msgtype":"m.text","body":"* fixed",
                    "m.relates_to":{"rel_type":"m.replace","event_id":"$m:example.org"}}}|}
  in
  let rules = Push.default_ruleset ~user_id:alice in
  let n e = Push_evaluator.notification_for_event rules (ctx ()) e in
  (* The condition addresses [content.m\.mentions.user_ids], so the event
     matches only if the dot inside "m.mentions" is escaped. *)
  check_bool "is_user_mention notifies" true (n mention).notify;
  check_bool "is_user_mention highlights" true (n mention).highlight;
  check_string "is_user_mention sound" "default"
    (Option.value (n mention).sound ~default:"");
  check_bool "suppress_edits wins over message" false (n edit).notify

let test_default_ruleset_basics () =
  let rules = Push.default_ruleset ~user_id:alice in
  let n ?(c = ctx ()) e = Push_evaluator.notification_for_event rules c e in
  check_bool "a plain message notifies" true (n (message "hello")).notify;
  check_bool "a plain message does not highlight" false
    (n (message "hello")).highlight;
  check_bool "our own message never matches" false
    (n (message ~sender:"@alice:example.org" "hello")).notify;
  let notice =
    event_of
      {|{"type":"m.room.message","event_id":"$n:example.org",
         "sender":"@bob:example.org","origin_server_ts":1,
         "content":{"msgtype":"m.notice","body":"beep"}}|}
  in
  check_bool "suppress_notices" false (n notice).notify;
  let reaction =
    event_of
      {|{"type":"m.reaction","event_id":"$r:example.org",
         "sender":"@bob:example.org","origin_server_ts":1,
         "content":{"m.relates_to":{"rel_type":"m.annotation","event_id":"$m","key":"👍"}}}|}
  in
  check_bool "reactions do not notify" false (n reaction).notify;
  (* One-to-one rooms get a sound; larger rooms do not. *)
  check_string "1:1 sound" "default"
    (Option.value (n ~c:(ctx ~member_count:2 ()) (message "hi")).sound
       ~default:"");
  check_bool "group room has no sound" true
    ((n ~c:(ctx ~member_count:5 ()) (message "hi")).sound = None)

let test_rule_precedence_and_enabled () =
  let suppress =
    rule ~rule_id:"suppress"
      ~conditions:
        [
          Push.Condition.Event_match
            { key = "type"; pattern = "m.room.message" };
        ]
      []
  in
  let base = Push.default_ruleset ~user_id:alice in
  let with_override = { base with override = base.override @ [ suppress ] } in
  check_bool "an override beats the underride that would notify" false
    (Push_evaluator.notification_for_event with_override (ctx ()) (message "hi"))
      .notify;
  let disabled = { suppress with enabled = false } in
  let with_disabled = { base with override = base.override @ [ disabled ] } in
  check_bool "a disabled rule is skipped" true
    (Push_evaluator.notification_for_event with_disabled (ctx ()) (message "hi"))
      .notify;
  (* Content rules are consulted before room, sender and underride ones. *)
  let content_rule =
    Push.Rule.v ~rule_id:(Push.Rule_id.content "c") ~pattern:"banana"
      [ Push.Action.Notify ]
  in
  let with_content = { base with content = [ content_rule ] } in
  let matched name rules =
    match
      Push_evaluator.find_matching_rule rules (ctx ()) (message "a banana")
    with
    | Some r -> Push.Rule_id.id r.rule_id
    | None -> Alcotest.failf "expected the %s rule to match" name
  in
  check_string "content rule matched" "c" (matched "content" with_content);
  let sender_rule =
    Push.Rule.v
      ~rule_id:(Push.Rule_id.sender (uid "@bob:example.org"))
      [ Push.Action.Notify ]
  in
  let with_sender = { base with sender = [ sender_rule ] } in
  check_string "sender rule matched" "@bob:example.org"
    (matched "sender" with_sender);
  let room_rule =
    Push.Rule.v
      ~rule_id:(Push.Rule_id.room (rid "!r:example.org"))
      [ Push.Action.Notify ]
  in
  let with_room = { base with room = [ room_rule ] } in
  check_string "room rule matched" "!r:example.org" (matched "room" with_room)

let test_ruleset_codec_roundtrip () =
  let rules = Push.default_ruleset ~user_id:alice in
  match Jsont_bytesrw.encode_string Push.Ruleset.jsont rules with
  | Error e -> Alcotest.failf "encode: %s" e
  | Ok s -> (
      match Jsont_bytesrw.decode_string Push.Ruleset.jsont s with
      | Error e -> Alcotest.failf "decode: %s" e
      | Ok back ->
          check_int "override rules"
            (List.length rules.override)
            (List.length back.override);
          check_int "underride rules"
            (List.length rules.underride)
            (List.length back.underride);
          (* The value-carrying conditions must survive the round trip. *)
          let mention = List.nth back.override 4 in
          check_string "rule id" ".m.rule.is_user_mention"
            (Push.Rule_id.id mention.rule_id);
          check_bool "value kept" true
            (match mention.conditions with
            | [ Push.Condition.Event_property_contains _ ] -> true
            | _ -> false))

let encoded_ruleset rules =
  match Jsont_bytesrw.encode_string Push.Ruleset.global_jsont rules with
  | Ok json -> json
  | Error error -> Alcotest.failf "encode push rules: %s" error

let response_with_push_rules ?(next_batch = "push-rules") content =
  match
    Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
      (Printf.sprintf
         {|{"next_batch":%S,"account_data":{"events":[{"type":"m.push_rules","content":%s}]}}|}
         next_batch content)
  with
  | Ok response -> response
  | Error error -> Alcotest.failf "bad push-rules response: %s" error

let response_without_push_rules () =
  match
    Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
      {|{"next_batch":"no-push-rules"}|}
  with
  | Ok response -> response
  | Error error -> Alcotest.failf "bad response without push rules: %s" error

let custom_push_rules ~actions =
  notify_rules
    [
      rule ~rule_id:"custom-message"
        ~conditions:
          [
            Push.Condition.Event_match
              { key = "type"; pattern = "m.room.message" };
          ]
        actions;
    ]

let test_push_rules_current_response () =
  let custom = custom_push_rules ~actions:[] in
  let state = Sync_service.create ~user_id:alice () in
  let state, _ =
    Sync_service.apply state (response_with_push_rules (encoded_ruleset custom))
  in
  check_bool "current m.push_rules becomes active" true
    (Push.Ruleset.equal (Sync_service.ruleset state) custom);
  let last = custom_push_rules ~actions:[ Push.Action.Notify ] in
  let duplicate_response =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        (Printf.sprintf
           {|{"next_batch":"duplicate-push-rules","account_data":{"events":[{"type":"m.push_rules","content":%s},{"type":"m.push_rules","content":%s}]}}|}
           (encoded_ruleset custom) (encoded_ruleset last))
    with
    | Ok response -> response
    | Error error ->
        Alcotest.failf "bad duplicate push-rules response: %s" error
  in
  let state, _ = Sync_service.apply state duplicate_response in
  check_bool "the last duplicate push-rules event wins" true
    (Push.Ruleset.equal (Sync_service.ruleset state) last)

let test_push_rules_same_batch_evaluation () =
  let custom = custom_push_rules ~actions:[] in
  let response =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        (Printf.sprintf
           {|{"next_batch":"same-batch-rules","account_data":{"events":[{"type":"m.push_rules","content":%s}]},"rooms":{"join":{"!rules:example.org":{"timeline":{"events":[{"type":"m.room.message","event_id":"$rules:example.org","sender":"@bob:example.org","origin_server_ts":1,"content":{"msgtype":"m.text","body":"hello"}}]}}}}}|}
           (encoded_ruleset custom))
    with
    | Ok response -> response
    | Error error -> Alcotest.failf "bad same-batch response: %s" error
  in
  let state, _ =
    Sync_service.apply ~coverage:Sync_service.unknown_state_coverage
      (Sync_service.create ~user_id:alice ())
      response
  in
  let room =
    match Sync_service.find_room state (rid "!rules:example.org") with
    | Some room -> room
    | None -> Alcotest.fail "same-batch room missing"
  in
  check_int "same-batch custom rules suppress notification" 0
    room.local_notification_count

let test_push_rules_restore_from_store () =
  let custom = custom_push_rules ~actions:[ Push.Action.Notify ] in
  let store = Store.memory () in
  let state = Sync_service.create ~user_id:alice () in
  let state, _ =
    Sync_service.apply state (response_with_push_rules (encoded_ruleset custom))
  in
  Sync_service.persist store state;
  let restored = Sync_service.of_store store ~user_id:alice () in
  check_bool "stored m.push_rules is restored" true
    (Push.Ruleset.equal (Sync_service.ruleset restored) custom)

let test_push_rules_absent_preserves_prior () =
  let custom = custom_push_rules ~actions:[ Push.Action.Notify ] in
  let state = Sync_service.create ~user_id:alice ~ruleset:custom () in
  let state, _ = Sync_service.apply state (response_without_push_rules ()) in
  check_bool "absent m.push_rules preserves explicit rules" true
    (Push.Ruleset.equal (Sync_service.ruleset state) custom)

let test_push_rules_malformed_preserves_prior () =
  let custom = custom_push_rules ~actions:[ Push.Action.Notify ] in
  let state = Sync_service.create ~user_id:alice ~ruleset:custom () in
  let malformed = {|{"global":{"override":"not-a-list"}}|} in
  let state, _ =
    Sync_service.apply state (response_with_push_rules malformed)
  in
  check_bool "malformed m.push_rules preserves prior rules" true
    (Push.Ruleset.equal (Sync_service.ruleset state) custom)

let test_push_rules_current_beats_stored () =
  let old_rules = custom_push_rules ~actions:[] in
  let new_rules = custom_push_rules ~actions:[ Push.Action.Notify ] in
  let store = Store.memory () in
  let state = Sync_service.create ~user_id:alice () in
  let state, _ =
    Sync_service.apply state
      (response_with_push_rules (encoded_ruleset old_rules))
  in
  Sync_service.persist store state;
  let restored = Sync_service.of_store store ~user_id:alice () in
  let restored, _ =
    Sync_service.apply restored
      (response_with_push_rules ~next_batch:"new-rules"
         (encoded_ruleset new_rules))
  in
  check_bool "current m.push_rules beats stored rules" true
    (Push.Ruleset.equal (Sync_service.ruleset restored) new_rules)

let ev_at n sender =
  event_of
    (Printf.sprintf
       {|{"type":"m.room.message","event_id":"$e%d:example.org","sender":"%s",
          "origin_server_ts":%d,"content":{"msgtype":"m.text","body":"m%d"}}|}
       n sender (1000 + n) n)

let test_receipt_ingestion () =
  let content =
    {|{"type":"m.receipt","content":{
        "$e2:example.org":{"m.read":{"@alice:example.org":{"ts":42}}},
        "$e1:example.org":{"m.read.private":{"@alice:example.org":{"ts":10}}},
        "$e9:example.org":{"m.read":{"@bob:example.org":{"ts":99}}}}}|}
  in
  let r =
    Read_state.ingest_receipt_event ~user_id:alice Read_state.empty
      (json_of content)
  in
  check_string "public read" "$e2:example.org"
    (match Read_state.public_read r with
    | Some x -> Id.Event_id.to_string x.event_id
    | None -> "");
  check_string "private read" "$e1:example.org"
    (match Read_state.private_read r with
    | Some x -> Id.Event_id.to_string x.event_id
    | None -> "");
  check_string "latest is the newer ts" "$e2:example.org"
    (match Read_state.latest_read r with
    | Some e -> Id.Event_id.to_string e
    | None -> "");
  (* A threaded receipt belongs to the thread, not the room. *)
  let threaded =
    {|{"type":"m.receipt","content":{
        "$e5:example.org":{"m.read":{"@alice:example.org":{"ts":90,"thread_id":"$t"}}}}}|}
  in
  let r2 =
    Read_state.ingest_receipt_event ~user_id:alice r (json_of threaded)
  in
  check_string "threaded receipt leaves main read unchanged" "$e2:example.org"
    (match Read_state.public_read r2 with
    | Some x -> Id.Event_id.to_string x.event_id
    | None -> "");
  check_string "threaded receipt is retained" "$e5:example.org"
    (match Read_state.thread_public_read r2 ~thread_id:(eid "$t") with
    | Some x -> Id.Event_id.to_string x.event_id
    | None -> "");
  check_string "thread id is validated and exposed" "$t"
    (match Read_state.thread_ids r2 with
    | [ thread_id ] -> Id.Event_id.to_string thread_id
    | _ -> "");
  let fully =
    Read_state.ingest_fully_read r
      (json_of
         {|{"type":"m.fully_read","content":{"event_id":"$e3:example.org"}}|})
  in
  check_string "fully read" "$e3:example.org"
    (match Read_state.fully_read fully with
    | Some e -> Id.Event_id.to_string e
    | None -> "")

let test_threaded_receipts_persist_and_count () =
  let thread_id = eid "$thread:example.org" in
  let event ~id ~sender ?thread () =
    let relation =
      match thread with
      | None -> ""
      | Some root ->
          Printf.sprintf
            ",\"m.relates_to\":{\"rel_type\":\"m.thread\",\"event_id\":%S}" root
    in
    event_of
      (Printf.sprintf
         {|{"type":"m.room.message","event_id":%S,"sender":%S,
            "origin_server_ts":1,"content":{"msgtype":"m.text","body":"x"%s}}|}
         id sender relation)
  in
  let events =
    [
      event ~id:"$thread:example.org" ~sender:"@bob:example.org" ();
      event ~id:"$reply1:example.org" ~sender:"@bob:example.org"
        ~thread:"$thread:example.org" ();
      event ~id:"$other-reply:example.org" ~sender:"@alice:example.org"
        ~thread:"$other:example.org" ();
      event ~id:"$reply2:example.org" ~sender:"@bob:example.org"
        ~thread:"$thread:example.org" ();
      event ~id:"$own-reply:example.org" ~sender:"@alice:example.org"
        ~thread:"$thread:example.org" ();
      event ~id:"$main-own:example.org" ~sender:"@alice:example.org" ();
      event ~id:"$reply3:example.org" ~sender:"@bob:example.org"
        ~thread:"$thread:example.org" ();
    ]
  in
  let receipt =
    Read_state.ingest_receipt_event ~user_id:alice Read_state.empty
      (json_of
         {|{"type":"m.receipt","content":{"$reply1:example.org":{"m.read":{"@alice:example.org":{"ts":10,"thread_id":"$thread:example.org"}},"m.read.private":{"@alice:example.org":{"ts":11,"thread_id":"$thread:example.org"}}}}}|})
  in
  let encoded =
    match Jsont_bytesrw.encode_string Read_state.jsont receipt with
    | Ok value -> value
    | Error error -> Alcotest.failf "could not encode read state: %s" error
  in
  let restored =
    match Jsont_bytesrw.decode_string Read_state.jsont encoded with
    | Ok value -> value
    | Error error -> Alcotest.failf "could not decode read state: %s" error
  in
  check_string "thread receipt survives persistence" "$reply1:example.org"
    (match Read_state.thread_public_read restored ~thread_id with
    | Some value -> Id.Event_id.to_string value.event_id
    | None -> "");
  check_string "private thread receipt survives persistence"
    "$reply1:example.org"
    (match Read_state.thread_private_read restored ~thread_id with
    | Some value -> Id.Event_id.to_string value.event_id
    | None -> "");
  check_string "latest thread receipt survives persistence"
    "$reply1:example.org"
    (match Read_state.thread_latest_read restored ~thread_id with
    | Some value -> Id.Event_id.to_string value
    | None -> "");
  check_int "thread read position excludes other threads and main timeline" 3
    (Option.get
       (Read_state.latest_read_in_thread ~user_id:alice ~thread_id restored
          events));
  let rules = Push.default_ruleset ~user_id:alice in
  let notification = Push_evaluator.notification_for_event rules (ctx ()) in
  let counts =
    Read_state.count_unread_in_thread ~user_id:alice ~thread_id ~notification
      restored events
  in
  check_int "only unread messages in the selected thread" 1 counts.unread;
  check_int "only selected thread notifications" 1 counts.notifications;
  let duplicate_threads =
    match
      Jsont_bytesrw.decode_string Read_state.jsont
        {|{"threads":[{"thread_id":"$thread:example.org","public_read":{"event_id":"$reply1:example.org","ts":10}},{"thread_id":"$thread:example.org","private_read":{"event_id":"$reply1:example.org","ts":11}}]}|}
    with
    | Ok value -> value
    | Error error -> Alcotest.failf "duplicate thread fixture: %s" error
  in
  check_int "duplicate persisted threads merge to one key" 1
    (List.length (Read_state.thread_ids duplicate_threads));
  check_bool "duplicate persisted public receipt is retained" true
    (Option.is_some
       (Read_state.thread_public_read duplicate_threads ~thread_id));
  check_bool "duplicate persisted private receipt is retained" true
    (Option.is_some
       (Read_state.thread_private_read duplicate_threads ~thread_id));
  let invalid_thread_state =
    Read_state.ingest_receipt_event ~user_id:alice Read_state.empty
      (json_of
         {|{"type":"m.receipt","content":{"$x:example.org":{"m.read":{"@alice:example.org":{"thread_id":"not-an-event"}}}}}|})
  in
  check_int "invalid thread ids are ignored" 0
    (List.length (Read_state.thread_ids invalid_thread_state));
  (* Missing thread data is the legacy storage shape and decodes cleanly. *)
  match Jsont_bytesrw.decode_string Read_state.jsont {|{}|} with
  | Ok legacy ->
      check_int "legacy read state has no threads" 0
        (List.length (Read_state.thread_ids legacy))
  | Error error -> Alcotest.failf "legacy read state did not decode: %s" error

(* A receipt with no timestamp must not silently overwrite one that has a
   timestamp: {!Read_state.val-ingest_receipt_event}'s "not older" guarantee
   would otherwise not hold for a receipt the sending server left [ts] off
   of, which the .mli documents as a real, not merely theoretical, case. *)
let test_receipt_without_timestamp_does_not_regress () =
  let timestamped =
    {|{"type":"m.receipt","content":{
        "$e2:example.org":{"m.read":{"@alice:example.org":{"ts":42}}}}}|}
  in
  let r =
    Read_state.ingest_receipt_event ~user_id:alice Read_state.empty
      (json_of timestamped)
  in
  let untimestamped =
    {|{"type":"m.receipt","content":{
        "$e5:example.org":{"m.read":{"@alice:example.org":{}}}}}|}
  in
  let r2 =
    Read_state.ingest_receipt_event ~user_id:alice r (json_of untimestamped)
  in
  check_string "the timestamped receipt is kept" "$e2:example.org"
    (match Read_state.public_read r2 with
    | Some x -> Id.Event_id.to_string x.event_id
    | None -> "");
  (* An empty state has nothing to compare against, so the first receipt for
     a room is always accepted even without a timestamp. *)
  let r3 =
    Read_state.ingest_receipt_event ~user_id:alice Read_state.empty
      (json_of untimestamped)
  in
  check_string "a first, timestamp-less receipt is still accepted"
    "$e5:example.org"
    (match Read_state.public_read r3 with
    | Some x -> Id.Event_id.to_string x.event_id
    | None -> "")

let test_unread_computation () =
  let events = List.map (fun n -> ev_at n "@bob:example.org") [ 1; 2; 3; 4 ] in
  let rules = Push.default_ruleset ~user_id:alice in
  let notification = Push_evaluator.notification_for_event rules (ctx ()) in
  let all =
    Read_state.count_unread ~user_id:alice ~notification Read_state.empty events
  in
  check_int "no receipt: everything is unread" 4 all.unread;
  check_int "and everything notifies" 4 all.notifications;
  let receipts =
    Read_state.v
      ~public_read:
        { event_id = eid "$e2:example.org"; ts = Some (Timestamp.of_ms 1L) }
      ()
  in
  let after =
    Read_state.count_unread ~user_id:alice ~notification receipts events
  in
  check_int "only events after the receipt" 2 after.unread;
  (* An event the user sent is an implicit receipt. *)
  let mixed =
    [
      ev_at 1 "@bob:example.org";
      ev_at 2 "@alice:example.org";
      ev_at 3 "@bob:example.org";
    ]
  in
  let own =
    Read_state.count_unread ~user_id:alice ~notification Read_state.empty mixed
  in
  check_int "implicit receipt on the user's own event" 1 own.unread;
  (* The user's own events, edits and state events never count. *)
  let noise =
    [
      ev_at 5 "@alice:example.org";
      event_of
        {|{"type":"m.room.topic","event_id":"$s:example.org","state_key":"",
           "sender":"@bob:example.org","origin_server_ts":1,"content":{"topic":"x"}}|};
      event_of
        {|{"type":"m.room.message","event_id":"$ed:example.org",
           "sender":"@bob:example.org","origin_server_ts":1,
           "content":{"msgtype":"m.text","body":"* x",
                      "m.relates_to":{"rel_type":"m.replace","event_id":"$e1:example.org"}}}|};
    ]
  in
  let n =
    Read_state.count_unread ~user_id:alice ~notification Read_state.empty noise
  in
  check_int "nothing unread" 0 n.unread;
  (* Direct thread replies are not part of the room-wide horizon. In
     particular, an own reply must not advance the implicit read position and
     a remote reply must not contribute a room notification. *)
  let thread_reply sender id =
    event_of
      (Printf.sprintf
         {|{"type":"m.room.message","event_id":%S,"sender":%S,
            "origin_server_ts":1,"content":{"msgtype":"m.text",
            "body":"reply","m.relates_to":{"rel_type":"m.thread",
            "event_id":"$root:example.org"}}}|}
         id sender)
  in
  let thread_events =
    [
      thread_reply "@alice:example.org" "$own-reply:example.org";
      thread_reply "@bob:example.org" "$remote-reply:example.org";
    ]
  in
  let thread_counts =
    Read_state.count_unread ~user_id:alice ~notification Read_state.empty
      thread_events
  in
  check_int "thread replies are excluded from room unread" 0
    thread_counts.unread;
  check_int "thread replies are excluded from room notifications" 0
    thread_counts.notifications;
  check_bool "main-timeline classifier excludes direct replies" false
    (Read_state.is_main_timeline_event (List.hd thread_events));
  check_bool "main-timeline classifier keeps ordinary events" true
    (Read_state.is_main_timeline_event (List.hd events))

let member ~user ~name ~membership =
  Printf.sprintf
    {|{"type":"m.room.member","state_key":"%s","sender":"%s",
       "event_id":"$mem%s:example.org","origin_server_ts":1,
       "content":{"membership":"%s","displayname":"%s"}}|}
    user user name membership name

let sync1 =
  Printf.sprintf
    {|{
  "next_batch": "s1",
  "account_data": {"events":[
    {"type":"m.direct","content":{"@bob:example.org":["!dm:example.org"]}}
  ]},
  "rooms": {
    "join": {
      "!named:example.org": {
        "summary": {"m.heroes":["@bob:example.org"],
                    "m.joined_member_count":3,"m.invited_member_count":0},
        "state": {"events":[
          {"type":"m.room.name","state_key":"","sender":"@alice:example.org",
           "event_id":"$n1:example.org","origin_server_ts":1,
           "content":{"name":"The Lounge"}},
          {"type":"m.room.topic","state_key":"","sender":"@alice:example.org",
           "event_id":"$t1:example.org","origin_server_ts":1,
           "content":{"topic":"chatter"}},
          {"type":"m.room.avatar","state_key":"","sender":"@alice:example.org",
           "event_id":"$a1:example.org","origin_server_ts":1,
           "content":{"url":"mxc://example.org/lounge"}},
          {"type":"m.room.encryption","state_key":"","sender":"@alice:example.org",
           "event_id":"$e1:example.org","origin_server_ts":1,
           "content":{"algorithm":"m.megolm.v1.aes-sha2"}},
          {"type":"m.room.power_levels","state_key":"","sender":"@alice:example.org",
           "event_id":"$p1:example.org","origin_server_ts":1,
           "content":{"users":{"@bob:example.org":50},"users_default":0,
                      "notifications":{"room":50}}},
          {"type":"m.room.create","state_key":"","sender":"@alice:example.org",
           "event_id":"$create:example.org","origin_server_ts":1,
           "content":{"room_version":"11","type":"m.space"}},
          {"type":"m.room.join_rules","state_key":"","sender":"@alice:example.org",
           "event_id":"$join:example.org","origin_server_ts":1,
           "content":{"join_rule":"invite"}},
          {"type":"m.room.history_visibility","state_key":"","sender":"@alice:example.org",
           "event_id":"$history:example.org","origin_server_ts":1,
           "content":{"history_visibility":"shared"}},
          {"type":"m.room.guest_access","state_key":"","sender":"@alice:example.org",
           "event_id":"$guest:example.org","origin_server_ts":1,
           "content":{"guest_access":"forbidden"}},
          {"type":"org.matrix.msc1763.retention","state_key":"","sender":"@alice:example.org",
           "event_id":"$retention:example.org","origin_server_ts":1,
           "content":{"max_lifetime":604800000}},
          {"type":"m.room.pinned_events","state_key":"","sender":"@alice:example.org",
           "event_id":"$pins:example.org","origin_server_ts":1,
           "content":{"pinned":["$old:example.org"]}},
          {"type":"com.example.project","state_key":"board","sender":"@alice:example.org",
           "event_id":"$custom1:example.org","origin_server_ts":1,
           "content":{"revision":1,"private":"kept"}},
          {"type":"m.room.member_hints","state_key":"","sender":"@alice:example.org",
           "event_id":"$services:example.org","origin_server_ts":1,
           "content":{"service_members":["@bot:example.org"]}},
          {"type":"io.element.functional_members","state_key":"","sender":"@alice:example.org",
           "event_id":"$legacy-services:example.org","origin_server_ts":1,
           "content":{"service_members":["@legacybot:example.org"]}},
          %s, %s, %s
        ]},
        "timeline": {"events":[], "limited": false, "prev_batch": "p-named"},
        "unread_notifications": {"notification_count":2,"highlight_count":0},
        "account_data": {"events":[
          {"type":"m.tag","content":{"tags":{"m.favourite":{"order":0.25}}}}
        ]}
      },
      "!aliased:example.org": {
        "summary": {"m.joined_member_count":4,"m.invited_member_count":0},
        "state": {"events":[
          {"type":"m.room.canonical_alias","state_key":"","sender":"@alice:example.org",
           "event_id":"$c1:example.org","origin_server_ts":1,
           "content":{"alias":"#lounge:example.org"}}
        ]},
        "timeline": {"events":[]}
      },
      "!dm:example.org": {
        "summary": {"m.heroes":["@bob:example.org"],
                    "m.joined_member_count":2,"m.invited_member_count":0},
        "state": {"events":[%s, %s]},
        "timeline": {"events":[]}
      },
      "!crowd:example.org": {
        "summary": {"m.heroes":["@bob:example.org","@carol:example.org"],
                    "m.joined_member_count":10,"m.invited_member_count":0},
        "state": {"events":[%s, %s]},
        "timeline": {"events":[]}
      }
    },
    "invite": {
      "!invited:example.org": {"invite_state":{"events":[
        {"type":"m.room.name","state_key":"","sender":"@bob:example.org",
         "content":{"name":"Secret"}},
        {"type":"m.room.member","state_key":"@alice:example.org",
         "sender":"@bob:example.org",
         "content":{"membership":"invite","is_direct":true}}
      ]}}
    }
  }
}|}
    (member ~user:"@alice:example.org" ~name:"Alice" ~membership:"join")
    (member ~user:"@bob:example.org" ~name:"Bob" ~membership:"join")
    (member ~user:"@bot:example.org" ~name:"Helper" ~membership:"join")
    (member ~user:"@alice:example.org" ~name:"Alice" ~membership:"join")
    (member ~user:"@bob:example.org" ~name:"Bob" ~membership:"join")
    (member ~user:"@bob:example.org" ~name:"Bob" ~membership:"join")
    (member ~user:"@carol:example.org" ~name:"Carol" ~membership:"join")

let sync2 =
  {|{
  "next_batch": "s2",
  "rooms": {
    "join": {
      "!named:example.org": {
        "timeline": {"events":[
          {"type":"m.room.message","event_id":"$msg1:example.org",
           "sender":"@bob:example.org","origin_server_ts":5000,
           "content":{"msgtype":"m.text","body":"hey Alice, look at this"}},
          {"type":"com.example.project","state_key":"board",
           "event_id":"$custom2:example.org","sender":"@bob:example.org",
           "origin_server_ts":4999,
           "content":{"revision":2,"private":"still kept"}},
          {"type":"m.room.redaction","event_id":"$red1:example.org",
           "sender":"@bob:example.org","origin_server_ts":5001,
           "content":{"redacts":"$old:example.org"}}
        ], "limited": true, "prev_batch": "p-named-2"}
      }
    }
  }
}|}

(* The server defaults plus the legacy display-name rule, which is no longer
   a Matrix 1.11 default but is still accepted as a user-defined rule. *)
let sync_ruleset =
  let base = Push.default_ruleset ~user_id:alice in
  {
    base with
    override =
      base.override
      @ [
          Push.Rule.v
            ~rule_id:(Push.Rule_id.override ".m.rule.contains_display_name")
            ~conditions:[ Push.Condition.Contains_display_name ]
            [
              Push.Action.Notify;
              Push.Action.Set_tweak (Push.Tweak.Highlight true);
            ];
        ];
  }

let room_of state id =
  match Sync_service.find_room state (rid id) with
  | Some r -> r
  | None -> Alcotest.failf "room %s missing from state" id

let test_room_specific_push_display_name () =
  let response =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        {|{
          "next_batch":"per-room-name",
          "rooms":{"join":{"!per-room:example.org":{
            "summary":{"m.joined_member_count":2,"m.invited_member_count":0},
            "state":{"events":[
              {"type":"m.room.member","state_key":"@alice:example.org",
               "sender":"@alice:example.org","event_id":"$alice-name:example.org",
               "origin_server_ts":1,
               "content":{"membership":"join","displayname":"Room Alice"}},
              {"type":"m.room.member","state_key":"@bob:example.org",
               "sender":"@bob:example.org","event_id":"$bob-name:example.org",
               "origin_server_ts":1,
               "content":{"membership":"join","displayname":"Bob"}}
            ]},
            "timeline":{"events":[
              {"type":"m.room.message","event_id":"$room-mention:example.org",
               "sender":"@bob:example.org","origin_server_ts":2,
               "content":{"msgtype":"m.text","body":"hello Room Alice"}}
            ]}
          }}}
        }|}
    with
    | Ok response -> response
    | Error error -> Alcotest.failf "bad room display-name fixture: %s" error
  in
  let room_id = rid "!per-room:example.org" in
  let state =
    Sync_service.create ~user_id:alice ~display_name:"Global Alice"
      ~ruleset:sync_ruleset ()
  in
  let state, _ =
    Sync_service.apply ~coverage:Sync_service.complete_state_coverage state
      response
  in
  check_string "room membership name wins" "Room Alice"
    (Push_evaluator.Context.display_name
       (Sync_service.push_context state room_id));
  check_int "same-batch room-name mention highlights" 1
    (room_of state "!per-room:example.org").local_highlight_count;
  check_string "unknown room keeps global fallback" "Global Alice"
    (Push_evaluator.Context.display_name
       (Sync_service.push_context state (rid "!unknown:example.org")));
  let store = Store.memory () in
  Sync_service.persist store state;
  let reopened =
    Sync_service.of_store store ~user_id:alice ~display_name:"Changed Global" ()
  in
  check_string "persisted membership name still wins" "Room Alice"
    (Push_evaluator.Context.display_name
       (Sync_service.push_context reopened room_id))

(* Drive two syncs through the mock homeserver, threading [next_batch]. *)
let run_two_syncs () =
  let log, fetch =
    mock_seq [ Fetch_mock.respond sync1; Fetch_mock.respond sync2 ]
  in
  let client = client_of fetch in
  let state =
    Sync_service.create ~user_id:alice ~display_name:"Alice"
      ~ruleset:sync_ruleset ()
  in
  let step ~coverage state =
    let params =
      { Sync.default_params with since = Sync_service.next_batch state }
    in
    match Sync.sync_once client ~params () with
    | Error e -> Alcotest.failf "sync failed: %s" (Error.to_string e)
    | Ok response -> Sync_service.apply ~coverage state response
  in
  let state, changes1 =
    step ~coverage:Sync_service.complete_state_coverage state
  in
  let state, changes2 =
    step ~coverage:Sync_service.unknown_state_coverage state
  in
  (log, state, changes1, changes2)

let test_sync_room_names () =
  let _, state, _, _ = run_two_syncs () in
  check_string "m.room.name wins" "The Lounge"
    (Sync_service.display_name (room_of state "!named:example.org"));
  check_bool "and is recorded as Named" true
    (match (room_of state "!named:example.org").display_name with
    | Store.Named _ -> true
    | _ -> false);
  check_string "canonical alias is next" "#lounge:example.org"
    (Sync_service.display_name (room_of state "!aliased:example.org"));
  check_bool "and is recorded as Aliased" true
    (match (room_of state "!aliased:example.org").display_name with
    | Store.Aliased _ -> true
    | _ -> false);
  check_string "heroes name a two-person room" "Bob"
    (Sync_service.display_name (room_of state "!dm:example.org"));
  check_string "and count the rest" "Bob, Carol, and 8 others"
    (Sync_service.display_name (room_of state "!crowd:example.org"));
  check_bool "computed names are Calculated" true
    (match (room_of state "!crowd:example.org").display_name with
    | Store.Calculated _ -> true
    | _ -> false);
  check_string "an invite is named from its stripped state" "Secret"
    (Sync_service.display_name (room_of state "!invited:example.org"))

let test_sync_room_metadata () =
  let _, state, _, _ = run_two_syncs () in
  let named = room_of state "!named:example.org" in
  check_string "topic" "chatter" (Option.value named.topic ~default:"");
  check_string "avatar" "mxc://example.org/lounge"
    (Option.value named.avatar_url ~default:"");
  check_bool "encryption recorded" true (named.encryption <> None);
  check_int "joined members" 3 named.joined_member_count;
  check_int "invited members" 0 named.invited_member_count;
  check_string "prev_batch" "p-named-2"
    (Option.value named.prev_batch ~default:"");
  check_bool "tags" true (List.mem_assoc "m.favourite" named.tags);
  check_bool "membership" true (named.membership = Store.Joined);
  check_bool "not a dm" false named.is_dm;
  check_bool "m.direct makes a room a dm" true
    (room_of state "!dm:example.org").is_dm;
  check_bool "an invite keeps its own is_direct hint" true
    (room_of state "!invited:example.org").is_dm;
  check_bool "invited membership" true
    ((room_of state "!invited:example.org").membership = Store.Invited)

let test_inviter_lookup () =
  let member_json ?(state_key = "@alice:example.org") ~sender ~content () =
    Printf.sprintf
      {|{"type":"m.room.member","state_key":%S,"sender":%S,"content":%s}|}
      state_key sender content
  in
  let response ~batch member =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        (Printf.sprintf
           {|{"next_batch":%S,"rooms":{"invite":{"!inviter:example.org":{"invite_state":{"events":[%s]}}}}}|}
           batch member)
    with
    | Ok response -> response
    | Error error -> Alcotest.failf "bad invite response: %s" error
  in
  let step state ~batch =
    fst
      (Sync_service.apply state
         (response ~batch
            (member_json ~sender:"@bob:example.org"
               ~content:{|{"membership":"invite"}|} ())))
  in
  let state = step (Sync_service.create ~user_id:alice ()) ~batch:"invite-1" in
  check_string "invite sender is available" "@bob:example.org"
    (match Sync_service.inviter state (rid "!inviter:example.org") with
    | Some user -> Id.User_id.to_string user
    | None -> "");
  let state =
    fst
      (Sync_service.apply state
         (response ~batch:"invite-2"
            (member_json ~sender:"@carol:example.org"
               ~content:{|{"membership":"invite"}|} ())))
  in
  check_string "replacement invite sender is current" "@carol:example.org"
    (match Sync_service.inviter state (rid "!inviter:example.org") with
    | Some user -> Id.User_id.to_string user
    | None -> "");
  let wrong_membership =
    fst
      (Sync_service.apply
         (Sync_service.create ~user_id:alice ())
         (response ~batch:"invite-join"
            (member_json ~sender:"@bob:example.org"
               ~content:{|{"membership":"join"}|} ())))
  in
  check_bool "non-invite membership has no inviter" true
    (Sync_service.inviter wrong_membership (rid "!inviter:example.org") = None);
  let wrong_state_key =
    fst
      (Sync_service.apply
         (Sync_service.create ~user_id:alice ())
         (response ~batch:"invite-other-member"
            (member_json ~state_key:"@carol:example.org"
               ~sender:"@bob:example.org" ~content:{|{"membership":"invite"}|}
               ())))
  in
  check_bool "another member's invite has no inviter" true
    (Sync_service.inviter wrong_state_key (rid "!inviter:example.org") = None);
  let malformed =
    fst
      (Sync_service.apply
         (Sync_service.create ~user_id:alice ())
         (response ~batch:"invite-malformed"
            (member_json ~sender:"@bob:example.org" ~content:{|{}|} ())))
  in
  check_bool "malformed invite state has no inviter" true
    (Sync_service.inviter malformed (rid "!inviter:example.org") = None);
  check_bool "missing room has no inviter" true
    (Sync_service.inviter
       (Sync_service.create ~user_id:alice ())
       (rid "!missing:example.org")
    = None)

let test_durable_room_state () =
  let _, state, changes1, changes2 = run_two_syncs () in
  let room_id = rid "!named:example.org" in
  let room = room_of state "!named:example.org" in
  let event ty ?state_key () =
    Sync_service.find_state_event state room_id
      ~event_type:(Event.Event_type.of_string ty)
      ?state_key ()
  in
  check_bool "create state cached" true (event "m.room.create" () <> None);
  check_bool "join rule cached" true (event "m.room.join_rules" () <> None);
  check_bool "history visibility cached" true
    (event "m.room.history_visibility" () <> None);
  check_bool "guest access cached" true (event "m.room.guest_access" () <> None);
  check_bool "retention cached" true
    (event "org.matrix.msc1763.retention" () <> None);
  check_bool "pins cached" true (event "m.room.pinned_events" () <> None);
  let custom = Option.get (event "com.example.project" ~state_key:"board" ()) in
  check_string "newest state event replaces the old one" "$custom2:example.org"
    (Option.fold ~none:"" ~some:Id.Event_id.to_string custom.event_id);
  check_int "unknown state content is retained" 2
    (Option.value
       (Option.bind
          (Matrix_proto.Json.find_mem "revision" custom.content)
          Matrix_proto.Json.as_int)
       ~default:0);
  let create = Option.get (event "m.room.create" ()) in
  let create_content =
    match Jsont.Json.decode Event.Room_create_content.jsont create.content with
    | Ok content -> content
    | Error e -> Alcotest.failf "cached create content: %s" e
  in
  check_string "typed create codec remains usable" "11"
    (Option.value
       (Event.Room_create_content.room_version create_content)
       ~default:"");
  check_bool "classic joined state is complete" true
    (room.state_completeness = Store.Complete);
  check_bool "encryption state absence/presence is authoritative" true
    room.encryption_state_complete;
  let first_room =
    List.find
      (fun c -> Id.Room_id.equal c.Sync_service.changed_room_id room_id)
      changes1.room_changes
  in
  check_bool "initial non-limited member state is complete" true
    first_room.info.members_complete;
  check_bool "a limited timeline preserves established member completeness" true
    room.members_complete;
  check_bool "the second change is limited" true
    (List.hd changes2.room_changes).limited;
  let member_ids f =
    List.map Id.User_id.to_string (f state room_id) |> List.sort String.compare
  in
  Alcotest.(check (list string))
    "encryption recipients retain service users"
    [ "@alice:example.org"; "@bob:example.org"; "@bot:example.org" ]
    (member_ids Sync_service.members);
  Alcotest.(check (list string))
    "human members exclude service users"
    [ "@alice:example.org"; "@bob:example.org" ]
    (member_ids Sync_service.human_members);
  Alcotest.(check (list string))
    "stable member hints win over legacy" [ "@bot:example.org" ]
    (List.map Id.User_id.to_string (Sync_service.service_members room)
    |> List.sort String.compare);
  check_int "human count excludes an active service user" 2
    (Sync_service.human_member_count room);
  (* Persisting and reopening the pure state must not empty the encryption
     recipients or the power-level context before the next sync. *)
  let store = Store.memory () in
  Sync_service.persist store state;
  let reopened = Sync_service.of_store store ~user_id:alice () in
  Alcotest.(check (list string))
    "members survive restart"
    (member_ids Sync_service.members)
    (List.map Id.User_id.to_string (Sync_service.members reopened room_id)
    |> List.sort String.compare);
  check_bool "power levels survive restart" true
    (Push_evaluator.Context.power_levels
       (Sync_service.push_context reopened room_id)
    <> None);
  let reopened_custom =
    Sync_service.find_state_event reopened room_id
      ~event_type:(Event.Event_type.of_string "com.example.project")
      ~state_key:"board" ()
  in
  check_bool "unknown state survives restart" true (reopened_custom <> None);
  let refreshed =
    Sync_service.replace_members state room_id
      [
        {
          Rooms.user_id = alice;
          display_name = Some "Alice";
          avatar_url = None;
          membership = Event.Membership.Join;
        };
        {
          Rooms.user_id = uid "@carol:example.org";
          display_name = Some "Carol";
          avatar_url = None;
          membership = Event.Membership.Invite;
        };
        {
          Rooms.user_id = uid "@gone:example.org";
          display_name = None;
          avatar_url = None;
          membership = Event.Membership.Leave;
        };
      ]
  in
  let refreshed_room = Option.get (Sync_service.find_room refreshed room_id) in
  check_bool "an endpoint refresh marks members complete" true
    refreshed_room.members_complete;
  check_int "the joined count follows the snapshot" 1
    refreshed_room.joined_member_count;
  check_int "the invited count follows the snapshot" 1
    refreshed_room.invited_member_count;
  Alcotest.(check (list string))
    "only active refreshed members are encryption recipients"
    [ "@alice:example.org"; "@carol:example.org" ]
    (List.map Id.User_id.to_string (Sync_service.members refreshed room_id)
    |> List.sort String.compare);
  let refreshed_store = Store.memory () in
  Sync_service.persist refreshed_store refreshed;
  let refreshed_reopened =
    Sync_service.of_store refreshed_store ~user_id:alice ()
  in
  Alcotest.(check (list string))
    "the refreshed recipient snapshot survives restart"
    [ "@alice:example.org"; "@carol:example.org" ]
    (List.map Id.User_id.to_string
       (Sync_service.members refreshed_reopened room_id)
    |> List.sort String.compare)

let test_incremental_room_without_state_stays_unknown () =
  let response =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        {|{
  "next_batch":"incremental",
  "rooms":{"join":{
    "!empty:example.org":{"timeline":{"events":[],"limited":false}}
  }}
}|}
    with
    | Ok response -> response
    | Error e -> Alcotest.failf "bad incremental sync fixture: %s" e
  in
  let state = Sync_service.create ~user_id:alice () in
  let state, _ =
    Sync_service.apply ~coverage:Sync_service.unknown_state_coverage state
      response
  in
  let room = room_of state "!empty:example.org" in
  check_bool "no state is not promoted to partial state" true
    (room.state_completeness = Store.No_state)

let test_full_state_replaces_projection () =
  let _, state, _, _ = run_two_syncs () in
  let response =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        (Printf.sprintf
           {|{
  "next_batch":"full-refresh",
  "rooms":{"join":{
    "!named:example.org":{
      "state":{"events":[%s]},
      "timeline":{"events":[],"limited":false}
    }
  }}
}|}
           (member ~user:"@alice:example.org" ~name:"Alice" ~membership:"join"))
    with
    | Ok response -> response
    | Error e -> Alcotest.failf "bad full-state fixture: %s" e
  in
  let state, _ =
    Sync_service.apply ~coverage:Sync_service.complete_state_coverage state
      response
  in
  let room_id = rid "!named:example.org" in
  let room = room_of state "!named:example.org" in
  check_int "only the snapshot state remains" 1 (List.length room.state_events);
  check_bool "state absent from a full snapshot is removed" true
    (Sync_service.find_state_event state room_id
       ~event_type:(Event.Event_type.of_string "com.example.project")
       ~state_key:"board" ()
    = None);
  check_bool "projection-derived room name is cleared" true (room.name = None);
  check_bool "projection-derived encryption is cleared" true
    (room.encryption = None);
  Alcotest.(check (list string))
    "members are rebuilt from the snapshot" [ "@alice:example.org" ]
    (List.map Id.User_id.to_string (Sync_service.members state room_id));
  check_bool "power levels absent from the snapshot are cleared" true
    (Push_evaluator.Context.power_levels
       (Sync_service.push_context state room_id)
    = None)

let test_encryption_completeness_does_not_survive_membership_transition () =
  let joined =
    event_of
      {|{"type":"m.room.encryption","state_key":"","sender":"@alice:example.org",
         "event_id":"$encryption:example.org","origin_server_ts":1,
         "content":{"algorithm":"m.megolm.v1.aes-sha2"}}|}
  in
  let leave =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        (Printf.sprintf
           {|{"next_batch":"leave","rooms":{"leave":{"!transition:example.org":
             {"state":{"events":[%s]},"timeline":{"events":[]}}}}}|}
           (member ~user:"@alice:example.org" ~name:"Alice" ~membership:"leave"))
    with
    | Ok response -> response
    | Error e -> Alcotest.failf "bad leave fixture: %s" e
  in
  let rejoin =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        {|{"next_batch":"rejoin","rooms":{"join":{"!transition:example.org":
           {"timeline":{"events":[],"limited":false}}}}}|}
    with
    | Ok response -> response
    | Error e -> Alcotest.failf "bad rejoin fixture: %s" e
  in
  let explicit_encryption =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        {|{"next_batch":"encryption","rooms":{"join":{"!transition:example.org":
           {"state":{"events":[{"type":"m.room.encryption","state_key":"",
             "sender":"@alice:example.org","event_id":"$encryption-2:example.org",
             "origin_server_ts":2,"content":{"algorithm":"m.megolm.v1.aes-sha2"}}]},
            "timeline":{"events":[],"limited":false}}}}}|}
    with
    | Ok response -> response
    | Error e -> Alcotest.failf "bad explicit-encryption fixture: %s" e
  in
  let state = Sync_service.create ~user_id:alice () in
  let initial =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        (Printf.sprintf
           {|{"next_batch":"joined","rooms":{"join":{"!transition:example.org":
             {"state":{"events":[%s]},"timeline":{"events":[]}}}}}|}
           (Jsont_bytesrw.encode_string Event.Raw_event.jsont joined
           |> Result.get_ok))
    with
    | Ok response -> response
    | Error e -> Alcotest.failf "bad joined fixture: %s" e
  in
  let state, _ =
    Sync_service.apply ~coverage:Sync_service.complete_state_coverage state
      initial
  in
  check_bool "joined full state establishes encryption completeness" true
    (room_of state "!transition:example.org").encryption_state_complete;
  let state, _ =
    Sync_service.apply ~coverage:Sync_service.unknown_state_coverage state leave
  in
  check_bool "leave invalidates encryption completeness" false
    (room_of state "!transition:example.org").encryption_state_complete;
  let state, _ =
    Sync_service.apply ~coverage:Sync_service.unknown_state_coverage state
      rejoin
  in
  check_bool "unknown rejoin does not resurrect encryption completeness" false
    (room_of state "!transition:example.org").encryption_state_complete;
  let state, _ =
    Sync_service.apply ~coverage:Sync_service.unknown_state_coverage state
      explicit_encryption
  in
  check_bool "an explicit encryption event re-establishes presence" true
    (room_of state "!transition:example.org").encryption_state_complete

(* [is_dm] is derived from the current [m.direct], not latched permanently
   true the first time it is seen: a room already known un-marks when a
   later response's [m.direct] no longer lists it. *)
let test_is_dm_tracks_m_direct () =
  let mark_dm =
    {|{
  "next_batch": "d1",
  "account_data": {"events":[
    {"type":"m.direct","content":{"@bob:example.org":["!dm2:example.org"]}}
  ]},
  "rooms": {"join": {"!dm2:example.org": {"state": {"events":[]},
                                           "timeline": {"events":[]}}}}
}|}
  in
  let unmark_dm =
    {|{
  "next_batch": "d2",
  "account_data": {"events":[
    {"type":"m.direct","content":{}}
  ]},
  "rooms": {"join": {"!dm2:example.org": {"state": {"events":[]},
                                           "timeline": {"events":[]}}}}
}|}
  in
  let _, fetch =
    mock_seq [ Fetch_mock.respond mark_dm; Fetch_mock.respond unmark_dm ]
  in
  let client = client_of fetch in
  let state =
    Sync_service.create ~user_id:alice ~display_name:"Alice"
      ~ruleset:sync_ruleset ()
  in
  let step state =
    let params =
      { Sync.default_params with since = Sync_service.next_batch state }
    in
    match Sync.sync_once client ~params () with
    | Error e -> Alcotest.failf "sync failed: %s" (Error.to_string e)
    | Ok response -> fst (Sync_service.apply state response)
  in
  let state = step state in
  check_bool "marked as a dm" true (room_of state "!dm2:example.org").is_dm;
  let state = step state in
  check_bool "un-marked once m.direct no longer lists it" false
    (room_of state "!dm2:example.org").is_dm

let test_hero_profile_tracks_member_state_and_refresh () =
  let room_id = rid "!hero-profile:example.org" in
  let decode label json =
    match Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont json with
    | Ok response -> response
    | Error error -> Alcotest.failf "bad %s response: %s" label error
  in
  let initial =
    decode "initial hero profile"
      {|{
  "next_batch":"hero-1",
  "rooms":{"join":{"!hero-profile:example.org":{
    "summary":{"m.heroes":["@bob:example.org"],"m.joined_member_count":2},
    "state":{"events":[{
      "type":"m.room.member","state_key":"@bob:example.org",
      "sender":"@bob:example.org","event_id":"$hero-1:example.org",
      "origin_server_ts":1,
      "content":{"membership":"join","displayname":"Bob","avatar_url":"mxc://example.org/old"}
    }]},
    "timeline":{"events":[]}
  }}}
}|}
  in
  let updated =
    decode "updated hero profile"
      {|{
  "next_batch":"hero-2",
  "rooms":{"join":{"!hero-profile:example.org":{
    "state":{"events":[{
      "type":"m.room.member","state_key":"@bob:example.org",
      "sender":"@bob:example.org","event_id":"$hero-2:example.org",
      "origin_server_ts":2,
      "content":{"membership":"join","displayname":"Bobby","avatar_url":"mxc://example.org/new"}
    }]},
    "timeline":{"events":[]}
  }}}
}|}
  in
  let hero state =
    match (room_of state "!hero-profile:example.org").heroes with
    | [ hero ] -> hero
    | heroes -> Alcotest.failf "expected one hero, got %d" (List.length heroes)
  in
  let check_profile label expected_name expected_avatar state =
    let hero = hero state in
    Alcotest.(check (option string))
      (label ^ " display name") expected_name hero.display_name;
    Alcotest.(check (option string))
      (label ^ " avatar") expected_avatar hero.avatar_url
  in
  let state = Sync_service.create ~user_id:alice () in
  let state, _ =
    Sync_service.apply ~coverage:Sync_service.complete_state_coverage state
      initial
  in
  check_profile "initial member state" (Some "Bob")
    (Some "mxc://example.org/old") state;
  let state, _ = Sync_service.apply state updated in
  check_profile "incremental member state" (Some "Bobby")
    (Some "mxc://example.org/new") state;
  let refreshed =
    Sync_service.replace_members state room_id
      [
        {
          Rooms.user_id = uid "@bob:example.org";
          display_name = Some "Robert";
          avatar_url =
            Some
              (Result.get_ok
                 (Media.Mxc.of_string "mxc://example.org/refreshed"));
          membership = Event.Membership.Join;
        };
      ]
  in
  check_profile "/members refresh" (Some "Robert")
    (Some "mxc://example.org/refreshed") refreshed;
  let cleared =
    Sync_service.replace_members refreshed room_id
      [
        {
          Rooms.user_id = uid "@bob:example.org";
          display_name = None;
          avatar_url = None;
          membership = Event.Membership.Join;
        };
      ]
  in
  check_profile "/members field removal" None None cleared

let marked_unread_response next_batch events =
  let events = String.concat "," events in
  match
    Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
      (Printf.sprintf
         {|{"next_batch":%S,"rooms":{"join":{"!marked:example.org":
            {"account_data":{"events":[%s]},"timeline":{"events":[]}}}}}|}
         next_batch events)
  with
  | Ok response -> response
  | Error e -> Alcotest.failf "bad marked-unread fixture: %s" e

let marked_event type_ unread =
  Printf.sprintf {|{"type":%S,"content":{"unread":%b}}|} type_ unread

let apply_marked state next_batch events =
  Sync_service.apply state (marked_unread_response next_batch events) |> fst

let test_marked_unread_source_precedence () =
  let state = Sync_service.create ~user_id:alice () in
  let state =
    apply_marked state "unstable"
      [ marked_event "com.famedly.marked_unread" true ]
  in
  let room = room_of state "!marked:example.org" in
  check_bool "unstable marker is applied" true room.marked_unread;
  check_bool "initial marker source is unstable" true
    (room.marked_unread_source = Store.Unstable);
  let state =
    apply_marked state "stable" [ marked_event "m.marked_unread" false ]
  in
  let room = room_of state "!marked:example.org" in
  check_bool "stable marker is applied" false room.marked_unread;
  check_bool "stable source is recorded" true
    (room.marked_unread_source = Store.Stable);
  let state =
    apply_marked state "unstable-again"
      [ marked_event "com.famedly.marked_unread" true ]
  in
  let room = room_of state "!marked:example.org" in
  check_bool "a later unstable marker is ignored" false room.marked_unread;
  check_bool "stable source remains recorded" true
    (room.marked_unread_source = Store.Stable);
  let state = Sync_service.create ~user_id:alice () in
  let state =
    apply_marked state "same-batch"
      [
        marked_event "com.famedly.marked_unread" true;
        marked_event "m.marked_unread" false;
      ]
  in
  let room = room_of state "!marked:example.org" in
  check_bool "stable marker wins within one response" false room.marked_unread;
  check_bool "same-batch source is stable" true
    (room.marked_unread_source = Store.Stable)

let test_threaded_receipt_preserves_marked_unread () =
  let state = Sync_service.create ~user_id:alice () in
  let state =
    apply_marked state "marked-thread" [ marked_event "m.marked_unread" true ]
  in
  let receipt_response ~batch ~event_id ~body =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        (Printf.sprintf
           {|{"next_batch":%S,"rooms":{"join":{"!marked:example.org":{"ephemeral":{"events":[{"type":"m.receipt","content":{%S:{"m.read":{"@alice:example.org":%s}}}}]},"timeline":{"events":[]}}}}}|}
           batch event_id body)
    with
    | Ok response -> response
    | Error error -> Alcotest.failf "bad receipt response: %s" error
  in
  let state, _ =
    Sync_service.apply state
      (receipt_response ~batch:"thread-receipt" ~event_id:"$reply:example.org"
         ~body:{|{"ts":10,"thread_id":"$root:example.org"}|})
  in
  let room = room_of state "!marked:example.org" in
  check_bool "a threaded receipt preserves marked unread" true
    room.marked_unread;
  let receipts = Sync_service.receipts state (rid "!marked:example.org") in
  check_bool "threaded receipt did not advance main receipt" true
    (Read_state.public_read receipts = None);
  check_string "threaded receipt advanced its own horizon" "$reply:example.org"
    (match
       Read_state.thread_public_read receipts
         ~thread_id:(eid "$root:example.org")
     with
    | Some receipt -> Id.Event_id.to_string receipt.event_id
    | None -> "");
  let state, _ =
    Sync_service.apply state
      (receipt_response ~batch:"main-receipt" ~event_id:"$main:example.org"
         ~body:{|{"ts":11}|})
  in
  check_bool "a main receipt clears marked unread" false
    (room_of state "!marked:example.org").marked_unread

let test_marked_unread_source_survives_restart () =
  Eio_main.run @@ fun env ->
  let tmp = Filename.temp_file "matrix-marked-unread" ".d" in
  Unix.unlink tmp;
  Unix.mkdir tmp 0o700;
  let dir = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  let store = Store.on_disk ~dir in
  let state = Sync_service.create ~user_id:alice () in
  let state =
    apply_marked state "stable" [ marked_event "m.marked_unread" false ]
  in
  Sync_service.persist store state;
  (match Store.flush store with
  | Ok () -> ()
  | Error e -> Alcotest.failf "marked-unread flush: %s" (Error.to_string e));
  let reopened = Store.on_disk ~dir in
  let state = Sync_service.of_store reopened ~user_id:alice () in
  let state =
    apply_marked state "unstable-after-restart"
      [ marked_event "com.famedly.marked_unread" true ]
  in
  let room = room_of state "!marked:example.org" in
  check_bool "restart preserves stable precedence" false room.marked_unread;
  check_bool "restart preserves marker source" true
    (room.marked_unread_source = Store.Stable)

let test_presence_reflects_the_last_response_only () =
  let with_presence =
    {|{"next_batch":"p1","presence":{"events":[
        {"type":"m.presence","sender":"@bob:example.org",
         "content":{"presence":"online"}}]}}|}
  in
  let without_presence = {|{"next_batch":"p2"}|} in
  let _, fetch =
    mock_seq
      [ Fetch_mock.respond with_presence; Fetch_mock.respond without_presence ]
  in
  let client = client_of fetch in
  let state =
    Sync_service.create ~user_id:alice ~display_name:"Alice"
      ~ruleset:sync_ruleset ()
  in
  let step state =
    let params =
      { Sync.default_params with since = Sync_service.next_batch state }
    in
    match Sync.sync_once client ~params () with
    | Error e -> Alcotest.failf "sync failed: %s" (Error.to_string e)
    | Ok response -> fst (Sync_service.apply state response)
  in
  let state = step state in
  check_int "one presence event after the first response" 1
    (List.length (Sync_service.presence state));
  let state = step state in
  check_int "cleared once a response carries none" 0
    (List.length (Sync_service.presence state))

let test_sync_counts_and_latest_event () =
  let _, state, changes1, changes2 = run_two_syncs () in
  let named = room_of state "!named:example.org" in
  check_int "server notification count" 2 named.notification_count;
  check_int "server highlight count" 0 named.highlight_count;
  check_int "local unread" 1 named.local_unread_count;
  check_int "local notifications" 1 named.local_notification_count;
  check_int "local highlight from the display-name rule" 1
    named.local_highlight_count;
  check_string "latest event is the message, not the redaction"
    "$msg1:example.org"
    (match named.latest_event with
    | Some e -> Option.fold ~none:"" ~some:Id.Event_id.to_string e.event_id
    | None -> "");
  check_bool "last_active_ts advanced" true (named.last_active_ts = 5001L);
  check_string "first batch" "s1" changes1.batch;
  check_string "second batch" "s2" changes2.batch;
  check_int "first sync reports five rooms" 5
    (List.length changes1.room_changes);
  check_int "second sync reports one" 1 (List.length changes2.room_changes);
  let c = List.hd changes2.room_changes in
  check_int "three new timeline events" 3 (List.length c.timeline);
  check_int "the change carries the unread delta" 1 c.unread.highlights;
  check_bool "previous state is reported" true (c.previous <> None)

let test_latest_event_plaintext_policy () =
  Eio_main.run @@ fun env ->
  let room_id = rid "!policy:example.org" in
  let encrypted_event =
    {|{"type":"m.room.encrypted","event_id":"$cipher:example.org","sender":"@bob:example.org","origin_server_ts":42,"content":{"algorithm":"m.megolm.v1.aes-sha2","ciphertext":"opaque"}}|}
  in
  let response =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        (Printf.sprintf
           {|{"next_batch":"policy","rooms":{"join":{"!policy:example.org":{"summary":{"m.joined_member_count":2,"m.invited_member_count":0},"timeline":{"events":[%s]}}}}}|}
           encrypted_event)
    with
    | Ok response -> response
    | Error error -> Alcotest.failf "bad policy response: %s" error
  in
  let sender_key =
    match Ck.Curve25519.Public.of_bytes (String.make 32 '\001') with
    | Ok key -> key
    | Error (`Msg error) -> Alcotest.failf "bad test sender key: %s" error
  in
  let decrypt _room_id _event =
    Ok
      {
        Encryption.decrypted_type = "m.room.message";
        decrypted_content =
          json_of {|{"msgtype":"m.text","body":"secret plaintext"}|};
        decrypted_room_id = room_id;
        decrypted_sender = uid "@bob:example.org";
        decrypted_sender_key = sender_key;
        decrypted_claimed_ed25519 = None;
        decrypted_session_id = Id.Session_id.of_string_exn "policy-session";
        decrypted_message_index = 0;
        decrypted_verification = Encryption.Unknown_device;
      }
  in
  let default_path = Filename.temp_file "matrix-policy-default" ".d" in
  Unix.unlink default_path;
  Unix.mkdir default_path 0o700;
  let default_dir = Eio.Path.(Eio.Stdenv.fs env / default_path) in
  let default_store = Store.on_disk ~dir:default_dir in
  let default_state = Sync_service.of_store default_store ~user_id:alice () in
  let default_state, _ = Sync_service.apply ~decrypt default_state response in
  let default_info = room_of default_state "!policy:example.org" in
  check_string "default latest event remains ciphertext" "m.room.encrypted"
    (match default_info.latest_event with
    | Some event -> Event.Event_type.to_string event.type_
    | None -> "");
  Sync_service.persist default_store default_state;
  (match Store.flush default_store with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "default policy store flush: %s" (Error.to_string error));
  let default_bytes =
    Eio.Path.load Eio.Path.(default_dir / "base_state.json")
  in
  check_bool "default store file has no plaintext" false
    (contains default_bytes "secret plaintext");
  let default_reopened = Store.on_disk ~dir:default_dir in
  let default_reopened_info =
    match Store.find_room default_reopened room_id with
    | Some info -> info
    | None -> Alcotest.fail "default policy room missing after restart"
  in
  check_string "default store restart remains ciphertext" "m.room.encrypted"
    (match default_reopened_info.latest_event with
    | Some event -> Event.Event_type.to_string event.type_
    | None -> "");
  let mismatch_state =
    Sync_service.create ~user_id:alice ~plaintext_policy:Store.Store_plaintext
      ()
  in
  let mismatch_raised =
    try
      Sync_service.persist default_store mismatch_state;
      false
    with Invalid_argument _ -> true
  in
  check_bool "policy mismatch cannot persist plaintext into secure store" true
    mismatch_raised;
  let path = Filename.temp_file "matrix-policy-store" ".d" in
  Unix.unlink path;
  Unix.mkdir path 0o700;
  let dir = Eio.Path.(Eio.Stdenv.fs env / path) in
  let store =
    Store.on_disk_with_policy ~dir ~plaintext_policy:Store.Store_plaintext
  in
  let state = Sync_service.of_store store ~user_id:alice () in
  let state, _ = Sync_service.apply ~decrypt state response in
  Sync_service.persist store state;
  (match Store.flush store with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "policy store flush: %s" (Error.to_string error));
  let opt_in_bytes = Eio.Path.load Eio.Path.(dir / "base_state.json") in
  check_bool "opt-in store file retains plaintext" true
    (contains opt_in_bytes "secret plaintext");
  let reopened =
    Store.on_disk_with_policy ~dir ~plaintext_policy:Store.Store_plaintext
  in
  let restored = Sync_service.of_store reopened ~user_id:alice () in
  let restored_info = room_of restored "!policy:example.org" in
  check_string "opt-in latest event is plaintext after restart" "m.room.message"
    (match restored_info.latest_event with
    | Some event -> Event.Event_type.to_string event.type_
    | None -> "");
  Unix.unlink (Filename.concat default_path "base_state.json");
  Unix.unlink (Filename.concat default_path ".profile.lock");
  Unix.rmdir default_path;
  Unix.unlink (Filename.concat path "base_state.json");
  Unix.unlink (Filename.concat path ".profile.lock");
  Unix.rmdir path

let test_sync_token_threading () =
  let log, state, _, _ = run_two_syncs () in
  let reqs = requests log in
  check_int "two requests" 2 (List.length reqs);
  let first = List.nth reqs 0 and second = List.nth reqs 1 in
  check_bool "the first sync has no since" false (contains first.url "since=");
  check_bool "the second threads next_batch" true
    (contains second.url "since=s1");
  check_string "state token" "s2"
    (Option.value (Sync_service.next_batch state) ~default:"")

let test_sync_hooks () =
  let log, fetch = mock_seq [ Fetch_mock.respond sync1 ] in
  let client = client_of fetch in
  ignore log;
  let hooks = Sync_service.Hooks.create () in
  let seen_events = ref 0 and seen_responses = ref 0 in
  Sync_service.Hooks.on_room_event hooks (fun _ _ -> incr seen_events);
  Sync_service.Hooks.on_response hooks (fun _ _ _ -> incr seen_responses);
  let state =
    Sync_service.create ~user_id:alice ~display_name:"Alice"
      ~ruleset:sync_ruleset ()
  in
  match Sync.sync_once client ~params:Sync.default_params () with
  | Error e -> Alcotest.failf "sync failed: %s" (Error.to_string e)
  | Ok response ->
      let state, changes = Sync_service.apply state response in
      check_int "classic sync has no MSC4262 profile changes" 0
        (List.length changes.profile_changes);
      Sync_service.Hooks.run hooks state response changes;
      check_int "response hook ran once" 1 !seen_responses;
      check_int "no timeline events in the first sync" 0 !seen_events

let queue_of ?store ?media_store () =
  let random =
    Matrix_client.Random.of_source
      (Eio.Flow.string_source (String.make 4096 'k'))
  in
  Send_queue.create ~random ~user_id:alice ?store ?media_store ()

let queue_of_random_bytes bytes =
  let random = Matrix_client.Random.of_source (Eio.Flow.string_source bytes) in
  Send_queue.create ~random ~user_id:alice ()

(* A backend which can fail the local-to-remote transition lets the queue tests
   exercise the durable upload-result hand-off without relying on process
   termination. *)
module Flaky_media_backend = struct
  type t = {
    inner : Media_store.t;
    mutable fail_replace : bool;
    mutable fail_ignore : bool;
    mutable fail_unprotect : bool;
  }

  let retention t = Media_store.retention t.inner
  let set_retention t value = Media_store.set_retention t.inner value

  let add ?ignore_retention ?protected ?owner ?now t key ~data =
    Media_store.add ?ignore_retention ?protected ?owner ?now t.inner key ~data

  let get ~now t key = Media_store.get ~now t.inner key
  let protect t key = Media_store.protect t.inner key

  let unprotect t key =
    if t.fail_unprotect then
      Error (Error.Network_error "injected media unprotect failure")
    else Media_store.unprotect t.inner key

  let is_protected t key = Media_store.is_protected t.inner key

  let set_ignore_retention t key value =
    if t.fail_ignore then
      Error (Error.Network_error "injected media retention-flag failure")
    else Media_store.set_ignore_retention t.inner key value

  let replace_key t ~from_ ~to_ =
    if t.fail_replace then
      Error (Error.Network_error "injected media move failure")
    else Media_store.replace_key t.inner ~from_ ~to_

  let remove t key = Media_store.remove t.inner key
  let remove_uri t uri = Media_store.remove_uri t.inner uri

  let prune_local ~owner ~keep ~older_than t =
    Media_store.prune_local ~owner ~keep ~older_than t.inner

  let clean ~now t = Media_store.clean ~now t.inner
  let last_cleanup t = Media_store.last_cleanup t.inner
  let set_last_cleanup t value = Media_store.set_last_cleanup t.inner value
  let close t = Media_store.close t.inner
end

let flaky_media_store () =
  let backend =
    {
      Flaky_media_backend.inner = Media_store.memory ();
      fail_replace = false;
      fail_ignore = false;
      fail_unprotect = false;
    }
  in
  (backend, Media_store.v (module Flaky_media_backend) backend)

let ok_send = {|{"event_id":"$sent:example.org"}|}

let test_send_queue_success () =
  let log, fetch = mock_seq [ Fetch_mock.respond ok_send ] in
  let client = client_of fetch in
  let q = queue_of () in
  let r =
    Send_queue.send_text q ~room_id:(rid "!r:example.org") ~body:"hello"
  in
  (* Local echo before anything has been sent. *)
  let echo = Send_queue.local_echo q r in
  check_bool "echo has no event id" true (echo.event_id = None);
  check_string "echo sender" "@alice:example.org"
    (Id.User_id.to_string echo.sender);
  check_string "echo carries the txn id" (Send_queue.txn_id r)
    (match echo.unsigned with
    | Some u ->
        Option.fold ~none:"" ~some:Id.Transaction_id.to_string
          (Event.Unsigned.transaction_id u)
    | None -> "");
  check_int "one request queued" 1 (Send_queue.pending_count q);
  (match Send_queue.send_one q client r with
  | Send_queue.Sent_ok e ->
      check_string "event id" "$sent:example.org" (Id.Event_id.to_string e)
  | Send_queue.Retry_in _ -> Alcotest.fail "unexpected retry"
  | Send_queue.Uploaded_ok _ -> Alcotest.fail "unexpected upload"
  | Send_queue.Failed e ->
      Alcotest.failf "unexpected failure: %s" (Error.to_string e));
  check_bool "queue is empty" true (Send_queue.is_empty q);
  let req = List.hd (requests log) in
  check_string "method" "PUT" req.meth;
  check_string "url"
    (Printf.sprintf
       "https://hs.example/_matrix/client/v3/rooms/!r:example.org/send/m.room.message/%s"
       (Send_queue.txn_id r))
    req.url;
  check_string "body" {|{"msgtype":"m.text","body":"hello"}|}
    (Option.value req.body ~default:"")

let test_send_queue_dependencies () =
  let q = queue_of () in
  let room = rid "!r:example.org" in
  let parent = Send_queue.send_text q ~room_id:room ~body:"parent" in
  let child =
    Send_queue.enqueue
      ~depends_on:[ Send_queue.id parent ]
      q ~room_id:room
      (Send_queue.Event
         { event_type = "m.room.message"; content = Jsont.Json.object' [] })
  in
  check_bool "dependent follows ready parent" true
    (Send_queue.next q room = Some parent);
  let fake _ _ = Ok (eid "$parent:example.org") in
  ignore
    (Send_queue.send_one q ~send:fake
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       parent);
  check_bool "dependent becomes ready" true (Send_queue.next q room = Some child);
  check_int "resolved parent" 1
    (List.length (Send_queue.resolved_dependencies child));
  ignore (Send_queue.cancel q child)

let test_send_queue_recursive_cancel () =
  let q = queue_of () in
  let room = rid "!r:example.org" in
  let a = Send_queue.send_text q ~room_id:room ~body:"a" in
  let b =
    Send_queue.enqueue
      ~depends_on:[ Send_queue.id a ]
      q ~room_id:room
      (Send_queue.Event
         { event_type = "m.room.message"; content = Jsont.Json.object' [] })
  in
  let c =
    Send_queue.enqueue
      ~depends_on:[ Send_queue.id b ]
      q ~room_id:room
      (Send_queue.Event
         { event_type = "m.room.message"; content = Jsont.Json.object' [] })
  in
  ignore (Send_queue.cancel q a);
  check_bool "cancel propagates" true
    (Send_queue.status b = Send_queue.Cancelled
    && Send_queue.status c = Send_queue.Cancelled)

(* A send callback can yield to cancellation before it returns its response.
   The queue must turn that race into one durable redaction, rather than
   losing the intent or redacting twice. *)
let test_send_queue_cancel_in_flight_redaction () =
  let store = Store.memory () and room = rid "!r:example.org" in
  let q = queue_of ~store () in
  let r = Send_queue.send_text q ~room_id:room ~body:"secret" in
  let parent_event = eid "$parent:example.org" in
  let first_txn = Send_queue.txn_id r in
  let callback_calls = ref 0 in
  let before_restart = ref None in
  let send _ request =
    incr callback_calls;
    check_bool "callback sees sending request" true
      (Send_queue.status request = Send_queue.Sending);
    check_bool "first cancellation accepted" true
      (Send_queue.cancel_with_reason ~reason:"mistake" q request = `In_flight);
    check_bool "repeated cancellation is accepted but not duplicated" true
      (Send_queue.cancel_with_reason ~reason:"different" q request = `In_flight);
    before_restart := Some (queue_of ~store ());
    Ok parent_event
  in
  (match
     Send_queue.send_one q ~send
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       r
   with
  | Send_queue.Retry_in 0. -> ()
  | _ -> Alcotest.fail "a raced send should leave one redaction pending");
  check_int "only one attempt so far" 1 !callback_calls;
  let redaction_txn = Send_queue.txn_id r in
  check_bool "redaction has a new stable transaction id" true
    (redaction_txn <> first_txn);
  check_bool "request became a redaction" true
    (match Send_queue.kind r with
    | Send_queue.Redaction { event_id = target; reason = Some "mistake" } ->
        target = parent_event
    | _ -> false);
  let restored = queue_of ~store () in
  let rr =
    match Send_queue.requests restored with
    | [ request ] -> request
    | _ -> Alcotest.fail "expected the compensating redaction after restart"
  in
  check_string "redaction transaction survives restart" redaction_txn
    (Send_queue.txn_id rr);
  check_bool "reason survives restart" true
    (match Send_queue.kind rr with
    | Send_queue.Redaction { reason = Some "mistake"; _ } -> true
    | _ -> false);
  let before = Option.get !before_restart in
  let before_request =
    match Send_queue.requests before with
    | [ request ] -> request
    | _ ->
        Alcotest.fail "expected the in-flight intent before parent resolution"
  in
  check_bool "restart before resolution keeps the parent local" true
    (match Send_queue.kind before_request with
    | Send_queue.Event _ ->
        Send_queue.status before_request = Send_queue.Pending
    | _ -> false);
  (match
     Send_queue.send_one before
       ~send:(fun _ _ -> Ok (eid "$parent-again:example.org"))
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       before_request
   with
  | Send_queue.Retry_in 0. -> ()
  | _ -> Alcotest.fail "restarted in-flight intent should resolve to redaction");
  check_string "pre-resolution restart keeps redaction transaction"
    redaction_txn
    (Send_queue.txn_id before_request);
  (match
     Send_queue.send_one restored
       ~send:(fun _ _ -> Ok (eid "$redaction:example.org"))
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       rr
   with
  | Send_queue.Sent_ok _ -> ()
  | Send_queue.Uploaded_ok _ -> Alcotest.fail "unexpected upload"
  | _ -> Alcotest.fail "the persisted redaction should send");
  check_bool "redaction was removed after success" true
    (Send_queue.is_empty restored)

let test_send_queue_cancel_in_flight_failure () =
  let store = Store.memory () and room = rid "!r:example.org" in
  let q = queue_of ~store () in
  let r = Send_queue.send_text q ~room_id:room ~body:"secret" in
  let child =
    Send_queue.enqueue
      ~depends_on:[ Send_queue.id r ]
      q ~room_id:room
      (Send_queue.Event
         { event_type = "m.room.message"; content = Jsont.Json.object' [] })
  in
  let send _ request =
    ignore (Send_queue.cancel_with_reason ~reason:"mistake" q request);
    Error (Error.Network_error "lost")
  in
  (match
     Send_queue.send_one q ~send
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       r
   with
  | Send_queue.Failed _ -> ()
  | _ -> Alcotest.fail "a cancelled failed attempt should not retry");
  check_bool "failed parent is removed" true
    (Send_queue.status r = Send_queue.Cancelled);
  check_bool "failed parent's dependent is removed" true
    (Send_queue.status child = Send_queue.Cancelled);
  check_bool "restart does not resurrect local cancellation" true
    (Send_queue.is_empty (queue_of ~store ()))

let test_send_queue_existing_cancel_txn_does_not_draw_randomness () =
  (* The first 16 bytes create the request transaction and the next 16 bytes
     create its cancellation transaction. Any eager fallback allocation after
     that would hit EOF even though the existing cancellation id is sufficient. *)
  let q = queue_of_random_bytes (String.make 32 'r') in
  let room = rid "!r:example.org" in
  let request = Send_queue.send_text q ~room_id:room ~body:"secret" in
  let original_txn = Send_queue.txn_id request in
  (match
     Send_queue.send_one q
       ~send:(fun _ request ->
         check_bool "cancellation intent is recorded" true
           (Send_queue.cancel_with_reason ~reason:"mistake" q request
           = `In_flight);
         Ok (eid "$sent-before-cancel:example.org"))
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       request
   with
  | Send_queue.Retry_in 0. -> ()
  | _ -> Alcotest.fail "existing cancellation id should resolve without a draw");
  check_bool "compensating redaction has a distinct transaction" true
    (Send_queue.txn_id request <> original_txn)

let test_send_queue_cancel_random_failure_is_atomic () =
  (* Queue creation consumes the only available transaction id. Cancellation
     then fails before its intent is mutated, and the original exception must
     propagate while leaving the request retryable. *)
  let q = queue_of_random_bytes (String.make 16 'r') in
  let request =
    Send_queue.send_text q ~room_id:(rid "!r:example.org") ~body:"secret"
  in
  let raised =
    try
      ignore
        (Send_queue.send_one q
           ~send:(fun _ request ->
             ignore (Send_queue.cancel_with_reason ~reason:"mistake" q request);
             Ok (eid "$unreachable:example.org"))
           (client_of (Fetch_mock.client (fun _ -> assert false)))
           request);
      false
    with End_of_file -> true
  in
  check_bool "random cancellation failure propagates" true raised;
  check_bool "random cancellation failure restores Pending" true
    (Send_queue.status request = Send_queue.Pending);
  check_bool "request remains queued after random cancellation failure" true
    (List.exists
       (fun candidate -> Send_queue.id candidate = Send_queue.id request)
       (Send_queue.requests q))

let test_send_queue_retry_random_failure_is_atomic () =
  (* The request consumes the only random bytes. A retryable transport error
     therefore fails while computing jitter, not while making the request. *)
  let q = queue_of_random_bytes (String.make 16 'r') in
  let room = rid "!r:example.org" in
  let request = Send_queue.send_text q ~room_id:room ~body:"retry" in
  let raised =
    try
      ignore
        (Send_queue.send_one q
           ~send:(fun _ _ -> Error (Error.Network_error "offline"))
           (client_of (Fetch_mock.client (fun _ -> assert false)))
           request);
      false
    with End_of_file -> true
  in
  check_bool "retry jitter random failure propagates" true raised;
  check_bool "retry jitter random failure restores Pending" true
    (Send_queue.status request = Send_queue.Pending);
  check_bool "retryable request remains next" true
    (Send_queue.next q room = Some request)

let test_send_queue_repairs_missing_cancel_txn () =
  (* Build the fixture's parent transaction with the same deterministic random
     source used by [queue_of].  A repair that allocates before accounting for
     the current persisted record would reuse this exact id. *)
  let parent_txn =
    Send_queue.txn_id
      (Send_queue.send_text (queue_of ()) ~room_id:(rid "!r:example.org")
         ~body:"legacy")
  in
  let store = Store.memory () in
  let raw_slot = Store.Slot.v ~name:"send_queue" Matrix_proto.Json.Codec.json in
  let raw =
    Printf.sprintf
      {|[{"id":"1","room_id":"!r:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"legacy"}},"txn_id":%S,"created_at":"0","attempts":"0","wedged":false,"dependencies":[],"resolved":[],"cancel_requested":true,"cancel_reason":"legacy reason"}]|}
      parent_txn
  in
  (match Store.Slot.set store raw_slot (json_of raw) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "legacy queue fixture: %s" (Error.to_string e));
  let q = queue_of ~store () in
  let request =
    match Send_queue.requests q with
    | [ request ] -> request
    | _ -> Alcotest.fail "expected one repaired legacy request"
  in
  (match
     Send_queue.send_one q
       ~send:(fun _ _ -> Ok (eid "$legacy-parent:example.org"))
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       request
   with
  | Send_queue.Retry_in 0. -> ()
  | _ -> Alcotest.fail "legacy cancellation should resolve to a redaction");
  check_bool "repaired cancellation id differs from parent" true
    (Send_queue.txn_id request <> parent_txn);
  let restored = queue_of ~store () in
  let repaired =
    match Send_queue.requests restored with
    | [ request ] -> request
    | _ -> Alcotest.fail "expected repaired redaction after restart"
  in
  check_string "repaired cancellation id persists"
    (Send_queue.txn_id request)
    (Send_queue.txn_id repaired)

let test_send_queue_dependency_validation () =
  let q = queue_of () and room = rid "!r:example.org" in
  let p = Send_queue.send_text q ~room_id:room ~body:"p" in
  let invalid f =
    try
      f ();
      false
    with Invalid_argument _ -> true
  in
  let empty =
    Send_queue.Event
      { event_type = "m.room.message"; content = Jsont.Json.object' [] }
  in
  check_bool "duplicate" true
    (invalid (fun () ->
         ignore
           (Send_queue.enqueue
              ~depends_on:[ Send_queue.id p; Send_queue.id p ]
              q ~room_id:room empty)));
  check_bool "missing" true
    (invalid (fun () ->
         ignore (Send_queue.enqueue ~depends_on:[ 9999 ] q ~room_id:room empty)));
  check_bool "cross-room" true
    (invalid (fun () ->
         ignore
           (Send_queue.enqueue
              ~depends_on:[ Send_queue.id p ]
              q ~room_id:(rid "!other:example.org") empty)))

let test_send_queue_retry_wedge_blocks () =
  let q = queue_of () and room = rid "!r:example.org" in
  let p = Send_queue.send_text q ~room_id:room ~body:"p" in
  ignore
    (Send_queue.enqueue
       ~depends_on:[ Send_queue.id p ]
       q ~room_id:room
       (Send_queue.Event
          { event_type = "m.room.message"; content = Jsont.Json.object' [] }));
  let c = client_of (Fetch_mock.client (fun _ -> assert false)) in
  ignore
    (Send_queue.send_one q
       ~send:(fun _ _ -> Error (Error.Network_error "offline"))
       c p);
  check_bool "retry blocks" true (Send_queue.next q room = Some p);
  ignore
    (Send_queue.send_one q
       ~send:(fun _ _ -> Error (Error.Policy_denied "denied"))
       c p);
  check_bool "wedge blocks" true (Send_queue.next q room = None)

let test_send_queue_resolved_reload () =
  let store = Store.memory () and room = rid "!r:example.org" in
  let q = queue_of ~store () in
  let p = Send_queue.send_text q ~room_id:room ~body:"p" in
  ignore
    (Send_queue.enqueue
       ~depends_on:[ Send_queue.id p ]
       q ~room_id:room
       (Send_queue.Event
          { event_type = "m.room.message"; content = Jsont.Json.object' [] }));
  let c = client_of (Fetch_mock.client (fun _ -> assert false)) in
  ignore
    (Send_queue.send_one q
       ~send:(fun _ _ -> Ok (eid "$resolved:example.org"))
       c p);
  let child = List.hd (Send_queue.requests (queue_of ~store ())) in
  check_bool "resolved reload" true
    (Send_queue.dependencies child = []
    && Send_queue.resolved_dependencies child
       = [ (Send_queue.id p, eid "$resolved:example.org") ])

let test_send_queue_typed_dependency_result_reload () =
  let store = Store.memory () in
  let random =
    Matrix_client.Random.of_source
      (Eio.Flow.string_source (String.make 4096 'u'))
  in
  let encrypted =
    Matrix_client.Encrypted_attachment.encrypt ~random "ciphertext source"
  in
  let metadata =
    Matrix_client.Encrypted_attachment.Metadata.to_json_string
      encrypted.metadata
  in
  let result_json =
    Printf.sprintf
      {|{"kind":"encrypted","mxc":"mxc://hs.example/uploaded","metadata":%s}|}
      metadata
  in
  let clear_result_json =
    {|{"kind":"clear","mxc":"mxc://hs.example/clear-upload"}|}
  in
  let raw =
    Printf.sprintf
      {|[{"id":1,"room_id":"!r:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{}},"txn_id":"txn","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[{"id":7,"result":%s},{"id":8,"result":%s},{"id":9,"result":{"kind":"unknown","mxc":"mxc://hs.example/bad"}},{"id":10,"event_id":"$legacy:example.org"}],"cancel_requested":false},{"id":2,"room_id":"!r:example.org","kind":{"tag":"upload","upload_role":"original","upload_content_type":"application/octet-stream"},"txn_id":"bad-upload","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false}]|}
      clear_result_json result_json
  in
  let raw_slot = Store.Slot.v ~name:"send_queue" Matrix_proto.Json.Codec.json in
  (match Store.Slot.set store raw_slot (json_of raw) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "typed result fixture: %s" (Error.to_string e));
  let q = queue_of ~store () in
  let request =
    match Send_queue.requests q with
    | [ request ] -> request
    | _ -> Alcotest.fail "malformed upload state should be dropped"
  in
  let results = Send_queue.dependency_results request in
  check_int "valid typed and legacy results survive" 3 (List.length results);
  check_bool "clear result is typed" true
    (match List.assoc_opt 7 results with
    | Some (Send_queue.Upload (Send_queue.Clear_upload { mxc })) ->
        Matrix_client.Media.Mxc.to_string mxc = "mxc://hs.example/clear-upload"
    | _ -> false);
  check_bool "encrypted result is typed" true
    (match List.assoc_opt 8 results with
    | Some
        (Send_queue.Upload
           (Send_queue.Encrypted_upload { mxc; metadata = restored })) ->
        Matrix_client.Media.Mxc.to_string mxc = "mxc://hs.example/uploaded"
        && Matrix_client.Encrypted_attachment.Metadata.key restored
           = Matrix_client.Encrypted_attachment.Metadata.key encrypted.metadata
    | _ -> false);
  check_bool "malformed result is filtered from event compatibility view" true
    (List.assoc_opt 9 results = None);
  check_bool "legacy event result remains available" true
    (List.assoc_opt 10 results
    = Some (Send_queue.Event_id (eid "$legacy:example.org")));
  Send_queue.save q;
  let restarted = queue_of ~store () in
  let request =
    match Send_queue.requests restarted with
    | [ request ] -> request
    | _ ->
        Alcotest.fail "malformed upload state should stay dropped after restart"
  in
  check_int "typed result survives a second restart" 3
    (List.length (Send_queue.dependency_results request));
  check_int "event-only compatibility projection excludes upload" 1
    (List.length (Send_queue.resolved_dependencies request))

let test_send_queue_rejects_non_object_content () =
  let queue = queue_of () in
  Alcotest.check_raises "content is rejected before enqueue"
    (Invalid_argument
       "Matrix_client.Json_codec.merge_extra_content: object expected")
    (fun () ->
      ignore
        (Send_queue.send_message queue ~room_id:(rid "!r:example.org")
           ~event_type:"m.room.message" ~content:(json_of "[]")));
  check_bool "a rejected event leaves no queue state" true
    (Send_queue.is_empty queue)

let test_send_queue_malformed_persisted_graph_is_quarantined () =
  let store = Store.memory () in
  let media_store = Media_store.memory () in
  let raw_slot = Store.Slot.v ~name:"send_queue" Matrix_proto.Json.Codec.json in
  (* Invalid content, extension data and dependency edges drop the affected
     record and its descendants. An invalid attachment additionally drops its
     upload parents so bytes cannot be sent without their visible event. *)
  let raw =
    {|[
      {"id":1,"room_id":"!r:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"kept"}},"txn_id":"safe","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"extra_content":{},"cancel_requested":false},
      {"id":2,"room_id":"!r:example.org","kind":{"tag":"upload","upload_role":"original","upload_content_type":"application/octet-stream","upload_data_base64":"Ynl0ZXM="},"txn_id":"upload","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":3,"room_id":"!r:example.org","kind":{"tag":"attachment","content":[],"attachment_original":2},"txn_id":"bad-attachment","created_at":0,"attempts":0,"wedged":false,"dependencies":[2],"resolved":[],"cancel_requested":false},
      {"id":4,"room_id":"!r:example.org","kind":{"tag":"event","event_type":"m.room.message","content":[]},"txn_id":"bad-event","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"extra_content":{},"cancel_requested":false},
      {"id":5,"room_id":"!r:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"dependent"}},"txn_id":"dependent","created_at":0,"attempts":0,"wedged":false,"dependencies":[4],"resolved":[],"cancel_requested":false},
      {"id":6,"room_id":"!r:example.org","kind":{"tag":"upload","upload_role":"original","upload_content_type":"application/octet-stream","upload_data_base64":"b3RoZXI="},"txn_id":"orphaned-upload","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":7,"room_id":"!r:example.org","kind":{"tag":"attachment","content":{"msgtype":"m.file","body":"missing edge"},"attachment_original":6},"txn_id":"missing-edge","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":8,"room_id":"!r:example.org","kind":{"tag":"future-kind","event_type":"m.room.message","content":{"msgtype":"m.text","body":"must not downgrade"}},"txn_id":"unknown-kind","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":9,"room_id":"!r:example.org","kind":{"tag":"reaction","key":"+1"},"txn_id":"incomplete-reaction","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":10,"room_id":"!r:example.org","kind":{"tag":"redaction"},"txn_id":"incomplete-redaction","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":11,"room_id":"!r:example.org","kind":{"tag":"attachment","content":[],"attachment_original":12},"txn_id":"forward-child","created_at":0,"attempts":0,"wedged":false,"dependencies":[12],"resolved":[],"cancel_requested":false},
      {"id":12,"room_id":"!r:example.org","kind":{"tag":"upload","upload_role":"original","upload_content_type":"application/octet-stream","upload_data_base64":"Zm9yd2FyZA"},"txn_id":"forward-upload","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":13,"room_id":"!other:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"cross-room edge"}},"txn_id":"cross-room","created_at":0,"attempts":0,"wedged":false,"dependencies":[4],"resolved":[],"cancel_requested":false},
      {"id":14,"room_id":"!other:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"other room"}},"txn_id":"other-room","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":15,"room_id":"!r:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"duplicate one"}},"txn_id":"duplicate-one","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":15,"room_id":"!other:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"duplicate two"}},"txn_id":"duplicate-two","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":16,"room_id":"!r:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"bad extension"}},"txn_id":"bad-extra","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"extra_content":[],"cancel_requested":false},
      {"id":17,"room_id":"!r:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"extension child"}},"txn_id":"bad-extra-child","created_at":0,"attempts":0,"wedged":false,"dependencies":[16],"resolved":[],"cancel_requested":false},
      {"id":18,"room_id":"!r:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"cache on event"}},"txn_id":"event-cache","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"upload_cache_uri":"mxc://send-queue.localhost/event-cache","cancel_requested":false},
      {"id":19,"room_id":"!r:example.org","kind":{"tag":"reaction","relates_to":"$event:example.org"},"txn_id":"missing-reaction-key","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":20,"room_id":"!r:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"empty cancel transaction"}},"txn_id":"cancel-parent","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":true,"cancel_txn_id":""},
      {"id":21,"room_id":"!r:example.org","kind":{"tag":"upload","upload_role":"original","upload_content_type":"application/octet-stream","upload_data_base64":"ZWRpdA=="},"txn_id":"edit-upload","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":22,"room_id":"!r:example.org","kind":{"tag":"attachment","content":{"msgtype":"m.file","body":"before"},"attachment_original":21},"txn_id":"edit-parent","created_at":0,"attempts":0,"wedged":false,"dependencies":[21],"resolved":[],"pending_edit_content":{"msgtype":"m.file","body":"after"},"pending_edit_txn_id":"","cancel_requested":false},
      {"id":23,"room_id":"!r:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"duplicate transaction one"}},"txn_id":"same-transaction","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":24,"room_id":"!other:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"duplicate transaction two"}},"txn_id":"same-transaction","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":0,"room_id":"!r:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"nonpositive id"}},"txn_id":"zero-id","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false}
    ]|}
  in
  (match Store.Slot.set store raw_slot (json_of raw) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "malformed queue fixture: %s" (Error.to_string e));
  let restored = Send_queue.requests (queue_of ~store ~media_store ()) in
  check_int "only independent valid requests remain" 2 (List.length restored);
  let event = List.find (fun request -> Send_queue.id request = 1) restored in
  check_int "the independent request is retained" 1 (Send_queue.id event);
  check_bool "an unrelated room record is retained" true
    (List.exists (fun request -> Send_queue.id request = 14) restored);
  check_bool "malformed cross-room dependency is not stripped" false
    (List.exists (fun request -> Send_queue.id request = 13) restored);
  check_bool "both copies of a duplicate global id are quarantined" false
    (List.exists (fun request -> Send_queue.id request = 15) restored);
  check_bool "valid extra content remains safe" true
    (match Send_queue.payload event with
    | Send_queue.Send { content; _ } ->
        Matrix_proto.Json.find_string "body" content = Some "kept"
    | Send_queue.Redact _ | Send_queue.Upload_payload _ -> false);
  let absent txn_id =
    let key =
      Media_store.
        { uri = Media_store.local_uri ~txn_id; format = Media_store.File }
    in
    Media_store.get ~now:Ptime.epoch media_store key = Ok None
  in
  check_bool "invalid attachment leaves no cached upload" true (absent "upload");
  check_bool "missing dependency leaves no cached upload" true
    (absent "orphaned-upload");
  check_bool "forward attachment leaves no cached upload" true
    (absent "forward-upload");
  check_bool "invalid edit transaction leaves no cached upload" true
    (absent "edit-upload");

  (* A wrong-typed member in one record must not discard unrelated queue
     entries. The malformed attachment still carries enough identity for its
     upload parent to be quarantined. *)
  let typed_store = Store.memory () in
  let typed_media_store = Media_store.memory () in
  let typed_raw_slot =
    Store.Slot.v ~name:"send_queue" Matrix_proto.Json.Codec.json
  in
  let typed_raw =
    {|[
      {"id":1,"room_id":"!r:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"kept"}},"txn_id":"typed-safe","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":2,"room_id":"!r:example.org","kind":{"tag":"upload","upload_role":"original","upload_content_type":"application/octet-stream","upload_data_base64":"cGFyZW50"},"txn_id":"typed-upload","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":3,"room_id":"!r:example.org","kind":{"tag":"attachment","content":{"msgtype":"m.file","body":"broken"},"attachment_original":2},"txn_id":"typed-attachment","created_at":0,"attempts":0,"wedged":false,"dependencies":[2],"resolved":[{"id":"not-an-integer","event_id":"$event:example.org"}],"cancel_requested":false},
      {"id":4,"room_id":"!r:example.org","kind":"wrong-type","txn_id":"typed-kind","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false}
    ]|}
  in
  (match Store.Slot.set typed_store typed_raw_slot (json_of typed_raw) with
  | Ok () -> ()
  | Error e ->
      Alcotest.failf "typed malformed queue fixture: %s" (Error.to_string e));
  let typed_restored =
    Send_queue.requests
      (queue_of ~store:typed_store ~media_store:typed_media_store ())
  in
  check_bool "wrong-typed records do not discard valid records" true
    (List.exists (fun request -> Send_queue.id request = 1) typed_restored);
  check_bool "wrong-typed attachment parent is quarantined" true
    (not
       (List.exists (fun request -> Send_queue.id request = 2) typed_restored));
  check_bool "wrong-typed records are not restored as sendable" true
    (not
       (List.exists (fun request -> Send_queue.id request = 3) typed_restored));
  check_bool "wrong-type kind record is quarantined" true
    (not
       (List.exists (fun request -> Send_queue.id request = 4) typed_restored))

let test_send_queue_arbitrary_txn_cache_identity () =
  let store = Store.memory () in
  let media_store = Media_store.memory () in
  let raw_slot = Store.Slot.v ~name:"send_queue" Matrix_proto.Json.Codec.json in
  let txn_id = "m/valid transaction" in
  let raw =
    {|[{"id":1,"room_id":"!r:example.org","kind":{"tag":"upload","upload_role":"original","upload_content_type":"application/octet-stream","upload_data_base64":"Ynl0ZXM="},"txn_id":"m/valid transaction","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false}]|}
  in
  (match Store.Slot.set store raw_slot (json_of raw) with
  | Ok () -> ()
  | Error e ->
      Alcotest.failf "arbitrary transaction fixture: %s" (Error.to_string e));
  let queue = queue_of ~store ~media_store () in
  let request =
    match Send_queue.requests queue with
    | [ request ] -> request
    | _ -> Alcotest.fail "arbitrary transaction upload was not restored"
  in
  check_string "wire transaction id is unchanged" txn_id
    (Send_queue.txn_id request);
  let local_key =
    Media_store.
      { uri = Media_store.local_uri ~txn_id; format = Media_store.File }
  in
  check_bool "inline upload imports under its derived local identity" true
    (Media_store.get ~now:Ptime.epoch media_store local_key = Ok (Some "bytes"));
  let seen = ref None in
  let remote =
    Result.get_ok (Media.Mxc.of_string "mxc://hs.example/restored")
  in
  let upload ?on_progress:_ _ request =
    match Send_queue.kind request with
    | Send_queue.Upload_request { data; _ } ->
        seen := Some data;
        Ok (Send_queue.Clear_upload { mxc = remote })
    | _ -> Alcotest.fail "restored request is not an upload"
  in
  (match
     Send_queue.send_one queue ~upload
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       request
   with
  | Send_queue.Uploaded_ok _ -> ()
  | _ -> Alcotest.fail "restored upload did not complete");
  Alcotest.(check (option string))
    "the cache supplies the exact upload bytes" (Some "bytes") !seen

let test_send_queue_allocates_unique_transactions () =
  let queue = queue_of () in
  let room_id = rid "!r:example.org" in
  let first = Send_queue.send_text queue ~room_id ~body:"first" in
  let second = Send_queue.send_text queue ~room_id ~body:"second" in
  check_bool "a repeating injected random source cannot reuse a transaction"
    true
    (Send_queue.txn_id first <> Send_queue.txn_id second)

let test_send_queue_persisted_cache_identity () =
  let cache_key uri = Media_store.{ uri; format = Media_store.File } in
  let add media_store key data =
    match
      Media_store.add ~ignore_retention:true ~protected:true media_store key
        ~data
    with
    | Ok () -> ()
    | Error error -> Alcotest.failf "cache fixture: %s" (Error.to_string error)
  in
  let raw_slot = Store.Slot.v ~name:"send_queue" Matrix_proto.Json.Codec.json in
  let restore raw media_store =
    let store = Store.memory () in
    (match Store.Slot.set store raw_slot (json_of raw) with
    | Ok () -> ()
    | Error error ->
        Alcotest.failf "cache identity fixture: %s" (Error.to_string error));
    queue_of ~store ~media_store ()
  in
  let legacy_uri =
    Result.get_ok (Media.Mxc.of_string "mxc://send-queue.localhost/mlegacy")
  in
  let legacy_store = Media_store.memory () in
  add legacy_store (cache_key legacy_uri) "legacy bytes";
  let legacy =
    restore
      {|[{"id":1,"room_id":"!r:example.org","kind":{"tag":"upload","upload_role":"original","upload_content_type":"application/octet-stream","upload_data_base64":""},"txn_id":"mlegacy","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false,"upload_cache_uri":"mxc://send-queue.localhost/mlegacy"}]|}
      legacy_store
  in
  let legacy_request = List.hd (Send_queue.requests legacy) in
  let seen = ref None in
  let uploaded =
    Result.get_ok (Media.Mxc.of_string "mxc://hs.example/legacy")
  in
  let upload ?on_progress:_ _ request =
    match Send_queue.kind request with
    | Send_queue.Upload_request { data; _ } ->
        seen := Some data;
        Ok (Send_queue.Clear_upload { mxc = uploaded })
    | _ -> Alcotest.fail "legacy cache request is not an upload"
  in
  (match
     Send_queue.send_one legacy ~upload
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       legacy_request
   with
  | Send_queue.Uploaded_ok _ -> ()
  | _ -> Alcotest.fail "bound legacy cache did not upload");
  Alcotest.(check (option string))
    "pre-v2 cache identity remains readable" (Some "legacy bytes") !seen;
  let owner_uri = Media_store.local_uri ~txn_id:"cache-owner" in
  let owner_key = cache_key owner_uri in
  let mismatched_store = Media_store.memory () in
  add mismatched_store owner_key "owner bytes";
  (* A v2 cache media id is itself a legal opaque transaction id. It must not
     be mistaken for a pre-v2 identity and allowed to claim the first
     transaction's bytes. *)
  let claiming_txn = Media.Mxc.media_id owner_uri in
  let raw =
    Printf.sprintf
      {|[{"id":1,"room_id":"!r:example.org","kind":{"tag":"upload","upload_role":"original","upload_content_type":"application/octet-stream","upload_data_base64":"aW5saW5l"},"txn_id":%S,"created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false,"upload_cache_uri":%S}]|}
      claiming_txn
      (Media.Mxc.to_string owner_uri)
  in
  let mismatched = restore raw mismatched_store in
  let request = List.hd (Send_queue.requests mismatched) in
  check_bool "mismatched cache identity wedges the record" true
    (Send_queue.status request = Send_queue.Wedged);
  let called = ref false in
  let upload ?on_progress:_ _ _ =
    called := true;
    Alcotest.fail "mismatched cache bytes reached the upload callback"
  in
  ignore
    (Send_queue.send_one mismatched ~upload
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       request);
  check_bool "mismatched cache is not uploaded" false !called;
  check_bool "the other cache entry is untouched" true
    (Media_store.get ~now:Ptime.epoch mismatched_store owner_key
    = Ok (Some "owner bytes"))

let test_send_queue_upload_node () =
  let store = Store.memory () and room = rid "!r:example.org" in
  let q = queue_of ~store () in
  let upload =
    Send_queue.upload q ~room_id:room ~role:`Original
      ~content_type:"application/octet-stream" ~data:"abcdef" ()
  in
  let child =
    Send_queue.enqueue
      ~depends_on:[ Send_queue.id upload ]
      q ~room_id:room
      (Send_queue.Event
         { event_type = "m.room.message"; content = Jsont.Json.object' [] })
  in
  let progress = ref [] in
  let registered_progress = ref [] in
  let terminal_durable = ref false in
  Send_queue.on_progress q (fun _ p ->
      registered_progress := !registered_progress @ [ p.current_bytes ];
      if p.current_bytes = p.total_bytes then
        let persisted = queue_of ~store () in
        terminal_durable :=
          List.exists
            (fun request ->
              Send_queue.id request = Send_queue.id child
              && Send_queue.dependencies request = [])
            (Send_queue.requests persisted));
  let upload_sender ?on_progress _ _ =
    Option.iter
      (fun report ->
        report 4L;
        report 2L;
        report 6L)
      on_progress;
    Ok
      (Send_queue.Clear_upload
         {
           mxc =
             Result.get_ok
               (Matrix_client.Media.Mxc.of_string "mxc://hs.example/uploaded");
         })
  in
  let client = client_of (Fetch_mock.client (fun _ -> assert false)) in
  (match
     Send_queue.send_one q ~upload:upload_sender
       ~on_progress:(fun _ p -> progress := !progress @ [ p.current_bytes ])
       client upload
   with
  | Send_queue.Uploaded_ok _ -> ()
  | _ -> Alcotest.fail "upload node should resolve with an MXC");
  check_bool "upload was removed after result propagation" true
    ((match Send_queue.status upload with
       | Send_queue.Uploaded _ -> true
       | _ -> false)
    && Send_queue.next q room = Some child);
  check_bool "upload result reaches dependent" true
    (match Send_queue.dependency_results child with
    | [ (_, Send_queue.Upload (Send_queue.Clear_upload { mxc })) ] ->
        Matrix_client.Media.Mxc.to_string mxc = "mxc://hs.example/uploaded"
    | _ -> false);
  check_bool "progress is monotonic and terminal" true
    (!progress = [ 0L; 4L; 6L ]);
  check_bool "registered progress is emitted directly by the core" true
    (!registered_progress = [ 0L; 4L; 6L ]);
  check_bool "terminal progress follows durable result propagation" true
    !terminal_durable;
  let encrypted =
    Matrix_client.Encrypted_attachment.encrypt
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 4096 'e')))
      "secret plaintext"
  in
  let encrypted_request =
    Send_queue.upload_encrypted q ~room_id:room ~role:`Original
      ~content_type:"application/octet-stream" ~encrypted ()
  in
  check_bool "encrypted queue stores ciphertext only" true
    (match Send_queue.kind encrypted_request with
    | Send_queue.Upload_request { data; encrypted_metadata = Some _; _ } ->
        data = encrypted.ciphertext && data <> "secret plaintext"
    | _ -> false);
  let restarted = queue_of ~store () in
  check_bool "encrypted upload survives restart" true
    (List.exists
       (fun request -> Send_queue.id request = Send_queue.id encrypted_request)
       (Send_queue.requests restarted));
  let zero =
    Send_queue.upload q ~room_id:room ~role:`Thumbnail
      ~content_type:"application/octet-stream" ~data:"" ()
  in
  let zero_progress = ref [] in
  let zero_durable = ref false in
  Send_queue.on_progress q (fun request p ->
      if Send_queue.id request = Send_queue.id zero then begin
        zero_progress := !zero_progress @ [ p.current_bytes ];
        if p.current_bytes = p.total_bytes then
          zero_durable :=
            not
              (List.exists
                 (fun request -> Send_queue.id request = Send_queue.id zero)
                 (Send_queue.requests (queue_of ~store ())))
      end);
  (match
     Send_queue.send_one q
       ~upload:(fun ?on_progress _ _ ->
         Option.iter (fun report -> report 0L) on_progress;
         Ok
           (Send_queue.Clear_upload
              {
                mxc =
                  Result.get_ok
                    (Matrix_client.Media.Mxc.of_string "mxc://hs.example/empty");
              }))
       client zero
   with
  | Send_queue.Uploaded_ok _ -> ()
  | _ -> Alcotest.fail "zero-byte upload should resolve");
  check_bool "zero-byte progress is emitted after persistence" true
    (!zero_progress = [ 0L ] && !zero_durable)

let test_send_queue_upload_retry_cancel () =
  let store = Store.memory () and room = rid "!r:example.org" in
  let q = queue_of ~store () in
  let upload =
    Send_queue.upload q ~room_id:room ~role:`Thumbnail ~content_type:"image/png"
      ~data:"png bytes" ()
  in
  let child =
    Send_queue.enqueue
      ~depends_on:[ Send_queue.id upload ]
      q ~room_id:room
      (Send_queue.Event
         { event_type = "m.room.message"; content = Jsont.Json.object' [] })
  in
  let client = client_of (Fetch_mock.client (fun _ -> assert false)) in
  (match
     Send_queue.send_one q
       ~upload:(fun ?on_progress:_ _ _ -> Error (Error.Network_error "offline"))
       client upload
   with
  | Send_queue.Retry_in _ -> ()
  | _ -> Alcotest.fail "a failed upload should remain retryable");
  check_bool "failed upload remains pending" true
    (Send_queue.status upload = Send_queue.Pending);
  let restarted = queue_of ~store () in
  let restored_upload =
    List.find
      (fun request -> Send_queue.id request = Send_queue.id upload)
      (Send_queue.requests restarted)
  in
  let restored_child =
    List.find
      (fun request -> Send_queue.id request = Send_queue.id child)
      (Send_queue.requests restarted)
  in
  check_bool "retry restart retains upload payload" true
    (match Send_queue.kind restored_upload with
    | Send_queue.Upload_request { role = `Thumbnail; data; _ } ->
        data = "png bytes"
    | _ -> false);
  ignore (Send_queue.cancel restarted restored_upload);
  check_bool "cancel removes upload dependants" true
    (Send_queue.status restored_upload = Send_queue.Cancelled
    && Send_queue.status restored_child = Send_queue.Cancelled
    && Send_queue.is_empty restarted
    && Send_queue.is_empty (queue_of ~store ()))

let test_send_queue_media_store_lifecycle () =
  let store = Store.memory () and media_store = Media_store.memory () in
  let room = rid "!r:example.org" in
  let q = queue_of ~store ~media_store () in
  let request =
    Send_queue.upload q ~room_id:room ~role:`Original
      ~content_type:"application/octet-stream" ~data:"cached bytes" ()
  in
  let local_mxc = Media_store.local_uri ~txn_id:(Send_queue.txn_id request) in
  let local_key = Media_store.{ uri = local_mxc; format = File } in
  check_bool "upload is cached before queue persistence" true
    (Media_store.get ~now:Ptime.epoch media_store local_key
    = Ok (Some "cached bytes"));
  check_bool "local upload is protected" true
    (Media_store.is_protected media_store local_key = Ok true);
  let restarted = queue_of ~store ~media_store () in
  let restored =
    List.find
      (fun r -> Send_queue.id r = Send_queue.id request)
      (Send_queue.requests restarted)
  in
  check_bool "cache-backed snapshot elides upload bytes" true
    (match Send_queue.kind restored with
    | Send_queue.Upload_request { data; _ } -> String.equal data ""
    | _ -> false);
  let seen_data = ref None in
  let remote_uri =
    Result.get_ok (Media.Mxc.of_string "mxc://hs.example/remote")
  in
  let upload ?on_progress:_ _ request =
    match Send_queue.kind request with
    | Send_queue.Upload_request { data; _ } ->
        seen_data := Some data;
        Ok (Send_queue.Clear_upload { mxc = remote_uri })
    | _ -> Error (Error.Network_error "not an upload")
  in
  (match
     Send_queue.send_one restarted ~upload
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       restored
   with
  | Send_queue.Uploaded_ok _ -> ()
  | _ -> Alcotest.fail "cache-backed upload should resolve");
  let remote_key = Media_store.{ uri = remote_uri; format = File } in
  check_bool "worker reads exact cached bytes" true
    (!seen_data = Some "cached bytes");
  check_bool "local entry moved to remote" true
    (Media_store.get ~now:Ptime.epoch media_store local_key = Ok None
    && Media_store.get ~now:Ptime.epoch media_store remote_key
       = Ok (Some "cached bytes"));
  check_bool "remote entry is no longer protected" true
    (Media_store.is_protected media_store remote_key = Ok false);
  let missing_media = Media_store.memory () in
  let missing_queue = queue_of ~media_store:missing_media () in
  let missing =
    Send_queue.upload missing_queue ~room_id:room ~role:`Original
      ~content_type:"application/octet-stream" ~data:"missing" ()
  in
  let missing_uri = Media_store.local_uri ~txn_id:(Send_queue.txn_id missing) in
  ignore
    (Media_store.remove missing_media
       Media_store.{ uri = missing_uri; format = File });
  let called = ref false in
  let never_upload ?on_progress:_ _ _ =
    called := true;
    Error (Error.Network_error "must not upload")
  in
  (match
     Send_queue.send_one missing_queue ~upload:never_upload
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       missing
   with
  | Send_queue.Failed (Error.Policy_denied _) -> ()
  | _ -> Alcotest.fail "missing cache should wedge without transport");
  check_bool "missing cache never invokes upload" true (not !called);
  let cancelled_media = Media_store.memory () in
  let cancelled_queue = queue_of ~media_store:cancelled_media () in
  let cancelled =
    Send_queue.upload cancelled_queue ~room_id:room ~role:`Thumbnail
      ~content_type:"image/png" ~data:"thumbnail" ()
  in
  let cancelled_key =
    Media_store.
      {
        uri = Media_store.local_uri ~txn_id:(Send_queue.txn_id cancelled);
        format = File;
      }
  in
  ignore (Send_queue.cancel cancelled_queue cancelled);
  check_bool "cancelling removes local cache" true
    (Media_store.get ~now:Ptime.epoch cancelled_media cancelled_key = Ok None);
  (* A pre-media-store queue record is imported once on restore, retaining the
     same local URI used by a newly enqueued request. *)
  let legacy_store = Store.memory () in
  let legacy_queue = queue_of ~store:legacy_store () in
  let legacy =
    Send_queue.upload legacy_queue ~room_id:room ~role:`Original
      ~content_type:"application/octet-stream" ~data:"legacy bytes" ()
  in
  let imported_media = Media_store.memory () in
  ignore (queue_of ~store:legacy_store ~media_store:imported_media ());
  let imported_key =
    Media_store.
      {
        uri = Media_store.local_uri ~txn_id:(Send_queue.txn_id legacy);
        format = File;
      }
  in
  check_bool "legacy inline upload imports into media store" true
    (Media_store.get ~now:Ptime.epoch imported_media imported_key
    = Ok (Some "legacy bytes"));
  (* A record which claims cache-backed persistence must never fall back to
     its inline field when the cache URI is malformed or points at remote
     media. *)
  let check_invalid_cache_uri label upload_cache_uri =
    let invalid_store = Store.memory () in
    let raw_slot =
      Store.Slot.v ~name:"send_queue" Matrix_proto.Json.Codec.json
    in
    let raw =
      Printf.sprintf
        {|[{"id":1,"room_id":"!r:example.org","kind":{"tag":"upload","upload_role":"original","upload_content_type":"application/octet-stream","upload_data":"must not upload"},"txn_id":"invalid-cache","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false,"upload_cache_uri":%S}]|}
        upload_cache_uri
    in
    (match Store.Slot.set invalid_store raw_slot (json_of raw) with
    | Ok () -> ()
    | Error e -> Alcotest.failf "%s fixture: %s" label (Error.to_string e));
    let invalid_media = Media_store.memory () in
    let invalid_queue =
      queue_of ~store:invalid_store ~media_store:invalid_media ()
    in
    let invalid =
      match Send_queue.requests invalid_queue with
      | [ request ] -> request
      | _ -> Alcotest.failf "%s fixture was not restored" label
    in
    check_bool
      (label ^ " restores wedged")
      true
      (Send_queue.status invalid = Send_queue.Wedged);
    let called = ref false in
    let upload ?on_progress:_ _ _ =
      called := true;
      Error (Error.Network_error "must not upload")
    in
    (match
       Send_queue.send_one invalid_queue ~upload
         (client_of (Fetch_mock.client (fun _ -> assert false)))
         invalid
     with
    | Send_queue.Failed (Error.Policy_denied _) -> ()
    | _ -> Alcotest.failf "%s should restore wedged" label);
    check_bool (label ^ " does not invoke transport") true (not !called)
  in
  check_invalid_cache_uri "malformed cache URI" "not-an-mxc";
  check_invalid_cache_uri "remote cache URI" "mxc://hs.example/mremote"

let test_send_queue_media_orphan_reconcile () =
  Eio_main.run @@ fun env ->
  let root = Filename.temp_file "matrix-send-queue" ".d" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  let dir = Eio.Path.(Eio.Stdenv.fs env / root) in
  let media_path = Filename.concat root "media.sqlite" in
  let owner_for_path path =
    let material = Id.User_id.to_string alice ^ "\000" ^ path in
    "send-queue:" ^ Digestif.SHA256.(digest_string material |> to_hex)
  in
  let owner = owner_for_path root in
  let media () =
    match Sqlite.create_media_store media_path with
    | Ok store -> store
    | Error error ->
        Alcotest.failf "open orphan media store: %s" (Error.to_string error)
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> if Sys.file_exists path then Unix.unlink path)
        [
          media_path;
          media_path ^ "-shm";
          media_path ^ "-wal";
          Filename.concat root "base_state.json";
          Filename.concat root ".profile.lock";
        ];
      if Sys.file_exists root then Unix.rmdir root)
    (fun () ->
      let media_store = media () in
      let store = Store.on_disk ~dir in
      let queue = queue_of ~store ~media_store () in
      let active =
        Send_queue.upload queue
          ~room_id:(rid "!orphan:example.org")
          ~role:`Original ~content_type:"application/octet-stream"
          ~data:"active" ()
      in
      (match Store.flush store with
      | Ok () -> ()
      | Error error -> Alcotest.failf "flush queue: %s" (Error.to_string error));
      let old_time =
        match Ptime.of_float_s (Unix.gettimeofday () -. 86_400. -. 60.) with
        | Some time -> time
        | None -> Alcotest.fail "invalid orphan timestamp"
      in
      let orphan_key =
        Media_store.
          {
            uri = Media_store.local_uri ~txn_id:"crashed-orphan";
            format = File;
          }
      in
      let other_key =
        Media_store.
          { uri = Media_store.local_uri ~txn_id:"other-queue"; format = File }
      in
      let remote_key =
        Media_store.
          {
            uri = Result.get_ok (Media.Mxc.of_string "mxc://hs.example/remote");
            format = File;
          }
      in
      (match
         Media_store.add ~owner ~protected:true ~now:old_time media_store
           orphan_key ~data:"orphan"
       with
      | Ok () -> ()
      | Error error -> Alcotest.failf "add orphan: %s" (Error.to_string error));
      (match
         Media_store.add
           ~owner:(owner_for_path (root ^ ".other"))
           ~protected:true ~now:old_time media_store other_key ~data:"other"
       with
      | Ok () -> ()
      | Error error -> Alcotest.failf "add other: %s" (Error.to_string error));
      (match
         Media_store.add ~owner ~now:old_time media_store remote_key
           ~data:"remote"
       with
      | Ok () -> ()
      | Error error -> Alcotest.failf "add remote: %s" (Error.to_string error));
      Media_store.close media_store;
      let media_store = media () in
      let restored = queue_of ~store:(Store.on_disk ~dir) ~media_store () in
      let active_key =
        Media_store.
          {
            uri = Media_store.local_uri ~txn_id:(Send_queue.txn_id active);
            format = File;
          }
      in
      check_bool "crash orphan is reclaimed" true
        (Media_store.get ~now:Ptime.epoch media_store orphan_key = Ok None);
      check_bool "restored active upload is kept" true
        (Media_store.get ~now:Ptime.epoch media_store active_key
        = Ok (Some "active"));
      check_bool "other queue owner is untouched" true
        (Media_store.get ~now:Ptime.epoch media_store other_key
        = Ok (Some "other"));
      check_bool "remote media is untouched" true
        (Media_store.get ~now:Ptime.epoch media_store remote_key
        = Ok (Some "remote"));
      ignore restored;
      Media_store.close media_store)

let test_send_queue_media_store_cancel_after_upload () =
  let media_store = Media_store.memory () and room = rid "!r:example.org" in
  let q = queue_of ~media_store () in
  let request =
    Send_queue.upload q ~room_id:room ~role:`Original
      ~content_type:"application/octet-stream" ~data:"cancel race" ()
  in
  let remote_uri =
    Result.get_ok (Media.Mxc.of_string "mxc://hs.example/cancelled")
  in
  let upload ?on_progress:_ _ request =
    ignore (Send_queue.cancel q request);
    Ok (Send_queue.Clear_upload { mxc = remote_uri })
  in
  (match
     Send_queue.send_one q ~upload
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       request
   with
  | Send_queue.Failed _ -> ()
  | _ -> Alcotest.fail "cancelled upload should report failure");
  let key = Media_store.{ uri = remote_uri; format = File } in
  check_bool "successful remote media survives cancellation" true
    (Media_store.get ~now:Ptime.epoch media_store key = Ok (Some "cancel race"));
  check_bool "cancelled upload leaves no queue request" true
    (Send_queue.is_empty q)

let test_send_queue_upload_result_restart_boundary () =
  let store = Store.memory () and room = rid "!r:example.org" in
  let backend, media_store = flaky_media_store () in
  let encrypted =
    Matrix_client.Encrypted_attachment.encrypt
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 4096 'd')))
      "durable bytes"
  in
  let q = queue_of ~store ~media_store () in
  let request =
    Send_queue.upload_encrypted q ~room_id:room ~role:`Thumbnail
      ~content_type:"image/png" ~encrypted ()
  in
  let child =
    Send_queue.enqueue
      ~depends_on:[ Send_queue.id request ]
      q ~room_id:room
      (Send_queue.Event
         { event_type = "m.room.message"; content = Jsont.Json.object' [] })
  in
  let remote_uri =
    Result.get_ok (Media.Mxc.of_string "mxc://hs.example/durable")
  in
  let calls = ref 0 in
  let expected_metadata = encrypted.Attachment.metadata in
  let upload ?on_progress:_ _ _ =
    incr calls;
    Ok
      (Send_queue.Encrypted_upload
         { mxc = remote_uri; metadata = expected_metadata })
  in
  backend.fail_replace <- true;
  (match
     Send_queue.send_one q ~upload
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       request
   with
  | Send_queue.Failed (Error.Network_error _) -> ()
  | _ -> Alcotest.fail "cache transition failure should wedge the upload");
  check_int "transport called once before transition failure" 1 !calls;
  check_bool "transition failure wedges the live request" true
    (Send_queue.status request = Send_queue.Wedged);

  (* Recreating the queue is the crash boundary.  The persisted upload result
     must keep the request recoverable.  Move the cache entry first to model a
     crash after [replace_key] but before the final queue save. *)
  let local_key =
    Media_store.
      {
        uri = Media_store.local_uri ~txn_id:(Send_queue.txn_id request);
        format = File;
      }
  in
  let remote_key = Media_store.{ uri = remote_uri; format = File } in
  backend.fail_replace <- false;
  (match
     Media_store.replace_key media_store ~from_:local_key ~to_:remote_key
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "simulate completed cache move: %s" (Error.to_string error));
  let restarted = queue_of ~store ~media_store () in
  let restored =
    match
      List.find_opt
        (fun candidate -> Send_queue.id candidate = Send_queue.id request)
        (Send_queue.requests restarted)
    with
    | Some request -> request
    | None -> Alcotest.fail "failed transition lost the queued upload"
  in
  check_bool "restart retains wedged transitional request" true
    (Send_queue.status restored = Send_queue.Wedged);
  Alcotest.(check unit)
    "unwedge is available" ()
    (Send_queue.unwedge restarted restored);
  (match
     Send_queue.send_one restarted ~upload
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       restored
   with
  | Send_queue.Uploaded_ok (Send_queue.Encrypted_upload { mxc; metadata }) ->
      check_bool "recovery returns the original remote result" true
        (Media.Mxc.equal mxc remote_uri && metadata = expected_metadata)
  | _ -> Alcotest.fail "durable upload result should resume without transport");
  check_int "restart recovery does not reupload" 1 !calls;
  check_bool "recovery leaves remote bytes" true
    (Media_store.get ~now:Ptime.epoch media_store remote_key
    = Ok (Some encrypted.ciphertext));
  check_bool "recovery removes the queue record" true
    (match Send_queue.requests restarted with
    | [ persisted_child ] -> (
        Send_queue.id persisted_child = Send_queue.id child
        && Send_queue.dependencies persisted_child = []
        &&
        match Send_queue.dependency_results persisted_child with
        | [
         (_, Send_queue.Upload (Send_queue.Encrypted_upload { mxc; metadata }));
        ] ->
            Media.Mxc.equal mxc remote_uri && metadata = expected_metadata
        | _ -> false)
    | _ -> false)

let test_send_queue_upload_result_flag_failure () =
  let store = Store.memory () and room = rid "!r:example.org" in
  let backend, media_store = flaky_media_store () in
  let q = queue_of ~store ~media_store () in
  let request =
    Send_queue.upload q ~room_id:room ~role:`Original
      ~content_type:"application/octet-stream" ~data:"post-move bytes" ()
  in
  let remote_uri =
    Result.get_ok (Media.Mxc.of_string "mxc://hs.example/post-move")
  in
  let remote_key = Media_store.{ uri = remote_uri; format = File } in
  let calls = ref 0 in
  let upload ?on_progress:_ _ _ =
    incr calls;
    Ok (Send_queue.Clear_upload { mxc = remote_uri })
  in
  backend.fail_unprotect <- true;
  (match
     Send_queue.send_one q ~upload
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       request
   with
  | Send_queue.Failed (Error.Network_error _) -> ()
  | _ -> Alcotest.fail "post-move flag failure should wedge the upload");
  check_bool "bytes were moved before flag cleanup failed" true
    (Media_store.get ~now:Ptime.epoch media_store remote_key
    = Ok (Some "post-move bytes"));
  check_bool "post-move cache entry remains protected" true
    (Media_store.is_protected media_store remote_key = Ok true);
  let restarted = queue_of ~store ~media_store () in
  let restored =
    match Send_queue.requests restarted with
    | [ request ] -> request
    | _ -> Alcotest.fail "post-move transition was not restored"
  in
  backend.fail_unprotect <- false;
  Send_queue.unwedge restarted restored;
  (match
     Send_queue.send_one restarted ~upload
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       restored
   with
  | Send_queue.Uploaded_ok (Send_queue.Clear_upload { mxc }) ->
      check_bool "restored result keeps its MXC" true
        (Media.Mxc.equal mxc remote_uri)
  | _ -> Alcotest.fail "post-move transition should resume");
  check_int "flag cleanup recovery does not reupload" 1 !calls;
  check_bool "recovered remote cache entry is evictable" true
    (Media_store.is_protected media_store remote_key = Ok false)

let test_send_queue_malformed_upload_result_is_local () =
  let store = Store.memory () in
  let raw_slot = Store.Slot.v ~name:"send_queue" Matrix_proto.Json.Codec.json in
  let raw =
    {|[
      {"id":1,"room_id":"!r:example.org","kind":{"tag":"upload","upload_role":"original","upload_content_type":"application/octet-stream","upload_data":"bytes"},"txn_id":"bad-result","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false,"upload_result":{"kind":"encrypted","mxc":"mxc://hs.example/bad","metadata":{}}},
      {"id":2,"room_id":"!r:example.org","kind":{"tag":"attachment","content":{"msgtype":"m.file","body":"bad child"},"attachment_original":1},"txn_id":"bad-child","created_at":0,"attempts":0,"wedged":false,"dependencies":[1],"resolved":[],"cancel_requested":false},
      {"id":3,"room_id":"!r:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"safe"}},"txn_id":"safe","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false},
      {"id":4,"room_id":"!r:example.org","kind":{"tag":"event","event_type":"m.room.message","content":{"msgtype":"m.text","body":"forged"}},"txn_id":"forged","created_at":0,"attempts":0,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false,"upload_result":{"kind":"clear","mxc":"mxc://hs.example/forged"}}
    ]|}
  in
  (match Store.Slot.set store raw_slot (json_of raw) with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "malformed transition fixture: %s" (Error.to_string error));
  let restored = queue_of ~store () in
  check_bool "only the unrelated queue record survives" true
    (match Send_queue.requests restored with
    | [ request ] -> (
        Send_queue.id request = 3
        &&
        match Send_queue.kind request with
        | Send_queue.Event { content; _ } ->
            Matrix_proto.Json.find_string "body" content = Some "safe"
        | _ -> false)
    | _ -> false)

let test_send_queue_upload_result_without_cache () =
  (* A process may add a media store after an upload was already handed off
     without one.  Absence of [upload_cache_uri] means there is no cache move
     to finish; it must not be reinterpreted as a missing remote cache entry. *)
  let store = Store.memory () in
  let raw_slot = Store.Slot.v ~name:"send_queue" Matrix_proto.Json.Codec.json in
  let raw =
    {|[{"id":1,"room_id":"!r:example.org","kind":{"tag":"upload","upload_role":"original","upload_content_type":"application/octet-stream","upload_data":"inline"},"txn_id":"no-cache","created_at":0,"attempts":1,"wedged":false,"dependencies":[],"resolved":[],"cancel_requested":false,"upload_result":{"kind":"clear","mxc":"mxc://hs.example/already-uploaded"}}]|}
  in
  (match Store.Slot.set store raw_slot (json_of raw) with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "no-cache transition fixture: %s" (Error.to_string error));
  let media_store = Media_store.memory () in
  let queue = queue_of ~store ~media_store () in
  let request =
    match Send_queue.requests queue with
    | [ request ] -> request
    | _ -> Alcotest.fail "no-cache transition was not restored"
  in
  let called = ref false in
  (match
     Send_queue.send_one queue
       ~upload:(fun ?on_progress:_ _ _ ->
         called := true;
         Error (Error.Network_error "must not reupload"))
       (client_of (Fetch_mock.client (fun _ -> assert false)))
       request
   with
  | Send_queue.Uploaded_ok (Send_queue.Clear_upload { mxc }) ->
      check_string "stored no-cache result survives"
        "mxc://hs.example/already-uploaded" (Media.Mxc.to_string mxc)
  | _ -> Alcotest.fail "no-cache transition should complete from its result");
  check_bool "no-cache transition skips transport" true (not !called)

let test_send_queue_attachment_substitution () =
  let store = Store.memory () and room = rid "!r:example.org" in
  let q = queue_of ~store () in
  let base_content =
    json_of
      {|{"msgtype":"m.image","body":"caption","m.relates_to":{"rel_type":"m.thread","event_id":"$base:example.org"}}|}
  in
  let extra_content =
    json_of
      {|{"msgtype":"m.bad","body":"bad","url":"mxc://hs.example/evil","file":{"url":"mxc://hs.example/evil"},"info":{"thumbnail_file":{"url":"mxc://hs.example/evil"},"vendor_info":true},"vendor":true}|}
  in
  let callback_ids = ref [] in
  let callback_sizes = ref [] in
  Send_queue.on_change q (fun request ->
      callback_ids := Send_queue.id request :: !callback_ids;
      callback_sizes := List.length (Send_queue.requests q) :: !callback_sizes);
  let attachment =
    Send_queue.send_attachment ~extra_content q ~room_id:room ~base_content
      ~original:
        (Send_queue.attachment_upload ~content_type:"image/png"
           ~data:"original bytes" ())
      ~thumbnail:
        (Send_queue.attachment_upload ~content_type:"image/png"
           ~data:"thumbnail bytes" ())
      ()
  in
  let initial_callback_ids = List.rev !callback_ids in
  let initial_callback_sizes = List.rev !callback_sizes in
  check_bool "atomic callbacks are in ID order" true
    (initial_callback_ids = List.sort Int.compare initial_callback_ids
    && initial_callback_sizes = [ 3; 3; 3 ]);
  let initial = queue_of ~store () in
  check_int "attachment graph is durable before any upload" 3
    (List.length (Send_queue.requests initial));
  check_int "two upload nodes are persisted" 2
    (List.length
       (List.filter
          (fun request ->
            match Send_queue.kind request with
            | Send_queue.Upload_request _ -> true
            | _ -> false)
          (Send_queue.requests initial)));
  check_int "only attachment is an event echo" 1
    (List.length
       (List.filter
          (fun request ->
            match Send_queue.kind request with
            | Send_queue.Attachment _ -> true
            | _ -> false)
          (Send_queue.requests initial)));
  let uploads =
    List.filter_map
      (fun request ->
        match Send_queue.kind request with
        | Send_queue.Upload_request { role; _ } -> Some (role, request)
        | _ -> None)
      (Send_queue.requests q)
  in
  let original = List.find (fun (role, _) -> role = `Original) uploads |> snd in
  let thumbnail =
    List.find (fun (role, _) -> role = `Thumbnail) uploads |> snd
  in
  let mxc value = Result.get_ok (Matrix_client.Media.Mxc.of_string value) in
  let upload_sender ?on_progress:_ _ request =
    match Send_queue.kind request with
    | Send_queue.Upload_request { role = `Original; _ } ->
        Ok (Send_queue.Clear_upload { mxc = mxc "mxc://hs.example/original" })
    | Send_queue.Upload_request { role = `Thumbnail; _ } ->
        Ok (Send_queue.Clear_upload { mxc = mxc "mxc://hs.example/thumb" })
    | _ -> Error (Error.Network_error "not an upload")
  in
  let log, fetch = mock_seq [ Fetch_mock.respond ok_send ] in
  let client = client_of fetch in
  ignore (Send_queue.send_one q ~upload:upload_sender client original);
  let q_after_original = queue_of ~store () in
  let thumbnail =
    List.find
      (fun request -> Send_queue.id request = Send_queue.id thumbnail)
      (Send_queue.requests q_after_original)
  in
  ignore
    (Send_queue.send_one q_after_original ~upload:upload_sender client thumbnail);
  let q_before_event = queue_of ~store () in
  let attachment =
    List.find
      (fun request -> Send_queue.id request = Send_queue.id attachment)
      (Send_queue.requests q_before_event)
  in
  let echoed = Send_queue.local_echo q_before_event attachment in
  check_bool "attachment has one visible base echo" true
    (Matrix_proto.Json.find_string "body" echoed.content = Some "caption"
    && Matrix_proto.Json.find_string "url" echoed.content
       = Some "mxc://hs.example/evil");
  (match Send_queue.send_one q_before_event client attachment with
  | Send_queue.Sent_ok _ -> ()
  | _ -> Alcotest.fail "attachment event should send after both uploads");
  let content =
    match requests log with
    | [ { body = Some body; _ } ] -> json_of body
    | _ -> Alcotest.fail "expected one attachment event request"
  in
  check_bool "generated original URL wins" true
    (Matrix_proto.Json.find_string "url" content
    = Some "mxc://hs.example/original");
  check_bool "generated file collision is removed" true
    (Matrix_proto.Json.find_mem "file" content = None);
  check_bool "typed message fields win over extras" true
    (Matrix_proto.Json.find_string "msgtype" content = Some "m.image"
    && Matrix_proto.Json.find_string "body" content = Some "caption");
  check_bool "base relation wins and vendor fields survive" true
    (Matrix_proto.Json.find_mem "m.relates_to" content
     = Matrix_proto.Json.find_mem "m.relates_to" base_content
    && Matrix_proto.Json.find_bool "vendor" content = Some true);
  let info = Matrix_proto.Json.find_mem "info" content in
  check_bool "clear thumbnail uses thumbnail_url" true
    (match info with
    | Some info ->
        Matrix_proto.Json.find_string "thumbnail_url" info
        = Some "mxc://hs.example/thumb"
        && Matrix_proto.Json.find_mem "thumbnail_file" info = None
        && Matrix_proto.Json.find_bool "vendor_info" info = Some true
    | None -> false);
  let encrypted_original =
    Matrix_client.Encrypted_attachment.encrypt
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 4096 'e')))
      "original plaintext"
  in
  let encrypted_thumbnail =
    Matrix_client.Encrypted_attachment.encrypt
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 4096 't')))
      "thumbnail plaintext"
  in
  let encrypted_queue = queue_of () in
  let encrypted_attachment =
    Send_queue.send_attachment encrypted_queue ~room_id:room
      ~base_content:
        (json_of
           {|{"msgtype":"m.image","body":"encrypted","url":"mxc://hs.example/stale","info":{"thumbnail_url":"mxc://hs.example/stale-thumb","thumbnail_file":{"url":"mxc://hs.example/stale-thumb-file"}}}|})
      ~original:
        (Send_queue.attachment_upload_encrypted
           ~content_type:"application/octet-stream"
           ~encrypted:encrypted_original ())
      ~thumbnail:
        (Send_queue.attachment_upload_encrypted
           ~content_type:"application/octet-stream"
           ~encrypted:encrypted_thumbnail ())
      ()
  in
  let encrypted_upload_sender ?on_progress:_ _ request =
    match Send_queue.kind request with
    | Send_queue.Upload_request { role = `Original; _ } ->
        Ok
          (Send_queue.Encrypted_upload
             {
               mxc = mxc "mxc://hs.example/encrypted-original";
               metadata = encrypted_original.metadata;
             })
    | Send_queue.Upload_request { role = `Thumbnail; _ } ->
        Ok
          (Send_queue.Encrypted_upload
             {
               mxc = mxc "mxc://hs.example/encrypted-thumbnail";
               metadata = encrypted_thumbnail.metadata;
             })
    | _ -> Error (Error.Network_error "not an encrypted upload")
  in
  let encrypted_original_request, encrypted_thumbnail_request =
    match
      List.filter_map
        (fun request ->
          match Send_queue.kind request with
          | Send_queue.Upload_request { role = `Original; _ } ->
              Some (`Original, request)
          | Send_queue.Upload_request { role = `Thumbnail; _ } ->
              Some (`Thumbnail, request)
          | _ -> None)
        (Send_queue.requests encrypted_queue)
    with
    | [ (`Original, original); (`Thumbnail, thumbnail) ] -> (original, thumbnail)
    | _ -> Alcotest.fail "encrypted attachment upload nodes are missing"
  in
  let encrypted_log, encrypted_fetch =
    mock_seq [ Fetch_mock.respond ok_send ]
  in
  let encrypted_client = client_of encrypted_fetch in
  ignore
    (Send_queue.send_one encrypted_queue ~upload:encrypted_upload_sender
       encrypted_client encrypted_original_request);
  let encrypted_thumbnail_request =
    List.find
      (fun request ->
        Send_queue.id request = Send_queue.id encrypted_thumbnail_request)
      (Send_queue.requests encrypted_queue)
  in
  ignore
    (Send_queue.send_one encrypted_queue ~upload:encrypted_upload_sender
       encrypted_client encrypted_thumbnail_request);
  let encrypted_attachment =
    List.find
      (fun request ->
        Send_queue.id request = Send_queue.id encrypted_attachment)
      (Send_queue.requests encrypted_queue)
  in
  (match
     Send_queue.send_one encrypted_queue encrypted_client encrypted_attachment
   with
  | Send_queue.Sent_ok _ -> ()
  | _ -> Alcotest.fail "encrypted attachment event should send");
  let encrypted_content =
    match requests encrypted_log with
    | [ { body = Some body; _ } ] -> json_of body
    | _ -> Alcotest.fail "expected one encrypted attachment event request"
  in
  check_bool "encrypted original uses file" true
    (Matrix_proto.Json.find_mem "url" encrypted_content = None
    &&
    match Matrix_proto.Json.find_mem "file" encrypted_content with
    | Some file ->
        Matrix_proto.Json.find_string "url" file
        = Some "mxc://hs.example/encrypted-original"
    | None -> false);
  check_bool "encrypted thumbnail uses thumbnail_file" true
    (match Matrix_proto.Json.find_mem "info" encrypted_content with
    | Some info -> (
        Matrix_proto.Json.find_mem "thumbnail_url" info = None
        &&
        match Matrix_proto.Json.find_mem "thumbnail_file" info with
        | Some file ->
            Matrix_proto.Json.find_string "url" file
            = Some "mxc://hs.example/encrypted-thumbnail"
        | None -> false)
    | None -> false);
  (* The encrypted, no-thumbnail form is also persisted as a two-node graph
     across a restart. *)
  let no_thumb_store = Store.memory () in
  let no_thumb_queue = queue_of ~store:no_thumb_store () in
  let no_thumb =
    Send_queue.send_attachment no_thumb_queue ~room_id:room
      ~base_content:
        (json_of
           {|{"msgtype":"m.file","body":"secret","info":{"thumbnail_url":"mxc://hs.example/stale-thumb","thumbnail_file":{"url":"mxc://hs.example/stale-thumb-file"},"vendor":true}}|})
      ~original:
        (Send_queue.attachment_upload_encrypted
           ~content_type:"application/octet-stream"
           ~encrypted:encrypted_original ())
      ()
  in
  let no_thumb_restarted = queue_of ~store:no_thumb_store () in
  check_int "no-thumbnail graph survives immediate restart" 2
    (List.length (Send_queue.requests no_thumb_restarted));
  let no_thumb_upload =
    List.find
      (fun request ->
        match Send_queue.kind request with
        | Send_queue.Upload_request _ -> true
        | _ -> false)
      (Send_queue.requests no_thumb_restarted)
  in
  let no_thumb_sender ?on_progress:_ _ _ =
    Ok
      (Send_queue.Encrypted_upload
         {
           mxc = mxc "mxc://hs.example/encrypted";
           metadata = encrypted_original.metadata;
         })
  in
  let no_thumb_log, no_thumb_fetch = mock_seq [ Fetch_mock.respond ok_send ] in
  let no_thumb_client = client_of no_thumb_fetch in
  ignore
    (Send_queue.send_one no_thumb_restarted ~upload:no_thumb_sender
       no_thumb_client no_thumb_upload);
  let no_thumb =
    List.find
      (fun request -> Send_queue.id request = Send_queue.id no_thumb)
      (Send_queue.requests no_thumb_restarted)
  in
  ignore (Send_queue.send_one no_thumb_restarted no_thumb_client no_thumb);
  let no_thumb_content =
    match requests no_thumb_log with
    | [ { body = Some body; _ } ] -> json_of body
    | _ -> Alcotest.fail "expected encrypted attachment request"
  in
  check_bool "encrypted original substitutes file and removes url" true
    (Matrix_proto.Json.find_mem "url" no_thumb_content = None
    && Matrix_proto.Json.find_mem "file" no_thumb_content <> None);
  check_bool "no thumbnail removes stale sources but keeps info" true
    (match Matrix_proto.Json.find_mem "info" no_thumb_content with
    | Some info ->
        Matrix_proto.Json.find_mem "thumbnail_url" info = None
        && Matrix_proto.Json.find_mem "thumbnail_file" info = None
        && Matrix_proto.Json.find_bool "vendor" info = Some true
    | None -> false);
  let missing_queue = queue_of () in
  let missing_parent =
    Send_queue.upload missing_queue ~room_id:room ~role:`Original
      ~content_type:"image/png" ~data:"original" ()
  in
  let missing =
    Send_queue.enqueue missing_queue ~room_id:room
      (Send_queue.Attachment
         {
           content = base_content;
           original_upload = Send_queue.id missing_parent;
           thumbnail_upload = None;
         })
  in
  let no_fetch = client_of (Fetch_mock.client (fun _ -> assert false)) in
  ignore
    (Send_queue.send_one missing_queue
       ~upload:(fun ?on_progress:_ _ _ ->
         Ok
           (Send_queue.Clear_upload
              { mxc = mxc "mxc://hs.example/missing-result" }))
       no_fetch missing_parent);
  ignore (Send_queue.send_one missing_queue no_fetch missing);
  check_bool "missing typed result wedges safely" true
    (Send_queue.status missing = Send_queue.Wedged);
  let wrong_role_queue = queue_of () in
  let wrong_role_parent =
    Send_queue.upload wrong_role_queue ~room_id:room ~role:`Thumbnail
      ~content_type:"image/png" ~data:"thumbnail" ()
  in
  let wrong_role_rejected =
    try
      ignore
        (Send_queue.enqueue wrong_role_queue ~room_id:room
           (Send_queue.Attachment
              {
                content = base_content;
                original_upload = Send_queue.id wrong_role_parent;
                thumbnail_upload = None;
              }));
      false
    with Invalid_argument _ -> true
  in
  check_bool "attachment rejects a thumbnail as original" true
    wrong_role_rejected;
  let cancel_store = Store.memory () in
  let cancel_queue = queue_of ~store:cancel_store () in
  let cancel_attachment =
    Send_queue.send_attachment cancel_queue ~room_id:room ~base_content
      ~original:
        (Send_queue.attachment_upload ~content_type:"image/png"
           ~data:"cancelled original" ())
      ()
  in
  ignore (Send_queue.cancel cancel_queue cancel_attachment);
  check_bool "cancelling an attachment drops its local upload parents" true
    (Send_queue.is_empty cancel_queue
    && Send_queue.is_empty (queue_of ~store:cancel_store ()));
  let in_flight_store = Store.memory () in
  let in_flight_queue = queue_of ~store:in_flight_store () in
  let in_flight_attachment =
    Send_queue.send_attachment in_flight_queue ~room_id:room ~base_content
      ~original:
        (Send_queue.attachment_upload ~content_type:"image/png"
           ~data:"in-flight original" ())
      ()
  in
  let in_flight_upload =
    List.find
      (fun request ->
        match Send_queue.kind request with
        | Send_queue.Upload_request _ -> true
        | _ -> false)
      (Send_queue.requests in_flight_queue)
  in
  let in_flight_client =
    client_of (Fetch_mock.client (fun _ -> assert false))
  in
  let in_flight_outcome =
    Send_queue.send_one in_flight_queue
      ~upload:(fun ?on_progress:_ _ _ ->
        ignore (Send_queue.cancel in_flight_queue in_flight_attachment);
        Ok (Send_queue.Clear_upload { mxc = mxc "mxc://hs.example/cancelled" }))
      in_flight_client in_flight_upload
  in
  check_bool "in-flight attachment cancellation discards upload result" true
    (match in_flight_outcome with
    | Send_queue.Failed _ ->
        Send_queue.status in_flight_upload = Send_queue.Cancelled
        && Send_queue.is_empty in_flight_queue
        && Send_queue.is_empty (queue_of ~store:in_flight_store ())
    | _ -> false);
  let cancel_after_store = Store.memory () in
  let cancel_after_queue = queue_of ~store:cancel_after_store () in
  let cancel_after_attachment =
    Send_queue.send_attachment cancel_after_queue ~room_id:room ~base_content
      ~original:
        (Send_queue.attachment_upload ~content_type:"image/png"
           ~data:"original bytes" ())
      ~thumbnail:
        (Send_queue.attachment_upload ~content_type:"image/png"
           ~data:"thumbnail bytes" ())
      ()
  in
  let cancel_after_original =
    List.find
      (fun request ->
        match Send_queue.kind request with
        | Send_queue.Upload_request { role = `Original; _ } -> true
        | _ -> false)
      (Send_queue.requests cancel_after_queue)
  in
  let no_fetch = client_of (Fetch_mock.client (fun _ -> assert false)) in
  ignore
    (Send_queue.send_one cancel_after_queue ~upload:upload_sender no_fetch
       cancel_after_original);
  let cancel_after_attachment =
    List.find
      (fun request ->
        Send_queue.id request = Send_queue.id cancel_after_attachment)
      (Send_queue.requests cancel_after_queue)
  in
  ignore (Send_queue.cancel cancel_after_queue cancel_after_attachment);
  check_bool "cancelling after one upload drops the remaining graph" true
    (Send_queue.is_empty cancel_after_queue
    && Send_queue.is_empty (queue_of ~store:cancel_after_store ()))

let test_send_queue_local_echo_media_sources () =
  let room = rid "!local-echo:example.org" in
  let media_store = Media_store.memory () in
  (* [queue_of] deliberately uses a repeated source for deterministic tests;
     this graph needs distinct transaction IDs because each upload is keyed by
     its transaction ID in the media cache. *)
  let random =
    Matrix_client.Random.of_source
      (Eio.Flow.string_source
         (String.init 4096 (fun index -> Char.chr (index mod 251))))
  in
  let queue = Send_queue.create ~random ~user_id:alice ~media_store () in
  let base_content =
    json_of
      {|{"msgtype":"m.image","body":"caption","url":"mxc://hs.example/stale","file":{"url":"mxc://hs.example/stale-file"},"m.relates_to":{"rel_type":"m.thread","event_id":"$root:example.org"},"info":{"thumbnail_url":"mxc://hs.example/stale-thumb","thumbnail_file":{"url":"mxc://hs.example/stale-thumb-file"},"vendor":true},"vendor_extra":true}|}
  in
  let attachment =
    Send_queue.send_attachment queue ~room_id:room ~base_content
      ~original:
        (Send_queue.attachment_upload ~content_type:"image/png"
           ~data:"original-local-bytes" ())
      ~thumbnail:
        (Send_queue.attachment_upload ~content_type:"image/png"
           ~data:"thumbnail-local-bytes" ())
      ()
  in
  let upload role =
    List.find
      (fun request ->
        match Send_queue.kind request with
        | Send_queue.Upload_request { role = actual; _ } -> actual = role
        | _ -> false)
      (Send_queue.requests queue)
  in
  let original = upload `Original and thumbnail = upload `Thumbnail in
  let local_key request =
    Media_store.
      {
        uri = Media_store.local_uri ~txn_id:(Send_queue.txn_id request);
        format = File;
      }
  in
  let original_key = local_key original
  and thumbnail_key = local_key thumbnail in
  let local_uri key = Media.Mxc.to_string key.Media_store.uri in
  let echoed = Send_queue.local_echo queue attachment in
  check_bool "clear local echo exposes original URL" true
    (Matrix_proto.Json.find_string "url" echoed.content
     = Some (local_uri original_key)
    && Matrix_proto.Json.find_mem "file" echoed.content = None);
  check_bool "clear local echo exposes thumbnail URL" true
    (match Matrix_proto.Json.find_mem "info" echoed.content with
    | Some info ->
        Matrix_proto.Json.find_string "thumbnail_url" info
        = Some (local_uri thumbnail_key)
        && Matrix_proto.Json.find_mem "thumbnail_file" info = None
        && Matrix_proto.Json.find_bool "vendor" info = Some true
    | None -> false);
  check_bool "local echo keeps typed and vendor fields" true
    (Matrix_proto.Json.find_string "body" echoed.content = Some "caption"
    && Matrix_proto.Json.find_mem "m.relates_to" echoed.content
       = Matrix_proto.Json.find_mem "m.relates_to" base_content
    && Matrix_proto.Json.find_bool "vendor_extra" echoed.content = Some true);
  (match Media_store.get ~now:Ptime.epoch media_store original_key with
  | Ok (Some bytes) ->
      check_string "local original bytes resolve exactly" "original-local-bytes"
        bytes
  | Ok None ->
      Alcotest.failf "local original cache entry is missing (%s)"
        (local_uri original_key)
  | Error error ->
      Alcotest.failf "local original cache read failed: %s"
        (Error.to_string error));
  check_bool "local thumbnail bytes resolve exactly" true
    (Media_store.get ~now:Ptime.epoch media_store thumbnail_key
    = Ok (Some "thumbnail-local-bytes"));
  let remote_uri =
    Result.get_ok (Media.Mxc.of_string "mxc://hs.example/echo")
  in
  let client = client_of (Fetch_mock.client (fun _ -> assert false)) in
  let upload_sender ?on_progress:_ _ request =
    match Send_queue.kind request with
    | Send_queue.Upload_request { role = `Original; _ } ->
        Ok (Send_queue.Clear_upload { mxc = remote_uri })
    | _ -> Error (Error.Network_error "unexpected upload")
  in
  (match Send_queue.send_one queue ~upload:upload_sender client original with
  | Send_queue.Uploaded_ok _ -> ()
  | _ -> Alcotest.fail "original upload should resolve");
  let echoed = Send_queue.local_echo queue attachment in
  check_bool "resolved original wins while thumbnail stays local" true
    (Matrix_proto.Json.find_string "url" echoed.content
     = Some "mxc://hs.example/echo"
    &&
    match Matrix_proto.Json.find_mem "info" echoed.content with
    | Some info ->
        Matrix_proto.Json.find_string "thumbnail_url" info
        = Some (local_uri thumbnail_key)
    | None -> false);
  let encrypted_original =
    Attachment.encrypt
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 4096 'l')))
      "encrypted-local-bytes"
  in
  let encrypted_queue = queue_of ~media_store:(Media_store.memory ()) () in
  let encrypted_attachment =
    Send_queue.send_attachment encrypted_queue ~room_id:room
      ~base_content:
        (json_of
           {|{"msgtype":"m.file","body":"encrypted","url":"mxc://hs.example/stale","file":{"url":"mxc://hs.example/stale-file"},"info":{"thumbnail_url":"mxc://hs.example/stale-thumb"}}|})
      ~original:
        (Send_queue.attachment_upload_encrypted
           ~content_type:"application/octet-stream"
           ~encrypted:encrypted_original ())
      ()
  in
  let encrypted_echo =
    Send_queue.local_echo encrypted_queue encrypted_attachment
  in
  let encrypted_upload =
    List.find
      (fun request ->
        match Send_queue.kind request with
        | Send_queue.Upload_request { role = `Original; _ } -> true
        | _ -> false)
      (Send_queue.requests encrypted_queue)
  in
  let encrypted_key = local_key encrypted_upload in
  check_bool "encrypted local echo exposes file URL only" true
    (Matrix_proto.Json.find_string "url" encrypted_echo.content = None
    &&
    match Matrix_proto.Json.find_mem "file" encrypted_echo.content with
    | Some file ->
        Matrix_proto.Json.find_string "url" file
        = Some (local_uri encrypted_key)
    | None -> false)

let test_send_queue_local_echo_media_restart () =
  Eio_main.run @@ fun env ->
  let root = Filename.temp_file "matrix-local-echo" ".d" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  let dir = Eio.Path.(Eio.Stdenv.fs env / root) in
  let media_path = Filename.concat root "media.sqlite" in
  let open_media () =
    match Sqlite.create_media_store media_path with
    | Ok media -> media
    | Error error ->
        Alcotest.failf "open local-echo media store: %s" (Error.to_string error)
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> if Sys.file_exists path then Unix.unlink path)
        [
          media_path;
          media_path ^ "-shm";
          media_path ^ "-wal";
          Filename.concat root "base_state.json";
          Filename.concat root ".profile.lock";
        ];
      if Sys.file_exists root then Unix.rmdir root)
    (fun () ->
      let media_store = open_media () in
      let store = Store.on_disk ~dir in
      let queue = queue_of ~store ~media_store () in
      let room = rid "!local-restart:example.org" in
      let encrypted =
        Attachment.encrypt
          ~random:
            (Matrix_client.Random.of_source
               (Eio.Flow.string_source (String.make 4096 'p')))
          "persisted encrypted bytes"
      in
      let attachment =
        Send_queue.send_attachment queue ~room_id:room
          ~base_content:
            (json_of
               {|{"msgtype":"m.file","body":"persisted","url":"mxc://hs.example/stale","info":{"thumbnail_file":{"url":"mxc://hs.example/stale-thumb"},"vendor":true}}|})
          ~original:
            (Send_queue.attachment_upload_encrypted
               ~content_type:"application/octet-stream" ~encrypted ())
          ()
      in
      let upload =
        List.find
          (fun request ->
            match Send_queue.kind request with
            | Send_queue.Upload_request { role = `Original; _ } -> true
            | _ -> false)
          (Send_queue.requests queue)
      in
      let key =
        Media_store.
          {
            uri = Media_store.local_uri ~txn_id:(Send_queue.txn_id upload);
            format = File;
          }
      in
      let echoed = Send_queue.local_echo queue attachment in
      check_bool "persisted queue initially echoes encrypted local file" true
        (match Matrix_proto.Json.find_mem "file" echoed.content with
        | Some file ->
            Matrix_proto.Json.find_string "url" file
            = Some (Media.Mxc.to_string key.uri)
        | None -> false);
      check_bool "persisted local bytes are exact" true
        (Media_store.get ~now:Ptime.epoch media_store key
        = Ok (Some encrypted.ciphertext));
      (match Store.flush store with
      | Ok () -> ()
      | Error error ->
          Alcotest.failf "flush local-echo queue: %s" (Error.to_string error));
      Media_store.close media_store;
      let reopened_media = open_media () in
      let reopened_queue =
        queue_of ~store:(Store.on_disk ~dir) ~media_store:reopened_media ()
      in
      let restored =
        List.find
          (fun request -> Send_queue.id request = Send_queue.id attachment)
          (Send_queue.requests reopened_queue)
      in
      let echoed = Send_queue.local_echo reopened_queue restored in
      check_bool "encrypted local echo survives queue and media restart" true
        (Matrix_proto.Json.find_string "url" echoed.content = None
        &&
        match Matrix_proto.Json.find_mem "file" echoed.content with
        | Some file ->
            Matrix_proto.Json.find_string "url" file
            = Some (Media.Mxc.to_string key.uri)
        | None -> false);
      check_bool "persisted media remains byte exact after restart" true
        (Media_store.get ~now:Ptime.epoch reopened_media key
        = Ok (Some encrypted.ciphertext));
      Media_store.close reopened_media)

let test_send_queue_attachment_caption_edit () =
  let room = rid "!r:example.org" in
  let store = Store.memory () in
  let q = queue_of ~store () in
  let attachment =
    Send_queue.send_attachment q ~room_id:room
      ~base_content:(json_of {|{"msgtype":"m.image","body":"old"}|})
      ~original:
        (Send_queue.attachment_upload ~content_type:"image/png"
           ~filename:"photo.png" ~data:"bytes" ())
      ()
  in
  let old_txn = Send_queue.txn_id attachment in
  let mentions = json_of {|{"user_ids":["@alice:example.org"]}|} in
  (match
     Send_queue.edit_attachment_caption ~formatted_body:"<b>new</b>" ~mentions q
       attachment ~caption:(Some "new")
   with
  | Ok Send_queue.Updated -> ()
  | _ -> Alcotest.fail "pending attachment caption was not updated");
  check_string "pending attachment retains transaction" old_txn
    (Send_queue.txn_id attachment);
  let restarted = queue_of ~store () in
  let attachment =
    List.find
      (fun request -> Send_queue.id request = Send_queue.id attachment)
      (Send_queue.requests restarted)
  in
  let echo = Send_queue.local_echo restarted attachment in
  check_string "caption survives restart" "new"
    (Option.value
       (Matrix_proto.Json.find_string "body" echo.content)
       ~default:"");
  check_string "format survives restart" "<b>new</b>"
    (Option.value
       (Matrix_proto.Json.find_string "formatted_body" echo.content)
       ~default:"");
  (match
     Send_queue.edit_attachment_caption restarted attachment ~caption:None
   with
  | Ok Send_queue.Updated -> ()
  | _ -> Alcotest.fail "clearing attachment caption failed");
  let cleared = Send_queue.local_echo restarted attachment in
  check_string "clearing caption restores filename" "photo.png"
    (Option.value
       (Matrix_proto.Json.find_string "body" cleared.content)
       ~default:"");
  check_bool "clearing caption removes filename" true
    (Matrix_proto.Json.find_mem "filename" cleared.content = None);
  check_bool "clearing caption removes formatting" true
    (Matrix_proto.Json.find_mem "formatted_body" cleared.content = None
    && Matrix_proto.Json.find_mem "m.mentions" cleared.content = None);
  let invalid = json_of {|"bad"|} in
  (match
     Send_queue.edit_attachment_caption ~mentions:invalid restarted attachment
       ~caption:(Some "ignored")
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "invalid mentions were accepted");
  let explicit =
    Send_queue.send_attachment restarted ~room_id:room
      ~base_content:
        (json_of
           {|{"msgtype":"m.image","body":"caption","filename":"base.png"}|})
      ~original:
        (Send_queue.attachment_upload ~content_type:"image/png"
           ~filename:"upload.png" ~data:"bytes" ())
      ()
  in
  (match
     Send_queue.edit_attachment_caption restarted explicit ~caption:None
   with
  | Ok Send_queue.Updated -> ()
  | _ -> Alcotest.fail "explicit filename caption clear failed");
  let explicit_echo = Send_queue.local_echo restarted explicit in
  check_string "base filename wins over upload filename" "base.png"
    (Option.value
       (Matrix_proto.Json.find_string "body" explicit_echo.content)
       ~default:"");
  let malformed =
    Send_queue.send_attachment restarted ~room_id:room
      ~base_content:(json_of {|{"msgtype":"m.image","body":4}|})
      ~original:
        (Send_queue.attachment_upload ~content_type:"image/png" ~data:"bytes" ())
      ()
  in
  (match
     Send_queue.edit_attachment_caption restarted malformed
       ~caption:(Some "new")
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "malformed body was accepted");
  let foreign_queue = queue_of () in
  let foreign =
    Send_queue.send_attachment foreign_queue ~room_id:room
      ~base_content:(json_of {|{"msgtype":"m.image","body":"foreign"}|})
      ~original:
        (Send_queue.attachment_upload ~content_type:"image/png" ~data:"bytes" ())
      ()
  in
  match
    Send_queue.edit_attachment_caption restarted foreign ~caption:(Some "bad")
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "foreign attachment was accepted"

let test_send_queue_attachment_caption_in_flight () =
  let room = rid "!r:example.org" in
  let store = Store.memory () in
  let q = queue_of ~store () in
  let attachment =
    Send_queue.send_attachment q ~room_id:room
      ~base_content:
        (json_of
           {|{"msgtype":"m.file","body":"old","info":{"size":4,"vendor":true}}|})
      ~original:
        (Send_queue.attachment_upload ~content_type:"application/octet-stream"
           ~data:"bytes" ())
      ()
  in
  let original =
    List.find
      (fun request ->
        match Send_queue.kind request with
        | Send_queue.Upload_request { role = `Original; _ } -> true
        | _ -> false)
      (Send_queue.requests q)
  in
  let client = client_of (Fetch_mock.client (fun _ -> assert false)) in
  ignore
    (Send_queue.send_one q
       ~upload:(fun ?on_progress:_ _ _ ->
         Ok
           (Send_queue.Clear_upload
              {
                mxc =
                  Result.get_ok
                    (Matrix_client.Media.Mxc.of_string "mxc://hs/file");
              }))
       client original);
  let parent_txn = Send_queue.txn_id attachment in
  let send _ request =
    let before =
      match Send_queue.content_for_send request with
      | Ok (_, content) -> Matrix_proto.Json.find_string "body" content
      | Error _ -> None
    in
    match
      Send_queue.edit_attachment_caption q request ~caption:(Some "first")
    with
    | Ok Send_queue.Deferred -> (
        match
          Send_queue.edit_attachment_caption q request ~caption:(Some "latest")
        with
        | Ok Send_queue.Deferred ->
            let after =
              match Send_queue.content_for_send request with
              | Ok (_, content) -> Matrix_proto.Json.find_string "body" content
              | Error _ -> None
            in
            if before <> after then
              Alcotest.fail "deferred edit changed the parent wire content"
            else Ok (eid "$parent:example.org")
        | _ -> Alcotest.fail "repeated in-flight caption was not deferred")
    | _ -> Alcotest.fail "in-flight caption was not deferred"
  in
  (match Send_queue.send_one q ~send client attachment with
  | Send_queue.Retry_in _ -> ()
  | _ -> Alcotest.fail "parent with deferred caption did not create edit");
  check_bool "deferred edit changes transaction only after parent success" true
    (not (String.equal parent_txn (Send_queue.txn_id attachment)));
  let restarted = queue_of ~store () in
  let edited =
    List.find
      (fun request -> Send_queue.id request = Send_queue.id attachment)
      (Send_queue.requests restarted)
  in
  let sent_content = ref None in
  let edit_attempts = ref 0 in
  let send_edit _ request =
    incr edit_attempts;
    (sent_content :=
       match Send_queue.payload request with
       | Send_queue.Send { content; _ } -> Some content
       | _ -> None);
    if !edit_attempts = 1 then Error (Error.Network_error "offline")
    else Ok (eid "$edit:example.org")
  in
  let edit_txn = Send_queue.txn_id edited in
  (match Send_queue.send_one restarted ~send:send_edit client edited with
  | Send_queue.Retry_in _ -> ()
  | _ -> Alcotest.fail "deferred edit failure was not retryable");
  check_string "replacement transaction survives retry" edit_txn
    (Send_queue.txn_id edited);
  (match Send_queue.send_one restarted ~send:send_edit client edited with
  | Send_queue.Sent_ok _ -> ()
  | _ -> Alcotest.fail "deferred edit did not send");
  check_bool "replacement contains latest media caption" true
    (match !sent_content with
    | Some content -> (
        match Matrix_proto.Json.find_mem "m.new_content" content with
        | Some new_content ->
            Matrix_proto.Json.find_string "body" new_content = Some "latest"
        | None -> false)
    | None -> false);
  check_bool "replacement retains resolved media fields" true
    (match !sent_content with
    | Some content ->
        Matrix_proto.Json.find_string "msgtype" content = Some "m.file"
        && Matrix_proto.Json.find_string "url" content = Some "mxc://hs/file"
        && (match Matrix_proto.Json.find_mem "info" content with
          | Some info -> Matrix_proto.Json.find_bool "vendor" info = Some true
          | None -> false)
        && Matrix_proto.Json.find_mem "m.new_content" content <> None
    | None -> false)

let test_send_queue_attachment_caption_cancel_race () =
  let room = rid "!r:example.org" in
  let q = queue_of () in
  let attachment =
    Send_queue.send_attachment q ~room_id:room
      ~base_content:(json_of {|{"msgtype":"m.image","body":"old"}|})
      ~original:
        (Send_queue.attachment_upload ~content_type:"image/png" ~data:"bytes" ())
      ()
  in
  let upload =
    List.find
      (fun request ->
        match Send_queue.kind request with
        | Send_queue.Upload_request _ -> true
        | _ -> false)
      (Send_queue.requests q)
  in
  let client = client_of (Fetch_mock.client (fun _ -> assert false)) in
  ignore
    (Send_queue.send_one q
       ~upload:(fun ?on_progress:_ _ _ ->
         Ok
           (Send_queue.Clear_upload
              {
                mxc =
                  Result.get_ok
                    (Matrix_client.Media.Mxc.of_string "mxc://hs/race");
              }))
       client upload);
  let send _ request =
    match
      Send_queue.edit_attachment_caption q request ~caption:(Some "discarded")
    with
    | Ok Send_queue.Deferred -> (
        ignore (Send_queue.cancel q request);
        match
          Send_queue.edit_attachment_caption q request ~caption:(Some "late")
        with
        | Error _ -> Ok (eid "$race:example.org")
        | Ok _ -> Alcotest.fail "caption edit won after cancellation")
    | _ -> Alcotest.fail "race edit was not deferred"
  in
  match Send_queue.send_one q ~send client attachment with
  | Send_queue.Retry_in _ -> (
      match Send_queue.kind attachment with
      | Send_queue.Redaction _ -> ()
      | _ -> Alcotest.fail "cancellation did not win over caption edit")
  | _ -> Alcotest.fail "cancelled caption parent did not become redaction"

let test_send_queue_diamond_cancel_callbacks () =
  let q = queue_of () and room = rid "!r:example.org" in
  let a = Send_queue.send_text q ~room_id:room ~body:"a" in
  let empty =
    Send_queue.Event
      { event_type = "m.room.message"; content = Jsont.Json.object' [] }
  in
  let b =
    Send_queue.enqueue ~depends_on:[ Send_queue.id a ] q ~room_id:room empty
  in
  let c =
    Send_queue.enqueue ~depends_on:[ Send_queue.id a ] q ~room_id:room empty
  in
  let d =
    Send_queue.enqueue
      ~depends_on:[ Send_queue.id b; Send_queue.id c ]
      q ~room_id:room empty
  in
  let seen = ref [] in
  Send_queue.on_change q (fun r ->
      if Send_queue.status r = Send_queue.Cancelled then
        seen := !seen @ [ Send_queue.id r ]);
  ignore (Send_queue.cancel q a);
  check_bool "diamond cancellation order" true
    (!seen
    = [ Send_queue.id a; Send_queue.id b; Send_queue.id c; Send_queue.id d ])

let test_send_queue_edit_payload () =
  let log, fetch = mock_seq [ Fetch_mock.respond ok_send ] in
  let client = client_of fetch in
  let q = queue_of () in
  let target = eid "$target:example.org" in
  let r =
    Send_queue.send_edit q ~room_id:(rid "!r:example.org") ~event_id:target
      ~new_body:"fixed" ~formatted_body:"<b>fixed</b>" ()
  in
  (match Send_queue.send_one q client r with
  | Send_queue.Sent_ok _ -> ()
  | Send_queue.Uploaded_ok _ -> Alcotest.fail "unexpected upload"
  | Send_queue.Retry_in _ -> Alcotest.fail "unexpected retry"
  | Send_queue.Failed e ->
      Alcotest.failf "unexpected failure: %s" (Error.to_string e));
  let req = List.hd (requests log) in
  check_string "edit payload"
    {|{"msgtype":"m.text","body":"* fixed","format":"org.matrix.custom.html","formatted_body":"* <b>fixed</b>","m.new_content":{"msgtype":"m.text","body":"fixed","format":"org.matrix.custom.html","formatted_body":"<b>fixed</b>"},"m.relates_to":{"rel_type":"m.replace","event_id":"$target:example.org"}}|}
    (Option.value req.body ~default:"")

let test_send_queue_edit_persistence () =
  let store = Store.memory () in
  let q = queue_of ~store () in
  let target = eid "$target:example.org" in
  let r =
    Send_queue.send_edit q ~room_id:(rid "!r:example.org") ~event_id:target
      ~new_body:"fixed" ~formatted_body:"<b>fixed</b>" ()
  in
  Send_queue.save q;
  let restored =
    match Send_queue.requests (queue_of ~store ()) with
    | [ restored ] -> restored
    | requests ->
        Alcotest.failf "expected one restored edit, got %d"
          (List.length requests)
  in
  check_string "transaction id survives reload" (Send_queue.txn_id r)
    (Send_queue.txn_id restored);
  let encoded request =
    match Send_queue.payload request with
    | Send_queue.Send { content; _ } -> (
        match
          Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json content
        with
        | Ok content -> content
        | Error error -> Alcotest.fail error)
    | Send_queue.Redact _ | Send_queue.Upload_payload _ ->
        Alcotest.fail "edit became a non-event payload"
  in
  check_string "edit payload survives reload" (encoded r) (encoded restored)

let test_send_queue_edit_retry () =
  let log, fetch =
    mock_seq
      [
        Fetch_mock.respond ~status:429
          {|{"errcode":"M_LIMIT_EXCEEDED","error":"slow down"}|};
        Fetch_mock.respond ok_send;
      ]
  in
  let client = client_of fetch in
  let q = queue_of () in
  let r =
    Send_queue.send_edit q ~room_id:(rid "!r:example.org")
      ~event_id:(eid "$target:example.org")
      ~new_body:"fixed" ()
  in
  (match Send_queue.send_one q client r with
  | Send_queue.Retry_in _ -> ()
  | _ -> Alcotest.fail "expected an edit retry");
  (match Send_queue.send_one q client r with
  | Send_queue.Sent_ok _ -> ()
  | Send_queue.Uploaded_ok _ -> Alcotest.fail "unexpected upload"
  | _ -> Alcotest.fail "expected the edit retry to succeed");
  match requests log with
  | [ first; second ] ->
      check_string "the edit retry is idempotent" first.url second.url
  | requests ->
      Alcotest.failf "expected two edit attempts, got %d" (List.length requests)

(* An exception from [send] (standing in for a cancelled fiber) must not
   leave the request stuck at [Sending] with no way back: [send_one]
   restores it to [Pending] before the exception propagates. *)
let test_send_queue_exception_does_not_wedge () =
  let _, fetch = mock_seq [] in
  let client = client_of fetch in
  let q = queue_of () in
  let r =
    Send_queue.send_text q ~room_id:(rid "!r:example.org") ~body:"hello"
  in
  (try
     ignore
       (Send_queue.send_one q ~send:(fun _ _ -> raise Exit) client r
         : Send_queue.outcome)
   with Exit -> ());
  check_bool "the request is Pending, not stuck" true
    (Send_queue.status r = Send_queue.Pending);
  check_int "still one request queued" 1 (Send_queue.pending_count q)

let test_send_queue_rate_limited () =
  let log, fetch =
    mock_seq
      [
        Fetch_mock.respond ~status:429
          {|{"errcode":"M_LIMIT_EXCEEDED","error":"slow down","retry_after_ms":2000}|};
        Fetch_mock.respond ok_send;
      ]
  in
  let client = client_of fetch in
  let q = queue_of () in
  let r =
    Send_queue.send_text q ~room_id:(rid "!r:example.org") ~body:"hello"
  in
  (match Send_queue.send_one q client r with
  | Send_queue.Retry_in d ->
      Alcotest.(check (float 0.001)) "retry_after_ms" 2.0 d
  | _ -> Alcotest.fail "expected a retry");
  check_bool "still pending" true (Send_queue.status r = Send_queue.Pending);
  check_int "one attempt" 1 (Send_queue.attempts r);
  check_bool "still at the head" true
    (Send_queue.next q (rid "!r:example.org") <> None);
  (match Send_queue.send_one q client r with
  | Send_queue.Sent_ok _ -> ()
  | Send_queue.Uploaded_ok _ -> Alcotest.fail "unexpected upload"
  | _ -> Alcotest.fail "expected success on the retry");
  let reqs = requests log in
  check_int "two attempts on the wire" 2 (List.length reqs);
  check_string "the same transaction id both times" (List.nth reqs 0).url
    (List.nth reqs 1).url

let test_send_queue_forbidden () =
  let _, fetch =
    mock_seq
      [
        Fetch_mock.respond ~status:403
          {|{"errcode":"M_FORBIDDEN","error":"no"}|};
      ]
  in
  let client = client_of fetch in
  let q = queue_of () in
  let room = rid "!r:example.org" in
  let r = Send_queue.send_text q ~room_id:room ~body:"hello" in
  (match Send_queue.send_one q client r with
  | Send_queue.Failed (Error.Matrix_error m) ->
      check_string "errcode" "M_FORBIDDEN" (Error.errcode_to_string m.errcode)
  | Send_queue.Failed e -> Alcotest.failf "wrong error: %s" (Error.to_string e)
  | _ -> Alcotest.fail "expected an unrecoverable failure");
  check_bool "wedged" true
    (match Send_queue.status r with Send_queue.Wedged -> true | _ -> false);
  check_bool "the error is available" true (Send_queue.last_error r <> None);
  check_bool "a wedged head blocks the room" true (Send_queue.next q room = None);
  check_bool "the request is still queued" false (Send_queue.is_empty q);
  Send_queue.unwedge q r;
  check_bool "unwedging frees the queue" true (Send_queue.next q room <> None)

let test_send_queue_backoff () =
  let random =
    Matrix_client.Random.of_source
      (Eio.Flow.string_source (String.make 4096 'k'))
  in
  let q =
    Send_queue.create ~random ~user_id:alice ~base_delay_ms:500
      ~max_delay_ms:4000 ()
  in
  let r = Send_queue.send_text q ~room_id:(rid "!r:example.org") ~body:"x" in
  (* [retry_delay] doubles with the attempt count, applies bounded jitter and
     stops at the cap. Repeated 'k' bytes make this value deterministic. *)
  Alcotest.(check (float 0.001))
    "first jittered delay" 0.4598
    (Send_queue.retry_delay q r);
  let capped_q =
    Send_queue.create ~random ~user_id:alice ~base_delay_ms:500
      ~max_delay_ms:100 ()
  in
  let capped =
    Send_queue.send_text capped_q ~room_id:(rid "!r:example.org") ~body:"x"
  in
  Alcotest.(check (float 0.001))
    "jittered delay remains capped" 0.1
    (Send_queue.retry_delay capped_q capped);
  check_bool "network errors are retried" true
    (match Send_queue.classify q r (Error.Network_error "offline") with
    | Send_queue.Retry_in _ -> true
    | _ -> false);
  check_bool "5xx is retried" true
    (match
       Send_queue.classify q r (Error.Http_error { status = 502; body = "" })
     with
    | Send_queue.Retry_in _ -> true
    | _ -> false);
  check_bool "other 4xx is fatal" true
    (match
       Send_queue.classify q r (Error.Http_error { status = 404; body = "" })
     with
    | Send_queue.Failed _ -> true
    | _ -> false);
  check_bool "a JSON failure is fatal" true
    (match Send_queue.classify q r (Error.Json_error "bad") with
    | Send_queue.Failed _ -> true
    | _ -> false);
  check_bool "a policy denial is fatal" true
    (match Send_queue.classify q r (Error.Policy_denied "off origin") with
    | Send_queue.Failed _ -> true
    | _ -> false);
  check_bool "a TLS failure is fatal" true
    (match Send_queue.classify q r (Error.Tls_error "bad certificate") with
    | Send_queue.Failed _ -> true
    | _ -> false);
  let unknown =
    Error.Matrix_error
      {
        errcode = Error.M_UNKNOWN;
        error = "ambiguous";
        retry_after_ms = None;
        soft_logout = None;
      }
  in
  check_bool "M_UNKNOWN is retryable before an attempt" true
    (match Send_queue.classify q r unknown with
    | Send_queue.Retry_in _ -> true
    | _ -> false);
  let _, unknown_fetch =
    mock_seq
      [
        Fetch_mock.respond ~status:400 {|{"errcode":"M_UNKNOWN"}|};
        Fetch_mock.respond ~status:400 {|{"errcode":"M_UNKNOWN"}|};
      ]
  in
  let unknown_client = client_of unknown_fetch in
  ignore (Send_queue.send_one q unknown_client r);
  check_bool "M_UNKNOWN is fatal after its first retry" true
    (match Send_queue.send_one q unknown_client r with
    | Send_queue.Failed _ -> true
    | _ -> false)

let test_send_queue_persistence () =
  let store = Store.memory () in
  let q = queue_of ~store () in
  let extra = json_of {|{"vendor":"saved","body":"not-the-typed-body"}|} in
  let a =
    Send_queue.send_text q ~room_id:(rid "!r:example.org") ~body:"one"
      ~extra_content:extra
  in
  let b =
    Send_queue.send_redaction q ~room_id:(rid "!r:example.org")
      ~event_id:(eid "$x:example.org") ~reason:"spam" ()
  in
  Send_queue.save q;
  let q2 = queue_of ~store () in
  let restored = Send_queue.requests q2 in
  check_int "both requests came back" 2 (List.length restored);
  check_string "first txn id" (Send_queue.txn_id a)
    (Send_queue.txn_id (List.nth restored 0));
  check_string "second txn id" (Send_queue.txn_id b)
    (Send_queue.txn_id (List.nth restored 1));
  check_bool "the redaction kept its kind" true
    (match Send_queue.kind (List.nth restored 1) with
    | Send_queue.Redaction { reason = Some "spam"; _ } -> true
    | _ -> false);
  let encoded request =
    match Send_queue.payload request with
    | Send_queue.Send { content; _ } -> (
        match
          Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json content
        with
        | Ok content -> content
        | Error error -> Alcotest.fail error)
    | Send_queue.Redact _ | Send_queue.Upload_payload _ ->
        Alcotest.fail "expected an event payload"
  in
  check_string "queued extra content survives restart"
    {|{"msgtype":"m.text","body":"one","vendor":"saved"}|}
    (encoded (List.hd restored));
  (* Sent requests are not persisted. *)
  ignore (Send_queue.cancel q a);
  Send_queue.save q;
  let q3 = queue_of ~store () in
  check_int "a cancelled request is dropped" 1
    (List.length (Send_queue.requests q3))

let test_send_queue_reaction_extra_persistence () =
  let store = Store.memory () in
  let q = queue_of ~store () in
  let extra =
    json_of {|{"vendor":"reaction","m.relates_to":{"event_id":"$wrong"}}|}
  in
  let reaction =
    Send_queue.send_reaction q ~room_id:(rid "!r:example.org")
      ~relates_to:(eid "$target:example.org")
      ~key:"+1" ~extra_content:extra
  in
  Send_queue.save q;
  let restored =
    match Send_queue.requests (queue_of ~store ()) with
    | [ request ] -> request
    | _ -> Alcotest.fail "expected one persisted reaction"
  in
  check_bool "reaction kind remains public Reaction" true
    (match Send_queue.kind restored with
    | Send_queue.Reaction _ -> true
    | _ -> false);
  let encoded request =
    match Send_queue.payload request with
    | Send_queue.Send { content; _ } -> (
        match
          Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json content
        with
        | Ok content -> content
        | Error error -> Alcotest.fail error)
    | Send_queue.Redact _ | Send_queue.Upload_payload _ ->
        Alcotest.fail "expected a reaction event payload"
  in
  check_string "reaction extras survive restart"
    {|{"m.relates_to":{"rel_type":"m.annotation","event_id":"$target:example.org","key":"+1"},"vendor":"reaction"}|}
    (encoded restored);
  check_string "reaction transaction stays stable"
    (Send_queue.txn_id reaction)
    (Send_queue.txn_id restored)

let test_timeline_edits_and_redactions () =
  let t = Timeline.create ~room_id:(rid "!r:example.org") () in
  let m1 = ev_at 1 "@bob:example.org" in
  Timeline.add t m1;
  Timeline.add t (ev_at 2 "@bob:example.org");
  check_int "two items" 2 (Timeline.length t);
  (* A duplicate is ignored. *)
  Timeline.add t m1;
  check_int "still two" 2 (Timeline.length t);
  (* An edit changes the target rather than becoming an item. *)
  Timeline.add t
    (event_of
       {|{"type":"m.room.message","event_id":"$ed:example.org",
          "sender":"@bob:example.org","origin_server_ts":9,
          "content":{"msgtype":"m.text","body":"* fixed",
                     "m.new_content":{"msgtype":"m.text","body":"fixed"},
                     "m.relates_to":{"rel_type":"m.replace","event_id":"$e1:example.org"}}}|});
  check_int "an edit adds no item" 2 (Timeline.length t);
  let i1 = Option.get (Timeline.find t (eid "$e1:example.org")) in
  check_bool "replacement recorded" true (Timeline.replacement i1 <> None);
  (* A redaction empties the target. *)
  Timeline.add t
    (event_of
       {|{"type":"m.room.redaction","event_id":"$r:example.org",
          "sender":"@bob:example.org","origin_server_ts":10,
          "content":{"redacts":"$e2:example.org"}}|});
  check_int "a redaction adds no item" 2 (Timeline.length t);
  let i2 = Option.get (Timeline.find t (eid "$e2:example.org")) in
  check_bool "redacted" true (Timeline.redacted i2);
  check_bool "content is emptied" true
    (match Timeline.content i2 with Jsont.Object ([], _) -> true | _ -> false);
  (* Room versions 1–10 carried the target beside [content]. *)
  Timeline.add t (ev_at 3 "@bob:example.org");
  Timeline.add t
    (event_of
       {|{"type":"m.room.redaction","event_id":"$old-r:example.org",
          "sender":"@bob:example.org","origin_server_ts":11,
          "redacts":"$e3:example.org","content":{"reason":"spam"}}|});
  check_int "a pre-v11 redaction adds no item" 3 (Timeline.length t);
  let i3 = Option.get (Timeline.find t (eid "$e3:example.org")) in
  check_bool "pre-v11 target redacted" true (Timeline.redacted i3)

(* Back-pagination can fetch an edit and the message it edits in the same
   page: {!Timeline.prepend} must apply the edit to its target even though
   the batch visits them in an order that puts the edit before the target
   is inserted. *)
let test_timeline_prepend_applies_edit_in_same_batch () =
  let t = Timeline.create ~room_id:(rid "!r:example.org") () in
  let original =
    event_of
      {|{"type":"m.room.message","event_id":"$orig:example.org",
         "sender":"@bob:example.org","origin_server_ts":1,
         "content":{"msgtype":"m.text","body":"typo"}}|}
  in
  let edit =
    event_of
      {|{"type":"m.room.message","event_id":"$ed:example.org",
         "sender":"@bob:example.org","origin_server_ts":2,
         "content":{"msgtype":"m.text","body":"* fixed",
                    "m.new_content":{"msgtype":"m.text","body":"fixed"},
                    "m.relates_to":{"rel_type":"m.replace",
                                    "event_id":"$orig:example.org"}}}|}
  in
  (* Oldest first, as [Messages.get_messages]'s page is reordered to before
     being passed to [prepend]. *)
  Timeline.prepend t [ original; edit ];
  check_int "the edit did not become its own item" 1 (Timeline.length t);
  let i = Option.get (Timeline.find t (eid "$orig:example.org")) in
  check_bool "the edit is applied to the original" true
    (Timeline.replacement i <> None)

let test_timeline_local_echo () =
  let t = Timeline.create ~room_id:(rid "!r:example.org") () in
  let q = queue_of () in
  let r = Send_queue.send_text q ~room_id:(rid "!r:example.org") ~body:"hi" in
  Timeline.add t ~local_echo:true (Send_queue.local_echo q r);
  check_int "the echo is an item" 1 (Timeline.length t);
  check_bool "marked as an echo" true
    (Timeline.local_echo (List.hd (Timeline.items t)));
  let remote =
    event_of
      (Printf.sprintf
         {|{"type":"m.room.message","event_id":"$real:example.org",
            "sender":"@alice:example.org","origin_server_ts":11,
            "content":{"msgtype":"m.text","body":"hi"},
            "unsigned":{"transaction_id":"%s"}}|}
         (Send_queue.txn_id r))
  in
  Timeline.add t remote;
  check_int "the echo was replaced, not appended" 1 (Timeline.length t);
  let i = List.hd (Timeline.items t) in
  check_bool "no longer an echo" false (Timeline.local_echo i);
  check_string "carries the server's event id" "$real:example.org"
    (Option.fold ~none:"" ~some:Id.Event_id.to_string
       (Timeline.event i).event_id)

let test_timeline_pagination () =
  let older =
    {|{"start":"p1","end":"p0","chunk":[
        {"type":"m.room.message","event_id":"$o2:example.org",
         "sender":"@bob:example.org","origin_server_ts":2,
         "content":{"msgtype":"m.text","body":"two"}},
        {"type":"m.room.message","event_id":"$o1:example.org",
         "sender":"@bob:example.org","origin_server_ts":1,
         "content":{"msgtype":"m.text","body":"one"}}],
      "state":[]}|}
  in
  let log, fetch = mock_seq [ Fetch_mock.respond older ] in
  let client = client_of fetch in
  let t = Timeline.create ~room_id:(rid "!r:example.org") () in
  Timeline.add t (ev_at 3 "@bob:example.org");
  Timeline.set_prev_batch t (Some "p1");
  (match Timeline.paginate_back client t ~limit:10 () with
  | Error e -> Alcotest.failf "paginate: %s" (Error.to_string e)
  | Ok chunk -> check_int "two older events" 2 (List.length chunk));
  check_int "prepended" 3 (Timeline.length t);
  check_string "oldest first" "$o1:example.org"
    (Option.fold ~none:"" ~some:Id.Event_id.to_string
       (Timeline.event (List.hd (Timeline.items t))).event_id);
  check_string "prev_batch advanced" "p0"
    (Option.value (Timeline.prev_batch t) ~default:"");
  let req = List.hd (requests log) in
  check_string "method" "GET" req.meth;
  check_bool "asks for the backward direction" true (contains req.url "dir=b")

let profile_updates users : Sliding_sync.Response.profiles = { users }

let profile_string state user_id field =
  match Sync_service.find_profile_field state user_id field with
  | Some (Jsont.String (value, _)) -> value
  | Some _ -> Alcotest.failf "profile field %S is not a string" field
  | None -> Alcotest.failf "profile field %S is absent" field

let test_profile_updates () =
  let bob = uid "@bob:example.org" in
  let charlie = uid "@charlie:example.org" in
  let opaque = json_of {|{"nested":{"works":true},"items":[1,"two"]}|} in
  let state = Sync_service.create ~user_id:alice () in
  let state, initial_changes =
    Sync_service.apply_profile_updates state
      (profile_updates
         [
           ( bob,
             Sliding_sync.Response.Updated
               [ ("displayname", json_of {|"Bob"|}) ] );
           ( alice,
             Sliding_sync.Response.Updated
               [
                 ("displayname", json_of {|"Alice"|});
                 ("avatar_url", json_of {|"mxc://example.org/alice"|});
                 ("com.example.opaque", opaque);
               ] );
         ])
  in
  Alcotest.(check (list string))
    "profiles and changes are sorted by user id"
    [ "@alice:example.org"; "@bob:example.org" ]
    (List.map
       (fun (user_id, _) -> Id.User_id.to_string user_id)
       (Sync_service.profiles state));
  Alcotest.(check (list string))
    "initial changes are sorted"
    [ "@alice:example.org"; "@bob:example.org" ]
    (List.map
       (fun (change : Sync_service.profile_change) ->
         Id.User_id.to_string change.changed_user_id)
       initial_changes);
  check_bool "new profile reports no previous value" true
    ((List.hd initial_changes).previous_profile = None);
  Alcotest.(check (list string))
    "profile fields are sorted"
    [ "avatar_url"; "com.example.opaque"; "displayname" ]
    (Option.get (Sync_service.find_profile state alice) |> List.map fst);
  check_bool "unknown JSON is retained" true
    (match Sync_service.find_profile_field state alice "com.example.opaque" with
    | Some value -> Jsont.Json.equal value opaque
    | None -> false);
  let updated_opaque =
    json_of {|{"items":[1,"two"],"nested":{"works":false}}|}
  in
  let state, changes =
    Sync_service.apply_profile_updates state
      (profile_updates
         [
           ( alice,
             Sliding_sync.Response.Updated
               [
                 ("displayname", json_of {|"Alice Updated"|});
                 ("avatar_url", json_of "null");
                 ("com.example.opaque", updated_opaque);
               ] );
         ])
  in
  check_int "one user changed" 1 (List.length changes);
  check_string "changed user" "@alice:example.org"
    (Id.User_id.to_string (List.hd changes).changed_user_id);
  check_string "updated field" "Alice Updated"
    (profile_string state alice "displayname");
  check_bool "explicit null deletes one field" true
    (Sync_service.find_profile_field state alice "avatar_url" = None);
  check_string "unrelated user is unchanged" "Bob"
    (profile_string state bob "displayname");
  let unchanged, changes =
    Sync_service.apply_profile_updates state
      (profile_updates
         [
           ( alice,
             Sliding_sync.Response.Updated
               [
                 ("displayname", json_of {|"Alice Updated"|});
                 ( "com.example.opaque",
                   json_of {|{"nested":{"works":false},"items":[1,"two"]}|} );
               ] );
         ])
  in
  check_int "semantic no-op reports nothing" 0 (List.length changes);
  check_string "no-op preserves the current value" "Alice Updated"
    (profile_string unchanged alice "displayname");
  let state, changes =
    Sync_service.apply_profile_updates state
      (profile_updates [ (charlie, Sliding_sync.Response.Updated []) ])
  in
  check_int "empty new profile is a change" 1 (List.length changes);
  check_bool "empty new profile remains present" true
    (Sync_service.find_profile state charlie = Some []);
  let state, changes =
    Sync_service.apply_profile_updates state
      (profile_updates [ (bob, Sliding_sync.Response.Dropped) ])
  in
  check_int "whole-user drop is reported" 1 (List.length changes);
  check_bool "whole-user drop removes the profile" true
    (Sync_service.find_profile state bob = None);
  let _, changes =
    Sync_service.apply_profile_updates state
      (profile_updates [ (bob, Sliding_sync.Response.Dropped) ])
  in
  check_int "dropping an absent user is a no-op" 0 (List.length changes)

let test_profile_persistence () =
  Eio_main.run @@ fun env ->
  let tmp = Filename.temp_file "matrix-profile-store" ".d" in
  Unix.unlink tmp;
  Unix.mkdir tmp 0o700;
  let dir = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  let bob = uid "@bob:example.org" in
  let opaque = json_of {|{"vendor":[true,3,{"x":null}]}|} in
  let store = Store.on_disk ~dir in
  let state = Sync_service.create ~user_id:alice () in
  let state, _ =
    Sync_service.apply_profile_updates state
      (profile_updates
         [
           ( alice,
             Sliding_sync.Response.Updated
               [
                 ("displayname", json_of {|"Alice"|});
                 ("com.example.vendor", opaque);
               ] );
           ( bob,
             Sliding_sync.Response.Updated
               [ ("displayname", json_of {|"Bob"|}) ] );
         ])
  in
  Sync_service.persist store state;
  (match Store.flush store with
  | Ok () -> ()
  | Error error -> Alcotest.failf "profile flush: %s" (Error.to_string error));
  let reopened = Store.on_disk ~dir in
  let restored = Sync_service.of_store reopened ~user_id:alice () in
  check_string "profile survives restart" "Alice"
    (profile_string restored alice "displayname");
  check_bool "opaque JSON survives restart" true
    (match
       Sync_service.find_profile_field restored alice "com.example.vendor"
     with
    | Some value -> Jsont.Json.equal value opaque
    | None -> false);
  let restored, _ =
    Sync_service.apply_profile_updates restored
      (profile_updates [ (alice, Sliding_sync.Response.Dropped) ])
  in
  Sync_service.persist reopened restored;
  (match Store.flush reopened with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "profile-drop flush: %s" (Error.to_string error));
  let final = Store.on_disk ~dir in
  let final_state = Sync_service.of_store final ~user_id:alice () in
  check_bool "dropped profile stays absent after restart" true
    (Sync_service.find_profile final_state alice = None);
  check_string "unrelated persisted profile remains" "Bob"
    (profile_string final_state bob "displayname")

let sample_room () =
  let base =
    Store.empty_room_info ~room_id:(rid "!s:example.org")
      ~membership:Store.Joined
  in
  {
    base with
    name = Some "Saved";
    topic = Some "persisted";
    avatar_url = Some "mxc://example.org/x";
    encryption = Some (json_of {|{"algorithm":"m.megolm.v1.aes-sha2"}|});
    heroes =
      [
        {
          Store.user_id = uid "@bob:example.org";
          display_name = Some "Bob";
          avatar_url = None;
        };
      ];
    joined_member_count = 7;
    invited_member_count = 1;
    is_dm = true;
    display_name = Store.Named "Saved";
    notification_count = 3;
    highlight_count = 1;
    local_unread_count = 4;
    local_notification_count = 2;
    local_highlight_count = 1;
    latest_event =
      Some
        {
          (ev_at 1 "@bob:example.org") with
          unsigned = Some (Event.Unsigned.make ~age:5L ());
        };
    prev_batch = Some "p-saved";
    tags = [ ("m.favourite", Some 0.5) ];
    last_active_ts = 1234L;
    recency_stamp = None;
    state_events =
      [
        {
          Store.event_type = Event.Event_type.Room_member;
          state_key = "@bob:example.org";
          content =
            json_of {|{"membership":"join","displayname":"Persisted Bob"}|};
          sender = Some (uid "@bob:example.org");
          event_id = Some (eid "$member:example.org");
          origin_server_ts = Some (Timestamp.of_ms 2L);
        };
        {
          Store.event_type = Event.Event_type.Room_power_levels;
          state_key = "";
          content =
            json_of
              {|{"users":{"@bob:example.org":50},"notifications":{"room":50}}|};
          sender = Some alice;
          event_id = Some (eid "$powers:example.org");
          origin_server_ts = Some (Timestamp.of_ms 3L);
        };
        {
          Store.event_type = Event.Event_type.Custom "com.example.persisted";
          state_key = "key";
          content = json_of {|{"opaque":true}|};
          sender = Some alice;
          event_id = Some (eid "$opaque:example.org");
          origin_server_ts = Some (Timestamp.of_ms 4L);
        };
      ];
    state_completeness = Store.Complete;
    members_complete = true;
    encryption_state_complete = true;
  }

let extra_slot = Store.Slot.v ~name:"extra" Matrix_proto.Json.Codec.json

let test_store_roundtrip () =
  Eio_main.run @@ fun env ->
  let tmp = Filename.temp_file "matrix-base-store" ".d" in
  Unix.unlink tmp;
  Unix.mkdir tmp 0o700;
  let dir = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  let store = Store.on_disk ~dir in
  check_bool "a fresh store is clean" false (Store.dirty store);
  Store.set_next_batch store "s42";
  Store.set_room store (sample_room ());
  Store.set_account_data store "m.direct"
    (json_of {|{"@bob:example.org":["!s:example.org"]}|});
  Store.set_receipts store (rid "!s:example.org")
    (Read_state.v
       ~public_read:
         { event_id = eid "$e1:example.org"; ts = Some (Timestamp.of_ms 7L) }
       ~fully_read:(eid "$e0:example.org") ());
  Store.replace_profiles store
    [
      ( uid "@bob:example.org",
        [
          ("z.vendor", json_of {|{"opaque":true}|});
          ("displayname", json_of {|"Stored Bob"|});
        ] );
    ];
  (match Store.Slot.set store extra_slot (json_of {|{"hello":1}|}) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "slot: %s" (Error.to_string e));
  check_bool "dirty" true (Store.dirty store);
  (match Store.flush store with
  | Ok () -> ()
  | Error e -> Alcotest.failf "flush: %s" (Error.to_string e));
  check_bool "clean after flush" false (Store.dirty store);
  let path = Eio.Path.(dir / "base_state.json") in
  let legacy_bytes =
    Eio.Path.load path
    |> replace_once ~needle:{|"origin_server_ts": 1001|}
         ~replacement:{|"origin_server_ts": "1001"|}
    |> replace_once ~needle:{|"age": 5|} ~replacement:{|"age": "5"|}
    |> replace_once ~needle:{|"origin_server_ts": 2|}
         ~replacement:{|"origin_server_ts": "2"|}
    |> replace_once ~needle:{|"ts": 7|} ~replacement:{|"ts": "7"|}
  in
  Eio.Path.save ~create:(`Or_truncate 0o600) path legacy_bytes;
  let mode = (Unix.stat (Filename.concat tmp "base_state.json")).Unix.st_perm in
  check_int "written 0600" 0o600 mode;
  (* Reopen and check everything survived. *)
  let reopened = Store.on_disk ~dir in
  check_string "next_batch" "s42"
    (Option.value (Store.next_batch reopened) ~default:"");
  check_int "one room" 1 (List.length (Store.rooms reopened));
  let r = Option.get (Store.find_room reopened (rid "!s:example.org")) in
  check_string "name" "Saved" (Option.value r.name ~default:"");
  check_string "display name" "Saved"
    (Store.display_name_to_string r.display_name);
  check_int "joined count" 7 r.joined_member_count;
  check_bool "is_dm" true r.is_dm;
  check_int "local highlight" 1 r.local_highlight_count;
  check_string "latest event" "$e1:example.org"
    (match r.latest_event with
    | Some e -> Option.fold ~none:"" ~some:Id.Event_id.to_string e.event_id
    | None -> "");
  Alcotest.(check (option int64))
    "latest event unsigned age" (Some 5L)
    (Option.bind r.latest_event (fun event ->
         Option.bind event.unsigned Event.Unsigned.age));
  check_string "hero display name" "Bob"
    (match r.heroes with
    | [ h ] -> Option.value h.display_name ~default:""
    | _ -> "");
  check_bool "tags" true (List.mem_assoc "m.favourite" r.tags);
  check_int "cached state" 3 (List.length r.state_events);
  check_bool "state completeness" true (r.state_completeness = Store.Complete);
  check_bool "member completeness" true r.members_complete;
  check_bool "encryption completeness" true r.encryption_state_complete;
  check_bool "unknown cached state" true
    (Store.find_state_event r
       ~event_type:(Event.Event_type.Custom "com.example.persisted")
       ~state_key:"key" ()
    <> None);
  check_bool "account data" true
    (Store.find_account_data reopened "m.direct" <> None);
  Alcotest.(check (list string))
    "profile fields are stored deterministically"
    [ "displayname"; "z.vendor" ]
    (match Store.profiles reopened with
    | [ (_, profile) ] -> List.map fst profile
    | profiles ->
        Alcotest.failf "expected one stored profile, got %d"
          (List.length profiles));
  check_string "receipt" "$e1:example.org"
    (match Store.receipts reopened (rid "!s:example.org") with
    | Some rr -> (
        match Read_state.public_read rr with
        | Some x -> Id.Event_id.to_string x.event_id
        | None -> "")
    | None -> "");
  check_bool "kv slot" true
    (match Store.Slot.find reopened extra_slot with
    | Ok (Some _) -> true
    | _ -> false);
  (* A state built from the store resumes incrementally. *)
  let state = Sync_service.of_store reopened ~user_id:alice () in
  check_string "state resumes the sync token" "s42"
    (Option.value (Sync_service.next_batch state) ~default:"");
  check_int "and its rooms" 1 (List.length (Sync_service.rooms state));
  Alcotest.(check (list string))
    "and its encryption recipients" [ "@bob:example.org" ]
    (List.map Id.User_id.to_string
       (Sync_service.members state (rid "!s:example.org")));
  check_bool "and its power levels" true
    (Push_evaluator.Context.power_levels
       (Sync_service.push_context state (rid "!s:example.org"))
    <> None);
  check_string "and its global profile" "Stored Bob"
    (profile_string state (uid "@bob:example.org") "displayname");
  (* A send queue persists through the same file. *)
  let queue = queue_of ~store:reopened () in
  let r =
    Send_queue.send_text queue ~room_id:(rid "!s:example.org") ~body:"later"
  in
  Send_queue.save queue;
  (match Store.flush reopened with
  | Ok () -> ()
  | Error e -> Alcotest.failf "flush: %s" (Error.to_string e));
  let again = Store.on_disk ~dir in
  let queue2 = queue_of ~store:again () in
  check_int "the queue survived" 1 (List.length (Send_queue.requests queue2));
  check_string "with its transaction id" (Send_queue.txn_id r)
    (Send_queue.txn_id (List.hd (Send_queue.requests queue2)));
  Store.clear again;
  (match Store.flush again with Ok () -> () | Error _ -> ());
  let empty = Store.on_disk ~dir in
  check_int "cleared" 0 (List.length (Store.rooms empty));
  check_int "profiles cleared" 0 (List.length (Store.profiles empty))

let test_store_v1_migration () =
  Eio_main.run @@ fun env ->
  let tmp = Filename.temp_file "matrix-base-store-v1" ".d" in
  Unix.unlink tmp;
  Unix.mkdir tmp 0o700;
  let dir = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  let path = Eio.Path.(dir / "base_state.json") in
  (* This is the complete shape written before the versioned state projection
     was added. In particular, a room has only [core] and [counts]. *)
  Eio.Path.save ~create:(`Or_truncate 0o600) path
    {|{
  "next_batch":"legacy",
  "rooms":[{
    "core":{"room_id":"!legacy:example.org","membership":"join",
            "name":"Before migration","heroes":[],
            "display_name":{"kind":"named","value":"Before migration"}},
    "counts":{"joined_member_count":2,"invited_member_count":0,
              "is_dm":false,"notification_count":0,"highlight_count":0,
              "local_unread_count":0,"local_notification_count":0,
              "local_highlight_count":0,"marked_unread":false,"tags":{},
              "last_active_ts":0}
  }],
  "account_data":{},"receipts":{},"kv":{}
}|};
  let store = Store.on_disk ~dir in
  check_string "legacy token" "legacy"
    (Option.value (Store.next_batch store) ~default:"");
  let room = Option.get (Store.find_room store (rid "!legacy:example.org")) in
  check_string "legacy room" "Before migration"
    (Option.value room.name ~default:"");
  check_string "legacy display name" "Before migration"
    (Store.display_name_to_string room.display_name);
  check_int "legacy joined count" 2 room.joined_member_count;
  check_int "old files default to no cached state" 0
    (List.length room.state_events);
  check_bool "old files report unknown completeness" true
    (room.state_completeness = Store.No_state);
  check_int "v1 files start with no profiles" 0
    (List.length (Store.profiles store));
  Store.set_next_batch store "migrated";
  (match Store.flush store with
  | Ok () -> ()
  | Error e -> Alcotest.failf "migrating flush: %s" (Error.to_string e));
  let written = Eio.Path.load path in
  check_bool "rewritten document is versioned" true
    (contains written {|"format_version": 4|});
  let reopened = Store.on_disk ~dir in
  check_string "rewritten token reloads" "migrated"
    (Option.value (Store.next_batch reopened) ~default:"")

let test_store_v2_migration () =
  Eio_main.run @@ fun env ->
  let tmp = Filename.temp_file "matrix-base-store-v2" ".d" in
  Unix.unlink tmp;
  Unix.mkdir tmp 0o700;
  let dir = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  let path = Eio.Path.(dir / "base_state.json") in
  Eio.Path.save ~create:(`Or_truncate 0o600) path
    {|{"format_version":2,"next_batch":"v2","rooms":[],
       "account_data":{},"receipts":{},"kv":{}}|};
  let store = Store.on_disk ~dir in
  check_string "v2 token" "v2"
    (Option.value (Store.next_batch store) ~default:"");
  check_int "v2 files start with no profiles" 0
    (List.length (Store.profiles store));
  Store.set_next_batch store "v4";
  (match Store.flush store with
  | Ok () -> ()
  | Error error -> Alcotest.failf "v2 migration: %s" (Error.to_string error));
  let written = Eio.Path.load path in
  check_bool "v2 rewrites as v4" true (contains written {|"format_version": 4|});
  let reopened = Store.on_disk ~dir in
  check_string "v4 token reloads" "v4"
    (Option.value (Store.next_batch reopened) ~default:"")

let test_store_stale_flush_conflict () =
  Eio_main.run @@ fun env ->
  let tmp = Filename.temp_file "matrix-base-store-conflict" ".d" in
  Unix.unlink tmp;
  Unix.mkdir tmp 0o700;
  let dir = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  let initial = Store.on_disk ~dir in
  Store.set_next_batch initial "initial";
  (match Store.flush initial with
  | Ok () -> ()
  | Error e -> Alcotest.failf "initial flush: %s" (Error.to_string e));
  let first = Store.on_disk ~dir in
  let stale = Store.on_disk ~dir in
  Store.set_next_batch first "first";
  (match Store.flush first with
  | Ok () -> ()
  | Error e -> Alcotest.failf "first flush: %s" (Error.to_string e));
  Store.set_next_batch stale "stale";
  (match Store.flush stale with
  | Error (Error.Policy_denied _) -> ()
  | Ok () -> Alcotest.fail "stale flush unexpectedly succeeded"
  | Error e -> Alcotest.failf "wrong stale flush error: %s" (Error.to_string e));
  check_bool "stale store remains dirty" true (Store.dirty stale);
  let after_conflict = Store.on_disk ~dir in
  check_string "conflict leaves newer disk state intact" "first"
    (Option.value (Store.next_batch after_conflict) ~default:"");
  let fresh = Store.on_disk ~dir in
  Store.set_next_batch fresh "fresh";
  (match Store.flush fresh with
  | Ok () -> ()
  | Error e -> Alcotest.failf "fresh flush: %s" (Error.to_string e));
  let final = Store.on_disk ~dir in
  check_string "freshly reopened handle can write" "fresh"
    (Option.value (Store.next_batch final) ~default:"")

let test_local_unread_counts_transition () =
  let _, state, _, _ = run_two_syncs () in
  let room_id = rid "!named:example.org" in
  let room = room_of state "!named:example.org" in
  let current =
    {
      Read_state.unread = room.local_unread_count;
      notifications = room.local_notification_count;
      highlights = room.local_highlight_count;
    }
  in
  let same = Sync_service.with_local_unread_counts state ~room_id current in
  check_bool "equal local counts preserve the physical state" true
    (same == state);
  let unknown =
    Sync_service.with_local_unread_counts state
      ~room_id:(rid "!unknown:example.org")
      current
  in
  check_bool "unknown room preserves the physical state" true (unknown == state);
  let changed = { Read_state.unread = 9; notifications = 8; highlights = 7 } in
  let updated = Sync_service.with_local_unread_counts state ~room_id changed in
  check_bool "changed local counts create a new state" true (updated != state);
  let updated_room = room_of updated "!named:example.org" in
  check_int "unread field changes" 9 updated_room.local_unread_count;
  check_int "notification field changes" 8 updated_room.local_notification_count;
  check_int "highlight field changes" 7 updated_room.local_highlight_count

let test_store_rejects_future_format () =
  Eio_main.run @@ fun env ->
  let tmp = Filename.temp_file "matrix-base-store-future" ".d" in
  Unix.unlink tmp;
  Unix.mkdir tmp 0o700;
  let dir = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(dir / "base_state.json")
    {|{"format_version":99,"next_batch":"unsafe","rooms":[],
       "account_data":{},"receipts":{},"kv":{}}|};
  let store = Store.on_disk ~dir in
  check_bool "a future document is not interpreted as version 3" true
    (Store.next_batch store = None)

let () =
  Alcotest.run "matrix base client"
    [
      ( "push evaluator",
        [
          Alcotest.test_case "glob and word matching" `Quick
            test_glob_word_matching;
          Alcotest.test_case "event_match on the body" `Quick
            test_event_match_on_body;
          Alcotest.test_case "contains_display_name" `Quick
            test_contains_display_name;
          Alcotest.test_case "room_member_count" `Quick test_room_member_count;
          Alcotest.test_case "sender_notification_permission" `Quick
            test_sender_notification_permission;
          Alcotest.test_case "event_property_is and contains" `Quick
            test_event_property_conditions;
          Alcotest.test_case "the default ruleset" `Quick
            test_default_ruleset_basics;
          Alcotest.test_case "precedence and enabled" `Quick
            test_rule_precedence_and_enabled;
          Alcotest.test_case "ruleset codec round trip" `Quick
            test_ruleset_codec_roundtrip;
          Alcotest.test_case "current push rules" `Quick
            test_push_rules_current_response;
          Alcotest.test_case "same-batch push-rule evaluation" `Quick
            test_push_rules_same_batch_evaluation;
          Alcotest.test_case "push rules restore from store" `Quick
            test_push_rules_restore_from_store;
          Alcotest.test_case "absent push rules preserve prior" `Quick
            test_push_rules_absent_preserves_prior;
          Alcotest.test_case "malformed push rules preserve prior" `Quick
            test_push_rules_malformed_preserves_prior;
          Alcotest.test_case "current push rules beat stored" `Quick
            test_push_rules_current_beats_stored;
        ] );
      ( "read receipts",
        [
          Alcotest.test_case "m.receipt ingestion" `Quick test_receipt_ingestion;
          Alcotest.test_case "a receipt without a timestamp does not regress"
            `Quick test_receipt_without_timestamp_does_not_regress;
          Alcotest.test_case "threaded receipts persist and count" `Quick
            test_threaded_receipts_persist_and_count;
          Alcotest.test_case "unread computation" `Quick test_unread_computation;
        ] );
      ( "sync service",
        [
          Alcotest.test_case "room-specific push display name" `Quick
            test_room_specific_push_display_name;
          Alcotest.test_case "room display names" `Quick
            (run test_sync_room_names);
          Alcotest.test_case "room metadata" `Quick
            (run test_sync_room_metadata);
          Alcotest.test_case "inviter lookup" `Quick test_inviter_lookup;
          Alcotest.test_case "durable complete room state" `Quick
            (run test_durable_room_state);
          Alcotest.test_case "an empty incremental room has unknown state"
            `Quick test_incremental_room_without_state_stays_unknown;
          Alcotest.test_case "full state replaces the prior projection" `Quick
            (run test_full_state_replaces_projection);
          Alcotest.test_case
            "encryption completeness follows membership transitions" `Quick
            test_encryption_completeness_does_not_survive_membership_transition;
          Alcotest.test_case "is_dm tracks m.direct, including un-marking"
            `Quick
            (run test_is_dm_tracks_m_direct);
          Alcotest.test_case "hero profile follows member state and refresh"
            `Quick test_hero_profile_tracks_member_state_and_refresh;
          Alcotest.test_case "stable marked-unread source wins" `Quick
            (run test_marked_unread_source_precedence);
          Alcotest.test_case "threaded receipt preserves marked unread" `Quick
            (run test_threaded_receipt_preserves_marked_unread);
          Alcotest.test_case "marked-unread source survives restart" `Quick
            test_marked_unread_source_survives_restart;
          Alcotest.test_case "presence reflects only the last response" `Quick
            (run test_presence_reflects_the_last_response_only);
          Alcotest.test_case "counts and latest event" `Quick
            (run test_sync_counts_and_latest_event);
          Alcotest.test_case "local unread count transition" `Quick
            (run test_local_unread_counts_transition);
          Alcotest.test_case "latest event plaintext policy" `Quick
            test_latest_event_plaintext_policy;
          Alcotest.test_case "next_batch threading" `Quick
            (run test_sync_token_threading);
          Alcotest.test_case "MSC4262 profile patches" `Quick
            test_profile_updates;
          Alcotest.test_case "hooks" `Quick (run test_sync_hooks);
        ] );
      ( "send queue",
        [
          Alcotest.test_case "success and local echo" `Quick
            (run test_send_queue_success);
          Alcotest.test_case "edit payload" `Quick
            (run test_send_queue_edit_payload);
          Alcotest.test_case "edit persistence" `Quick
            (run test_send_queue_edit_persistence);
          Alcotest.test_case "edit retry" `Quick
            (run test_send_queue_edit_retry);
          Alcotest.test_case "an exception does not wedge the request" `Quick
            (run test_send_queue_exception_does_not_wedge);
          Alcotest.test_case "429 then success" `Quick
            (run test_send_queue_rate_limited);
          Alcotest.test_case "M_FORBIDDEN wedges" `Quick
            (run test_send_queue_forbidden);
          Alcotest.test_case "error classification" `Quick
            (run test_send_queue_backoff);
          Alcotest.test_case "persisted queue reload" `Quick
            (run test_send_queue_persistence);
          Alcotest.test_case "reaction extra persistence" `Quick
            (run test_send_queue_reaction_extra_persistence);
          Alcotest.test_case "dependency readiness" `Quick
            (run test_send_queue_dependencies);
          Alcotest.test_case "recursive dependency cancellation" `Quick
            (run test_send_queue_recursive_cancel);
          Alcotest.test_case "in-flight cancellation redacts after success"
            `Quick
            (run test_send_queue_cancel_in_flight_redaction);
          Alcotest.test_case "in-flight cancellation drops failed sends" `Quick
            (run test_send_queue_cancel_in_flight_failure);
          Alcotest.test_case "existing cancellation id avoids random draw"
            `Quick
            (run test_send_queue_existing_cancel_txn_does_not_draw_randomness);
          Alcotest.test_case "cancellation random failure is atomic" `Quick
            (run test_send_queue_cancel_random_failure_is_atomic);
          Alcotest.test_case "retry random failure is atomic" `Quick
            (run test_send_queue_retry_random_failure_is_atomic);
          Alcotest.test_case "legacy cancellation id repair" `Quick
            (run test_send_queue_repairs_missing_cancel_txn);
          Alcotest.test_case "dependency validation" `Quick
            (run test_send_queue_dependency_validation);
          Alcotest.test_case "retry and wedge block children" `Quick
            (run test_send_queue_retry_wedge_blocks);
          Alcotest.test_case "resolved dependency reload" `Quick
            (run test_send_queue_resolved_reload);
          Alcotest.test_case "typed dependency result reload" `Quick
            (run test_send_queue_typed_dependency_result_reload);
          Alcotest.test_case "non-object event content is rejected" `Quick
            test_send_queue_rejects_non_object_content;
          Alcotest.test_case "malformed persisted graph is quarantined" `Quick
            (run test_send_queue_malformed_persisted_graph_is_quarantined);
          Alcotest.test_case "arbitrary transaction cache identity" `Quick
            (run test_send_queue_arbitrary_txn_cache_identity);
          Alcotest.test_case "unique transaction allocation" `Quick
            (run test_send_queue_allocates_unique_transactions);
          Alcotest.test_case "persisted cache identity is bound" `Quick
            (run test_send_queue_persisted_cache_identity);
          Alcotest.test_case "upload node" `Quick
            (run test_send_queue_upload_node);
          Alcotest.test_case "upload retry and cancellation" `Quick
            (run test_send_queue_upload_retry_cancel);
          Alcotest.test_case "media store upload lifecycle" `Quick
            (run test_send_queue_media_store_lifecycle);
          Alcotest.test_case "media store orphan reconciliation" `Quick
            test_send_queue_media_orphan_reconcile;
          Alcotest.test_case "media store cancellation after upload" `Quick
            (run test_send_queue_media_store_cancel_after_upload);
          Alcotest.test_case "upload result restart boundary" `Quick
            (run test_send_queue_upload_result_restart_boundary);
          Alcotest.test_case "upload result flag cleanup recovery" `Quick
            (run test_send_queue_upload_result_flag_failure);
          Alcotest.test_case "malformed upload result is graph-local" `Quick
            (run test_send_queue_malformed_upload_result_is_local);
          Alcotest.test_case "upload result without cache" `Quick
            (run test_send_queue_upload_result_without_cache);
          Alcotest.test_case "attachment substitution" `Quick
            (run test_send_queue_attachment_substitution);
          Alcotest.test_case "local echo media sources" `Quick
            (run test_send_queue_local_echo_media_sources);
          Alcotest.test_case "local echo media restart" `Quick
            test_send_queue_local_echo_media_restart;
          Alcotest.test_case "attachment caption edit" `Quick
            (run test_send_queue_attachment_caption_edit);
          Alcotest.test_case "in-flight attachment caption edit" `Quick
            (run test_send_queue_attachment_caption_in_flight);
          Alcotest.test_case "attachment caption cancellation race" `Quick
            (run test_send_queue_attachment_caption_cancel_race);
          Alcotest.test_case "diamond cancellation callbacks" `Quick
            (run test_send_queue_diamond_cancel_callbacks);
        ] );
      ( "timeline",
        [
          Alcotest.test_case "edits and redactions" `Quick
            (run test_timeline_edits_and_redactions);
          Alcotest.test_case "prepend applies an edit in the same batch" `Quick
            (run test_timeline_prepend_applies_edit_in_same_batch);
          Alcotest.test_case "local echo reconciliation" `Quick
            (run test_timeline_local_echo);
          Alcotest.test_case "back-pagination" `Quick
            (run test_timeline_pagination);
        ] );
      ( "store",
        [
          Alcotest.test_case "json round trip on disk" `Quick
            test_store_roundtrip;
          Alcotest.test_case "global profile persistence and drops" `Quick
            test_profile_persistence;
          Alcotest.test_case "pre-state-cache migration" `Quick
            test_store_v1_migration;
          Alcotest.test_case "v2 profile migration" `Quick
            test_store_v2_migration;
          Alcotest.test_case "stale flush is rejected without clobbering" `Quick
            test_store_stale_flush_conflict;
          Alcotest.test_case "future store format is rejected" `Quick
            test_store_rejects_future_format;
        ] );
    ]
