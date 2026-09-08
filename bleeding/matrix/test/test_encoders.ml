(** Regression tests for jsont request-body {b encoders}.

    A [Jsont.Object.mem "x" codec] written without [~enc] builds a codec that
    decodes but cannot encode: [Jsont_bytesrw.encode_string] answers
    [Error "No encoder for member x"]. Request codecs written that way make the
    endpoint that uses them fail inside
    {!Matrix_client.Client.Http.encode_body}, before any HTTP request is built —
    so the failure is invisible to any test that only exercises decoding, and
    the endpoint is dead on arrival.

    Two layers of cover here:

    - {b Per-codec regressions.} One test per request codec that has lost an
      [~enc] projection at some point, driving the public function through a
      {!Fetch_mock} client and asserting on the body that actually left the
      library.
    - {b The encoder guard} ({!val-guarded}). A table of every
      request-body-building entry point reachable from {!Matrix_client}. Each is
      driven against a mock and must (a) not fail with an encoder error and (b)
      actually put a body on the wire. A member that loses its [~enc] makes its
      row fail, so the bug class cannot come back silently. Add a row whenever a
      new body-carrying endpoint lands. *)

module Client = Matrix_client.Client
module Error = Matrix_client.Error
module Account = Matrix_client.Account
module Account_data = Matrix_client.Account_data
module Auth = Matrix_client.Auth
module Uiaa = Matrix_client.Uiaa
module Backup = Matrix_client.Backup
module Calls = Matrix_client.Calls
module Dehydrated_device = Matrix_client.Dehydrated_device
module Delayed_events = Matrix_client.Delayed_events
module Devices = Matrix_client.Devices
module Directory = Matrix_client.Directory
module Keys = Matrix_client.Keys
module Messages = Matrix_client.Messages
module Presence = Matrix_client.Presence
module Profile = Matrix_client.Profile
module Push = Matrix_client.Push
module Push_rule = Matrix_proto.Push
module Receipts = Matrix_client.Receipts
module Relations = Matrix_client.Relations
module Report = Matrix_client.Report
module Room_keys = Matrix_client.Room_keys
module Rooms = Matrix_client.Rooms
module Search = Matrix_client.Search
module Spaces = Matrix_client.Spaces
module State = Matrix_client.State
module Tags = Matrix_client.Tags
module Thread_subscriptions = Matrix_client.Thread_subscriptions
module To_device = Matrix_client.To_device
module Typing = Matrix_client.Typing
module Id = Matrix_proto.Id

(* {1 Harness}

   The same [Fetch_mock] harness [test_matrix_client.ml] and [test_cs_api.ml]
   use, copied rather than shared so the test files stay independent. *)

let mock_env =
  object
    method secure_random =
      Eio.Flow.string_source (String.init 4096 (fun i -> Char.chr (i land 255)))
  end

type recorded = {
  meth : string;
  url : string;
  body : string option;  (** [None] for a request with no body. *)
}

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

let mock handler =
  let log = ref [] in
  let client =
    Fetch_mock.client (fun req ->
        record log req;
        if
          String.ends_with ~suffix:"/_matrix/client/versions"
            (Fetch.Middleware.Url.to_string req.url)
        then Fetch_mock.respond {|{"versions":["v1.20"]}|} req
        else handler req)
  in
  (log, client)

let default_homeserver = "https://hs.example"

let client_of fetch =
  let config =
    Client.config ~homeserver:(Uriz.of_string_exn default_homeserver) ()
  in
  Client.create ~config ~fetch ~random:(Matrix_client.Random.of_env mock_env)

let uid s = Result.get_ok (Id.User_id.of_string s)
let mxc s = Result.get_ok (Matrix_client.Media.Mxc.of_string s)
let did s = Result.get_ok (Id.Device_id.of_string s)
let rid s = Result.get_ok (Id.Room_id.of_string s)
let sid s = Result.get_ok (Id.Session_id.of_string s)
let eid s = Result.get_ok (Id.Event_id.of_string s)
let alias s = Result.get_ok (Id.Room_alias.of_string s)

let test_session : Client.session =
  {
    user_id = uid "@alice:example.org";
    access_token = "syt_secret_token";
    device_id = did "TESTDEVICE";
    refresh_token = None;
  }

let logged_in fetch = Client.with_session (client_of fetch) test_session
let requests log = List.rev !log

let one_request log =
  match requests log with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected exactly one request, got %d" (List.length rs)

let last_request log =
  match List.rev (requests log) with
  | r :: _ -> r
  | [] -> Alcotest.fail "no request reached the network"

let run f () = Eio_mock.Backend.run f
let json body = Fetch_mock.respond body

let json_value text =
  match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json text with
  | Ok value -> value
  | Error msg -> Alcotest.failf "invalid test JSON: %s" msg

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "expected Ok, got error: %s" (Error.to_string e)

(* {2 Reading a recorded body back}

   The assertions decode the body the library sent and look members up by
   path, rather than pinning a byte-for-byte serialisation: what matters is
   that the value survived the round trip under the right member names. *)

let decode_body r =
  match r.body with
  | None -> Alcotest.fail "request carried no body"
  | Some s -> (
      match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json s with
      | Ok j -> j
      | Error e -> Alcotest.failf "body is not JSON (%s): %s" e s)

let rec find_path (j : Jsont.json) = function
  | [] -> Some j
  | n :: rest -> (
      match j with
      | Jsont.Object (ms, _) -> (
          match Jsont.Json.find_mem n ms with
          | Some (_, v) -> find_path v rest
          | None -> None)
      | _ -> None)

let path_of_string p = String.split_on_char '/' p

(* [str r p] is the string at slash-separated path [p] of [r]'s body. *)
let str r p =
  match find_path (decode_body r) (path_of_string p) with
  | Some (Jsont.String (s, _)) -> Some s
  | _ -> None

let bool_ r p =
  match find_path (decode_body r) (path_of_string p) with
  | Some (Jsont.Bool (b, _)) -> Some b
  | _ -> None

let num r p =
  match find_path (decode_body r) (path_of_string p) with
  | Some (Jsont.Number (f, _)) -> Some f
  | _ -> None

(* [names r p] are the member names of the object at path [p]. *)
let names r p =
  match find_path (decode_body r) (path_of_string p) with
  | Some (Jsont.Object (ms, _)) ->
      Some (List.sort compare (Jsont.Json.object_names ms))
  | _ -> None

let check_str p expected r =
  Alcotest.(check (option string)) p (Some expected) (str r p)

let check_bool p expected r =
  Alcotest.(check (option bool)) p (Some expected) (bool_ r p)

let check_num p expected r =
  Alcotest.(check (option (float 0.001))) p (Some expected) (num r p)

(* {1 Per-codec regressions}

   Each of these drove [Client.Http.encode_body] to
   [Error (Json_error "No encoder for member ...")] before the fix, so the
   function never reached the network at all. *)

(* {2 Account} *)

let test_account_request_email_token () =
  let log, fetch = mock (json {|{"sid":"s1"}|}) in
  ignore
    (ok
       (Account.request_email_token (logged_in fetch) ~email:"a@example.org"
          ~client_secret:"sec" ~send_attempt:2));
  let r = one_request log in
  check_str "email" "a@example.org" r;
  check_str "client_secret" "sec" r;
  check_num "send_attempt" 2. r

let test_account_request_msisdn_token () =
  let log, fetch = mock (json {|{"sid":"s1"}|}) in
  ignore
    (ok
       (Account.request_msisdn_token (logged_in fetch) ~country:"GB"
          ~phone_number:"7700900000" ~client_secret:"sec" ~send_attempt:1));
  let r = one_request log in
  check_str "country" "GB" r;
  check_str "phone_number" "7700900000" r;
  check_str "client_secret" "sec" r;
  check_num "send_attempt" 1. r

let test_account_add_3pid () =
  let log, fetch = mock (json "{}") in
  ok (Account.add_threepid (logged_in fetch) ~client_secret:"sec" ~sid:"s1");
  let r = one_request log in
  check_str "client_secret" "sec" r;
  check_str "sid" "s1" r

let test_account_delete_3pid () =
  let log, fetch = mock (json "{}") in
  ok
    (Account.delete_threepid (logged_in fetch) ~medium:Account.Email
       ~address:"a@example.org");
  let r = one_request log in
  check_str "medium" "email" r;
  check_str "address" "a@example.org" r

let test_account_change_password () =
  let log, fetch = mock (json "{}") in
  ok
    (Account.change_password (logged_in fetch) ~new_password:"hunter2"
       ~logout_devices:true ());
  let r = one_request log in
  check_str "new_password" "hunter2" r;
  check_bool "logout_devices" true r

let test_account_deactivate () =
  let log, fetch = mock (json "{}") in
  ok (Account.deactivate (logged_in fetch) ~erase:true ());
  check_bool "erase" true (one_request log)

let test_account_ignore_user () =
  (* [ignore_user] reads the current list, then writes it back: the GET is
     answered with an empty list, the PUT is the request under test. *)
  let responses = ref [ {|{"ignored_users":{}}|}; "{}" ] in
  let log, fetch =
    mock (fun req ->
        match !responses with
        | b :: rest ->
            responses := rest;
            Fetch_mock.respond b req
        | [] -> Alcotest.fail "unexpected third request")
  in
  ok (Account.ignore_user (logged_in fetch) ~user_id:(uid "@bob:example.org"));
  let r = last_request log in
  Alcotest.(check string) "method" "PUT" r.meth;
  Alcotest.(check (option (list string)))
    "ignored_users" (Some [ "@bob:example.org" ]) (names r "ignored_users")

let test_account_unignore_user () =
  let responses =
    ref
      [
        {|{"ignored_users":{"@bob:example.org":{},"@carol:example.org":{}}}|};
        "{}";
      ]
  in
  let log, fetch =
    mock (fun req ->
        match !responses with
        | b :: rest ->
            responses := rest;
            Fetch_mock.respond b req
        | [] -> Alcotest.fail "unexpected third request")
  in
  ok (Account.unignore_user (logged_in fetch) ~user_id:(uid "@bob:example.org"));
  let r = last_request log in
  Alcotest.(check (option (list string)))
    "ignored_users" (Some [ "@carol:example.org" ]) (names r "ignored_users")

(* {2 Devices} *)

let test_devices_update_device () =
  let log, fetch = mock (json "{}") in
  ok
    (Devices.update_device (logged_in fetch) ~device_id:(did "ABC")
       ~display_name:"laptop");
  check_str "display_name" "laptop" (one_request log)

let test_devices_delete_devices () =
  let log, fetch = mock (json "{}") in
  ok (Devices.delete_devices (logged_in fetch) ~device_ids:[ did "A"; did "B" ]);
  let r = one_request log in
  match find_path (decode_body r) [ "devices" ] with
  | Some (Jsont.Array (l, _)) ->
      Alcotest.(check int) "two devices" 2 (List.length l)
  | _ -> Alcotest.fail "no devices array"

(* {2 Presence} *)

let test_presence_set_presence () =
  let log, fetch = mock (json "{}") in
  ok
    (Presence.set_presence (logged_in fetch) ~presence:Presence.Online
       ~status_msg:"here" ());
  let r = one_request log in
  check_str "presence" "online" r;
  check_str "status_msg" "here" r

(* {2 Push} *)

let test_push_set_pusher () =
  let log, fetch = mock (json "{}") in
  ok
    (Push.set_pusher (logged_in fetch) ~pushkey:"key" ~kind:Push.Http
       ~app_id:"com.example" ~app_display_name:"Example"
       ~device_display_name:"Phone" ~lang:"en"
       ~data:
         {
           Push.url = Some "https://push.example/_matrix/push/v1/notify";
           format = None;
         }
       ());
  let r = one_request log in
  check_str "pushkey" "key" r;
  check_str "kind" "http" r;
  check_str "app_id" "com.example" r;
  check_str "app_display_name" "Example" r;
  check_str "device_display_name" "Phone" r;
  check_str "lang" "en" r;
  check_str "data/url" "https://push.example/_matrix/push/v1/notify" r

(* {2 Receipts} *)

let test_receipts_set_read_marker () =
  let log, fetch = mock (json "{}") in
  ok
    (Receipts.set_read_marker (logged_in fetch)
       ~room_id:(rid "!room:example.org") ~fully_read:(eid "$fully:example.org")
       ~read:(eid "$read:example.org")
       ~read_private:(eid "$private:example.org")
       ());
  match requests log with
  | [ marker; clear ] ->
      Alcotest.(check string) "marker method" "POST" marker.meth;
      Alcotest.(check string)
        "marker URL"
        "https://hs.example/_matrix/client/v3/rooms/!room:example.org/read_markers"
        marker.url;
      check_str "m.fully_read" "$fully:example.org" marker;
      check_str "m.read" "$read:example.org" marker;
      check_str "m.read.private" "$private:example.org" marker;
      Alcotest.(check string) "clear method" "PUT" clear.meth;
      Alcotest.(check string)
        "clear URL"
        "https://hs.example/_matrix/client/v3/user/@alice:example.org/rooms/!room:example.org/account_data/m.marked_unread"
        clear.url;
      Alcotest.(check (option string))
        "clear body" (Some {|{"unread":false}|}) clear.body
  | rs ->
      Alcotest.failf "expected marker then clear, got %d requests"
        (List.length rs)

let test_receipts_send_clears_marked_unread () =
  let log, fetch = mock (json "{}") in
  ok
    (Receipts.send_receipt (logged_in fetch) ~room_id:(rid "!room:example.org")
       ~event_id:(eid "$read:example.org") ());
  match requests log with
  | [ receipt; clear ] ->
      Alcotest.(check string) "receipt method" "POST" receipt.meth;
      Alcotest.(check string)
        "receipt URL"
        "https://hs.example/_matrix/client/v3/rooms/!room:example.org/receipt/m.read/$read:example.org"
        receipt.url;
      Alcotest.(check (option string)) "receipt body" (Some "{}") receipt.body;
      Alcotest.(check string) "clear method" "PUT" clear.meth;
      Alcotest.(check string)
        "clear URL"
        "https://hs.example/_matrix/client/v3/user/@alice:example.org/rooms/!room:example.org/account_data/m.marked_unread"
        clear.url;
      Alcotest.(check (option string))
        "clear body" (Some {|{"unread":false}|}) clear.body
  | rs ->
      Alcotest.failf "expected receipt then clear, got %d requests"
        (List.length rs)

let test_receipts_thread_and_fully_read_validation () =
  let log, fetch = mock (json "{}") in
  ok
    (Receipts.send_receipt (logged_in fetch) ~room_id:(rid "!room:example.org")
       ~event_id:(eid "$reply:example.org")
       ~thread_id:(eid "$thread:example.org")
       ());
  (match requests log with
  | [ receipt ] ->
      Alcotest.(check (option string))
        "threaded receipt body" (Some {|{"thread_id":"$thread:example.org"}|})
        receipt.body;
      Alcotest.(check string)
        "threaded receipt URL"
        "https://hs.example/_matrix/client/v3/rooms/!room:example.org/receipt/m.read/$reply:example.org"
        receipt.url
  | rs ->
      Alcotest.failf "expected threaded receipt only, got %d requests"
        (List.length rs));
  let rejected_log, rejected_fetch = mock (json "{}") in
  (match
     Receipts.send_receipt (logged_in rejected_fetch)
       ~room_id:(rid "!room:example.org")
       ~event_id:(eid "$marker:example.org")
       ~receipt_type:Receipts.Fully_read
       ~thread_id:(eid "$thread:example.org")
       ()
   with
  | Error (Error.Policy_denied _) -> ()
  | Ok () -> Alcotest.fail "fully-read receipt with thread unexpectedly sent"
  | Error error ->
      Alcotest.failf "wrong fully-read validation error: %s"
        (Error.to_string error));
  Alcotest.(check int)
    "rejected fully-read receipt made no request" 0
    (List.length (requests rejected_log))

let test_receipts_do_not_clear_after_failure () =
  let log, fetch = mock (fun req -> Fetch_mock.respond ~status:500 "{}" req) in
  match
    Receipts.send_receipt (logged_in fetch) ~room_id:(rid "!room:example.org")
      ~event_id:(eid "$read:example.org") ()
  with
  | Error _ ->
      Alcotest.(check int)
        "failed receipt has no clear" 1
        (List.length (requests log))
  | Ok () -> Alcotest.fail "failed receipt unexpectedly succeeded"

let test_receipts_propagate_clear_failure () =
  let n = ref 0 in
  let log, fetch =
    mock (fun req ->
        incr n;
        if !n = 1 then Fetch_mock.respond "{}" req
        else Fetch_mock.respond ~status:500 "{}" req)
  in
  match
    Receipts.send_receipt (logged_in fetch) ~room_id:(rid "!room:example.org")
      ~event_id:(eid "$read:example.org") ()
  with
  | Error _ ->
      Alcotest.(check int)
        "clear failure is second request" 2
        (List.length (requests log))
  | Ok () -> Alcotest.fail "clear failure unexpectedly succeeded"

(* {2 Relations} *)

let send_response = json {|{"event_id":"$sent:example.org"}|}

let test_relations_send_reaction () =
  let log, fetch = mock send_response in
  ignore
    (ok
       (Relations.send_reaction (logged_in fetch)
          ~room_id:(rid "!room:example.org")
          ~event_id:(eid "$target:example.org")
          ~key:"\xf0\x9f\x91\x8d"));
  let r = one_request log in
  check_str "m.relates_to/rel_type" "m.annotation" r;
  check_str "m.relates_to/event_id" "$target:example.org" r;
  check_str "m.relates_to/key" "\xf0\x9f\x91\x8d" r

let test_extra_content_does_not_override_typed_fields () =
  let log, fetch = mock send_response in
  let extra =
    Jsont.Json.object'
      [
        Jsont.Json.mem (Jsont.Json.name "vendor") (Jsont.Json.string "ok");
        Jsont.Json.mem (Jsont.Json.name "msgtype") (Jsont.Json.string "m.bad");
        Jsont.Json.mem (Jsont.Json.name "body") (Jsont.Json.string "bad");
        Jsont.Json.mem (Jsont.Json.name "url") (Jsont.Json.string "mxc://bad");
        Jsont.Json.mem (Jsont.Json.name "m.relates_to") (Jsont.Json.object' []);
      ]
  in
  ignore
    (ok
       (Messages.send_text (logged_in fetch) ~room_id:(rid "!room:example.org")
          ~body:"hello" ~extra_content:extra ()));
  ignore
    (ok
       (Messages.send_image (logged_in fetch) ~room_id:(rid "!room:example.org")
          ~body:"picture"
          ~url:(mxc "mxc://example.org/p")
          ~extra_content:extra ()));
  ignore
    (ok
       (Relations.send_reaction (logged_in fetch)
          ~room_id:(rid "!room:example.org")
          ~event_id:(eid "$target:example.org")
          ~key:"+1" ~extra_content:extra));
  match requests log with
  | [ text; image; reaction ] ->
      check_str "msgtype" "m.text" text;
      check_str "body" "hello" text;
      check_str "vendor" "ok" text;
      check_str "msgtype" "m.image" image;
      check_str "body" "picture" image;
      check_str "url" "mxc://example.org/p" image;
      check_str "vendor" "ok" image;
      check_str "m.relates_to/rel_type" "m.annotation" reaction;
      check_str "m.relates_to/event_id" "$target:example.org" reaction;
      check_str "m.relates_to/key" "+1" reaction;
      check_str "vendor" "ok" reaction
  | rs ->
      Alcotest.failf "expected three extra-content requests, got %d"
        (List.length rs)

let test_relations_edit_message () =
  let log, fetch = mock send_response in
  ignore
    (ok
       (Relations.edit_message (logged_in fetch)
          ~room_id:(rid "!room:example.org")
          ~event_id:(eid "$target:example.org")
          ~new_body:"fixed" ()));
  let r = one_request log in
  check_str "msgtype" "m.text" r;
  check_str "body" "* fixed" r;
  check_str "m.new_content/msgtype" "m.text" r;
  check_str "m.new_content/body" "fixed" r;
  check_str "m.relates_to/rel_type" "m.replace" r;
  check_str "m.relates_to/event_id" "$target:example.org" r

let test_relations_send_reply () =
  let log, fetch = mock send_response in
  ignore
    (ok
       (Relations.send_reply (logged_in fetch)
          ~room_id:(rid "!room:example.org")
          ~event_id:(eid "$target:example.org")
          ~body:"sure" ()));
  let r = one_request log in
  check_str "msgtype" "m.text" r;
  check_str "body" "sure" r;
  check_str "m.relates_to/m.in_reply_to/event_id" "$target:example.org" r

let test_relations_send_in_thread () =
  let log, fetch = mock send_response in
  ignore
    (ok
       (Relations.send_in_thread (logged_in fetch)
          ~room_id:(rid "!room:example.org")
          ~thread_root_id:(eid "$root:example.org")
          ~reply_to_id:(eid "$prev:example.org") ~body:"in thread" ()));
  let r = one_request log in
  check_str "msgtype" "m.text" r;
  check_str "body" "in thread" r;
  check_str "m.relates_to/rel_type" "m.thread" r;
  check_str "m.relates_to/event_id" "$root:example.org" r;
  (* [is_falling_back] is false exactly when an explicit reply target was
     given, per the threading fallback rules. *)
  check_bool "m.relates_to/is_falling_back" false r;
  check_str "m.relates_to/m.in_reply_to/event_id" "$prev:example.org" r

let test_relations_thread_falls_back () =
  let log, fetch = mock send_response in
  ignore
    (ok
       (Relations.send_in_thread (logged_in fetch)
          ~room_id:(rid "!room:example.org")
          ~thread_root_id:(eid "$root:example.org") ~body:"in thread" ()));
  let r = one_request log in
  check_bool "m.relates_to/is_falling_back" true r;
  (* A threadless client still needs somewhere to render this as a reply, so
     the fallback points at the thread root itself. *)
  check_str "m.relates_to/m.in_reply_to/event_id" "$root:example.org" r

(* {2 State} *)

let state_response = json {|{"event_id":"$state:example.org"}|}

let test_state_set_name () =
  let log, fetch = mock state_response in
  ignore
    (ok
       (State.set_name (logged_in fetch) ~room_id:(rid "!room:example.org")
          ~name:"Lounge"));
  check_str "name" "Lounge" (one_request log)

let test_state_set_topic () =
  let log, fetch = mock state_response in
  ignore
    (ok
       (State.set_topic (logged_in fetch) ~room_id:(rid "!room:example.org")
          ~topic:"chatter"));
  check_str "topic" "chatter" (one_request log)

let test_state_set_avatar () =
  let log, fetch = mock state_response in
  ignore
    (ok
       (State.set_avatar (logged_in fetch) ~room_id:(rid "!room:example.org")
          ~avatar_url:(mxc "mxc://example.org/pic")));
  check_str "url" "mxc://example.org/pic" (one_request log)

(* {2 Typing} *)

let check_typing_users name expected content =
  match Typing.users_of_content (json_value content) with
  | Ok users ->
      Alcotest.(check (list string))
        name expected
        (List.map Id.User_id.to_string users)
  | Error error ->
      Alcotest.failf "expected users, got %s" (Error.to_string error)

let check_typing_json_error name content =
  match Typing.users_of_content (json_value content) with
  | Error (Error.Json_error _) -> ()
  | Ok _ -> Alcotest.failf "expected Json_error for %s" name
  | Error error ->
      Alcotest.failf "expected Json_error for %s, got %s" name
        (Error.to_string error)

let test_typing_decode_valid () =
  check_typing_users "valid user_ids"
    [ "@bob:example.org"; "@alice:example.org"; "@bob:example.org" ]
    {|{"user_ids":["@bob:example.org","@alice:example.org","@bob:example.org"]}|}

let test_typing_decode_empty () =
  check_typing_users "empty user_ids" [] {|{"user_ids":[]}|}

let test_typing_decode_missing () =
  check_typing_json_error "missing user_ids" {|{}|}

let test_typing_decode_bad_id () =
  check_typing_json_error "bad user id" {|{"user_ids":["not-a-user-id"]}|}

let test_typing_decode_bad_shape () =
  check_typing_json_error "bad user_ids shape"
    {|{"user_ids":"@alice:example.org"}|}

let test_typing_decode_unknown_field () =
  check_typing_users "unknown content field" [] {|{"user_ids":[],"extra":true}|}

let test_typing_set_typing () =
  let log, fetch = mock (json "{}") in
  ok
    (Typing.set_typing (logged_in fetch) ~room_id:(rid "!room:example.org")
       ~typing:true ~timeout:30000 ());
  let r = one_request log in
  check_bool "typing" true r;
  check_num "timeout" 30000. r

let test_typing_stop () =
  let log, fetch = mock (json "{}") in
  ok
    (Typing.set_typing (logged_in fetch) ~room_id:(rid "!room:example.org")
       ~typing:false ());
  check_bool "typing" false (one_request log)

(* {1 The encoder guard}

   Every request-body-building entry point reachable from {!Matrix_client},
   driven against a permissive mock. The mock answers with a blob carrying the
   field names the various response codecs look for; a decode failure on the
   reply is fine here, since the assertion is about what went {e out}.

   Two things are checked per row:

   - the call did not fail with an encoder error (a missing [~enc] surfaces as
     [Json_error "No encoder for member ..."]), and
   - a request carrying a body actually reached the network — which is the
     part a missing [~enc] makes impossible, since [encode_body] fails before
     the request is built. *)

let generic_reply =
  {|{"event_id":"$e:example.org","room_id":"!r:example.org",
     "version":"1","etag":"etag","count":1,
     "delay_id":"delay1","sid":"sid1",
     "user_id":"@alice:example.org","access_token":"tok","device_id":"DEV",
     "device_id_":"DEV","chunk":[],"joined_rooms":[],
     "one_time_key_counts":{},"failures":{},"device_keys":{},
     "one_time_keys":{},"search_categories":{"room_events":{}},
     "results":[],"limited":false,"content":{},"servers":[],
     "delayed_events":[],"total_room_count_estimate":0}|}

(* [ignore_result] erases a call's success type so rows of different return
   types share one table. *)
let ignore_result r = Result.map (fun _ -> ()) r
let sdp_invite = Matrix_proto.Event.Sdp.v ~type_:"offer" ~sdp:"v=0"
let sdp_answer = Matrix_proto.Event.Sdp.v ~type_:"answer" ~sdp:"v=0"

let backup_session_data : Room_keys.key_backup_data =
  {
    first_message_index = 0;
    forwarded_count = 0;
    is_verified = true;
    session_data = { Backup.ephemeral = "eph"; ciphertext = "ct"; mac = "mac" };
  }

let power_levels =
  Matrix_proto.Event.Room_power_levels_content.make ~ban:50
    ~events:[ ("m.room.name", 100) ]
    ~events_default:0 ~invite:50 ~kick:50 ~redact:50 ~state_default:50
    ~users:[ ("@alice:example.org", 100) ]
    ~users_default:0
    ~notifications:[ ("room", 50) ]
    ()

(* Rows are [name, call]. [call] gets a logged-in client. *)
let guarded_calls : (string * (Client.t -> (unit, Error.t) result)) list =
  [
    (* Account *)
    ( "Account_data.set",
      fun t ->
        Account_data.set t
          ~event_type:(Matrix_proto.Event.Event_type.Custom "m.custom")
          ~content:(Jsont.Json.object' []) );
    ( "Account_data.set_room",
      fun t ->
        Account_data.set_room t ~room_id:(rid "!r:example.org")
          ~event_type:(Matrix_proto.Event.Event_type.Custom "m.custom")
          ~content:(Jsont.Json.object' []) );
    ( "Account.request_email_token",
      fun t ->
        ignore_result
          (Account.request_email_token t ~email:"a@example.org"
             ~client_secret:"s" ~send_attempt:1) );
    ( "Account.request_msisdn_token",
      fun t ->
        ignore_result
          (Account.request_msisdn_token t ~country:"GB" ~phone_number:"7700"
             ~client_secret:"s" ~send_attempt:1) );
    ( "Account.add_threepid",
      fun t -> Account.add_threepid t ~client_secret:"s" ~sid:"i" );
    ( "Account.delete_threepid",
      fun t ->
        Account.delete_threepid t ~medium:Account.Email ~address:"a@example.org"
    );
    ( "Account.change_password",
      fun t -> Account.change_password t ~new_password:"p" () );
    ("Account.deactivate", fun t -> Account.deactivate t ());
    (* Auth *)
    ( "Auth.login_password",
      fun t ->
        ignore_result
          (Auth.login_password t ~user:"alice" ~password:"hunter2" ()) );
    ( "Auth.login_token",
      fun t -> ignore_result (Auth.login_token t ~token:"tok" ()) );
    ( "Auth.refresh_token",
      fun t -> ignore_result (Auth.refresh_token t ~refresh_token:"r") );
    ("Auth.register", fun t -> ignore_result (Auth.register t ~username:"u" ()));
    ("Auth.get_login_token", fun t -> ignore_result (Auth.get_login_token t ()));
    ( "Uiaa.request_email_token (register)",
      fun t ->
        ignore_result
          (Uiaa.request_email_token t ~use:Uiaa.Register ~email:"a@example.org"
             ~client_secret:"s" ~send_attempt:1 ()) );
    ( "Uiaa.request_msisdn_token (register)",
      fun t ->
        ignore_result
          (Uiaa.request_msisdn_token t ~use:Uiaa.Register ~country:"GB"
             ~phone_number:"7700" ~client_secret:"s" ~send_attempt:1 ()) );
    ( "Uiaa.request_email_token (password)",
      fun t ->
        ignore_result
          (Uiaa.request_email_token t ~use:Uiaa.Password ~email:"a@example.org"
             ~client_secret:"s" ~send_attempt:1 ()) );
    ( "Uiaa.request_msisdn_token (password)",
      fun t ->
        ignore_result
          (Uiaa.request_msisdn_token t ~use:Uiaa.Password ~country:"GB"
             ~phone_number:"7700" ~client_secret:"s" ~send_attempt:1 ()) );
    (* Calls *)
    ( "Calls.send_invite",
      fun t ->
        ignore_result
          (Calls.send_invite t ~room_id:(rid "!r:example.org")
             ~call_id:(Calls.call_id_of_string "c")
             ~party_id:(Calls.party_id_of_string "p")
             ~offer:sdp_invite ~lifetime:60000 ()) );
    ( "Calls.send_answer",
      fun t ->
        ignore_result
          (Calls.send_answer t ~room_id:(rid "!r:example.org")
             ~call_id:(Calls.call_id_of_string "c")
             ~party_id:(Calls.party_id_of_string "p")
             ~answer:sdp_answer ()) );
    ( "Calls.send_candidates",
      fun t ->
        ignore_result
          (Calls.send_candidates t ~room_id:(rid "!r:example.org")
             ~call_id:(Calls.call_id_of_string "c")
             ~party_id:(Calls.party_id_of_string "p")
             ~candidates:[] ()) );
    ( "Calls.send_hangup",
      fun t ->
        ignore_result
          (Calls.send_hangup t ~room_id:(rid "!r:example.org")
             ~call_id:(Calls.call_id_of_string "c")
             ~party_id:(Calls.party_id_of_string "p")
             ()) );
    ( "Calls.send_reject",
      fun t ->
        ignore_result
          (Calls.send_reject t ~room_id:(rid "!r:example.org")
             ~call_id:(Calls.call_id_of_string "c")
             ~party_id:(Calls.party_id_of_string "p")
             ()) );
    (* Dehydrated devices *)
    ( "Dehydrated_device.put",
      fun t ->
        ignore_result
          (Dehydrated_device.put t ~device_id:(did "DEV")
             ~device_data:(Jsont.Json.object' []) ()) );
    ( "Dehydrated_device.get_events",
      fun t ->
        ignore_result (Dehydrated_device.get_events t ~device_id:(did "DEV") ())
    );
    (* Delayed events *)
    ( "Delayed_events.send_state",
      fun t ->
        ignore_result
          (Delayed_events.send_state t ~room_id:(rid "!r:example.org")
             ~event_type:"m.room.topic" ~state_key:""
             ~content:(Jsont.Json.object' []) ~delay_ms:1000) );
    ( "Delayed_events.update",
      fun t ->
        Delayed_events.update t
          ~delay_id:(Delayed_events.delay_id_of_string "d")
          ~action:Delayed_events.Send );
    (* Devices *)
    ( "Devices.update_device",
      fun t -> Devices.update_device t ~device_id:(did "D") ~display_name:"n" );
    ( "Devices.delete_devices",
      fun t -> Devices.delete_devices t ~device_ids:[ did "D" ] );
    (* Directory *)
    ( "Directory.create_alias",
      fun t ->
        Directory.create_alias t
          ~alias:(alias "#room:example.org")
          ~room_id:(rid "!r:example.org") );
    ( "Directory.set_visibility",
      fun t ->
        Directory.set_visibility t ~room_id:(rid "!r:example.org")
          ~visibility:Matrix_proto.Common.Visibility.Public );
    ( "Directory.search_public_rooms",
      fun t -> ignore_result (Directory.search_public_rooms t ~limit:10 ()) );
    (* Keys *)
    ("Keys.upload_keys", fun t -> ignore_result (Keys.upload_keys t ()));
    ( "Keys.query_keys",
      fun t ->
        ignore_result
          (Keys.query_keys t ~users:[ (uid "@bob:example.org", []) ] ()) );
    ( "Keys.claim_keys",
      fun t ->
        ignore_result
          (Keys.claim_keys t
             ~keys:
               [
                 (uid "@bob:example.org", [ (did "DEV", "signed_curve25519") ]);
               ]
             ()) );
    ( "Keys.upload_signatures",
      fun t ->
        ignore_result
          (Keys.upload_signatures t
             [ (uid "@bob:example.org", [ ("DEV", Jsont.Json.object' []) ]) ])
    );
    ("Keys.upload_signing_keys", fun t -> Keys.upload_signing_keys t ());
    (* Messages *)
    ( "Messages.send_text",
      fun t ->
        ignore_result
          (Messages.send_text t ~room_id:(rid "!r:example.org") ~body:"hi" ())
    );
    ( "Messages.send_emote",
      fun t ->
        ignore_result
          (Messages.send_emote t ~room_id:(rid "!r:example.org") ~body:"waves")
    );
    ( "Messages.send_notice",
      fun t ->
        ignore_result
          (Messages.send_notice t ~room_id:(rid "!r:example.org") ~body:"fyi")
    );
    ( "Messages.send_image",
      fun t ->
        ignore_result
          (Messages.send_image t ~room_id:(rid "!r:example.org") ~body:"pic"
             ~url:(mxc "mxc://example.org/i")
             ()) );
    ( "Messages.send_file",
      fun t ->
        ignore_result
          (Messages.send_file t ~room_id:(rid "!r:example.org") ~body:"doc"
             ~url:(mxc "mxc://example.org/f")
             ()) );
    ( "Messages.send_event",
      fun t ->
        ignore_result
          (Messages.send_event t ~room_id:(rid "!r:example.org")
             ~event_type:Matrix_proto.Event.Event_type.Room_message
             ~content:(Jsont.Json.object' [])) );
    ( "Messages.redact",
      fun t ->
        ignore_result
          (Messages.redact t ~room_id:(rid "!r:example.org")
             ~event_id:(eid "$e:example.org") ~reason:"spam" ()) );
    (* Presence *)
    ( "Presence.set_presence",
      fun t -> Presence.set_presence t ~presence:Presence.Online () );
    (* Profile *)
    ( "Profile.set_displayname",
      fun t -> Profile.set_displayname t ~displayname:"Alice" );
    ("Profile.clear_displayname", fun t -> Profile.clear_displayname t);
    ( "Profile.set_avatar_url",
      fun t -> Profile.set_avatar_url t ~avatar_url:(mxc "mxc://example.org/a")
    );
    ("Profile.clear_avatar_url", fun t -> Profile.clear_avatar_url t);
    ( "Profile.set_field",
      fun t ->
        Profile.set_field t ~key:"m.tz"
          ~value:(Jsont.Json.string "Europe/London") );
    (* Push *)
    ( "Push.set_push_rule",
      fun t ->
        Push.set_push_rule t
          (Push_rule.Rule_id.content "r")
          ~actions:[ Push_rule.Action.Notify ]
          () );
    ( "Push.set_enabled",
      fun t -> Push.set_enabled t (Push_rule.Rule_id.content "r") ~enabled:true
    );
    ( "Push.set_actions",
      fun t ->
        Push.set_actions t
          (Push_rule.Rule_id.content "r")
          ~actions:[ Push_rule.Action.Notify ] );
    ( "Push.set_pusher",
      fun t ->
        Push.set_pusher t ~pushkey:"k" ~kind:Push.Http ~app_id:"a"
          ~app_display_name:"A" ~device_display_name:"D" ~lang:"en"
          ~data:{ Push.url = Some "https://p.example/notify"; format = None }
          () );
    (* Receipts *)
    ( "Receipts.send_receipt",
      fun t ->
        Receipts.send_receipt t ~room_id:(rid "!r:example.org")
          ~event_id:(eid "$e:example.org") () );
    ( "Receipts.set_read_marker",
      fun t ->
        Receipts.set_read_marker t ~room_id:(rid "!r:example.org")
          ~fully_read:(eid "$e:example.org") () );
    (* Thread subscriptions *)
    ( "Thread_subscriptions.subscribe",
      fun t ->
        ignore_result
          (Thread_subscriptions.subscribe t ~room_id:(rid "!r:example.org")
             ~thread_root:(eid "$thread:example.org")
             ~automatic:(eid "$cause:example.org") ()) );
    (* Relations *)
    ( "Relations.send_reaction",
      fun t ->
        ignore_result
          (Relations.send_reaction t ~room_id:(rid "!r:example.org")
             ~event_id:(eid "$e:example.org") ~key:"+1") );
    ( "Relations.edit_message",
      fun t ->
        ignore_result
          (Relations.edit_message t ~room_id:(rid "!r:example.org")
             ~event_id:(eid "$e:example.org") ~new_body:"x" ()) );
    ( "Relations.send_reply",
      fun t ->
        ignore_result
          (Relations.send_reply t ~room_id:(rid "!r:example.org")
             ~event_id:(eid "$e:example.org") ~body:"x" ()) );
    ( "Relations.send_in_thread",
      fun t ->
        ignore_result
          (Relations.send_in_thread t ~room_id:(rid "!r:example.org")
             ~thread_root_id:(eid "$e:example.org") ~body:"x" ()) );
    (* Report *)
    ( "Report.event",
      fun t ->
        Report.event t ~room_id:(rid "!r:example.org")
          ~event_id:(eid "$e:example.org") ~reason:"spam" () );
    ("Report.room", fun t -> Report.room t ~room_id:(rid "!r:example.org") ());
    ( "Report.user",
      fun t -> Report.user t ~user_id:(uid "@bob:example.org") ~reason:"spam" ()
    );
    (* Room keys *)
    ( "Room_keys.create_version",
      fun t ->
        ignore_result
          (Room_keys.create_version t ~algorithm:"m.megolm_backup.v1"
             ~auth_data:(Jsont.Json.object' [])) );
    ( "Room_keys.update_version",
      fun t ->
        Room_keys.update_version t ~version:"1" ~algorithm:"m.megolm_backup.v1"
          ~auth_data:(Jsont.Json.object' []) );
    ( "Room_keys.put_keys",
      fun t ->
        ignore_result
          (Room_keys.put_keys t ~version:"1"
             [ ("!r:example.org", [ ("sess", backup_session_data) ]) ]) );
    ( "Room_keys.put_room_keys",
      fun t ->
        ignore_result
          (Room_keys.put_room_keys t ~version:"1"
             ~room_id:(rid "!r:example.org")
             [ ("sess", backup_session_data) ]) );
    ( "Room_keys.put_session_key",
      fun t ->
        ignore_result
          (Room_keys.put_session_key t ~version:"1"
             ~room_id:(rid "!r:example.org") ~session_id:(sid "sess")
             backup_session_data) );
    (* Rooms *)
    ("Rooms.create", fun t -> ignore_result (Rooms.create t ~name:"Room" ()));
    ( "Rooms.join",
      fun t ->
        ignore_result
          (Rooms.join t ~room_id_or_alias:(`Room_id (rid "!r:example.org")) ())
    );
    ( "Rooms.knock",
      fun t ->
        ignore_result
          (Rooms.knock t
             ~room_id_or_alias:(`Room_id (rid "!r:example.org"))
             ~reason:"hello" ()) );
    ( "Rooms.leave",
      fun t -> Rooms.leave t ~room_id:(rid "!r:example.org") ~reason:"bye" () );
    ( "Rooms.invite",
      fun t ->
        Rooms.invite t ~room_id:(rid "!r:example.org")
          ~user_id:(uid "@bob:example.org") () );
    ( "Rooms.kick",
      fun t ->
        Rooms.kick t ~room_id:(rid "!r:example.org")
          ~user_id:(uid "@bob:example.org") () );
    ( "Rooms.ban",
      fun t ->
        Rooms.ban t ~room_id:(rid "!r:example.org")
          ~user_id:(uid "@bob:example.org") () );
    ( "Rooms.unban",
      fun t ->
        Rooms.unban t ~room_id:(rid "!r:example.org")
          ~user_id:(uid "@bob:example.org") () );
    ( "Rooms.set_power_levels",
      fun t ->
        Rooms.set_power_levels t ~room_id:(rid "!r:example.org") ~power_levels
    );
    ( "Rooms.upgrade",
      fun t ->
        ignore_result
          (Rooms.upgrade t ~room_id:(rid "!r:example.org") ~new_version:"11" ())
    );
    (* Search *)
    ( "Search.room_events",
      fun t ->
        ignore_result (Search.room_events t ~criteria:(Search.v "hello") ()) );
    ( "Search.user_directory",
      fun t -> ignore_result (Search.user_directory t ~search_term:"bob" ()) );
    (* Spaces *)
    ( "Spaces.add_child",
      fun t ->
        ignore_result
          (Spaces.add_child t ~space:(rid "!s:example.org")
             ~child:(rid "!c:example.org") ()) );
    ( "Spaces.set_parent",
      fun t ->
        ignore_result
          (Spaces.set_parent t ~room:(rid "!c:example.org")
             ~parent:(rid "!s:example.org") ()) );
    (* State *)
    ( "State.set_state",
      fun t ->
        ignore_result
          (State.set_state t ~room_id:(rid "!r:example.org")
             ~event_type:Matrix_proto.Event.Event_type.Room_topic
             ~content:(Jsont.Json.object' []) ()) );
    ( "State.set_name",
      fun t ->
        ignore_result
          (State.set_name t ~room_id:(rid "!r:example.org") ~name:"n") );
    ( "State.set_topic",
      fun t ->
        ignore_result
          (State.set_topic t ~room_id:(rid "!r:example.org") ~topic:"tt") );
    ( "State.set_avatar",
      fun t ->
        ignore_result
          (State.set_avatar t ~room_id:(rid "!r:example.org")
             ~avatar_url:(mxc "mxc://example.org/a")) );
    (* Tags *)
    ( "Tags.set",
      fun t ->
        Tags.set t ~user_id:(uid "@alice:example.org")
          ~room_id:(rid "!r:example.org") ~tag:Tags.favourite ~order:0.5 () );
    (* To-device *)
    ( "To_device.send",
      fun t ->
        To_device.send t ~event_type:"m.room_key" ~txn_id:"txn"
          [
            ( uid "@bob:example.org",
              [ (To_device.Device (did "DEV"), Jsont.Json.object' []) ] );
          ] );
    (* Typing *)
    ( "Typing.set_typing",
      fun t ->
        Typing.set_typing t ~room_id:(rid "!r:example.org") ~typing:true () );
  ]

(* A missing [~enc] shows up as [Json_error "No encoder for member ..."] out of
   [Client.Http.encode_body]. Match on the message rather than the constructor so
   any encoder failure, however phrased, is caught. *)
let is_encoder_error = function
  | Error e ->
      let s = Error.to_string e in
      let needle = "No encoder" in
      let n = String.length needle and l = String.length s in
      let rec at i = i + n <= l && (String.sub s i n = needle || at (i + 1)) in
      at 0
  | Ok () -> false

let guard_case (name, call) =
  ( name,
    fun () ->
      Eio_mock.Backend.run @@ fun () ->
      let log, fetch = mock (json generic_reply) in
      let result = call (logged_in fetch) in
      if is_encoder_error result then
        Alcotest.failf "%s: request body could not be encoded: %s" name
          (match result with
          | Error e -> Error.to_string e
          | Ok () -> assert false);
      match requests log with
      | [] ->
          Alcotest.failf
            "%s: no request reached the network — the body never encoded" name
      | rs -> (
          let r = List.nth rs (List.length rs - 1) in
          match r.body with
          | Some _ -> ()
          | None ->
              Alcotest.failf "%s: last request (%s %s) carried no body" name
                r.meth r.url) )

(* {1 The pickle guard}

   {!Matrix_client.Session.Pickle} had the same bug the guard above watches
   for, in every member of every codec: pickling an Olm or Megolm session
   answered [Error "No encoder for member ..."]. These rows build a real
   value of each pickled type and pickle it; a member that loses its [~enc]
   makes its row fail. *)

module Olm = Matrix_client.Olm
module Pickle = Matrix_client.Session_pickle

(* [mock_env]'s 32 zero bytes are not enough for a key pair, let alone a
   Megolm ratchet. The pickles do not care what the bytes are. *)
let pickle_random =
  Matrix_client.Random.of_source
    (Eio.Flow.string_source (String.make 8192 '\019'))

let pickle_room_id = Matrix_proto.Id.Room_id.of_string_exn "!pickle:example.org"
let olm_err e = Format.asprintf "%a" Olm.pp_error e
let pickle_alice = Olm.Account.create ~random:pickle_random ()
let pickle_bob = Olm.Account.create ~random:pickle_random ()

let () =
  Olm.Account.generate_one_time_keys ~random:pickle_random pickle_alice 1;
  Olm.Account.generate_fallback_key ~random:pickle_random pickle_alice;
  Olm.Account.generate_one_time_keys ~random:pickle_random pickle_bob 1

let pickle_olm_session =
  let their_one_time_key =
    match Olm.Account.one_time_keys pickle_bob with
    | (_, k) :: _ -> k
    | [] -> Alcotest.fail "no one-time key to build a session with"
  in
  match
    Olm.Session.create_outbound ~random:pickle_random pickle_alice
      ~their_identity_key:(Olm.Account.curve25519_key pickle_bob)
      ~their_one_time_key
  with
  | Ok s -> s
  | Error e -> Alcotest.failf "create_outbound: %s" (olm_err e)

let pickle_megolm_outbound =
  Olm.Megolm.Outbound.create ~random:pickle_random ~room_id:pickle_room_id ()

let () =
  Olm.Megolm.Outbound.mark_shared_with pickle_megolm_outbound
    ~user_id:(uid "@bob:example.org") ~device_id:(did "BOBDEV")

let pickle_megolm_inbound =
  match
    Olm.Megolm.Inbound.of_session_key
      ~claimed_ed25519:(Olm.Account.ed25519_key pickle_alice)
      ~sender_key:(Olm.Account.curve25519_key pickle_alice)
      ~room_id:pickle_room_id
      ~session_key:(Olm.Megolm.Outbound.session_key pickle_megolm_outbound)
      ()
  with
  | Ok s -> s
  | Error e -> Alcotest.failf "of_session_key: %s" (olm_err e)

(* Rows are [name, pickle, unpickle-and-repickle]. Both directions run, so a
   codec that encodes to something it cannot read back fails too. *)
let pickled_values : (string * (unit -> (string, string) result)) list =
  let round pickle unpickle value () =
    let flatten = Result.map_error (fun (`Msg m) -> m) in
    match pickle value with
    | Error _ as e -> flatten e
    | Ok s -> flatten (Result.bind (unpickle s) pickle)
  in
  [
    ( "Pickle.pickle_account",
      round Pickle.pickle_account Pickle.unpickle_account pickle_alice );
    ( "Pickle.pickle_session",
      round Pickle.pickle_session Pickle.unpickle_session pickle_olm_session );
    ( "Pickle.pickle_megolm_inbound",
      round Pickle.pickle_megolm_inbound Pickle.unpickle_megolm_inbound
        pickle_megolm_inbound );
    ( "Pickle.pickle_megolm_outbound",
      round Pickle.pickle_megolm_outbound Pickle.unpickle_megolm_outbound
        pickle_megolm_outbound );
  ]

let pickle_case (name, call) =
  ( name,
    fun () ->
      match call () with
      | Error e -> Alcotest.failf "%s: could not be encoded: %s" name e
      | Ok "" -> Alcotest.failf "%s: encoded to nothing" name
      | Ok _ -> () )

(* The one-time key signing input used to be built with [Printf.sprintf]; it
   goes through the JSON encoder now, and must not have moved a byte. *)
let test_keys_one_time_key_signing_json () =
  let key = "9BiZEBoQKrgz1TDmpQKuTaxSGkxLDYPbXCsMLbNbLXk" in
  Alcotest.(check string)
    "canonical key object"
    (Printf.sprintf "{\"key\":\"%s\"}" key)
    (Keys.one_time_key_signing_json key);
  Alcotest.(check string)
    "canonical fallback-key object"
    (Printf.sprintf "{\"fallback\":true,\"key\":\"%s\"}" key)
    (Keys.one_time_key_signing_json ~fallback:true key)

let () =
  Alcotest.run "matrix encoders"
    [
      ( "account",
        [
          Alcotest.test_case "request_email_token" `Quick
            (run test_account_request_email_token);
          Alcotest.test_case "request_msisdn_token" `Quick
            (run test_account_request_msisdn_token);
          Alcotest.test_case "add_3pid" `Quick (run test_account_add_3pid);
          Alcotest.test_case "delete_3pid" `Quick (run test_account_delete_3pid);
          Alcotest.test_case "change_password" `Quick
            (run test_account_change_password);
          Alcotest.test_case "deactivate" `Quick (run test_account_deactivate);
          Alcotest.test_case "ignore_user" `Quick (run test_account_ignore_user);
          Alcotest.test_case "unignore_user" `Quick
            (run test_account_unignore_user);
        ] );
      ( "devices",
        [
          Alcotest.test_case "update_device" `Quick
            (run test_devices_update_device);
          Alcotest.test_case "delete_devices" `Quick
            (run test_devices_delete_devices);
        ] );
      ( "presence",
        [
          Alcotest.test_case "set_presence" `Quick
            (run test_presence_set_presence);
        ] );
      ( "push",
        [ Alcotest.test_case "set_pusher" `Quick (run test_push_set_pusher) ] );
      ( "receipts",
        [
          Alcotest.test_case "send receipt clears marked unread" `Quick
            (run test_receipts_send_clears_marked_unread);
          Alcotest.test_case "threaded receipt and fully-read validation" `Quick
            (run test_receipts_thread_and_fully_read_validation);
          Alcotest.test_case "failed receipt does not clear" `Quick
            (run test_receipts_do_not_clear_after_failure);
          Alcotest.test_case "clear failure is propagated" `Quick
            (run test_receipts_propagate_clear_failure);
          Alcotest.test_case "set_read_marker" `Quick
            (run test_receipts_set_read_marker);
        ] );
      ( "relations",
        [
          Alcotest.test_case "send_reaction" `Quick
            (run test_relations_send_reaction);
          Alcotest.test_case "extra content keeps typed fields" `Quick
            (run test_extra_content_does_not_override_typed_fields);
          Alcotest.test_case "edit_message" `Quick
            (run test_relations_edit_message);
          Alcotest.test_case "send_reply" `Quick (run test_relations_send_reply);
          Alcotest.test_case "send_in_thread" `Quick
            (run test_relations_send_in_thread);
          Alcotest.test_case "thread reply fallback" `Quick
            (run test_relations_thread_falls_back);
        ] );
      ( "state",
        [
          Alcotest.test_case "set_name" `Quick (run test_state_set_name);
          Alcotest.test_case "set_topic" `Quick (run test_state_set_topic);
          Alcotest.test_case "set_avatar" `Quick (run test_state_set_avatar);
        ] );
      ( "typing",
        [
          Alcotest.test_case "decode valid" `Quick test_typing_decode_valid;
          Alcotest.test_case "decode empty" `Quick test_typing_decode_empty;
          Alcotest.test_case "decode missing" `Quick test_typing_decode_missing;
          Alcotest.test_case "decode bad id" `Quick test_typing_decode_bad_id;
          Alcotest.test_case "decode bad shape" `Quick
            test_typing_decode_bad_shape;
          Alcotest.test_case "decode unknown field" `Quick
            test_typing_decode_unknown_field;
          Alcotest.test_case "set_typing" `Quick (run test_typing_set_typing);
          Alcotest.test_case "stop typing" `Quick (run test_typing_stop);
        ] );
      ( "encoder guard",
        List.map
          (fun row ->
            let name, f = guard_case row in
            Alcotest.test_case name `Quick f)
          guarded_calls );
      ( "keys",
        [
          Alcotest.test_case "one_time_key_signing_json" `Quick
            test_keys_one_time_key_signing_json;
        ] );
      ( "pickle guard",
        List.map
          (fun row ->
            let name, f = pickle_case row in
            Alcotest.test_case name `Quick f)
          pickled_values );
    ]
