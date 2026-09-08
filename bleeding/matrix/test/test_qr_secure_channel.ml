module Qr = Matrix_client.Qr_login.Msc4108
module S = Qr.Secure_channel
module M = Qr.Messages
module R = Matrix_client.Random

let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)

let auth_fixture fixture =
  let message =
    match M.of_string fixture with
    | Ok message -> message
    | Error error -> Alcotest.failf "fixture did not decode: %s" error
  in
  let encoded =
    match M.to_string message with
    | Ok encoded -> encoded
    | Error error -> Alcotest.failf "fixture did not encode: %s" error
  in
  check_string "exact auth message fixture" fixture encoded;
  message

let test_auth_message_fixtures () =
  ignore
    (auth_fixture
       {|{"type":"m.login.protocols","protocols":["device_authorization_grant","future_protocol"],"homeserver":"https://matrix.example/"}|});
  ignore
    (auth_fixture
       {|{"type":"m.login.protocol","device_authorization_grant":{"verification_uri":"https://id.matrix.org/device","verification_uri_complete":"https://id.matrix.org/device/abcde"},"protocol":"device_authorization_grant","device_id":"wjLpTLRqbqBzLs63aYaEv2Boi6cFEbbM/sSRQ2oAKk4"}|});
  ignore (auth_fixture {|{"type":"m.login.protocol_accepted"}|});
  ignore (auth_fixture {|{"type":"m.login.success"}|});
  ignore (auth_fixture {|{"type":"m.login.declined"}|});
  ignore
    (auth_fixture
       {|{"type":"m.login.failure","reason":"unsupported_protocol","homeserver":null}|});
  ignore
    (auth_fixture
       {|{"type":"m.login.secrets","cross_signing":{"master_key":"master","user_signing_key":"user","self_signing_key":"self"},"backup":{"algorithm":"m.megolm_backup.v1.curve25519-aes-sha2","backup_version":"2","key":"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"}}|})

let test_auth_message_optional_fields () =
  let grant =
    M.Login_protocol
      {
        device_authorization_grant =
          {
            verification_uri =
              Uriz.of_string_exn "https://id.matrix.org/device";
            verification_uri_complete = None;
          };
        protocol = M.Device_authorization_grant;
        device_id = "device";
      }
  in
  ignore (auth_fixture (Result.get_ok (M.to_string grant)));
  ignore
    (auth_fixture
       {|{"type":"m.login.failure","reason":"user_cancelled","homeserver":"https://matrix.example"}|});
  let padded =
    {|{"type":"m.login.secrets","cross_signing":{"master_key":"master","user_signing_key":"user","self_signing_key":"self"},"backup":{"algorithm":"m.megolm_backup.v1.curve25519-aes-sha2","backup_version":"2","key":"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="}}|}
  in
  let canonical =
    {|{"type":"m.login.secrets","cross_signing":{"master_key":"master","user_signing_key":"user","self_signing_key":"self"},"backup":{"algorithm":"m.megolm_backup.v1.curve25519-aes-sha2","backup_version":"2","key":"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"}}|}
  in
  let message =
    match M.of_string padded with
    | Ok message -> message
    | Error error ->
        Alcotest.failf "padded backup fixture did not decode: %s" error
  in
  check_string "canonical backup key" canonical
    (Result.get_ok (M.to_string message))

let test_auth_message_failure_reasons () =
  let reasons =
    [
      ("authorization_expired", M.Authorization_expired);
      ("device_already_exists", M.Device_already_exists);
      ("device_not_found", M.Device_not_found);
      ("unexpected_message_received", M.Unexpected_message_received);
      ("unsupported_protocol", M.Unsupported_protocol);
      ("user_cancelled", M.User_cancelled);
    ]
  in
  List.iter
    (fun (wire, reason) ->
      let message = M.Login_failure { reason; homeserver = None } in
      let encoded = Result.get_ok (M.to_string message) in
      let expected =
        Printf.sprintf
          {|{"type":"m.login.failure","reason":"%s","homeserver":null}|} wire
      in
      check_string "failure reason fixture" expected encoded)
    reasons

let test_auth_message_custom_values_roundtrip () =
  let messages =
    [
      M.Login_protocols
        {
          protocols = [ M.Custom_protocol "future_protocol" ];
          homeserver = Uriz.of_string_exn "https://matrix.example";
        };
      M.Login_failure
        { reason = M.Custom_failure "future_reason"; homeserver = None };
    ]
  in
  List.iter
    (fun message ->
      let encoded = Result.get_ok (M.to_string message) in
      let decoded = Result.get_ok (M.of_string encoded) in
      match (message, decoded) with
      | ( M.Login_protocols { protocols = [ M.Custom_protocol expected ]; _ },
          M.Login_protocols { protocols = [ M.Custom_protocol got ]; _ } )
      | ( M.Login_failure { reason = M.Custom_failure expected; _ },
          M.Login_failure { reason = M.Custom_failure got; _ } ) ->
          check_string "custom value" expected got
      | _ -> Alcotest.fail "custom value was not retained")
    messages

let test_auth_message_malformed () =
  let malformed =
    [
      {|{}|};
      {|{"type":42}|};
      {|{"type":"m.login.unknown"}|};
      {|{"type":"m.login.protocols","protocols":[],"homeserver":"not-a-url"}|};
      {|{"type":"m.login.protocol","protocol":"device_authorization_grant","device_id":"x"}|};
      {|{"type":"m.login.protocol","device_authorization_grant":{"verification_uri":"https://id.matrix.org/device#fragment"},"protocol":"x","device_id":"x"}|};
      {|{"type":"m.login.secrets","cross_signing":{},"backup":null}|};
      {|{"type":"m.login.secrets","cross_signing":{"master_key":"x","user_signing_key":"x","self_signing_key":"x"},"backup":{"algorithm":"future","backup_version":"1","key":"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"}}|};
      {|{"type":"m.login.secrets","cross_signing":{"master_key":"x","user_signing_key":"x","self_signing_key":"x"},"backup":{"algorithm":"m.megolm_backup.v1.curve25519-aes-sha2","backup_version":"1","key":"bad"}}|};
    ]
  in
  List.iter
    (fun fixture ->
      match M.of_string fixture with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "accepted malformed fixture: %s" fixture)
    malformed;
  let invalid_outgoing =
    M.Login_protocols
      {
        protocols = [ M.Device_authorization_grant ];
        homeserver = Uriz.of_string_exn "relative-homeserver";
      }
  in
  match M.to_string invalid_outgoing with
  | Error _ -> ()
  | Ok encoded -> Alcotest.failf "encoded an invalid outgoing URL: %s" encoded

type server = {
  mutable body : string;
  mutable etag : int;
  mutable closed : bool;
  mutable deletes : int;
  mutable wrong_ok : bool;
  mutable fail_put : bool;
}

let header name headers = List.assoc_opt name (Fetch.Header.to_list headers)

let etag value : Fetch.Header.etag option =
  Some { weak = false; tag = string_of_int value }

let wire_etag value = Printf.sprintf "\"%d\"" value

let transport server =
  let response ?(content_type = None) ~body () =
    {
      Qr.Rendezvous.status = 200;
      etag = etag server.etag;
      content_type = Option.map Fetch.Header.media content_type;
      body;
      expires_at = None;
    }
  in
  {
    Qr.Rendezvous.request =
      (fun ~method_ ~uri:_ ~headers ~body ->
        if server.closed then
          Ok
            {
              Qr.Rendezvous.status = 404;
              etag = etag server.etag;
              content_type = None;
              body = "closed";
              expires_at = None;
            }
        else
          match method_ with
          | `POST ->
              if
                header "Content-Type" headers <> Some "text/plain"
                || body <> Some ""
              then
                Alcotest.fail
                  "rendezvous create must send text/plain with an empty body";
              Ok
                {
                  Qr.Rendezvous.status = 200;
                  etag = etag 0;
                  content_type = Some (Fetch.Header.media "application/json");
                  body = {|{"url":"http://example.test/rendezvous"}|};
                  expires_at = None;
                }
          | `PUT -> (
              if server.fail_put then raise (Failure "simulated send failure");
              match (header "If-Match" headers, body) with
              | Some expected, Some body when expected = wire_etag server.etag
                ->
                  server.etag <- server.etag + 1;
                  server.body <- body;
                  Ok (response ~content_type:(Some "text/plain") ~body:"" ())
              | _ ->
                  Ok
                    {
                      Qr.Rendezvous.status = 412;
                      etag = etag server.etag;
                      content_type = None;
                      body = "etag mismatch";
                      expires_at = None;
                    })
          | `GET ->
              let current = wire_etag server.etag in
              if header "If-None-Match" headers = Some current then
                Ok
                  {
                    Qr.Rendezvous.status = 304;
                    etag = etag server.etag;
                    content_type = None;
                    body = "";
                    expires_at = None;
                  }
              else if server.body <> "" && server.wrong_ok && server.etag >= 2
              then begin
                server.wrong_ok <- false;
                Ok
                  (response ~content_type:(Some "text/plain")
                     ~body:(Matrix_proto.Base64.encode "forged")
                     ())
              end
              else
                Ok
                  (response ~content_type:(Some "text/plain") ~body:server.body
                     ())
          | `DELETE ->
              server.deletes <- server.deletes + 1;
              server.closed <- true;
              Ok (response ~body:"" ()));
    now = (fun () -> Ptime.epoch);
    sleep = (fun _ -> Eio.Fiber.yield ());
  }

let random seed = R.of_source (Eio.Flow.string_source (String.make 256 seed))

let unwrap = function
  | Ok value -> value
  | Error error ->
      Alcotest.failf "unexpected secure-channel error: %a" S.pp_error error

let unwrap_rendezvous = function
  | Ok value -> value
  | Error _ -> Alcotest.fail "unexpected rendezvous error"

let unwrap_ecies = function
  | Ok value -> value
  | Error _ -> Alcotest.fail "unexpected ECIES error"

let test_complete_handshake () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let server =
    {
      body = "";
      etag = 0;
      closed = false;
      deletes = 0;
      wrong_ok = false;
      fail_put = false;
    }
  in
  let transport = transport server in
  let displayed =
    unwrap
      (S.login ~transport
         ~rendezvous_server:(Uriz.of_string_exn "http://example.test")
         ~random:(random 'a') ())
  in
  let qr = S.qr_code displayed in
  let promise, promise_u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve promise_u
        (S.from_qr_code ~transport ~random:(random 'b')
           ~expected_intent:(Qr.Reciprocate (Uriz.of_string_exn "matrix.org"))
           qr));
  let almost = unwrap (S.connect displayed) in
  let scanner = unwrap (Eio.Promise.await promise) in
  check_int "check code" (S.check_code almost)
    (S.check_code_established scanner);
  let displayed =
    unwrap (S.confirm almost ~check_code:(S.check_code_established scanner))
  in
  (match S.send scanner "\xff" with
  | Error (S.Invalid_utf8 "message") -> ()
  | Error error ->
      Alcotest.failf "wrong invalid UTF-8 error: %a" S.pp_error error
  | Ok () -> Alcotest.fail "accepted invalid UTF-8 plaintext");
  unwrap (S.send scanner "hello");
  check_string "scanner to displayed" "hello" (unwrap (S.receive displayed));
  unwrap (S.send displayed "world");
  check_string "displayed to scanner" "world" (unwrap (S.receive scanner));
  unwrap (S.close displayed);
  unwrap (S.close displayed)

let test_auth_message_channel () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let server =
    {
      body = "";
      etag = 0;
      closed = false;
      deletes = 0;
      wrong_ok = false;
      fail_put = false;
    }
  in
  let transport = transport server in
  let displayed =
    unwrap
      (S.login ~transport
         ~rendezvous_server:(Uriz.of_string_exn "http://example.test")
         ~random:(random 'a') ())
  in
  let qr = S.qr_code displayed in
  let promise, promise_u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve promise_u
        (S.from_qr_code ~transport ~random:(random 'b')
           ~expected_intent:(Qr.Reciprocate (Uriz.of_string_exn "matrix.org"))
           qr));
  let almost = unwrap (S.connect displayed) in
  let scanner = unwrap (Eio.Promise.await promise) in
  let displayed =
    unwrap (S.confirm almost ~check_code:(S.check_code_established scanner))
  in
  let message = M.Login_protocol_accepted in
  unwrap (S.send_json scanner message);
  (match S.receive_json displayed with
  | Ok M.Login_protocol_accepted -> ()
  | Ok _ -> Alcotest.fail "received the wrong typed auth message"
  | Error error ->
      Alcotest.failf "failed to receive typed auth message: %a" S.pp_error error);
  unwrap
    (S.send_json displayed
       (M.Login_failure
          { reason = M.Custom_failure "future_reason"; homeserver = None }));
  (match S.receive_json scanner with
  | Ok
      (M.Login_failure
         { reason = M.Custom_failure "future_reason"; homeserver = None }) ->
      ()
  | Ok _ -> Alcotest.fail "received the wrong reverse typed auth message"
  | Error error ->
      Alcotest.failf "failed to receive reverse typed auth message: %a"
        S.pp_error error);
  unwrap (S.close displayed)

let test_rejects_intent_and_initial_body () =
  Eio_main.run @@ fun _ ->
  let server =
    {
      body = "";
      etag = 0;
      closed = false;
      deletes = 0;
      wrong_ok = false;
      fail_put = false;
    }
  in
  let transport = transport server in
  let displayed =
    unwrap
      (S.login ~transport
         ~rendezvous_server:(Uriz.of_string_exn "http://example.test")
         ~random:(random 'a') ())
  in
  let qr = S.qr_code displayed in
  (match
     S.from_qr_code ~transport ~random:(random 'b') ~expected_intent:Qr.Login qr
   with
  | Error S.Invalid_intent -> ()
  | Error error -> Alcotest.failf "wrong intent error: %a" S.pp_error error
  | Ok _ -> Alcotest.fail "accepted a QR code with the local intent");
  unwrap (S.cancel_displayed displayed);
  check_int "intent rejection leaves displayed channel cancellable" 1
    server.deletes

let test_nonempty_initial_body_is_ignored () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  (* A scanner drops the rendezvous' initial body, whether or not a custom
     server supplied one, before sending its ECIES INITIATE message. *)
  let server =
    {
      body = "unexpected";
      etag = 0;
      closed = false;
      deletes = 0;
      wrong_ok = false;
      fail_put = false;
    }
  in
  let transport = transport server in
  let displayed =
    unwrap
      (S.login ~transport
         ~rendezvous_server:(Uriz.of_string_exn "http://example.test")
         ~random:(random 'a') ())
  in
  let qr = S.qr_code displayed in
  let promise, promise_u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve promise_u
        (S.from_qr_code ~transport ~random:(random 'b')
           ~expected_intent:(Qr.Reciprocate (Uriz.of_string_exn "matrix.org"))
           qr));
  let almost = unwrap (S.connect displayed) in
  let scanner = unwrap (Eio.Promise.await promise) in
  let confirmed =
    unwrap (S.confirm almost ~check_code:(S.check_code_established scanner))
  in
  unwrap (S.close confirmed)

let test_wrong_initiate_is_strictly_rejected () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun _sw ->
  let server =
    {
      body = "";
      etag = 0;
      closed = false;
      deletes = 0;
      wrong_ok = false;
      fail_put = false;
    }
  in
  let transport = transport server in
  let displayed =
    unwrap
      (S.login ~transport
         ~rendezvous_server:(Uriz.of_string_exn "http://example.test")
         ~random:(random 'a') ())
  in
  let qr = S.qr_code displayed in
  let attacker = Qr.Ecies.create ~random:(random 'x') () in
  let rendezvous, initial =
    unwrap_rendezvous
      (Qr.Rendezvous.accept transport ~rendezvous_url:qr.rendezvous_url ())
  in
  check_string "rendezvous starts empty" "" initial;
  let _, encrypted =
    unwrap_ecies
      (Qr.Ecies.establish_outbound attacker ~recipient:qr.public_key
         ~initial_plaintext:"WRONG")
  in
  (match Qr.Rendezvous.send rendezvous encrypted with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "unexpected rendezvous send error");
  (match S.connect displayed with
  | Error
      (S.Secure_channel_message
         { expected = "MATRIX_QR_CODE_LOGIN_INITIATE"; received = "WRONG" }) ->
      ()
  | Error error -> Alcotest.failf "wrong message error: %a" S.pp_error error
  | Ok _ -> Alcotest.fail "accepted a wrong INITIATE message");
  (match S.connect displayed with
  | Error S.Consumed -> ()
  | Error error ->
      Alcotest.failf "wrong replay state error: %a" S.pp_error error
  | Ok _ -> Alcotest.fail "replayed a displayed channel");
  Alcotest.(check int) "failed connect closes rendezvous" 1 server.deletes;
  unwrap (S.cancel_displayed displayed)

let test_authenticated_invalid_utf8_is_rejected () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun _sw ->
  let server =
    {
      body = "";
      etag = 0;
      closed = false;
      deletes = 0;
      wrong_ok = false;
      fail_put = false;
    }
  in
  let transport = transport server in
  let displayed =
    unwrap
      (S.login ~transport
         ~rendezvous_server:(Uriz.of_string_exn "http://example.test")
         ~random:(random 'a') ())
  in
  let qr = S.qr_code displayed in
  let attacker = Qr.Ecies.create ~random:(random 'x') () in
  let rendezvous, _ =
    unwrap_rendezvous
      (Qr.Rendezvous.accept transport ~rendezvous_url:qr.rendezvous_url ())
  in
  let _, encrypted =
    unwrap_ecies
      (Qr.Ecies.establish_outbound attacker ~recipient:qr.public_key
         ~initial_plaintext:"\xff")
  in
  (match Qr.Rendezvous.send rendezvous encrypted with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "unexpected rendezvous send error");
  match S.connect displayed with
  | Error (S.Invalid_utf8 "secure-channel message") -> ()
  | Error error ->
      Alcotest.failf "wrong invalid UTF-8 error: %a" S.pp_error error
  | Ok _ -> Alcotest.fail "accepted authenticated invalid UTF-8"

let test_wrong_check_is_rejected () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let server =
    {
      body = "";
      etag = 0;
      closed = false;
      deletes = 0;
      wrong_ok = false;
      fail_put = false;
    }
  in
  let transport = transport server in
  let displayed =
    unwrap
      (S.login ~transport
         ~rendezvous_server:(Uriz.of_string_exn "http://example.test")
         ~random:(random 'a') ())
  in
  let qr = S.qr_code displayed in
  let promise, promise_u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve promise_u
        (S.from_qr_code ~transport ~random:(random 'b')
           ~expected_intent:(Qr.Reciprocate (Uriz.of_string_exn "matrix.org"))
           qr));
  let almost = unwrap (S.connect displayed) in
  let scanner = unwrap (Eio.Promise.await promise) in
  (match S.confirm almost ~check_code:((S.check_code almost + 1) mod 100) with
  | Error S.Invalid_check_code -> ()
  | Error error -> Alcotest.failf "wrong check-code error: %a" S.pp_error error
  | Ok _ -> Alcotest.fail "accepted a wrong check code");
  (match S.confirm almost ~check_code:0 with
  | Error S.Consumed -> ()
  | Error error ->
      Alcotest.failf "wrong check-code replay error: %a" S.pp_error error
  | Ok _ -> Alcotest.fail "reused an almost-established channel");
  unwrap (S.close scanner)

let test_wrong_ok_is_rejected () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let server =
    {
      body = "";
      etag = 0;
      closed = false;
      deletes = 0;
      wrong_ok = true;
      fail_put = false;
    }
  in
  let transport = transport server in
  let displayed =
    unwrap
      (S.login ~transport
         ~rendezvous_server:(Uriz.of_string_exn "http://example.test")
         ~random:(random 'a') ())
  in
  let qr = S.qr_code displayed in
  let promise, promise_u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve promise_u
        (S.from_qr_code ~transport ~random:(random 'b')
           ~expected_intent:(Qr.Reciprocate (Uriz.of_string_exn "matrix.org"))
           qr));
  let almost = unwrap (S.connect displayed) in
  (match Eio.Promise.await promise with
  | Error (S.Ecies_error Qr.Ecies.Authentication_failed) -> ()
  | Error error -> Alcotest.failf "wrong OK error: %a" S.pp_error error
  | Ok _ -> Alcotest.fail "accepted a forged LOGIN_OK message");
  unwrap (S.cancel_almost almost)

let test_concurrent_receive_is_busy () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let server =
    {
      body = "";
      etag = 0;
      closed = false;
      deletes = 0;
      wrong_ok = false;
      fail_put = false;
    }
  in
  let transport = transport server in
  let displayed =
    unwrap
      (S.login ~transport
         ~rendezvous_server:(Uriz.of_string_exn "http://example.test")
         ~random:(random 'a') ())
  in
  let qr = S.qr_code displayed in
  let promise, promise_u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve promise_u
        (S.from_qr_code ~transport ~random:(random 'b')
           ~expected_intent:(Qr.Reciprocate (Uriz.of_string_exn "matrix.org"))
           qr));
  let almost = unwrap (S.connect displayed) in
  let scanner = unwrap (Eio.Promise.await promise) in
  let displayed =
    unwrap (S.confirm almost ~check_code:(S.check_code_established scanner))
  in
  let receiving, receiving_u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve receiving_u (S.receive scanner));
  Eio.Fiber.yield ();
  (match S.receive scanner with
  | Error S.Busy -> ()
  | Error error ->
      Alcotest.failf "wrong concurrent-state error: %a" S.pp_error error
  | Ok _ -> Alcotest.fail "allowed concurrent receives");
  unwrap (S.send displayed "payload");
  (match Eio.Promise.await receiving with
  | Ok "payload" -> ()
  | Ok value -> Alcotest.failf "wrong received payload %S" value
  | Error error ->
      Alcotest.failf "receive failed after busy guard: %a" S.pp_error error);
  unwrap (S.close displayed)

let test_failed_send_poison_is_not_retried () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let server =
    {
      body = "";
      etag = 0;
      closed = false;
      deletes = 0;
      wrong_ok = false;
      fail_put = false;
    }
  in
  let transport = transport server in
  let displayed =
    unwrap
      (S.login ~transport
         ~rendezvous_server:(Uriz.of_string_exn "http://example.test")
         ~random:(random 'a') ())
  in
  let qr = S.qr_code displayed in
  let promise, promise_u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve promise_u
        (S.from_qr_code ~transport ~random:(random 'b')
           ~expected_intent:(Qr.Reciprocate (Uriz.of_string_exn "matrix.org"))
           qr));
  let almost = unwrap (S.connect displayed) in
  let scanner = unwrap (Eio.Promise.await promise) in
  let displayed =
    unwrap (S.confirm almost ~check_code:(S.check_code_established scanner))
  in
  server.fail_put <- true;
  (match S.send scanner "ambiguous" with
  | exception Failure _ -> ()
  | Error error ->
      Alcotest.failf "wrong failed-send result: %a" S.pp_error error
  | Ok () -> Alcotest.fail "accepted a failed send");
  (match S.send scanner "retry" with
  | Error S.Closed -> ()
  | Error error ->
      Alcotest.failf "wrong poisoned-state error: %a" S.pp_error error
  | Ok () -> Alcotest.fail "retried a poisoned send");
  ignore (S.close displayed)

let test_failed_receive_poison_is_not_retried () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let server =
    {
      body = "";
      etag = 0;
      closed = false;
      deletes = 0;
      wrong_ok = false;
      fail_put = false;
    }
  in
  let transport = transport server in
  let displayed =
    unwrap
      (S.login ~transport
         ~rendezvous_server:(Uriz.of_string_exn "http://example.test")
         ~random:(random 'a') ())
  in
  let qr = S.qr_code displayed in
  let promise, promise_u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve promise_u
        (S.from_qr_code ~transport ~random:(random 'b')
           ~expected_intent:(Qr.Reciprocate (Uriz.of_string_exn "matrix.org"))
           qr));
  let almost = unwrap (S.connect displayed) in
  let scanner = unwrap (Eio.Promise.await promise) in
  let displayed =
    unwrap (S.confirm almost ~check_code:(S.check_code_established scanner))
  in
  server.etag <- server.etag + 1;
  server.body <- Matrix_proto.Base64.encode "forged";
  (match S.receive scanner with
  | Error (S.Ecies_error Qr.Ecies.Authentication_failed) -> ()
  | Error error ->
      Alcotest.failf "wrong failed-receive result: %a" S.pp_error error
  | Ok _ -> Alcotest.fail "accepted forged established ciphertext");
  (match S.receive scanner with
  | Error S.Closed -> ()
  | Error error ->
      Alcotest.failf "wrong poisoned-state error: %a" S.pp_error error
  | Ok _ -> Alcotest.fail "retried a poisoned receive");
  ignore (S.close displayed)

let () =
  Alcotest.run "qr secure channel"
    [
      ( "handshake",
        [
          Alcotest.test_case "complete two-party handshake" `Quick
            test_complete_handshake;
          Alcotest.test_case "typed auth message channel" `Quick
            test_auth_message_channel;
          Alcotest.test_case "intent and initial body" `Quick
            test_rejects_intent_and_initial_body;
          Alcotest.test_case "non-empty initial body is ignored" `Quick
            test_nonempty_initial_body_is_ignored;
          Alcotest.test_case "wrong INITIATE" `Quick
            test_wrong_initiate_is_strictly_rejected;
          Alcotest.test_case "authenticated invalid UTF-8" `Quick
            test_authenticated_invalid_utf8_is_rejected;
          Alcotest.test_case "wrong check code" `Quick
            test_wrong_check_is_rejected;
          Alcotest.test_case "wrong OK" `Quick test_wrong_ok_is_rejected;
          Alcotest.test_case "concurrent receive is busy" `Quick
            test_concurrent_receive_is_busy;
          Alcotest.test_case "failed send poisons channel" `Quick
            test_failed_send_poison_is_not_retried;
          Alcotest.test_case "failed receive poisons channel" `Quick
            test_failed_receive_poison_is_not_retried;
          Alcotest.test_case "auth message exact fixture" `Quick
            test_auth_message_fixtures;
          Alcotest.test_case "auth message optional fields" `Quick
            test_auth_message_optional_fields;
          Alcotest.test_case "auth message failure reasons" `Quick
            test_auth_message_failure_reasons;
          Alcotest.test_case "auth message custom values" `Quick
            test_auth_message_custom_values_roundtrip;
          Alcotest.test_case "auth message malformed" `Quick
            test_auth_message_malformed;
        ] );
    ]
