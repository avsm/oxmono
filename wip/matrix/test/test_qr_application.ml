module Qr = Matrix_client.Qr_login.Msc4108
module A = Qr.Application
module M = Qr.Messages

let check_string = Alcotest.(check string)
let check_bool = Alcotest.(check bool)
let uri value = Uriz.of_string_exn value

let grant ?complete () =
  {
    M.verification_uri = uri "https://matrix.example/device";
    verification_uri_complete = Option.map uri complete;
  }

let secrets =
  {
    M.cross_signing =
      {
        master_key = "master";
        user_signing_key = "user";
        self_signing_key = "self";
      };
    backup = None;
  }

type scripted = {
  channel : string A.channel;
  sent : M.t list ref;
  received : M.t list ref;
}

let scripted ?(incoming = []) ?send_error ?receive_error () =
  let sent = ref [] in
  let received = ref incoming in
  let send message =
    match send_error with
    | Some error -> Error error
    | None ->
        sent := !sent @ [ message ];
        Ok ()
  in
  let receive () =
    match receive_error with
    | Some error -> Error error
    | None -> (
        match !received with
        | message :: rest ->
            received := rest;
            Ok message
        | [] -> Error "script exhausted")
  in
  { channel = { A.send; receive }; sent; received }

let check_messages name expected actual =
  let show message =
    match message with
    | M.Login_protocols _ -> "protocols"
    | M.Login_protocol _ -> "protocol"
    | M.Login_protocol_accepted -> "accepted"
    | M.Login_success -> "success"
    | M.Login_declined -> "declined"
    | M.Login_failure _ -> "failure"
    | M.Login_secrets _ -> "secrets"
  in
  Alcotest.(check (list string))
    name (List.map show expected) (List.map show actual)

let check_failure_reason expected = function
  | M.Login_failure { reason; _ } ->
      Alcotest.(check bool) "failure reason" true (reason = expected)
  | _ -> Alcotest.fail "expected a login failure"

let login_authorization ?(device_id = "device") ?(user_code = "ABCD")
    ?(await_token = fun () -> Ok "token") () =
  { A.grant = grant (); device_id; user_code; await_token }

let login_hooks ?(prepare = fun ~homeserver:_ -> Ok (login_authorization ()))
    ?(activate = fun ~device_id:_ _token -> Ok ())
    ?(import_secrets = fun _ -> Ok ()) ?(await_token = fun () -> Ok "token")
    ?(on_progress = fun _ -> ()) () =
  let prepare' ~homeserver =
    match prepare ~homeserver with
    | Ok authorization -> Ok { authorization with await_token }
    | Error error -> Error error
  in
  { A.prepare = prepare'; activate; import_secrets; on_progress }

let run_login ?start script hooks =
  A.run_login
    ~start:(Option.value start ~default:A.Await_protocols)
    ~channel:script.channel ~hooks

let test_login_happy_path () =
  let progress = ref [] in
  let activated = ref None in
  let imported = ref false in
  let script =
    scripted
      ~incoming:
        [
          M.Login_protocols
            {
              protocols = [ M.Device_authorization_grant ];
              homeserver = uri "https://matrix.example";
            };
          M.Login_protocol_accepted;
          M.Login_secrets secrets;
        ]
      ()
  in
  let hooks =
    login_hooks
      ~activate:(fun ~device_id token ->
        activated := Some (device_id, token);
        Ok ())
      ~import_secrets:(fun value ->
        imported := value = secrets;
        Ok ())
      ~on_progress:(fun value -> progress := !progress @ [ value ])
      ()
  in
  match run_login script hooks with
  | Error _ -> Alcotest.fail "happy-path login failed"
  | Ok () ->
      check_messages "login messages"
        [
          M.Login_protocol
            {
              device_authorization_grant = grant ();
              protocol = M.Device_authorization_grant;
              device_id = "device";
            };
          M.Login_success;
        ]
        !(script.sent);
      check_bool "imported secrets" true !imported;
      check_bool "activated token" true (!activated = Some ("device", "token"));
      check_bool "progress" true
        (!progress
        = [
            A.Starting;
            A.Waiting_for_token { user_code = "ABCD" };
            A.Syncing_secrets;
            A.Done;
          ])

let test_login_homeserver_known () =
  let homeserver = uri "https://known.example" in
  let seen = ref None in
  let script =
    scripted ~incoming:[ M.Login_protocol_accepted; M.Login_secrets secrets ] ()
  in
  let hooks =
    login_hooks
      ~prepare:(fun ~homeserver:value ->
        seen := value;
        Ok (login_authorization ()))
      ()
  in
  match run_login ~start:(A.Homeserver_known homeserver) script hooks with
  | Error _ -> Alcotest.fail "known-homeserver login failed"
  | Ok () ->
      check_bool "known homeserver" true (!seen = Some homeserver);
      check_messages "known-homeserver messages"
        [
          M.Login_protocol
            {
              device_authorization_grant = grant ();
              protocol = M.Device_authorization_grant;
              device_id = "device";
            };
          M.Login_success;
        ]
        !(script.sent)

let test_login_unsupported_protocol () =
  let script =
    scripted
      ~incoming:
        [
          M.Login_protocols
            {
              protocols = [ M.Custom_protocol "custom" ];
              homeserver = uri "https://matrix.example";
            };
        ]
      ()
  in
  match run_login script (login_hooks ()) with
  | Error A.No_supported_protocol -> (
      match !(script.sent) with
      | [ message ] -> check_failure_reason M.Unsupported_protocol message
      | _ -> Alcotest.fail "missing unsupported-protocol failure")
  | Error _ -> Alcotest.fail "wrong unsupported-protocol error"
  | Ok () -> Alcotest.fail "unsupported protocol accepted"

let test_login_token_failures () =
  let run await_token expected_error expected_message =
    let script = scripted ~incoming:[ M.Login_protocol_accepted ] () in
    let hooks = login_hooks ~await_token () in
    match
      run_login
        ~start:(A.Homeserver_known (uri "https://known.example"))
        script hooks
    with
    | Error error -> (
        check_bool "token failure" true (error = expected_error);
        match List.rev !(script.sent) with
        | message :: _ ->
            check_messages "token failure message" [ expected_message ]
              [ message ]
        | [] -> Alcotest.fail "missing token failure message")
    | Ok () -> Alcotest.fail "token failure accepted"
  in
  run (fun () -> Error A.Access_denied) A.Authorization_denied M.Login_declined;
  run
    (fun () -> Error A.Expired)
    A.Authorization_expired
    (M.Login_failure { reason = M.Authorization_expired; homeserver = None })

let test_login_unexpected_and_peer_failures () =
  let unexpected = scripted ~incoming:[ M.Login_success ] () in
  (match
     run_login
       ~start:(A.Homeserver_known (uri "https://known.example"))
       unexpected (login_hooks ())
   with
  | Error (A.Unexpected_message { expected; received = M.Login_success }) -> (
      check_string "unexpected expected type" "m.login.protocol_accepted"
        expected;
      match List.rev !(unexpected.sent) with
      | message :: _ ->
          check_failure_reason M.Unexpected_message_received message
      | [] -> Alcotest.fail "missing unexpected-message failure")
  | Error _ -> Alcotest.fail "wrong unexpected-message error"
  | Ok () -> Alcotest.fail "unexpected message accepted");
  let peer =
    scripted
      ~incoming:
        [ M.Login_failure { reason = M.User_cancelled; homeserver = None } ]
      ()
  in
  match
    run_login
      ~start:(A.Homeserver_known (uri "https://known.example"))
      peer (login_hooks ())
  with
  | Error (A.Peer_failure { reason = M.User_cancelled; homeserver = None }) ->
      ()
  | Error _ -> Alcotest.fail "wrong peer-failure error"
  | Ok () -> Alcotest.fail "peer failure accepted"

let test_login_unexpected_secrets () =
  let script =
    scripted ~incoming:[ M.Login_protocol_accepted; M.Login_success ] ()
  in
  match
    run_login
      ~start:(A.Homeserver_known (uri "https://known.example"))
      script (login_hooks ())
  with
  | Error (A.Unexpected_message { expected; received = M.Login_success }) -> (
      check_string "unexpected secrets expected type" "m.login.secrets" expected;
      match List.rev !(script.sent) with
      | message :: _ ->
          check_failure_reason M.Unexpected_message_received message
      | [] -> Alcotest.fail "missing unexpected-secrets failure")
  | Error _ -> Alcotest.fail "wrong unexpected-secrets error"
  | Ok () -> Alcotest.fail "unexpected secrets accepted"

let test_login_local_and_channel_errors () =
  let local =
    scripted
      ~incoming:
        [
          M.Login_protocols
            {
              protocols = [ M.Device_authorization_grant ];
              homeserver = uri "https://matrix.example";
            };
        ]
      ()
  in
  (match
     run_login local
       (login_hooks ~prepare:(fun ~homeserver:_ -> Error "prepare failed") ())
   with
  | Error (A.Local_error "prepare failed") -> ()
  | Error _ -> Alcotest.fail "wrong callback error"
  | Ok () -> Alcotest.fail "callback failure accepted");
  let channel = scripted ~receive_error:"receive failed" () in
  (match run_login channel (login_hooks ()) with
  | Error (A.Channel_error "receive failed") -> ()
  | Error _ -> Alcotest.fail "wrong channel error"
  | Ok () -> Alcotest.fail "channel failure accepted");
  let channel =
    scripted
      ~incoming:[ M.Login_protocol_accepted ]
      ~send_error:"send failed" ()
  in
  match
    run_login
      ~start:(A.Homeserver_known (uri "https://known.example"))
      channel (login_hooks ())
  with
  | Error (A.Channel_error "send failed") -> ()
  | Error _ -> Alcotest.fail "wrong send channel error"
  | Ok () -> Alcotest.fail "send channel failure accepted"

let test_login_callback_errors () =
  let activate = scripted ~incoming:[ M.Login_protocol_accepted ] () in
  (match
     run_login
       ~start:(A.Homeserver_known (uri "https://known.example"))
       activate
       (login_hooks
          ~activate:(fun ~device_id:_ _ -> Error "activate failed")
          ())
   with
  | Error (A.Local_error "activate failed") -> ()
  | Error _ -> Alcotest.fail "wrong activate callback error"
  | Ok () -> Alcotest.fail "activate callback failure accepted");
  let import =
    scripted ~incoming:[ M.Login_protocol_accepted; M.Login_secrets secrets ] ()
  in
  match
    run_login
      ~start:(A.Homeserver_known (uri "https://known.example"))
      import
      (login_hooks ~import_secrets:(fun _ -> Error "import failed") ())
  with
  | Error (A.Local_error "import failed") -> ()
  | Error _ -> Alcotest.fail "wrong import callback error"
  | Ok () -> Alcotest.fail "import callback failure accepted"

let test_messages_reject_duplicate_members () =
  let duplicate_type =
    {|{"type":"m.login.success","type":"m.login.declined"}|}
  in
  let duplicate_nested =
    {|{"type":"m.login.protocol","device_id":"device","protocol":"device_authorization_grant","device_authorization_grant":{"verification_uri":"https://matrix.example/device","verification_uri":"https://matrix.example/other"}}|}
  in
  check_bool "duplicate top-level member is rejected" true
    (Result.is_error (M.of_string duplicate_type));
  check_bool "duplicate nested member is rejected" true
    (Result.is_error (M.of_string duplicate_nested))

let grant_hooks ?(export_secrets = fun () -> Ok secrets)
    ?(device_exists = fun _ -> Ok false) ?(authorize = fun _ -> Ok A.Confirm)
    ?(await_device = fun _ -> Ok true) ?(on_progress = fun _ -> ()) () =
  { A.export_secrets; device_exists; authorize; await_device; on_progress }

let run_grant ?(start = A.Protocols_already_known) script hooks =
  A.run_grant ~start ~channel:script.channel ~hooks

let protocol_message ?(protocol = M.Device_authorization_grant) ?complete () =
  M.Login_protocol
    {
      device_authorization_grant = grant ?complete ();
      protocol;
      device_id = "new-device";
    }

let test_grant_happy_path_and_progress () =
  let progress = ref [] in
  let seen_uri = ref None in
  let script =
    scripted
      ~incoming:
        [
          protocol_message ~complete:"https://matrix.example/complete" ();
          M.Login_success;
        ]
      ()
  in
  let hooks =
    grant_hooks
      ~authorize:(fun verification_uri ->
        seen_uri := Some verification_uri;
        Ok A.Confirm)
      ~on_progress:(fun value -> progress := !progress @ [ value ])
      ()
  in
  match
    run_grant
      ~start:(A.Advertise_protocols (uri "https://matrix.example"))
      script hooks
  with
  | Error _ -> Alcotest.fail "happy-path grant failed"
  | Ok () ->
      check_string "verification URI" "https://matrix.example/complete"
        (Uriz.to_string (Option.get !seen_uri));
      check_messages "grant messages"
        [
          M.Login_protocols
            {
              protocols = [ M.Device_authorization_grant ];
              homeserver = uri "https://matrix.example";
            };
          M.Login_protocol_accepted;
          M.Login_secrets secrets;
        ]
        !(script.sent);
      check_bool "grant progress" true
        (!progress
        = [
            A.Grant_starting;
            A.Waiting_for_authorization
              { verification_uri = uri "https://matrix.example/complete" };
            A.Grant_syncing_secrets;
            A.Grant_done;
          ])

let test_grant_protocols_known_and_failures () =
  let known = scripted ~incoming:[ protocol_message (); M.Login_success ] () in
  (match run_grant known (grant_hooks ()) with
  | Ok () ->
      check_messages "known protocols"
        [ M.Login_protocol_accepted; M.Login_secrets secrets ]
        !(known.sent)
  | Error _ -> Alcotest.fail "protocols-known grant failed");
  let unsupported =
    scripted
      ~incoming:[ protocol_message ~protocol:(M.Custom_protocol "future") () ]
      ()
  in
  (match run_grant unsupported (grant_hooks ()) with
  | Error (A.Unsupported_protocol (M.Custom_protocol "future")) -> (
      match List.rev !(unsupported.sent) with
      | message :: _ -> check_failure_reason M.Unsupported_protocol message
      | [] -> Alcotest.fail "missing unsupported-protocol failure")
  | Error _ -> Alcotest.fail "wrong custom-protocol error"
  | Ok () -> Alcotest.fail "custom protocol accepted");
  let existing = scripted ~incoming:[ protocol_message () ] () in
  (match
     run_grant existing (grant_hooks ~device_exists:(fun _ -> Ok true) ())
   with
  | Error A.Device_already_exists -> (
      match List.rev !(existing.sent) with
      | message :: _ -> check_failure_reason M.Device_already_exists message
      | [] -> Alcotest.fail "missing existing-device failure")
  | Error _ -> Alcotest.fail "wrong existing-device error"
  | Ok () -> Alcotest.fail "existing device accepted");
  let cancelled = scripted ~incoming:[ protocol_message () ] () in
  match
    run_grant cancelled (grant_hooks ~authorize:(fun _ -> Ok A.Cancel) ())
  with
  | Error A.User_cancelled -> (
      match List.rev !(cancelled.sent) with
      | message :: _ -> check_failure_reason M.User_cancelled message
      | [] -> Alcotest.fail "missing cancellation failure")
  | Error _ -> Alcotest.fail "wrong cancellation error"
  | Ok () -> Alcotest.fail "cancel accepted"

let test_grant_device_missing_and_local_channel_errors () =
  let missing =
    scripted ~incoming:[ protocol_message (); M.Login_success ] ()
  in
  (match
     run_grant missing (grant_hooks ~await_device:(fun _ -> Ok false) ())
   with
  | Error A.Device_not_found -> (
      match List.rev !(missing.sent) with
      | message :: _ -> check_failure_reason M.Device_not_found message
      | [] -> Alcotest.fail "missing device failure was not sent")
  | Error _ -> Alcotest.fail "wrong missing-device error"
  | Ok () -> Alcotest.fail "missing device accepted");
  let local = scripted ~incoming:[ protocol_message () ] () in
  (match
     run_grant local
       (grant_hooks ~export_secrets:(fun () -> Error "export failed") ())
   with
  | Error (A.Local_error "export failed") -> ()
  | Error _ -> Alcotest.fail "wrong grant callback error"
  | Ok () -> Alcotest.fail "grant callback failure accepted");
  let channel = scripted ~receive_error:"receive failed" () in
  match run_grant channel (grant_hooks ()) with
  | Error (A.Channel_error "receive failed") -> ()
  | Error _ -> Alcotest.fail "wrong grant channel error"
  | Ok () -> Alcotest.fail "grant channel failure accepted"

let test_grant_peer_and_unexpected_messages () =
  let peer =
    scripted
      ~incoming:
        [ M.Login_failure { reason = M.User_cancelled; homeserver = None } ]
      ()
  in
  (match run_grant peer (grant_hooks ()) with
  | Error (A.Peer_failure { reason = M.User_cancelled; homeserver = None }) ->
      ()
  | Error _ -> Alcotest.fail "wrong grant peer-failure error"
  | Ok () -> Alcotest.fail "grant peer failure accepted");
  let unexpected = scripted ~incoming:[ M.Login_success ] () in
  match run_grant unexpected (grant_hooks ()) with
  | Error (A.Unexpected_message { expected; received = M.Login_success }) ->
      check_string "unexpected grant expected type" "m.login.protocol" expected
  | Error _ -> Alcotest.fail "wrong grant unexpected-message error"
  | Ok () -> Alcotest.fail "grant unexpected message accepted"

let test_grant_callback_errors () =
  let lookup = scripted ~incoming:[ protocol_message () ] () in
  (match
     run_grant lookup
       (grant_hooks ~device_exists:(fun _ -> Error "lookup failed") ())
   with
  | Error (A.Local_error "lookup failed") -> ()
  | Error _ -> Alcotest.fail "wrong device lookup error"
  | Ok () -> Alcotest.fail "device lookup failure accepted");
  let authorize = scripted ~incoming:[ protocol_message () ] () in
  (match
     run_grant authorize
       (grant_hooks ~authorize:(fun _ -> Error "authorize failed") ())
   with
  | Error (A.Local_error "authorize failed") -> ()
  | Error _ -> Alcotest.fail "wrong authorize callback error"
  | Ok () -> Alcotest.fail "authorize callback failure accepted");
  let await = scripted ~incoming:[ protocol_message (); M.Login_success ] () in
  match
    run_grant await
      (grant_hooks ~await_device:(fun _ -> Error "await failed") ())
  with
  | Error (A.Local_error "await failed") -> ()
  | Error _ -> Alcotest.fail "wrong await-device callback error"
  | Ok () -> Alcotest.fail "await-device callback failure accepted"

let () =
  Alcotest.run "qr application"
    [
      ( "login",
        [
          Alcotest.test_case "happy path" `Quick test_login_happy_path;
          Alcotest.test_case "known homeserver" `Quick
            test_login_homeserver_known;
          Alcotest.test_case "unsupported protocol" `Quick
            test_login_unsupported_protocol;
          Alcotest.test_case "token failures" `Quick test_login_token_failures;
          Alcotest.test_case "unexpected and peer failures" `Quick
            test_login_unexpected_and_peer_failures;
          Alcotest.test_case "unexpected secrets" `Quick
            test_login_unexpected_secrets;
          Alcotest.test_case "local and channel errors" `Quick
            test_login_local_and_channel_errors;
          Alcotest.test_case "callback errors" `Quick test_login_callback_errors;
          Alcotest.test_case "duplicate members rejected" `Quick
            test_messages_reject_duplicate_members;
        ] );
      ( "grant",
        [
          Alcotest.test_case "happy path and progress" `Quick
            test_grant_happy_path_and_progress;
          Alcotest.test_case "known protocols and failures" `Quick
            test_grant_protocols_known_and_failures;
          Alcotest.test_case "missing device and errors" `Quick
            test_grant_device_missing_and_local_channel_errors;
          Alcotest.test_case "peer and unexpected messages" `Quick
            test_grant_peer_and_unexpected_messages;
          Alcotest.test_case "callback errors" `Quick test_grant_callback_errors;
        ] );
    ]
