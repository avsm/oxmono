module Q = Matrix_client.Qr_login.Msc4108
module S = Q.Secure_channel
module M = Q.Messages
module Eio_qr = Matrix_eio.Qr_login.Msc4108.Application_eio

type rendezvous = {
  mutable body : string;
  mutable etag : int;
  mutable closed : bool;
  mutable deletes : int;
}

let etag value : Fetch.Header.etag option =
  Some { weak = false; tag = string_of_int value }

let wire_etag value = Printf.sprintf "\"%d\"" value
let header name headers = List.assoc_opt name (Fetch.Header.to_list headers)

let transport server =
  {
    Q.Rendezvous.request =
      (fun ~method_ ~uri:_ ~headers ~body ->
        if server.closed && String.equal server.body "" then
          Ok
            {
              status = 404;
              etag = etag 0;
              content_type = None;
              body = "closed";
              expires_at = None;
            }
        else
          match method_ with
          | `POST ->
              Ok
                {
                  status = 200;
                  etag = etag 0;
                  content_type = Some (Fetch.Header.media "application/json");
                  body = {|{"url":"http://example.test/rendezvous"}|};
                  expires_at = None;
                }
          | `GET ->
              if header "If-None-Match" headers = Some (wire_etag server.etag)
              then
                Ok
                  {
                    status = 304;
                    etag = etag server.etag;
                    content_type = None;
                    body = "";
                    expires_at = None;
                  }
              else
                Ok
                  {
                    status = 200;
                    etag = etag server.etag;
                    content_type = Some (Fetch.Header.media "text/plain");
                    body = server.body;
                    expires_at = None;
                  }
          | `PUT -> (
              match (header "If-Match" headers, body) with
              | Some tag, Some body when tag = wire_etag server.etag ->
                  server.body <- body;
                  server.etag <- server.etag + 1;
                  Ok
                    {
                      status = 200;
                      etag = etag server.etag;
                      content_type = Some (Fetch.Header.media "text/plain");
                      body = "";
                      expires_at = None;
                    }
              | _ ->
                  Ok
                    {
                      status = 412;
                      etag = etag server.etag;
                      content_type = None;
                      body = "etag";
                      expires_at = None;
                    })
          | `DELETE ->
              server.closed <- true;
              server.deletes <- server.deletes + 1;
              Ok
                {
                  status = 200;
                  etag = etag server.etag;
                  content_type = None;
                  body = "";
                  expires_at = None;
                });
    now = (fun () -> Ptime.epoch);
    sleep = (fun _ -> Eio.Fiber.yield ());
  }

let random () =
  Matrix_client.Random.of_source (Eio.Flow.string_source (String.make 4096 'x'))

let body_of_request (request : Fetch.Middleware.request) =
  match request.body with
  | Fetch.Empty -> None
  | Fetch.String body -> Some body
  | Fetch.Stream _ -> None

let json_field name = function
  | Jsont.Object (members, _) -> (
      match Jsont.Json.find_mem name members with
      | Some (_, value) -> Some value
      | None -> None)
  | _ -> None

let json_string_field name json =
  match json_field name json with
  | Some (Jsont.String (value, _)) -> Some value
  | _ -> None

let json_of_string body =
  Result.get_ok (Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json body)

let string_of_json json =
  Result.get_ok (Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json json)

let json_object members =
  Jsont.Json.object'
    (List.map
       (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value)
       members)

let json_string value = Jsont.Json.string value

let established sw =
  let server = { body = ""; etag = 0; closed = false; deletes = 0 } in
  let transport = transport server in
  let displayed =
    Result.get_ok
      (S.login ~transport
         ~rendezvous_server:(Uriz.of_string_exn "http://example.test")
         ~random:(random ()) ())
  in
  let qr = S.qr_code displayed in
  let promise, resolver = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve resolver
        (S.from_qr_code ~transport ~random:(random ())
           ~expected_intent:(Q.Reciprocate (Uriz.of_string_exn "matrix.org"))
           qr));
  let almost = Result.get_ok (S.connect displayed) in
  let scanner = Result.get_ok (Eio.Promise.await promise) in
  let displayed =
    Result.get_ok
      (S.confirm almost ~check_code:(S.check_code_established scanner))
  in
  (displayed, scanner, server)

let unauthenticated_client ~sw ~env ~fetch =
  Matrix_eio.Client.create ~sw ~env
    ~homeserver:(Uriz.of_string_exn "https://hs.example")
    ~fetch ()

(* A failed peer fiber must fail a protocol test instead of leaving its other
   half blocked on an in-memory rendezvous forever. *)
let run_bounded fn =
  Eio_main.run @@ fun env ->
  Eio.Time.with_timeout_exn env#clock 10. (fun () -> fn env)

let test_existing_session_is_rejected_and_closes () =
  run_bounded @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let calls = ref 0 in
  let fetch =
    Fetch_mock.client (fun request ->
        incr calls;
        Fetch_mock.respond ~status:599 "unexpected request" request)
  in
  let client = unauthenticated_client ~sw ~env ~fetch in
  let session : Matrix_client.Client.session =
    {
      user_id =
        Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:hs.example");
      device_id = Result.get_ok (Matrix_proto.Id.Device_id.of_string "OLD");
      access_token = "old";
      refresh_token = None;
    }
  in
  let client = Matrix_eio.Client.with_session client session in
  let displayed, _scanner, server = established sw in
  (try ignore (Eio_qr.login ~env displayed client ()) with _ -> ());
  Alcotest.(check int) "no HTTP before rejection" 0 !calls;
  Alcotest.(check int) "channel cleanup" 1 server.deletes

let test_adapter_login_reaches_authenticated_trust_gate () =
  run_bounded @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let requests = ref [] in
  let authenticated_calls = ref 0 in
  let upload_seen = ref false in
  let callback_after_upload = ref false in
  let fetch =
    Fetch_mock.client (fun request ->
        requests :=
          Fetch.Middleware.Url.to_string request.Fetch.Middleware.url
          :: !requests;
        let path =
          Uriz.path
            (Uriz.of_string_exn
               (Fetch.Middleware.Url.to_string request.Fetch.Middleware.url))
        in
        if String.ends_with ~suffix:"auth_metadata" path then
          Fetch_mock.respond
            {|{"issuer":"https://auth.example/","authorization_endpoint":"https://auth.example/oauth/authorize","token_endpoint":"https://auth.example/oauth/token","device_authorization_endpoint":"https://auth.example/oauth/device","account_management_actions_supported":[],"response_types_supported":["code"],"grant_types_supported":["authorization_code","refresh_token","urn:ietf:params:oauth:grant-type:device_code"],"response_modes_supported":["query"],"code_challenge_methods_supported":["S256"],"prompt_values_supported":[],"scopes_supported":["urn:matrix:client:api:*"]}|}
            request
        else if String.ends_with ~suffix:"/oauth/device" path then
          Fetch_mock.respond
            {|{"device_code":"device","user_code":"ABCD","verification_uri":"https://auth.example/device","expires_in":60,"interval":1}|}
            request
        else if String.ends_with ~suffix:"/oauth/token" path then
          Fetch_mock.respond {|{"access_token":"access","token_type":"Bearer"}|}
            request
        else if String.ends_with ~suffix:"/account/whoami" path then
          Fetch_mock.respond {|{"user_id":"@alice:hs.example"}|} request
        else if String.ends_with ~suffix:"/keys/upload" path then begin
          upload_seen := true;
          Fetch_mock.respond {|{"one_time_key_counts":{}}|} request
        end
        else if String.ends_with ~suffix:"/keys/query" path then
          Fetch_mock.respond
            {|{"failures":{},"device_keys":{},"master_keys":{},"self_signing_keys":{},"user_signing_keys":{}}|}
            request
        else Fetch_mock.respond ~status:404 "unhandled" request)
  in
  let displayed, scanner, server = established sw in
  let user_id =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:hs.example")
  in
  let identity = Matrix_client.Cross_signing.create_private_identity ~user_id in
  Matrix_client.Cross_signing.generate_private_keys ~random:(random ()) identity;
  let bundle = Result.get_ok (Q.Secrets.export ~private_identity:identity ()) in
  let client = unauthenticated_client ~sw ~env ~fetch in
  let promise, resolver = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      try
        let result =
          Eio_qr.login ~env ~client_id:"client"
            ~on_authenticated:(fun _session ~client_id ~expires_at ->
              incr authenticated_calls;
              if not (String.equal client_id "client") then
                Alcotest.fail "callback got the wrong client id";
              if expires_at <> None then
                Alcotest.fail "test token unexpectedly had an expiry";
              if !upload_seen then callback_after_upload := true)
            ~start:
              (Eio_qr.Homeserver_known
                 (Uriz.of_string_exn "HTTPS://HS.EXAMPLE:443/"))
            displayed client ()
        in
        Eio.Promise.resolve resolver (`Ok result)
      with exn -> Eio.Promise.resolve resolver (`Error exn));
  (match S.receive_json scanner with
  | Ok (M.Login_protocol _) -> (
      ignore (S.send_json scanner M.Login_protocol_accepted);
      (match S.receive_json scanner with
      | Ok M.Login_success ->
          ignore (S.send_json scanner (M.Login_secrets bundle))
      | _ -> Alcotest.fail "login did not reach success boundary");
      match Eio.Promise.await promise with
      | `Error _ -> ()
      | `Ok _ -> Alcotest.fail "login completed without a verified own device")
  | Ok (M.Login_failure { reason; _ }) ->
      Alcotest.failf "login failed before protocol: %s"
        (M.login_failure_reason_to_string reason)
  | Ok _ -> Alcotest.fail "login sent an unexpected application message"
  | Error error ->
      Alcotest.failf "channel receive failed: %a (requests: %s)" S.pp_error
        error
        (String.concat "," (List.rev !requests)));
  Alcotest.(check bool) "channel closed after trust failure" true server.closed;
  Alcotest.(check int)
    "authenticated callback exactly once" 1 !authenticated_calls;
  Alcotest.(check bool)
    "authenticated callback precedes key upload" false !callback_after_upload

let test_adapter_paired_login_success () =
  run_bounded @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let uploaded_json = ref None in
  let uploaded_device = ref None in
  let login_fetch =
    Fetch_mock.client (fun request ->
        let path =
          Uriz.path
            (Uriz.of_string_exn
               (Fetch.Middleware.Url.to_string request.Fetch.Middleware.url))
        in
        if String.ends_with ~suffix:"auth_metadata" path then
          Fetch_mock.respond
            {|{"issuer":"https://auth.example/","authorization_endpoint":"https://auth.example/oauth/authorize","token_endpoint":"https://auth.example/oauth/token","device_authorization_endpoint":"https://auth.example/oauth/device","account_management_actions_supported":[],"response_types_supported":["code"],"grant_types_supported":["authorization_code","refresh_token","urn:ietf:params:oauth:grant-type:device_code"],"response_modes_supported":["query"],"code_challenge_methods_supported":["S256"],"prompt_values_supported":[],"scopes_supported":["urn:matrix:client:api:*"]}|}
            request
        else if String.ends_with ~suffix:"/oauth/device" path then
          Fetch_mock.respond
            {|{"device_code":"device","user_code":"ABCD","verification_uri":"https://auth.example/device","expires_in":60,"interval":1}|}
            request
        else if String.ends_with ~suffix:"/oauth/token" path then
          Fetch_mock.respond {|{"access_token":"access","token_type":"Bearer"}|}
            request
        else if String.ends_with ~suffix:"/account/whoami" path then
          Fetch_mock.respond {|{"user_id":"@alice:hs.example"}|} request
        else if String.ends_with ~suffix:"/keys/upload" path then begin
          Option.iter
            (fun body ->
              let body = json_of_string body in
              match json_field "device_keys" body with
              | None -> ()
              | Some device_keys ->
                  uploaded_json := Some device_keys;
                  uploaded_device :=
                    Jsont.Json.decode Matrix_client.Keys.device_keys_jsont
                      device_keys
                    |> Result.to_option)
            (body_of_request request);
          Fetch_mock.respond {|{"one_time_key_counts":{}}|} request
        end
        else if String.ends_with ~suffix:"/keys/query" path then
          let device_keys =
            match (!uploaded_json, !uploaded_device) with
            | Some json, Some device ->
                let user = Matrix_proto.Id.User_id.to_string device.user_id in
                let device =
                  Matrix_proto.Id.Device_id.to_string device.device_id
                in
                json_object
                  [
                    ( "device_keys",
                      json_object [ (user, json_object [ (device, json) ]) ] );
                    ("master_keys", json_object []);
                    ("self_signing_keys", json_object []);
                    ("user_signing_keys", json_object []);
                  ]
            | _ ->
                json_object
                  [
                    ("device_keys", json_object []);
                    ("master_keys", json_object []);
                    ("self_signing_keys", json_object []);
                    ("user_signing_keys", json_object []);
                  ]
          in
          Fetch_mock.respond (string_of_json device_keys) request
        else Fetch_mock.respond ~status:404 "unhandled" request)
  in
  let seen_device_id = ref None in
  (* The grant handler learns the exact Curve25519-derived device ID from the
     first device lookup.  The suffix is intentionally decoded so this remains
     valid when a key contains a URI-escaped byte. *)
  let grant_fetch =
    Fetch_mock.client (fun request ->
        let path =
          Uriz.path
            (Uriz.of_string_exn
               (Fetch.Middleware.Url.to_string request.Fetch.Middleware.url))
        in
        if String.ends_with ~suffix:"/devices" path then
          let device_id = Option.value !seen_device_id ~default:"UNKNOWN" in
          let body =
            json_object
              [
                ( "devices",
                  Jsont.Json.list
                    [
                      json_object
                        [
                          ("device_id", json_string device_id);
                          ("display_name", Jsont.Json.null ());
                          ("last_seen_ip", Jsont.Json.null ());
                          ("last_seen_ts", Jsont.Json.null ());
                        ];
                    ] );
              ]
          in
          Fetch_mock.respond (string_of_json body) request
        else
          let marker_pos =
            try Some (String.rindex path '/') with Not_found -> None
          in
          match marker_pos with
          | Some pos when String.length path > pos + 1 ->
              let prefix = String.sub path 0 pos in
              if String.ends_with ~suffix:"/devices" prefix then begin
                (seen_device_id :=
                   match
                     Uriz.pct_decode
                       (String.sub path (pos + 1)
                          (String.length path - pos - 1))
                   with
                   | This device_id -> Some device_id
                   | Null -> Alcotest.fail "invalid device ID escape");
                Fetch_mock.respond ~status:404
                  {|{"errcode":"M_NOT_FOUND","error":"not found"}|} request
              end
              else Fetch_mock.respond ~status:404 "unhandled" request
          | _ -> Fetch_mock.respond ~status:404 "unhandled" request)
  in
  let user_id =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:hs.example")
  in
  let old_device_id =
    Result.get_ok (Matrix_proto.Id.Device_id.of_string "OLDDEVICE")
  in
  let session : Matrix_client.Client.session =
    {
      user_id;
      device_id = old_device_id;
      access_token = "old-access";
      refresh_token = None;
    }
  in
  let grant_client =
    Matrix_eio.Client.with_session
      (Matrix_eio.Client.create ~sw ~env
         ~homeserver:(Uriz.of_string_exn "https://hs.example")
         ~fetch:grant_fetch ())
      session
  in
  let grant_encryption =
    Matrix_eio.Encryption.create ~random:(random ()) ~user_id
      ~device_id:old_device_id ()
  in
  let identity = Matrix_client.Cross_signing.create_private_identity ~user_id in
  Matrix_client.Cross_signing.generate_private_keys ~random:(random ()) identity;
  let server = { body = ""; etag = 0; closed = false; deletes = 0 } in
  let rendezvous_transport = transport server in
  let login_client =
    Matrix_eio.Client.create ~sw ~env
      ~homeserver:(Uriz.of_string_exn "https://hs.example")
      ~fetch:login_fetch ()
  in
  let login_progress = ref [] in
  let grant_progress = ref [] in
  let persisted = ref 0 in
  let authenticated_calls = ref 0 in
  let authenticated_metadata = ref None in
  let login_code = ref None in
  let grant_code = ref None in
  let qr_p, qr_r = Eio.Promise.create () in
  let login_p, login_r = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      try
        let result =
          Eio_qr.Session.login ~env ~transport:rendezvous_transport
            ~rendezvous_server:(Uriz.of_string_exn "http://example.test")
            ~random:(random ()) ~client_id:"client"
            ~display_qr:(fun qr -> Eio.Promise.resolve qr_r qr)
            ~confirm_check_code:(fun code ->
              login_code := Some code;
              true)
            ~on_authenticated:(fun _session ~client_id ~expires_at ->
              incr authenticated_calls;
              authenticated_metadata := Some (client_id, expires_at))
            ~persist:(fun _ _ -> incr persisted)
            ~on_progress:(fun value ->
              login_progress := value :: !login_progress)
            login_client ()
        in
        Eio.Promise.resolve login_r (`Ok result)
      with exn -> Eio.Promise.resolve login_r (`Error exn));
  let grant_p, grant_r = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      try
        Eio_qr.Session.grant ~env ~transport:rendezvous_transport
          ~random:(random ())
          ~scan_qr:(fun () -> Eio.Promise.await qr_p)
          ~expected_intent:(Q.Reciprocate (Uriz.of_string_exn "matrix.org"))
          ~confirm_check_code:(fun code ->
            grant_code := Some code;
            true)
          ~authorize:(fun _ -> Eio_qr.Confirm)
          ~on_progress:(fun value -> grant_progress := value :: !grant_progress)
          ~private_identity:identity ~encryption:grant_encryption grant_client
          ();
        Eio.Promise.resolve grant_r `Ok
      with exn -> Eio.Promise.resolve grant_r (`Error exn));
  (match Eio.Promise.await grant_p with
  | `Ok -> ()
  | `Error exn ->
      Alcotest.failf "paired grant failed: %s" (Printexc.to_string exn));
  let result =
    match Eio.Promise.await login_p with
    | `Ok result -> result
    | `Error exn ->
        Alcotest.failf "paired login failed: %s" (Printexc.to_string exn)
  in
  let uploaded =
    match !uploaded_device with
    | Some device -> device
    | None -> Alcotest.fail "login did not upload device keys"
  in
  Alcotest.(check string)
    "Curve-derived device id is retained"
    (Matrix_proto.Id.Device_id.to_string uploaded.device_id)
    (Matrix_proto.Id.Device_id.to_string result.session.device_id);
  Alcotest.(check string)
    "encryption follows authenticated device"
    (Matrix_proto.Id.Device_id.to_string result.session.device_id)
    (Matrix_proto.Id.Device_id.to_string
       (Matrix_eio.Encryption.device_id result.encryption));
  let signature_count =
    List.fold_left
      (fun count (_, signatures) -> count + List.length signatures)
      0 uploaded.signatures
  in
  Alcotest.(check bool)
    "device keys carry account and self-signing signatures" true
    (signature_count >= 2);
  Alcotest.(check int) "private seeds persistence hook once" 1 !persisted;
  Alcotest.(check int) "authenticated callback once" 1 !authenticated_calls;
  Alcotest.(check (option string))
    "authenticated callback client id" (Some "client")
    (Option.map fst !authenticated_metadata);
  Alcotest.(check bool)
    "authenticated callback expiry" true
    (Option.fold ~none:false
       ~some:(fun (_, expiry) -> expiry = None)
       !authenticated_metadata);
  Alcotest.(check (option int))
    "login/grant check codes match" !login_code !grant_code;
  let has_progress predicate values = List.exists predicate values in
  Alcotest.(check bool)
    "session establishment reported" true
    (has_progress
       (function Eio_qr.Session.Establishing_channel -> true | _ -> false)
       !login_progress);
  Alcotest.(check bool)
    "session check-code wait reported" true
    (has_progress
       (function Eio_qr.Session.Awaiting_check_code _ -> true | _ -> false)
       !login_progress);
  Alcotest.(check bool)
    "session OAuth reported" true
    (has_progress
       (function Eio_qr.Session.OAuth _ -> true | _ -> false)
       !login_progress);
  Alcotest.(check bool)
    "session secrets reported" true
    (has_progress
       (function Eio_qr.Session.Secrets -> true | _ -> false)
       !login_progress);
  Alcotest.(check bool)
    "session trust reported" true
    (has_progress
       (function Eio_qr.Session.Trust_and_backup -> true | _ -> false)
       !login_progress);
  Alcotest.(check bool)
    "session done reported" true
    (has_progress
       (function Eio_qr.Session.Done -> true | _ -> false)
       !login_progress);
  Alcotest.(check bool)
    "grant session done reported" true
    (has_progress
       (function Eio_qr.Session.Done -> true | _ -> false)
       !grant_progress);
  Alcotest.(check bool)
    "both application channels clean up" true
    (server.closed && server.deletes >= 2)

let test_homeserver_mismatch_is_rejected_and_closes () =
  run_bounded @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let calls = ref 0 in
  let fetch =
    Fetch_mock.client (fun request ->
        incr calls;
        Fetch_mock.respond ~status:599 "unexpected request" request)
  in
  let client = unauthenticated_client ~sw ~env ~fetch in
  let displayed, _scanner, server = established sw in
  (try
     ignore
       (Eio_qr.login ~env
          ~start:
            (Eio_qr.Homeserver_known
               (Uriz.of_string_exn "https://other.example"))
          displayed client ())
   with _ -> ());
  Alcotest.(check int) "no HTTP before mismatch rejection" 0 !calls;
  Alcotest.(check int) "channel cleanup" 1 server.deletes

let test_grant_cancel_through_adapter () =
  run_bounded @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fetch =
    Fetch_mock.client (fun request ->
        let path =
          Uriz.path
            (Uriz.of_string_exn
               (Fetch.Middleware.Url.to_string request.Fetch.Middleware.url))
        in
        if String.ends_with ~suffix:"/devices/NEWDEVICE" path then
          Fetch_mock.respond ~status:404
            {|{"errcode":"M_NOT_FOUND","error":"not found"}|} request
        else Fetch_mock.respond ~status:404 "unhandled" request)
  in
  let client = unauthenticated_client ~sw ~env ~fetch in
  let user_id =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:hs.example")
  in
  let device_id =
    Result.get_ok (Matrix_proto.Id.Device_id.of_string "OLDDEVICE")
  in
  let session : Matrix_client.Client.session =
    { user_id; device_id; access_token = "access"; refresh_token = None }
  in
  let client = Matrix_eio.Client.with_session client session in
  let encryption =
    Matrix_eio.Encryption.create ~random:(random ()) ~user_id ~device_id ()
  in
  let identity = Matrix_client.Cross_signing.create_private_identity ~user_id in
  Matrix_client.Cross_signing.generate_private_keys ~random:(random ()) identity;
  let displayed, scanner, server = established sw in
  let promise, resolver = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      try
        Eio_qr.grant ~env ~advertise_protocols:false
          ~authorize:(fun _ -> Eio_qr.Cancel)
          ~private_identity:identity ~encryption displayed client ();
        Eio.Promise.resolve resolver `Ok
      with exn -> Eio.Promise.resolve resolver (`Error exn));
  (match
     S.send_json scanner
       (M.Login_protocol
          {
            device_authorization_grant =
              {
                verification_uri =
                  Uriz.of_string_exn "https://auth.example/device";
                verification_uri_complete = None;
              };
            protocol = M.Device_authorization_grant;
            device_id = "NEWDEVICE";
          })
   with
  | Ok () -> ()
  | Error error -> Alcotest.failf "sending protocol failed: %a" S.pp_error error);
  (match S.receive_json scanner with
  | Ok (M.Login_failure { reason = M.User_cancelled; homeserver = None }) -> ()
  | Ok message ->
      ignore message;
      Alcotest.fail "expected cancellation message"
  | Error error -> Alcotest.failf "channel receive failed: %a" S.pp_error error);
  (match Eio.Promise.await promise with
  | `Error _ -> ()
  | `Ok -> Alcotest.fail "cancelled grant completed");
  Eio.Fiber.yield ();
  Alcotest.(check bool) "channel cleanup" true server.closed

let test_session_requires_check_code_and_cleans_up () =
  run_bounded @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server = { body = ""; etag = 0; closed = false; deletes = 0 } in
  let transport = transport server in
  let client =
    unauthenticated_client ~sw ~env
      ~fetch:
        (Fetch_mock.client (fun request ->
             Fetch_mock.respond ~status:599 "unexpected request" request))
  in
  let scanner = ref None in
  let progress = ref [] in
  let failed = ref false in
  (try
     ignore
       (Eio_qr.Session.login ~env ~transport
          ~rendezvous_server:(Uriz.of_string_exn "http://example.test")
          ~random:(random ())
          ~display_qr:(fun qr ->
            Eio.Fiber.fork ~sw (fun () ->
                scanner :=
                  Some
                    (S.from_qr_code ~transport ~random:(random ())
                       ~expected_intent:
                         (Q.Reciprocate (Uriz.of_string_exn "matrix.org"))
                       qr)))
          ~confirm_check_code:(fun _ -> false)
          ~on_progress:(fun value -> progress := value :: !progress)
          client ())
   with
  | Eio.Io (Eio_qr.Session.Session_error Eio_qr.Session.Check_code_rejected, _)
    ->
      failed := true
  | _ -> ());
  Alcotest.(check bool) "check code is mandatory" true !failed;
  Alcotest.(check bool) "rendezvous cleanup" true server.closed;
  Alcotest.(check bool)
    "channel establishment reported" true
    (List.exists
       (function Eio_qr.Session.Establishing_channel -> true | _ -> false)
       !progress);
  Alcotest.(check bool)
    "check-code wait reported" true
    (List.exists
       (function Eio_qr.Session.Awaiting_check_code _ -> true | _ -> false)
       !progress);
  Option.iter
    (function Ok channel -> ignore (S.close channel) | Error _ -> ())
    !scanner

let () =
  Alcotest.run "qr eio application"
    [
      ( "adapter boundaries",
        [
          Alcotest.test_case "existing session rejection and cleanup" `Quick
            test_existing_session_is_rejected_and_closes;
          Alcotest.test_case "login account handoff and trust gate" `Quick
            test_adapter_login_reaches_authenticated_trust_gate;
          Alcotest.test_case "paired login success and signed upload" `Quick
            test_adapter_paired_login_success;
          Alcotest.test_case "homeserver mismatch and cleanup" `Quick
            test_homeserver_mismatch_is_rejected_and_closes;
          Alcotest.test_case "grant cancellation and cleanup" `Quick
            test_grant_cancel_through_adapter;
          Alcotest.test_case "session check-code gate and cleanup" `Quick
            test_session_requires_check_code_and_cleans_up;
        ] );
    ]
