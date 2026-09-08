module Recovery = Matrix_client.Recovery
module Client = Matrix_client.Client
module Error = Matrix_client.Error
module Encryption = Matrix_client.Encryption
module Encryption_driver = Matrix_client.Encryption_driver
module Backup = Matrix_client.Backup
module Cross_signing = Matrix_client.Cross_signing
module Crypto_key = Matrix_client.Crypto_key
module Keys = Matrix_client.Keys
module Uiaa = Matrix_client.Uiaa
module Secret_storage = Matrix_client.Secret_storage
module Secrets = Matrix_client.Secrets
module Base_client = Matrix_client.Base_client
module Store = Matrix_client.Store
module Id = Matrix_proto.Id

let mock_env =
  object
    method secure_random =
      Eio.Flow.string_source
        (String.init 65536 (fun i -> Char.chr (i land 255)))
  end

type recorded = { meth : string; url : string; body : string option }

let request_body (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> None
  | Fetch.String body -> Some body
  | Fetch.Stream _ -> Some "<stream>"

let mock handler =
  let log = ref [] in
  let fetch =
    Fetch_mock.client (fun (req : Fetch.Middleware.request) ->
        log :=
          {
            meth = Http.Method.to_string req.meth;
            url = Fetch.Middleware.Url.to_string req.url;
            body = request_body req;
          }
          :: !log;
        handler req)
  in
  (log, fetch)

let client_of fetch =
  let config =
    Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ()
  in
  Client.create ~config ~fetch ~random:(Matrix_client.Random.of_env mock_env)
  |> fun client ->
  Client.with_session client
    {
      Client.user_id =
        Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:example.org");
      access_token = "token";
      device_id = Result.get_ok (Matrix_proto.Id.Device_id.of_string "DEVICE");
      refresh_token = None;
    }

let requests log = List.rev !log

let path_has suffix url =
  let n = String.length suffix and m = String.length url in
  m >= n && String.sub url (m - n) n = suffix

let path_contains fragment url =
  let n = String.length fragment and m = String.length url in
  let rec loop index =
    index + n <= m && (String.sub url index n = fragment || loop (index + 1))
  in
  loop 0

let jname n = (n, Jsont.Meta.none)
let jstr = Jsont.Json.string

let jobj members =
  Jsont.Json.object'
    (List.map (fun (name, value) -> Jsont.Json.mem (jname name) value) members)

let encoded codec value =
  match Jsont.Json.encode codec value with
  | Ok json -> json
  | Error error -> Alcotest.failf "cannot encode recovery fixture: %s" error

let encoded_string codec value =
  match Jsont_bytesrw.encode_string ~format:Jsont.Minify codec value with
  | Ok body -> body
  | Error error -> Alcotest.failf "cannot encode recovery fixture: %s" error

let captured_auth_data body =
  let json =
    match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json body with
    | Ok json -> json
    | Error error -> Alcotest.failf "invalid captured request JSON: %s" error
  in
  let auth_data =
    match json with
    | Jsont.Object (members, _) ->
        Option.map snd (Jsont.Json.find_mem "auth_data" members)
    | _ -> None
  in
  match auth_data with
  | None -> Alcotest.fail "captured request has no auth_data"
  | Some auth_data -> (
      match Jsont.Json.decode Backup.megolm_v1_auth_data_jsont auth_data with
      | Ok auth_data -> auth_data
      | Error error -> Alcotest.failf "invalid captured auth_data: %s" error)

let check_backup_signatures ~identity ~machine auth_data =
  let user_id = Cross_signing.identity_user_id identity in
  let master_secret =
    match Cross_signing.master_secret identity with
    | Some secret -> secret
    | None -> Alcotest.fail "test identity has no master secret"
  in
  let master_key_id =
    Crypto_key.Key_id.v ~algorithm:"ed25519"
      ~id:
        (Crypto_key.Ed25519.Public.to_base64
           (Crypto_key.Ed25519.Private.public master_secret))
  in
  Alcotest.(check bool)
    "master signature verifies" true
    (match
       Backup.verify_auth_data_signature
         ~verify_key:(Crypto_key.Ed25519.Private.public master_secret)
         auth_data ~user_id ~key_id:master_key_id
     with
    | Backup.Valid_but_not_trusted | Backup.Valid_and_trusted -> true
    | Backup.Missing | Backup.Invalid -> false);
  let device_key_id =
    Crypto_key.Key_id.of_device ~algorithm:"ed25519"
      (Encryption.device_id machine)
  in
  let device_public, _ = Encryption.identity_keys machine in
  Alcotest.(check bool)
    "device signature verifies" true
    (match
       Backup.verify_auth_data_signature ~verify_key:device_public auth_data
         ~user_id ~key_id:device_key_id
     with
    | Backup.Valid_but_not_trusted | Backup.Valid_and_trusted -> true
    | Backup.Missing | Backup.Invalid -> false)

let test_recover_order_and_failures () =
  Eio_mock.Backend.run @@ fun () ->
  let user_id =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:example.org")
  in
  let device_id =
    Result.get_ok (Matrix_proto.Id.Device_id.of_string "DEVICE")
  in
  let random = Matrix_client.Random.of_env mock_env in
  let identity = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys ~random identity;
  let upload = Option.get (Cross_signing.build_upload identity) in
  let mismatching_identity = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys ~random mismatching_identity;
  let mismatching_upload =
    Option.get (Cross_signing.build_upload mismatching_identity)
  in
  let key = Secret_storage.generate_key ~random in
  let key_id = "recovery-test-key" in
  let description =
    Secret_storage.Key_description.v ~random ~name:"recovery test" key
  in
  let encode_seed secret =
    Matrix_proto.Base64.encode
      (Matrix_client.Crypto_key.Ed25519.Private.to_bytes secret)
  in
  let secret_body name value =
    let encrypted = Secret_storage.encrypt ~random key ~name value in
    jobj
      [
        ( "encrypted",
          jobj [ (key_id, encoded Secret_storage.Encrypted.jsont encrypted) ] );
      ]
  in
  let response_body (chosen_upload : Cross_signing.upload) =
    encoded_string Matrix_proto.Json.Codec.json
      (jobj
         [
           ("failures", jobj []);
           ("device_keys", jobj []);
           ( "master_keys",
             jobj
               [
                 ( Matrix_proto.Id.User_id.to_string user_id,
                   encoded Keys.cross_signing_key_jsont chosen_upload.master_key
                 );
               ] );
           ( "self_signing_keys",
             jobj
               [
                 ( Matrix_proto.Id.User_id.to_string user_id,
                   encoded Keys.cross_signing_key_jsont
                     chosen_upload.self_signing_key );
               ] );
           ( "user_signing_keys",
             jobj
               [
                 ( Matrix_proto.Id.User_id.to_string user_id,
                   encoded Keys.cross_signing_key_jsont
                     chosen_upload.user_signing_key );
               ] );
         ])
  in
  let run ~credential ~mismatch ~backup_failure =
    let query =
      response_body (if mismatch then mismatching_upload else upload)
    in
    let log, fetch =
      mock (fun req ->
          let url = Fetch.Middleware.Url.to_string req.url in
          if path_has "/keys/query" url then Fetch_mock.respond query req
          else if path_has "/account_data/m.secret_storage.default_key" url then
            Fetch_mock.respond (Printf.sprintf {|{"key":"%s"}|} key_id) req
          else if path_has ("/account_data/m.secret_storage.key." ^ key_id) url
          then
            Fetch_mock.respond
              (encoded_string Matrix_proto.Json.Codec.json
                 (encoded Secret_storage.Key_description.jsont description))
              req
          else if
            path_has
              ("/account_data/" ^ Secret_storage.secret_cross_signing_master)
              url
          then
            Fetch_mock.respond
              (encoded_string Matrix_proto.Json.Codec.json
                 (secret_body Secret_storage.secret_cross_signing_master
                    (encode_seed
                       (Option.get (Cross_signing.master_secret identity)))))
              req
          else if
            path_has
              ("/account_data/"
             ^ Secret_storage.secret_cross_signing_self_signing)
              url
          then
            Fetch_mock.respond
              (encoded_string Matrix_proto.Json.Codec.json
                 (secret_body Secret_storage.secret_cross_signing_self_signing
                    (encode_seed
                       (Option.get (Cross_signing.self_signing_secret identity)))))
              req
          else if
            path_has
              ("/account_data/"
             ^ Secret_storage.secret_cross_signing_user_signing)
              url
          then
            Fetch_mock.respond
              (encoded_string Matrix_proto.Json.Codec.json
                 (secret_body Secret_storage.secret_cross_signing_user_signing
                    (encode_seed
                       (Option.get (Cross_signing.user_signing_secret identity)))))
              req
          else if
            path_has
              ("/account_data/" ^ Secret_storage.secret_megolm_backup_v1)
              url
          then
            if backup_failure then
              Fetch_mock.respond
                (encoded_string Matrix_proto.Json.Codec.json
                   (secret_body Secret_storage.secret_megolm_backup_v1
                      "not-a-backup-key"))
                req
            else Fetch_mock.respond ~status:404 "absent" req
          else Fetch_mock.respond "{}" req)
    in
    let machine = Encryption.create ~random ~user_id ~device_id () in
    let encryption = Encryption_driver.v machine in
    let result = Recovery.recover (client_of fetch) ~encryption ~credential in
    (result, log, fetch, encryption)
  in
  let credential = Secret_storage.Recovery_key.encode key in
  let result, log, fetch, encryption =
    run ~credential ~mismatch:false ~backup_failure:false
  in
  let calls = requests log in
  (match result with
  | Ok recovered ->
      Alcotest.(check bool)
        "recovered master seed" true
        (Option.is_some
           (Cross_signing.master_secret recovered.private_identity));
      Alcotest.(check string)
        "store key id" key_id
        (Secrets.store_key_id recovered.store)
  | Error error -> Alcotest.failf "recovery failed: %a" Error.pp error);
  Alcotest.(check int) "recovery request count" 7 (List.length calls);
  List.iteri
    (fun index suffix ->
      Alcotest.(check bool)
        (Printf.sprintf "request %d order" index)
        true
        (path_has suffix (List.nth calls index).url))
    [
      "/account_data/m.secret_storage.default_key";
      "/account_data/m.secret_storage.key.recovery-test-key";
      "/account_data/m.cross_signing.master";
      "/account_data/m.cross_signing.self_signing";
      "/account_data/m.cross_signing.user_signing";
      "/keys/query";
      "/account_data/m.megolm_backup.v1";
    ];
  let bad_result, bad_log, _, _ =
    run ~credential:"not-the-recovery-key" ~mismatch:false ~backup_failure:false
  in
  let bad_calls = requests bad_log in
  Alcotest.(check bool) "bad credential fails" true (Result.is_error bad_result);
  Alcotest.(check int)
    "bad credential stops before secrets" 2 (List.length bad_calls);
  let mismatch_result, mismatch_log, _, _ =
    run ~credential ~mismatch:true ~backup_failure:false
  in
  let mismatch_calls = requests mismatch_log in
  Alcotest.(check bool)
    "cross-signing mismatch fails" true
    (Result.is_error mismatch_result);
  Alcotest.(check int)
    "cross-signing mismatch reaches query" 6
    (List.length mismatch_calls);
  let backup_result, backup_log, _, _ =
    run ~credential ~mismatch:false ~backup_failure:true
  in
  let backup_calls = requests backup_log in
  Alcotest.(check bool)
    "backup validation failure is returned" true
    (Result.is_error backup_result);
  Alcotest.(check int)
    "backup failure occurs after driver save boundary" 7
    (List.length backup_calls);
  let old_count = List.length calls in
  let reset_result =
    Recovery.recover_and_reset (client_of fetch) ~encryption ~credential ()
  in
  (match reset_result with
  | Ok reset ->
      Alcotest.(check bool)
        "recover-and-reset returns new key" true
        (String.length reset.recovery_key > 0)
  | Error error -> Alcotest.failf "recover-and-reset failed: %a" Error.pp error);
  let combined_calls = requests log in
  let rec drop n values =
    if n = 0 then values
    else match values with [] -> [] | _ :: rest -> drop (n - 1) rest
  in
  let reset_calls = drop old_count combined_calls in
  Alcotest.(check int)
    "recover-and-reset request count" 15 (List.length reset_calls);
  Alcotest.(check bool)
    "reset starts after old recovery reads" true
    (String.equal (List.nth reset_calls 7).meth "PUT"
    && path_contains "/account_data/m.secret_storage.key."
         (List.nth reset_calls 7).url);
  Alcotest.(check bool)
    "reset default key is last" true
    (path_has "/account_data/m.secret_storage.default_key"
       (List.nth reset_calls 14).url)

let test_recover_and_fix_backup_order () =
  Eio_mock.Backend.run @@ fun () ->
  let user_id =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:example.org")
  in
  let device_id =
    Result.get_ok (Matrix_proto.Id.Device_id.of_string "DEVICE")
  in
  let random = Matrix_client.Random.of_env mock_env in
  let identity = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys ~random identity;
  let upload = Option.get (Cross_signing.build_upload identity) in
  let ssss_key = Secret_storage.generate_key ~random in
  let key_id = "repair-key" in
  let description = Secret_storage.Key_description.v ~random ssss_key in
  let seed secret =
    Matrix_proto.Base64.encode
      (Matrix_client.Crypto_key.Ed25519.Private.to_bytes secret)
  in
  let secret_body name value =
    let encrypted = Secret_storage.encrypt ~random ssss_key ~name value in
    jobj
      [
        ( "encrypted",
          jobj [ (key_id, encoded Secret_storage.Encrypted.jsont encrypted) ] );
      ]
  in
  let query =
    encoded_string Matrix_proto.Json.Codec.json
      (jobj
         [
           ("failures", jobj []);
           ("device_keys", jobj []);
           ( "master_keys",
             jobj
               [
                 ( Matrix_proto.Id.User_id.to_string user_id,
                   encoded Keys.cross_signing_key_jsont upload.master_key );
               ] );
           ( "self_signing_keys",
             jobj
               [
                 ( Matrix_proto.Id.User_id.to_string user_id,
                   encoded Keys.cross_signing_key_jsont upload.self_signing_key
                 );
               ] );
           ( "user_signing_keys",
             jobj
               [
                 ( Matrix_proto.Id.User_id.to_string user_id,
                   encoded Keys.cross_signing_key_jsont upload.user_signing_key
                 );
               ] );
         ])
  in
  let other_backup = Backup.Decryption_key.generate ~random in
  let old_version =
    encoded_string Matrix_proto.Json.Codec.json
      (jobj
         [
           ("version", Jsont.Json.string "old-v1");
           ("algorithm", Jsont.Json.string Backup.backup_algorithm);
           ( "auth_data",
             Backup.auth_data_to_json
               {
                 Backup.public_key = Backup.Decryption_key.public other_backup;
                 signatures = [];
               } );
           ("count", Jsont.Json.int 0);
           ("etag", Jsont.Json.string "");
         ])
  in
  let old_backup = Backup.Decryption_key.generate ~random in
  let room_gets = ref 0 in
  let log, fetch =
    mock (fun req ->
        let meth = Http.Method.to_string req.meth in
        let url = Fetch.Middleware.Url.to_string req.url in
        if path_has "/keys/query" url then Fetch_mock.respond query req
        else if path_has "/account_data/m.secret_storage.default_key" url then
          Fetch_mock.respond (Printf.sprintf {|{"key":"%s"}|} key_id) req
        else if path_has ("/account_data/m.secret_storage.key." ^ key_id) url
        then
          Fetch_mock.respond
            (encoded_string Matrix_proto.Json.Codec.json
               (encoded Secret_storage.Key_description.jsont description))
            req
        else if path_has "/account_data/m.cross_signing.master" url then
          Fetch_mock.respond
            (encoded_string Matrix_proto.Json.Codec.json
               (secret_body Secret_storage.secret_cross_signing_master
                  (seed (Option.get (Cross_signing.master_secret identity)))))
            req
        else if path_has "/account_data/m.cross_signing.self_signing" url then
          Fetch_mock.respond
            (encoded_string Matrix_proto.Json.Codec.json
               (secret_body Secret_storage.secret_cross_signing_self_signing
                  (seed
                     (Option.get (Cross_signing.self_signing_secret identity)))))
            req
        else if path_has "/account_data/m.cross_signing.user_signing" url then
          Fetch_mock.respond
            (encoded_string Matrix_proto.Json.Codec.json
               (secret_body Secret_storage.secret_cross_signing_user_signing
                  (seed
                     (Option.get (Cross_signing.user_signing_secret identity)))))
            req
        else if path_has "/account_data/m.megolm_backup.v1" url then
          Fetch_mock.respond
            (encoded_string Matrix_proto.Json.Codec.json
               (secret_body Secret_storage.secret_megolm_backup_v1
                  (Backup.Decryption_key.to_base64 old_backup)))
            req
        else if path_has "/room_keys/version" url && meth = "GET" then begin
          let n = !room_gets in
          incr room_gets;
          if n < 2 then Fetch_mock.respond old_version req
          else Fetch_mock.respond ~status:404 "absent" req
        end
        else if path_has "/room_keys/version/old-v1" url then
          Fetch_mock.respond "{}" req
        else if path_has "/room_keys/version" url && meth = "POST" then
          Fetch_mock.respond {|{"version":"new-v1"}|} req
        else if meth = "PUT" then Fetch_mock.respond "{}" req
        else Fetch_mock.respond {|{"encrypted":{}}|} req)
  in
  let machine = Encryption.create ~random ~user_id ~device_id () in
  let encryption = Encryption_driver.v machine in
  let result =
    Recovery.recover_and_fix_backup (client_of fetch) ~encryption
      ~credential:(Secret_storage.Recovery_key.encode ssss_key)
  in
  (match result with
  | Ok _ -> ()
  | Error error -> Alcotest.failf "repair failed: %a" Error.pp error);
  Alcotest.(check (option string))
    "fresh backup is local" (Some "new-v1")
    (Encryption.backup_version machine);
  Alcotest.(check bool)
    "repair generated a decryption key" true
    (Encryption.backup_decryption_enabled machine);
  let calls = requests log in
  let compact = List.map (fun request -> (request.meth, request.url)) calls in
  let expected_prefixes =
    [
      ("GET", "/account_data/m.secret_storage.default_key");
      ("GET", "/account_data/m.secret_storage.key.repair-key");
      ("GET", "/account_data/m.cross_signing.master");
      ("GET", "/account_data/m.cross_signing.self_signing");
      ("GET", "/account_data/m.cross_signing.user_signing");
      ("POST", "/keys/query");
      ("GET", "/account_data/m.megolm_backup.v1");
      ("GET", "/room_keys/version");
      ("GET", "/room_keys/version");
      ("DELETE", "/room_keys/version/old-v1");
      ("GET", "/room_keys/version");
      ("GET", "/room_keys/version");
      ("PUT", "/account_data/m.key_backup");
      ("PUT", "/account_data/m.org.matrix.custom.backup_disabled");
      ("POST", "/room_keys/version");
    ]
  in
  List.iteri
    (fun index (meth, suffix) ->
      let actual_meth, actual_url = List.nth compact index in
      Alcotest.(check string)
        (Printf.sprintf "repair method %d" index)
        meth actual_meth;
      Alcotest.(check bool)
        (Printf.sprintf "repair path %d" index)
        true
        (path_contains suffix actual_url))
    expected_prefixes;
  let version_body = Option.get (List.nth calls 14).body in
  check_backup_signatures ~identity ~machine (captured_auth_data version_body);
  let rec drop n values =
    if n = 0 then values
    else match values with [] -> [] | _ :: rest -> drop (n - 1) rest
  in
  let exports = drop 15 compact in
  Alcotest.(check int)
    "four secrets exported into existing store" 8 (List.length exports);
  Alcotest.(check bool)
    "exports never replace key description/default" true
    (List.for_all
       (fun (_, url) ->
         (not (path_contains "/account_data/m.secret_storage.key." url))
         && not (path_has "/account_data/m.secret_storage.default_key" url))
       exports)

let test_recover_and_fix_backup_mismatch_zero_io () =
  Eio_mock.Backend.run @@ fun () ->
  let bob =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@bob:example.org")
  in
  let device_id =
    Result.get_ok (Matrix_proto.Id.Device_id.of_string "DEVICE")
  in
  let machine =
    Encryption.create
      ~random:(Matrix_client.Random.of_env mock_env)
      ~user_id:bob ~device_id ()
  in
  let log, fetch = mock (fun req -> Fetch_mock.respond "{}" req) in
  let result =
    Recovery.recover_and_fix_backup (client_of fetch)
      ~encryption:(Encryption_driver.v machine)
      ~credential:"unused"
  in
  Alcotest.(check bool)
    "repair user mismatch fails before I/O" true (Result.is_error result);
  Alcotest.(check int)
    "repair user mismatch makes no requests" 0
    (List.length (requests log))

let test_reset_key_mismatch_no_io () =
  Eio_mock.Backend.run @@ fun () ->
  let alice =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:example.org")
  in
  let bob =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@bob:example.org")
  in
  let device_id =
    Result.get_ok (Matrix_proto.Id.Device_id.of_string "DEVICE")
  in
  let identity = Cross_signing.create_private_identity ~user_id:bob in
  let machine =
    Encryption.create
      ~random:(Matrix_client.Random.of_env mock_env)
      ~user_id:alice ~device_id ()
  in
  let log, fetch = mock (fun req -> Fetch_mock.respond "{}" req) in
  let result =
    Recovery.reset_key (client_of fetch)
      ~encryption:(Encryption_driver.v machine)
      ~private_identity:identity ()
  in
  Alcotest.(check bool) "reset mismatch fails" true (Result.is_error result);
  Alcotest.(check int)
    "reset mismatch makes no request" 0
    (List.length (requests log))

let test_reset_key_failure_boundary () =
  Eio_mock.Backend.run @@ fun () ->
  let user_id =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:example.org")
  in
  let device_id =
    Result.get_ok (Matrix_proto.Id.Device_id.of_string "DEVICE")
  in
  let random = Matrix_client.Random.of_env mock_env in
  let identity = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys ~random identity;
  let machine = Encryption.create ~random ~user_id ~device_id () in
  let log, fetch =
    mock (fun req ->
        let meth = Http.Method.to_string req.meth in
        let url = Fetch.Middleware.Url.to_string req.url in
        if
          String.equal meth "PUT"
          && path_contains "/account_data/m.secret_storage.key." url
        then Fetch_mock.respond ~status:500 {|{"error":"reset stopped"}|} req
        else Fetch_mock.respond "{}" req)
  in
  let result =
    Recovery.reset_key (client_of fetch)
      ~encryption:(Encryption_driver.v machine)
      ~private_identity:identity ()
  in
  Alcotest.(check bool)
    "reset write failure is returned" true (Result.is_error result);
  Alcotest.(check int)
    "reset stops at first failed write" 1
    (List.length (requests log))

let test_check_state_short_circuits () =
  Eio_mock.Backend.run @@ fun () ->
  let user_id =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:example.org")
  in
  let device_id =
    Result.get_ok (Matrix_proto.Id.Device_id.of_string "DEVICE")
  in
  let random = Matrix_client.Random.of_env mock_env in
  let complete_identity = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys ~random complete_identity;
  let incomplete_identity = Cross_signing.create_private_identity ~user_id in
  let machine = Encryption.create ~random ~user_id ~device_id () in
  let check_state ~default ~stable ~unstable ~machine ~identity =
    let log, fetch =
      mock (fun req ->
          let url = Fetch.Middleware.Url.to_string req.url in
          if path_has "/account_data/m.secret_storage.default_key" url then
            if String.equal default "__absent__" then
              Fetch_mock.respond ~status:404 "absent" req
            else Fetch_mock.respond default req
          else if path_has "/account_data/m.key_backup" url then
            if String.equal stable "__absent__" then
              Fetch_mock.respond ~status:404 "absent" req
            else Fetch_mock.respond stable req
          else if
            path_has "/account_data/m.org.matrix.custom.backup_disabled" url
          then
            if String.equal unstable "__absent__" then
              Fetch_mock.respond ~status:404 "absent" req
            else Fetch_mock.respond unstable req
          else Fetch_mock.respond "{}" req)
    in
    let result =
      Recovery.check_state (client_of fetch)
        ~encryption:(Encryption_driver.v machine)
        ~private_identity:identity
    in
    (result, requests log)
  in
  let result, calls =
    check_state ~default:"__absent__" ~stable:"__absent__"
      ~unstable:"__absent__" ~machine ~identity:complete_identity
  in
  Alcotest.(check bool)
    "missing default disables" true
    (match result with Ok Recovery.Disabled -> true | _ -> false);
  Alcotest.(check int)
    "missing default stops after first GET" 1 (List.length calls);
  let result, calls =
    check_state ~default:{|{"key":42}|} ~stable:"__absent__"
      ~unstable:"__absent__" ~machine ~identity:complete_identity
  in
  Alcotest.(check bool)
    "malformed default disables" true
    (match result with Ok Recovery.Disabled -> true | _ -> false);
  Alcotest.(check int)
    "malformed default stops after first GET" 1 (List.length calls);
  let result, calls =
    check_state ~default:{|{"key":"key-id"}|} ~stable:"__absent__"
      ~unstable:{|{"disabled":true}|} ~machine ~identity:incomplete_identity
  in
  Alcotest.(check bool)
    "incomplete identity is incomplete" true
    (match result with Ok Recovery.Incomplete -> true | _ -> false);
  Alcotest.(check int) "incomplete identity skips markers" 1 (List.length calls);
  let result, calls =
    check_state ~default:{|{"key":"key-id"}|} ~stable:"__absent__"
      ~unstable:{|{"disabled":true}|} ~machine ~identity:complete_identity
  in
  Alcotest.(check bool)
    "unstable disabled marker enables" true
    (match result with Ok Recovery.Enabled -> true | _ -> false);
  Alcotest.(check int)
    "marker fallback fetches both markers" 3 (List.length calls);
  Alcotest.(check bool)
    "marker fallback is stable then unstable" true
    (path_has "/account_data/m.key_backup" (List.nth calls 1).url
    && path_has "/account_data/m.org.matrix.custom.backup_disabled"
         (List.nth calls 2).url);
  let active_machine = Encryption.create ~random ~user_id ~device_id () in
  let backup_key = Backup.Decryption_key.generate ~random in
  Encryption.enable_backup active_machine ~version:"backup-v1"
    ~decryption_key:backup_key
    (Backup.Decryption_key.public backup_key);
  let result, calls =
    check_state ~default:{|{"key":"key-id"}|} ~stable:"__absent__"
      ~unstable:"__absent__" ~machine:active_machine ~identity:complete_identity
  in
  Alcotest.(check bool)
    "active backup enables without markers" true
    (match result with Ok Recovery.Enabled -> true | _ -> false);
  Alcotest.(check int) "active backup skips markers" 1 (List.length calls);
  let bob =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@bob:example.org")
  in
  let mismatch_identity = Cross_signing.create_private_identity ~user_id:bob in
  let result, calls =
    check_state ~default:{|{"key":"key-id"}|} ~stable:"__absent__"
      ~unstable:"__absent__" ~machine ~identity:mismatch_identity
  in
  Alcotest.(check bool) "state mismatch fails" true (Result.is_error result);
  Alcotest.(check int) "state mismatch makes no request" 0 (List.length calls)

let manager_base user_id ?default ?stable ?unstable () =
  let store = Store.memory () in
  Store.set_next_batch store "manager-test";
  Option.iter
    (fun content ->
      Store.set_account_data store Secrets.default_key_event_type content)
    default;
  Option.iter
    (fun content ->
      Store.set_account_data store Recovery.key_backup_event_type content)
    stable;
  Option.iter
    (fun content ->
      Store.set_account_data store Recovery.backup_disabled_event_type content)
    unstable;
  Base_client.of_store store ~user_id ()

let manager_machine random user_id =
  let device_id =
    Result.get_ok (Matrix_proto.Id.Device_id.of_string "DEVICE")
  in
  Encryption_driver.v (Encryption.create ~random ~user_id ~device_id ())

let test_manager_local_projection_and_subscriptions () =
  Eio_mock.Backend.run @@ fun () ->
  let user_id =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:example.org")
  in
  let random = Matrix_client.Random.of_env mock_env in
  let client =
    client_of (snd (mock (fun req -> Fetch_mock.respond "{}" req)))
  in
  let encryption = manager_machine random user_id in
  let base = manager_base user_id () in
  let manager = Recovery.Manager.create client ~encryption ~base in
  Alcotest.(check bool)
    "missing default is disabled" true
    (Recovery.Manager.state manager = Recovery.Disabled);
  let seen = ref [] in
  let subscription =
    Recovery.Manager.subscribe manager (fun state -> seen := state :: !seen)
  in
  let subscriber_armed = ref false in
  let throwing_subscription =
    Recovery.Manager.subscribe manager (fun _ ->
        if !subscriber_armed then failwith "deliberate subscriber failure")
  in
  subscriber_armed := true;
  (* Immediate delivery plus no duplicate publication for the same value. *)
  let same_base = manager_base user_id () in
  ignore (Recovery.Manager.refresh_from_base manager same_base);
  Alcotest.(check int) "immediate callback only" 1 (List.length !seen);
  let present =
    manager_base user_id ~default:(jobj [ ("key", jstr "key-id") ]) ()
  in
  ignore (Recovery.Manager.refresh_from_base manager present);
  Alcotest.(check bool)
    "without identity is incomplete" true
    (Recovery.Manager.state manager = Recovery.Incomplete);
  Alcotest.(check int) "distinct transition callback" 2 (List.length !seen);
  Recovery.Manager.unsubscribe manager throwing_subscription;
  let complete = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys ~random complete;
  ignore (Recovery.Manager.set_private_identity manager (Some complete));
  Alcotest.(check bool)
    "complete identity still needs backup policy" true
    (Recovery.Manager.state manager = Recovery.Incomplete);
  let stable_disabled =
    manager_base user_id
      ~default:(jobj [ ("key", jstr "key-id") ])
      ~stable:(jobj [ ("enabled", Jsont.Json.bool false) ])
      ()
  in
  ignore (Recovery.Manager.refresh_from_base manager stable_disabled);
  Alcotest.(check bool)
    "stable disabled marker enables complete identity" true
    (Recovery.Manager.state manager = Recovery.Enabled);
  let malformed =
    manager_base user_id
      ~default:(jobj [ ("key", jstr "key-id") ])
      ~stable:(jobj [ ("enabled", jstr "bad") ])
      ~unstable:(jobj [ ("disabled", Jsont.Json.bool true) ])
      ()
  in
  ignore (Recovery.Manager.refresh_from_base manager malformed);
  Alcotest.(check bool)
    "malformed stable does not fall through" true
    (Recovery.Manager.state manager = Recovery.Incomplete);
  Recovery.Manager.unsubscribe manager subscription;
  ignore (Recovery.Manager.refresh_from_base manager (manager_base user_id ()));
  Alcotest.(check int) "unsubscribe stops callbacks" 4 (List.length !seen);
  let bob =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@bob:example.org")
  in
  let mismatched_identity =
    Cross_signing.create_private_identity ~user_id:bob
  in
  Cross_signing.generate_private_keys ~random mismatched_identity;
  Alcotest.check_raises "mismatching identity update fails"
    (Invalid_argument
       "Matrix_client.Recovery.Manager.set_private_identity: identity belongs \
        to a different user") (fun () ->
      ignore
        (Recovery.Manager.set_private_identity manager
           (Some mismatched_identity)));
  Alcotest.check_raises "mismatching base update fails"
    (Invalid_argument
       "Matrix_client.Recovery.Manager.refresh_from_base: base belongs to a \
        different user") (fun () ->
      ignore (Recovery.Manager.refresh_from_base manager (manager_base bob ())));
  Alcotest.check_raises "mismatching base fails before I/O"
    (Invalid_argument
       "Matrix_client.Recovery.Manager.create: client, base, encryption, and \
        identity belong to different users") (fun () ->
      ignore
        (Recovery.Manager.create client ~encryption ~base:(manager_base bob ())))

let test_manager_remote_without_identity_and_error () =
  Eio_mock.Backend.run @@ fun () ->
  let user_id =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:example.org")
  in
  let random = Matrix_client.Random.of_env mock_env in
  let default_body = {|{"key":"key-id"}|} in
  let calls = ref 0 in
  let _, fetch =
    mock (fun req ->
        incr calls;
        let url = Fetch.Middleware.Url.to_string req.url in
        if path_has "/account_data/m.secret_storage.default_key" url then
          Fetch_mock.respond default_body req
        else Fetch_mock.respond ~status:500 "broken" req)
  in
  let client = client_of fetch in
  let encryption = manager_machine random user_id in
  let manager =
    Recovery.Manager.create client ~encryption ~base:(manager_base user_id ())
  in
  (match Recovery.Manager.refresh manager with
  | Ok Recovery.Incomplete -> ()
  | Ok _ -> Alcotest.fail "unexpected remote state"
  | Error error ->
      Alcotest.failf "remote default failed: %s" (Error.to_string error));
  Alcotest.(check int) "identity-less refresh only gets default" 1 !calls;
  let complete = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys ~random complete;
  ignore (Recovery.Manager.set_private_identity manager (Some complete));
  Alcotest.(check bool)
    "identity update retains confirmed remote SSSS" true
    (Recovery.Manager.state manager = Recovery.Incomplete);
  let _, broken_fetch =
    mock (fun req -> Fetch_mock.respond ~status:500 "broken" req)
  in
  let broken =
    Recovery.Manager.create (client_of broken_fetch) ~encryption
      ~base:(manager_base user_id ())
  in
  (match Recovery.Manager.refresh broken with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected refresh failure");
  Alcotest.(check bool)
    "failed refresh keeps confirmed state" true
    (Recovery.Manager.state broken = Recovery.Disabled)

let test_manager_reset_identity_boundaries () =
  Eio_mock.Backend.run @@ fun () ->
  let user_id =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:example.org")
  in
  let device_id =
    Result.get_ok (Matrix_proto.Id.Device_id.of_string "DEVICE")
  in
  let random = Matrix_client.Random.of_env mock_env in
  let old_identity = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys ~random old_identity;
  let old_master = Option.get (Cross_signing.master_public old_identity) in
  let failed_log, failed_fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if path_has "/room_keys/version" url then
          Fetch_mock.respond ~status:500 "deletion failed" req
        else Fetch_mock.respond "{}" req)
  in
  let failed_client = client_of failed_fetch in
  let failed_encryption =
    Encryption_driver.v (Encryption.create ~random ~user_id ~device_id ())
  in
  let failed_manager =
    Recovery.Manager.create failed_client ~encryption:failed_encryption
      ~private_identity:old_identity ~base:(manager_base user_id ())
  in
  (match
     Recovery.Manager.reset_identity failed_manager
       ~auth_callback:(fun _ -> None)
       ()
   with
  | Uiaa.Uiaa_error _ -> ()
  | Uiaa.Uiaa_success _ | Uiaa.Uiaa_auth_required _ ->
      Alcotest.fail "backup deletion unexpectedly succeeded");
  Alcotest.(check bool)
    "deletion failure retains old identity" true
    (match Recovery.Manager.private_identity failed_manager with
    | Some identity ->
        Option.exists
          (Crypto_key.Ed25519.Public.equal old_master)
          (Cross_signing.master_public identity)
    | None -> false);
  Alcotest.(check int)
    "deletion failure stops before identity upload" 2
    (List.length (requests failed_log));

  let post_bodies = ref [] in
  let attempts = ref 0 in
  let signature_uploads = ref 0 in
  let device_uploads = ref 0 in
  let log, fetch =
    mock (fun req ->
        let meth = Http.Method.to_string req.meth in
        let url = Fetch.Middleware.Url.to_string req.url in
        if meth = "GET" && path_has "/room_keys/version" url then
          Fetch_mock.respond ~status:404 "absent" req
        else if meth = "POST" && path_has "/keys/upload" url then begin
          incr device_uploads;
          let body = Option.value ~default:"" (request_body req) in
          Alcotest.(check bool)
            "replacement device upload carries device keys" true
            (path_contains "\"device_keys\":" body);
          Alcotest.(check bool)
            "replacement device upload carries one-time keys" true
            (path_contains "\"one_time_keys\":" body);
          Alcotest.(check bool)
            "replacement device upload carries fallback keys" true
            (path_contains "\"fallback_keys\":" body);
          Fetch_mock.respond
            {|{"one_time_key_counts":{"signed_curve25519":10}}|} req
        end
        else if meth = "POST" && path_has "/keys/signatures/upload" url then begin
          incr signature_uploads;
          Fetch_mock.respond {|{"failures":{}}|} req
        end
        else if meth = "POST" && path_has "/keys/device_signing/upload" url then begin
          incr attempts;
          let body = Option.value ~default:"" (request_body req) in
          post_bodies := body :: !post_bodies;
          if String.contains body '"' && path_contains "\"auth\":" body then
            Fetch_mock.respond "{}" req
          else
            Fetch_mock.respond ~status:401
              {|{"flows":[{"stages":["m.login.dummy"]}],"session":"reset-session"}|}
              req
        end
        else if
          meth = "GET"
          && path_has "/account_data/m.secret_storage.default_key" url
        then Fetch_mock.respond ~status:404 "absent" req
        else if meth = "POST" && path_has "/room_keys/version" url then
          Fetch_mock.respond
            {|{"version":"replacement-backup","algorithm":"m.megolm_backup.v1.curve25519-aes-sha2","auth_data":{},"count":0,"etag":""}|}
            req
        else Fetch_mock.respond "{}" req)
  in
  let client = client_of fetch in
  let encryption =
    let machine = Encryption.create ~random ~user_id ~device_id () in
    let backup_key = Backup.Decryption_key.generate ~random in
    Encryption.enable_backup machine ~version:"old-backup"
      ~decryption_key:backup_key
      (Backup.Decryption_key.public backup_key);
    Encryption_driver.v machine
  in
  let manager =
    Recovery.Manager.create client ~encryption ~private_identity:old_identity
      ~base:(manager_base user_id ())
  in
  let first =
    Recovery.Manager.reset_identity manager ~auth_callback:(fun _ -> None) ()
  in
  (match first with
  | Uiaa.Uiaa_auth_required _ -> ()
  | Uiaa.Uiaa_success _ | Uiaa.Uiaa_error _ ->
      Alcotest.fail "first reset did not return the UIAA challenge");
  let second =
    Recovery.Manager.reset_identity manager
      ~auth_callback:(fun challenge ->
        Some (Uiaa.dummy_auth ?session:challenge.session ()))
      ()
  in
  (match second with
  | Uiaa.Uiaa_success identity ->
      Alcotest.(check bool)
        "successful reset installs complete identity" true
        (Option.is_some (Cross_signing.master_public identity))
  | Uiaa.Uiaa_auth_required _ -> Alcotest.fail "reset still needs UIAA"
  | Uiaa.Uiaa_error error -> Alcotest.failf "reset failed: %a" Error.pp error);
  Alcotest.(check int) "deletion then upload requests" 3 !attempts;
  Alcotest.(check int) "replacement device keys upload once" 1 !device_uploads;
  Alcotest.(check int)
    "replacement signs the current device once" 1 !signature_uploads;
  let bodies =
    List.rev !post_bodies
    |> List.filter (fun body -> not (path_contains "\"auth\":" body))
  in
  Alcotest.(check bool)
    "UIAA continuation reuses the upload body" true
    (match bodies with first :: second :: _ -> first = second | _ -> false);
  Alcotest.(check bool)
    "reset writes the disabled SSSS marker first" true
    (List.exists
       (fun request ->
         request.meth = "PUT"
         && path_has "/account_data/m.secret_storage.default_key" request.url)
       (requests log));
  let paths =
    requests log
    |> List.filter_map (fun request ->
        if request.meth = "POST" && path_has "/keys/upload" request.url then
          Some "device"
        else if
          request.meth = "POST"
          && path_has "/keys/device_signing/upload" request.url
        then Some "signing"
        else if
          request.meth = "POST"
          && path_has "/keys/signatures/upload" request.url
        then Some "signature"
        else None)
  in
  Alcotest.(check (list string))
    "device keys precede cross-signing"
    [ "device"; "signing"; "signing"; "signing"; "signature" ]
    paths;
  Alcotest.(check (option string))
    "active backup is recreated after identity reset"
    (Some "replacement-backup")
    (Encryption.backup_version (Encryption_driver.machine encryption));

  (* A machine restored from a snapshot may already have published its device
     keys. Resetting its cross-signing identity must not repeat that upload. *)
  let prepublished_uploads = ref 0 in
  let prepublished_log, prepublished_fetch =
    mock (fun req ->
        let meth = Http.Method.to_string req.meth in
        let url = Fetch.Middleware.Url.to_string req.url in
        if meth = "GET" && path_has "/room_keys/version" url then
          Fetch_mock.respond ~status:404 "absent" req
        else if meth = "POST" && path_has "/keys/upload" url then begin
          incr prepublished_uploads;
          Fetch_mock.respond
            {|{"one_time_key_counts":{"signed_curve25519":10}}|} req
        end
        else if meth = "POST" && path_has "/keys/device_signing/upload" url then
          Fetch_mock.respond "{}" req
        else if meth = "POST" && path_has "/keys/signatures/upload" url then
          Fetch_mock.respond {|{"failures":{}}|} req
        else if
          meth = "GET"
          && path_has "/account_data/m.secret_storage.default_key" url
        then Fetch_mock.respond ~status:404 "absent" req
        else Fetch_mock.respond "{}" req)
  in
  let prepublished_machine = Encryption.create ~random ~user_id ~device_id () in
  let prepublished_request =
    List.find_opt
      (function
        | Encryption.Keys_upload { device_keys = Some _; _ } -> true
        | _ -> false)
      (Encryption.outgoing_requests prepublished_machine)
    |> Option.get
  in
  Encryption.mark_sent prepublished_machine prepublished_request;
  let prepublished_manager =
    Recovery.Manager.create
      (client_of prepublished_fetch)
      ~encryption:(Encryption_driver.v prepublished_machine)
      ~private_identity:old_identity ~base:(manager_base user_id ())
  in
  (match
     Recovery.Manager.reset_identity prepublished_manager
       ~auth_callback:(fun _ -> None)
       ()
   with
  | Uiaa.Uiaa_success _ -> ()
  | Uiaa.Uiaa_auth_required _ | Uiaa.Uiaa_error _ ->
      Alcotest.fail "prepublished-device reset failed");
  Alcotest.(check int)
    "prepublished device keys are not uploaded again" 0 !prepublished_uploads;
  ignore prepublished_log

let test_manager_reset_identity_no_auto_backup_and_retry_cancel () =
  Eio_mock.Backend.run @@ fun () ->
  let user_id = Id.User_id.of_string_exn "@alice:example.org" in
  let device_id = Id.Device_id.of_string_exn "DEVICE" in
  let random = Matrix_client.Random.of_env mock_env in
  let old_identity = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys ~random old_identity;
  let run_reset ~base ~machine handler =
    let log, fetch = mock handler in
    let manager =
      Recovery.Manager.create (client_of fetch)
        ~encryption:(Encryption_driver.v machine)
        ~private_identity:old_identity ~base
    in
    (manager, log)
  in
  let no_backup_calls = ref 0 in
  let no_backup_machine = Encryption.create ~random ~user_id ~device_id () in
  let no_backup_base =
    manager_base user_id
      ~default:(jobj [ ("key", jstr "key-id") ])
      ~stable:(jobj [ ("enabled", Jsont.Json.bool false) ])
      ()
  in
  let no_backup_manager, no_backup_log =
    run_reset ~base:no_backup_base ~machine:no_backup_machine (fun req ->
        let meth = Http.Method.to_string req.meth in
        let url = Fetch.Middleware.Url.to_string req.url in
        if meth = "GET" && path_has "/room_keys/version" url then
          Fetch_mock.respond ~status:404 "absent" req
        else if meth = "POST" && path_has "/keys/device_signing/upload" url then (
          incr no_backup_calls;
          Fetch_mock.respond "{}" req)
        else if meth = "POST" && path_has "/keys/signatures/upload" url then
          Fetch_mock.respond {|{"failures":{}}|} req
        else if
          meth = "GET"
          && path_has "/account_data/m.secret_storage.default_key" url
        then Fetch_mock.respond ~status:404 "absent" req
        else Fetch_mock.respond "{}" req)
  in
  (match
     Recovery.Manager.reset_identity no_backup_manager
       ~auth_callback:(fun _ -> None)
       ()
   with
  | Uiaa.Uiaa_success _ -> ()
  | Uiaa.Uiaa_auth_required _ -> Alcotest.fail "unexpected no-backup challenge"
  | Uiaa.Uiaa_error error ->
      Alcotest.failf "no-backup reset failed: %a" Error.pp error);
  Alcotest.(check int) "no-backup reset uploads once" 1 !no_backup_calls;
  Alcotest.(check bool)
    "explicit disabled marker does not recreate backup" true
    (not
       (List.exists
          (fun request ->
            request.meth = "POST" && path_has "/room_keys/version" request.url)
          (requests no_backup_log)));

  let upload_bodies = ref [] in
  let delete_count = ref 0 in
  let remote_present = ref true in
  let remote_gets = ref 0 in
  let backup_create_count = ref 0 in
  let backup_failures = ref 2 in
  let machine = Encryption.create ~random ~user_id ~device_id () in
  let backup_key = Backup.Decryption_key.generate ~random in
  Encryption.enable_backup machine ~version:"old-backup"
    ~decryption_key:backup_key
    (Backup.Decryption_key.public backup_key);
  let base = manager_base user_id () in
  let manager, _log =
    run_reset ~base ~machine (fun req ->
        let meth = Http.Method.to_string req.meth in
        let url = Fetch.Middleware.Url.to_string req.url in
        if meth = "GET" && path_has "/room_keys/version" url then begin
          incr remote_gets;
          if !remote_present && !remote_gets = 1 then
            Fetch_mock.respond
              {|{"version":"old-backup","algorithm":"m.megolm_backup.v1.curve25519-aes-sha2","auth_data":{},"count":0,"etag":""}|}
              req
          else Fetch_mock.respond ~status:404 "absent" req
        end
        else if meth = "DELETE" && path_contains "/room_keys/version" url then begin
          incr delete_count;
          remote_present := false;
          Fetch_mock.respond "{}" req
        end
        else if meth = "POST" && path_has "/keys/device_signing/upload" url then begin
          upload_bodies :=
            Option.value ~default:"" (request_body req) :: !upload_bodies;
          Fetch_mock.respond "{}" req
        end
        else if meth = "POST" && path_has "/keys/signatures/upload" url then
          Fetch_mock.respond {|{"failures":{}}|} req
        else if meth = "POST" && path_has "/room_keys/version" url then begin
          incr backup_create_count;
          if !backup_failures > 0 then begin
            decr backup_failures;
            Fetch_mock.respond ~status:500 "backup create failed" req
          end
          else
            Fetch_mock.respond
              {|{"version":"replacement-backup","algorithm":"m.megolm_backup.v1.curve25519-aes-sha2","auth_data":{},"count":0,"etag":""}|}
              req
        end
        else if
          meth = "GET"
          && path_has "/account_data/m.secret_storage.default_key" url
        then Fetch_mock.respond ~status:404 "absent" req
        else Fetch_mock.respond "{}" req)
  in
  (match
     Recovery.Manager.reset_identity manager ~auth_callback:(fun _ -> None) ()
   with
  | Uiaa.Uiaa_error _ -> ()
  | Uiaa.Uiaa_success _ | Uiaa.Uiaa_auth_required _ ->
      Alcotest.fail "backup creation unexpectedly succeeded");
  let new_identity =
    match Recovery.Manager.private_identity manager with
    | Some identity -> identity
    | None -> Alcotest.fail "replacement identity was not installed"
  in
  Alcotest.(check bool)
    "backup failure keeps replacement identity" true
    (new_identity != old_identity);
  (match
     Recovery.Manager.reset_identity manager ~auth_callback:(fun _ -> None) ()
   with
  | Uiaa.Uiaa_error _ -> ()
  | Uiaa.Uiaa_success _ | Uiaa.Uiaa_auth_required _ ->
      Alcotest.fail "second backup creation unexpectedly succeeded");
  Recovery.Manager.cancel_pending_identity_reset manager;
  Alcotest.(check int) "cancel suppresses backup retry" 2 !backup_create_count;
  Alcotest.(check int)
    "reset issued remote deletion once, not on retry" 1 !delete_count;
  Alcotest.(check bool)
    "cancel keeps confirmed replacement identity" true
    (match Recovery.Manager.private_identity manager with
    | Some identity -> identity == new_identity
    | None -> false);
  Alcotest.(check (option string))
    "failed recreation leaves no local backup" None
    (Encryption.backup_version machine);
  Alcotest.(check bool)
    "retry body was not regenerated before cancellation" true
    (match List.rev !upload_bodies with
    | first :: second :: _ -> first = second
    | _ -> false)

let test_manager_reset_identity_device_upload_retry () =
  Eio_mock.Backend.run @@ fun () ->
  let user_id = Id.User_id.of_string_exn "@alice:example.org" in
  let device_id = Id.Device_id.of_string_exn "DEVICE" in
  let random = Matrix_client.Random.of_env mock_env in
  let old_identity = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys ~random old_identity;
  let upload_attempts = ref 0 in
  let upload_bodies = ref [] in
  let fetch =
    snd
      (mock (fun req ->
           let meth = Http.Method.to_string req.meth in
           let url = Fetch.Middleware.Url.to_string req.url in
           if meth = "GET" && path_has "/room_keys/version" url then
             Fetch_mock.respond ~status:404 "absent" req
           else if meth = "POST" && path_has "/keys/upload" url then begin
             incr upload_attempts;
             upload_bodies :=
               Option.value ~default:"" (request_body req) :: !upload_bodies;
             if !upload_attempts = 1 then
               Fetch_mock.respond ~status:500 "device upload failed" req
             else
               Fetch_mock.respond
                 {|{"one_time_key_counts":{"signed_curve25519":10}}|} req
           end
           else if meth = "POST" && path_has "/keys/device_signing/upload" url
           then Fetch_mock.respond "{}" req
           else if meth = "POST" && path_has "/keys/signatures/upload" url then
             Fetch_mock.respond {|{"failures":{}}|} req
           else if
             meth = "GET"
             && path_has "/account_data/m.secret_storage.default_key" url
           then Fetch_mock.respond ~status:404 "absent" req
           else Fetch_mock.respond "{}" req))
  in
  let machine = Encryption.create ~random ~user_id ~device_id () in
  let manager =
    Recovery.Manager.create (client_of fetch)
      ~encryption:(Encryption_driver.v machine)
      ~private_identity:old_identity ~base:(manager_base user_id ())
  in
  (match
     Recovery.Manager.reset_identity manager ~auth_callback:(fun _ -> None) ()
   with
  | Uiaa.Uiaa_error _ -> ()
  | Uiaa.Uiaa_success _ | Uiaa.Uiaa_auth_required _ ->
      Alcotest.fail "failed device upload unexpectedly reset identity");
  Alcotest.(check bool)
    "failed device upload remains pending" true
    (List.exists
       (function
         | Encryption.Keys_upload { device_keys = Some _; _ } -> true
         | _ -> false)
       (Encryption.outgoing_requests machine));
  (match
     Recovery.Manager.reset_identity manager ~auth_callback:(fun _ -> None) ()
   with
  | Uiaa.Uiaa_success _ -> ()
  | Uiaa.Uiaa_auth_required _ | Uiaa.Uiaa_error _ ->
      Alcotest.fail "device upload retry failed");
  Alcotest.(check int) "device upload retries exactly once" 2 !upload_attempts;
  Alcotest.(check bool)
    "device upload retry reuses complete request" true
    (match List.rev !upload_bodies with
    | first :: second :: _ -> String.equal first second
    | _ -> false)

let test_enable_boundaries () =
  Eio_mock.Backend.run @@ fun () ->
  let check_string = Alcotest.(check string) in
  let check_int = Alcotest.(check int) in
  let check_bool = Alcotest.(check bool) in
  let user_id =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:example.org")
  in
  let device_id =
    Result.get_ok (Matrix_proto.Id.Device_id.of_string "DEVICE")
  in
  let random = Matrix_client.Random.of_env mock_env in
  let identity = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys ~random identity;
  let machine = Encryption.create ~random ~user_id ~device_id () in
  let encryption = Encryption_driver.v machine in
  let log, fetch =
    mock (fun req ->
        let meth = Http.Method.to_string req.meth in
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.equal meth "GET" && path_has "/room_keys/version" url then
          Fetch_mock.respond ~status:404 "absent" req
        else if String.equal meth "POST" && path_has "/room_keys/version" url
        then Fetch_mock.respond {|{"version":"created-v1"}|} req
        else Fetch_mock.respond "{}" req)
  in
  let result =
    Recovery.enable (client_of fetch) ~encryption ~private_identity:identity ()
  in
  let enabled =
    match result with
    | Ok enabled -> enabled
    | Error error -> Alcotest.failf "enable failed: %a" Error.pp error
  in
  check_string "new backup version" "created-v1" enabled.backup_version;
  (match enabled.backup_upload with
  | Recovery.Not_waited -> ()
  | Recovery.Uploaded count ->
      Alcotest.failf "no-wait enable uploaded %d keys" count
  | Recovery.Upload_failed _ ->
      Alcotest.fail "no-wait enable reported upload failure");
  check_string "local backup version" "created-v1"
    (Option.get (Encryption.backup_version machine));
  let calls = requests log in
  check_int "new backup and recovery writes" 14 (List.length calls);
  let expected =
    [
      ("GET", "/room_keys/version");
      ("PUT", "/account_data/m.key_backup");
      ("PUT", "/account_data/m.org.matrix.custom.backup_disabled");
      ("POST", "/room_keys/version");
      ("PUT", "/account_data/m.secret_storage.key.");
      ("GET", "/account_data/m.cross_signing.master");
      ("PUT", "/account_data/m.cross_signing.master");
      ("GET", "/account_data/m.cross_signing.user_signing");
      ("PUT", "/account_data/m.cross_signing.user_signing");
      ("GET", "/account_data/m.cross_signing.self_signing");
      ("PUT", "/account_data/m.cross_signing.self_signing");
      ("GET", "/account_data/m.megolm_backup.v1");
      ("PUT", "/account_data/m.megolm_backup.v1");
      ("PUT", "/account_data/m.secret_storage.default_key");
    ]
  in
  List.iteri
    (fun index (expected_method, suffix) ->
      let request = List.nth calls index in
      check_string
        (Printf.sprintf "enable method %d" index)
        expected_method request.meth;
      Alcotest.(check bool)
        (Printf.sprintf "enable path %d" index)
        true
        (if index = 4 then path_contains suffix request.url
         else path_has suffix request.url))
    expected;
  let version_body = Option.get (List.nth calls 3).body in
  Alcotest.(check bool)
    "version auth data is signed" true
    (String.contains version_body 's');
  check_backup_signatures ~identity ~machine (captured_auth_data version_body);

  let existing_log, existing_fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if path_has "/room_keys/version" url then
          Fetch_mock.respond
            {|{"version":"server-v1","algorithm":"m.megolm_backup.v1.curve25519-aes-sha2","auth_data":{"public_key":"ignored"},"count":0,"etag":""}|}
            req
        else Fetch_mock.respond "{}" req)
  in
  let existing_machine = Encryption.create ~random ~user_id ~device_id () in
  let existing_result =
    Recovery.enable (client_of existing_fetch)
      ~encryption:(Encryption_driver.v existing_machine)
      ~private_identity:identity ()
  in
  Alcotest.(check bool)
    "existing server backup refuses" true
    (Result.is_error existing_result);
  Alcotest.(check int)
    "existing server backup makes no writes" 1
    (List.length (requests existing_log));

  let mismatch_log, mismatch_fetch =
    mock (fun req -> Fetch_mock.respond "{}" req)
  in
  let other_user =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@bob:example.org")
  in
  let other_identity =
    Cross_signing.create_private_identity ~user_id:other_user
  in
  let mismatch_result =
    Recovery.enable (client_of mismatch_fetch) ~encryption
      ~private_identity:other_identity ()
  in
  Alcotest.(check bool)
    "user mismatch fails" true
    (Result.is_error mismatch_result);
  Alcotest.(check int)
    "user mismatch makes no request" 0
    (List.length (requests mismatch_log));

  let reuse_log, reuse_fetch = mock (fun req -> Fetch_mock.respond "{}" req) in
  let reuse_machine = Encryption.create ~random ~user_id ~device_id () in
  let reuse_key = Backup.Decryption_key.generate ~random in
  Encryption.enable_backup reuse_machine ~version:"reused-v1"
    ~decryption_key:reuse_key
    (Backup.Decryption_key.public reuse_key);
  let reuse_encryption = Encryption_driver.v reuse_machine in
  let reused =
    Recovery.enable (client_of reuse_fetch) ~encryption:reuse_encryption
      ~private_identity:identity ()
  in
  let reused =
    match reused with
    | Ok reused -> reused
    | Error error -> Alcotest.failf "reuse enable failed: %a" Error.pp error
  in
  check_string "reused backup version" "reused-v1" reused.backup_version;
  let reuse_calls = requests reuse_log in
  check_int "reuse only store writes" 10 (List.length reuse_calls);
  Alcotest.(check bool)
    "reuse does not create backup" true
    (List.for_all
       (fun request -> not (path_has "/room_keys/version" request.url))
       reuse_calls);
  let reset =
    Recovery.reset_key (client_of reuse_fetch) ~encryption:reuse_encryption
      ~private_identity:identity ()
  in
  let reset =
    match reset with
    | Ok reset -> reset
    | Error error -> Alcotest.failf "reset key failed: %a" Error.pp error
  in
  check_string "reset key uses same backup version" "reused-v1"
    (Option.get (Encryption.backup_version reuse_machine));
  check_bool "reset returns a new store key" true
    (String.length reset.recovery_key > 0);
  let reset_calls = requests reuse_log in
  check_int "reset adds only store writes" 20 (List.length reset_calls);
  Alcotest.(check bool)
    "reset exports backup key" true
    (List.exists
       (fun request ->
         String.equal request.meth "PUT"
         && path_has "/account_data/m.megolm_backup.v1" request.url)
       (List.filteri (fun index _ -> index >= 10) reset_calls))

let json_http s =
  Result.get_ok (Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json s)

let check_bool_http = Alcotest.(check bool)

let marker_equal_http label expected actual =
  match (expected, actual) with
  | Recovery.Absent, Recovery.Absent -> ()
  | Recovery.Valid a, Recovery.Valid b -> check_bool_http label a b
  | Recovery.Malformed _, Recovery.Malformed _ -> ()
  | _ -> Alcotest.failf "%s: unexpected marker" label

let encryption_with_backup version =
  let random = Matrix_client.Random.of_env mock_env in
  let machine =
    Encryption.create ~random
      ~user_id:
        (Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:example.org"))
      ~device_id:(Result.get_ok (Matrix_proto.Id.Device_id.of_string "DEVICE"))
      ()
  in
  let key = Backup.Decryption_key.generate ~random in
  Encryption.enable_backup machine ~version ~decryption_key:key
    (Backup.Decryption_key.public key);
  Encryption_driver.v machine

let backup_version_json version =
  Printf.sprintf
    {|{"version":"%s","algorithm":"%s","auth_data":{},"count":0,"etag":""}|}
    version Backup.backup_algorithm

let test_disable_and_delete_backups () =
  Eio_mock.Backend.run @@ fun () ->
  let sequential_gets = ref 0 in
  let sequential_log, sequential_fetch =
    mock (fun req ->
        let meth = Http.Method.to_string req.meth in
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.equal meth "GET" then begin
          let response =
            match !sequential_gets with
            | 0 -> backup_version_json "v1"
            | 1 -> backup_version_json "v2"
            | _ -> "{\"errcode\":\"M_NOT_FOUND\"}"
          in
          incr sequential_gets;
          if !sequential_gets >= 3 then
            Fetch_mock.respond ~status:404 response req
          else Fetch_mock.respond response req
        end
        else if String.equal meth "DELETE" then Fetch_mock.respond "{}" req
        else Alcotest.failf "unexpected %s %s" meth url)
  in
  let sequential_encryption = encryption_with_backup "local-v1" in
  let result =
    Recovery.disable_and_delete_backups
      (client_of sequential_fetch)
      ~encryption:sequential_encryption
  in
  (match result with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "sequential deletion failed: %a" Error.pp error);
  Alcotest.(check (option string))
    "sequential deletion disables local backup" None
    (Encryption.backup_version
       (Encryption_driver.machine sequential_encryption));
  let sequential_calls = requests sequential_log in
  Alcotest.(check int)
    "two versions plus final GET" 5
    (List.length sequential_calls);
  List.iteri
    (fun index (expected_method, expected_path) ->
      let request = List.nth sequential_calls index in
      Alcotest.(check string)
        (Printf.sprintf "sequential method %d" index)
        expected_method request.meth;
      Alcotest.(check bool)
        (Printf.sprintf "sequential path %d" index)
        true
        (path_has expected_path request.url))
    [
      ("GET", "/room_keys/version");
      ("DELETE", "/room_keys/version/v1");
      ("GET", "/room_keys/version");
      ("DELETE", "/room_keys/version/v2");
      ("GET", "/room_keys/version");
    ];

  let absent_log, absent_fetch =
    mock (fun req ->
        if String.equal (Http.Method.to_string req.meth) "GET" then
          Fetch_mock.respond ~status:404 "absent" req
        else Alcotest.fail "no-local-backup case must not DELETE")
  in
  let absent_encryption =
    let random = Matrix_client.Random.of_env mock_env in
    let user_id =
      Result.get_ok (Matrix_proto.Id.User_id.of_string "@alice:example.org")
    in
    let device_id =
      Result.get_ok (Matrix_proto.Id.Device_id.of_string "DEVICE")
    in
    Encryption_driver.v (Encryption.create ~random ~user_id ~device_id ())
  in
  let result =
    Recovery.disable_and_delete_backups (client_of absent_fetch)
      ~encryption:absent_encryption
  in
  Alcotest.(check bool)
    "no local backup still succeeds" true (Result.is_ok result);
  Alcotest.(check int)
    "no local backup performs one GET" 1
    (List.length (requests absent_log));
  Alcotest.(check (option string))
    "no local backup remains disabled" None
    (Encryption.backup_version (Encryption_driver.machine absent_encryption));

  let mismatch_log, mismatch_fetch =
    mock (fun req -> Fetch_mock.respond "{}" req)
  in
  let bob =
    Result.get_ok (Matrix_proto.Id.User_id.of_string "@bob:example.org")
  in
  let random = Matrix_client.Random.of_env mock_env in
  let mismatch_machine =
    Encryption.create ~random ~user_id:bob
      ~device_id:(Result.get_ok (Matrix_proto.Id.Device_id.of_string "DEVICE"))
      ()
  in
  let mismatch_result =
    Recovery.disable_and_delete_backups (client_of mismatch_fetch)
      ~encryption:(Encryption_driver.v mismatch_machine)
  in
  Alcotest.(check bool)
    "session-machine mismatch fails" true
    (Result.is_error mismatch_result);
  Alcotest.(check int)
    "session-machine mismatch makes no I/O" 0
    (List.length (requests mismatch_log));

  let get_error_log, get_error_fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:500 {|{"errcode":"M_UNKNOWN"}|} req)
  in
  let get_error_encryption = encryption_with_backup "get-v1" in
  let get_error_result =
    Recovery.disable_and_delete_backups
      (client_of get_error_fetch)
      ~encryption:get_error_encryption
  in
  Alcotest.(check bool)
    "non-404 GET error is returned" true
    (Result.is_error get_error_result);
  Alcotest.(check (option string))
    "GET error preserves local backup" (Some "get-v1")
    (Encryption.backup_version (Encryption_driver.machine get_error_encryption));
  Alcotest.(check int)
    "GET error stops before DELETE" 1
    (List.length (requests get_error_log));

  let delete_error_log, delete_error_fetch =
    mock (fun req ->
        if String.equal (Http.Method.to_string req.meth) "GET" then
          Fetch_mock.respond (backup_version_json "delete-v1") req
        else Fetch_mock.respond ~status:500 {|{"errcode":"M_UNKNOWN"}|} req)
  in
  let delete_error_encryption = encryption_with_backup "delete-v1" in
  let delete_error_result =
    Recovery.disable_and_delete_backups
      (client_of delete_error_fetch)
      ~encryption:delete_error_encryption
  in
  Alcotest.(check bool)
    "non-404 DELETE error is returned" true
    (Result.is_error delete_error_result);
  Alcotest.(check (option string))
    "DELETE error preserves local backup" (Some "delete-v1")
    (Encryption.backup_version
       (Encryption_driver.machine delete_error_encryption));
  Alcotest.(check int)
    "DELETE error stops immediately" 2
    (List.length (requests delete_error_log));

  let delete_absent_gets = ref 0 in
  let delete_absent_log, delete_absent_fetch =
    mock (fun req ->
        let meth = Http.Method.to_string req.meth in
        if String.equal meth "GET" then begin
          incr delete_absent_gets;
          if !delete_absent_gets = 1 then
            Fetch_mock.respond (backup_version_json "race-v1") req
          else Fetch_mock.respond ~status:404 "absent" req
        end
        else
          Fetch_mock.respond ~status:404
            {|{"errcode":"M_NOT_FOUND","error":"already gone"}|} req)
  in
  let delete_absent_encryption = encryption_with_backup "race-v1" in
  let delete_absent_result =
    Recovery.disable_and_delete_backups
      (client_of delete_absent_fetch)
      ~encryption:delete_absent_encryption
  in
  Alcotest.(check bool)
    "DELETE not-found is idempotent" true
    (Result.is_ok delete_absent_result);
  Alcotest.(check (option string))
    "DELETE not-found eventually disables local" None
    (Encryption.backup_version
       (Encryption_driver.machine delete_absent_encryption));
  let delete_absent_calls = requests delete_absent_log in
  Alcotest.(check int)
    "DELETE not-found is followed by GET" 3
    (List.length delete_absent_calls);
  Alcotest.(check string)
    "DELETE not-found case starts with GET" "GET"
    (List.hd delete_absent_calls).meth;
  Alcotest.(check string)
    "DELETE not-found case deletes exact version" "DELETE"
    (List.nth delete_absent_calls 1).meth;
  Alcotest.(check bool)
    "DELETE not-found exact URL" true
    (path_has "/room_keys/version/race-v1" (List.nth delete_absent_calls 1).url);
  Alcotest.(check string)
    "DELETE not-found is followed by final GET" "GET"
    (List.nth delete_absent_calls 2).meth

let test_http_helpers () =
  Eio_mock.Backend.run @@ fun () ->
  let log, fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if path_has "/account_data/m.key_backup" url then
          Fetch_mock.respond {|{"enabled":true}|} req
        else if path_has "/account_data/m.org.matrix.custom.backup_disabled" url
        then Fetch_mock.respond ~status:404 "not present" req
        else Fetch_mock.respond "{}" req)
  in
  let markers =
    match Recovery.fetch_markers (client_of fetch) with
    | Ok markers -> markers
    | Error error -> Alcotest.failf "marker fetch failed: %a" Error.pp error
  in
  marker_equal_http "stable fetched" (Recovery.Valid true) markers.stable;
  marker_equal_http "bare 404 is absent" Recovery.Absent markers.unstable;
  let calls = requests log in
  Alcotest.(check int) "both marker GETs" 2 (List.length calls);
  Alcotest.(check string) "stable marker path" "GET" (List.hd calls).meth;
  Alcotest.(check bool)
    "stable marker URL" true
    (path_has "/account_data/m.key_backup" (List.hd calls).url);
  Alcotest.(check bool)
    "unstable marker URL" true
    (path_has "/account_data/m.org.matrix.custom.backup_disabled"
       (List.nth calls 1).url);
  Alcotest.(check (option string)) "GETs have no body" None (List.hd calls).body;

  let malformed_log, malformed_fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if path_has "/account_data/m.key_backup" url then
          Fetch_mock.respond "{broken" req
        else Fetch_mock.respond {|{"disabled":true}|} req)
  in
  let malformed =
    match Recovery.fetch_markers (client_of malformed_fetch) with
    | Ok markers -> markers
    | Error error ->
        Alcotest.failf "malformed marker fetch failed: %a" Error.pp error
  in
  marker_equal_http "malformed stable retained" (Recovery.Malformed "")
    malformed.stable;
  marker_equal_http "unstable follows malformed stable" (Recovery.Valid true)
    malformed.unstable;
  Alcotest.(check int)
    "malformed still fetches both" 2
    (List.length (requests malformed_log));

  let absent_log, absent_fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if path_has "/account_data/m.key_backup" url then
          Fetch_mock.respond ~status:404 {|{"errcode":"M_NOT_FOUND"}|} req
        else Fetch_mock.respond ~status:404 "plain 404" req)
  in
  let absent =
    match Recovery.fetch_markers (client_of absent_fetch) with
    | Ok markers -> markers
    | Error error ->
        Alcotest.failf "absent marker fetch failed: %a" Error.pp error
  in
  marker_equal_http "M_NOT_FOUND is absent" Recovery.Absent absent.stable;
  marker_equal_http "bare 404 remains absent" Recovery.Absent absent.unstable;
  Alcotest.(check int)
    "both absent marker GETs" 2
    (List.length (requests absent_log));

  let writes =
    [
      {
        Recovery.event_type = "m.recovery.first";
        content = json_http {|{"n":1}|};
      };
      {
        Recovery.event_type = "m.recovery.second";
        content = json_http {|{"n":2}|};
      };
    ]
  in
  let write_log, write_fetch = mock (fun req -> Fetch_mock.respond "{}" req) in
  let write_result = Recovery.apply_writes (client_of write_fetch) writes in
  Alcotest.(check bool) "writes succeed" true (Result.is_ok write_result);
  let write_calls = requests write_log in
  Alcotest.(check int) "writes preserve count" 2 (List.length write_calls);
  Alcotest.(check bool)
    "first write path" true
    (path_has "/account_data/m.recovery.first" (List.hd write_calls).url);
  Alcotest.(check string)
    "first write body" {|{"n":1}|}
    (Option.get (List.hd write_calls).body);
  Alcotest.(check bool)
    "second write path" true
    (path_has "/account_data/m.recovery.second" (List.nth write_calls 1).url);
  Alcotest.(check string)
    "second write body" {|{"n":2}|}
    (Option.get (List.nth write_calls 1).body);

  let short_log, short_fetch =
    mock (fun req -> Fetch_mock.respond ~status:500 {|{"error":"stop"}|} req)
  in
  let short_result = Recovery.apply_writes (client_of short_fetch) writes in
  Alcotest.(check bool)
    "first write error returned" true
    (Result.is_error short_result);
  Alcotest.(check int)
    "later writes are not attempted" 1
    (List.length (requests short_log));

  let enabled_log, enabled_fetch =
    mock (fun req -> Fetch_mock.respond "{}" req)
  in
  let enabled_result = Recovery.mark_backup_enabled (client_of enabled_fetch) in
  Alcotest.(check bool)
    "mark enabled succeeds" true
    (Result.is_ok enabled_result);
  let enabled_calls = requests enabled_log in
  Alcotest.(check int) "mark enabled count" 2 (List.length enabled_calls);
  Alcotest.(check bool)
    "mark enabled stable first" true
    (path_has "/account_data/m.key_backup" (List.hd enabled_calls).url);
  Alcotest.(check string)
    "mark enabled stable body" {|{"enabled":true}|}
    (Option.get (List.hd enabled_calls).body);
  Alcotest.(check bool)
    "mark enabled unstable second" true
    (path_has "/account_data/m.org.matrix.custom.backup_disabled"
       (List.nth enabled_calls 1).url);
  let disable_log, disable_fetch =
    mock (fun req -> Fetch_mock.respond "{}" req)
  in
  let disable_result =
    Recovery.disable_account_data ~default_key_id:"key-id"
      (client_of disable_fetch)
  in
  Alcotest.(check bool)
    "disable account-data plan succeeds" true
    (Result.is_ok disable_result);
  let disable_calls = requests disable_log in
  Alcotest.(check int)
    "disable writes its complete plan"
    (4 + List.length Recovery.known_secret_event_types)
    (List.length disable_calls);
  Alcotest.(check bool)
    "disable starts with key description" true
    (path_has "/account_data/m.secret_storage.key.key-id"
       (List.hd disable_calls).url);
  Alcotest.(check bool)
    "disable keeps stable marker order" true
    (path_has "/account_data/m.key_backup" (List.nth disable_calls 2).url);
  ()

let test_disable_lifecycle () =
  Eio_mock.Backend.run @@ fun () ->
  let log, fetch =
    mock (fun req ->
        let meth = Http.Method.to_string req.meth in
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.equal meth "DELETE" && path_has "/room_keys/version/v1" url
        then
          Fetch_mock.respond ~status:404
            {|{"errcode":"M_NOT_FOUND","error":"already gone"}|} req
        else if
          String.equal meth "GET"
          && path_has "/account_data/m.secret_storage.default_key" url
        then Fetch_mock.respond {|{"key":"ssss"}|} req
        else Fetch_mock.respond "{}" req)
  in
  let encryption = encryption_with_backup "v1" in
  let client = client_of fetch in
  (match Recovery.disable client ~encryption with
  | Ok () -> ()
  | Error error -> Alcotest.failf "disable failed: %a" Error.pp error);
  Alcotest.(check (option string))
    "local backup disabled" None
    (Encryption.backup_version (Encryption_driver.machine encryption));
  let calls = requests log in
  Alcotest.(check int)
    "complete disable request count"
    (2 + 4 + List.length Recovery.known_secret_event_types)
    (List.length calls);
  Alcotest.(check string)
    "server backup deleted first" "DELETE" (List.hd calls).meth;
  Alcotest.(check bool)
    "exact local backup version deleted" true
    (path_has "/room_keys/version/v1" (List.hd calls).url);
  Alcotest.(check bool)
    "default key fetched second" true
    (path_has "/account_data/m.secret_storage.default_key"
       (List.nth calls 1).url);
  Alcotest.(check bool)
    "default key description cleared first" true
    (path_has "/account_data/m.secret_storage.key.ssss" (List.nth calls 2).url);

  let failed_log, failed_fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:500
          {|{"errcode":"M_UNKNOWN","error":"stop"}|} req)
  in
  let still_enabled = encryption_with_backup "v2" in
  let failed =
    Recovery.disable (client_of failed_fetch) ~encryption:still_enabled
  in
  Alcotest.(check bool) "delete failure returned" true (Result.is_error failed);
  Alcotest.(check (option string))
    "failed delete keeps local backup" (Some "v2")
    (Encryption.backup_version (Encryption_driver.machine still_enabled));
  Alcotest.(check int)
    "delete failure stops lifecycle" 1
    (List.length (requests failed_log));

  let absent_log, absent_fetch =
    mock (fun req -> Fetch_mock.respond "{}" req)
  in
  let no_backup =
    let random = Matrix_client.Random.of_env mock_env in
    Encryption_driver.v
      (Encryption.create ~random
         ~user_id:
           (Result.get_ok
              (Matrix_proto.Id.User_id.of_string "@alice:example.org"))
         ~device_id:
           (Result.get_ok (Matrix_proto.Id.Device_id.of_string "DEVICE"))
         ())
  in
  (match Recovery.disable (client_of absent_fetch) ~encryption:no_backup with
  | Error (Error.Policy_denied _) -> ()
  | Error error -> Alcotest.failf "wrong no-backup error: %a" Error.pp error
  | Ok () -> Alcotest.fail "disable accepted a missing local backup");
  Alcotest.(check int)
    "no local backup makes no request" 0
    (List.length (requests absent_log))

let json s =
  Result.get_ok (Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json s)

let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)

let marker_equal label expected actual =
  match (expected, actual) with
  | Recovery.Absent, Recovery.Absent -> ()
  | Recovery.Valid a, Recovery.Valid b -> check_bool label a b
  | Recovery.Malformed _, Recovery.Malformed _ -> ()
  | _ -> Alcotest.failf "%s: unexpected marker" label

let test_markers () =
  marker_equal "stable absent" Recovery.Absent (Recovery.key_backup_marker None);
  marker_equal "stable valid" (Recovery.Valid true)
    (Recovery.key_backup_marker (Some (json {|{"enabled":true}|})));
  (match Recovery.key_backup_marker (Some (json {|{"enabled":"bad"}|})) with
  | Recovery.Malformed _ -> ()
  | _ -> Alcotest.fail "malformed stable marker was accepted");
  marker_equal "unstable valid" (Recovery.Valid true)
    (Recovery.backup_disabled_marker (Some (json {|{"disabled":true}|})));

  let base =
    {
      Recovery.secret_storage_enabled = Some true;
      cross_signing_complete = Some true;
      backup_enabled = Some false;
      stable_marker = Recovery.Absent;
      unstable_marker = Recovery.Valid true;
    }
  in
  check_bool "unstable disabled marker enables complete recovery" true
    (Recovery.state base = Recovery.Enabled);
  check_bool "stable marker wins over unstable fallback" true
    (Recovery.state { base with stable_marker = Recovery.Valid true }
    = Recovery.Incomplete);
  check_bool "stable disabled marker makes backup optional" true
    (Recovery.state { base with stable_marker = Recovery.Valid false }
    = Recovery.Enabled);
  check_bool "malformed stable marker does not use unstable fallback" true
    (Recovery.state { base with stable_marker = Recovery.Malformed "bad" }
    = Recovery.Incomplete);
  check_bool "unstable false leaves recovery incomplete" true
    (Recovery.state { base with unstable_marker = Recovery.Valid false }
    = Recovery.Incomplete);
  check_bool "enabled backup does not need a disable marker" true
    (Recovery.state
       {
         base with
         backup_enabled = Some true;
         stable_marker = Recovery.Absent;
         unstable_marker = Recovery.Absent;
       }
    = Recovery.Enabled);
  check_bool "disabled secret storage is disabled" true
    (Recovery.state { base with secret_storage_enabled = Some false }
    = Recovery.Disabled);
  check_bool "missing secret is incomplete" true
    (Recovery.state { base with cross_signing_complete = Some false }
    = Recovery.Incomplete);
  check_bool "missing observations are unknown" true
    (Recovery.state { base with backup_enabled = None } = Recovery.Unknown);
  check_bool "unobserved cross-signing state is unknown" true
    (Recovery.state { base with cross_signing_complete = None }
    = Recovery.Unknown)

let event_types writes = List.map (fun w -> w.Recovery.event_type) writes

let content_string write =
  Result.get_ok
    (Jsont_bytesrw.encode_string ~format:Jsont.Minify
       Matrix_proto.Json.Codec.json write.Recovery.content)

let test_write_plans () =
  let enabled = Recovery.mark_enabled_writes () in
  check_int "enable writes two markers" 2 (List.length enabled);
  check_string "stable marker is first" Recovery.key_backup_event_type
    (List.hd (event_types enabled));
  check_string "unstable marker is second" Recovery.backup_disabled_event_type
    (List.nth (event_types enabled) 1);
  check_string "stable enable body" {|{"enabled":true}|}
    (content_string (List.hd enabled));
  check_string "unstable enable body" {|{"disabled":false}|}
    (content_string (List.nth enabled 1));

  let disabled = Recovery.disable_writes ~default_key_id:"key-id" () in
  check_string "disable clears key description first"
    (Matrix_client.Secrets.key_event_type ~key_id:"key-id")
    (List.hd (event_types disabled));
  check_string "disable clears default key second"
    Matrix_client.Secrets.default_key_event_type
    (List.nth (event_types disabled) 1);
  check_string "disable writes stable marker third"
    Recovery.key_backup_event_type
    (List.nth (event_types disabled) 2);
  check_string "disable writes unstable marker fourth"
    Recovery.backup_disabled_event_type
    (List.nth (event_types disabled) 3);
  check_bool "disable clears all known secrets" true
    (List.length disabled = 4 + List.length Recovery.known_secret_event_types);
  Alcotest.(check (list string))
    "known secrets retain Rust order"
    [
      Matrix_client.Secret_storage.secret_cross_signing_master;
      Matrix_client.Secret_storage.secret_cross_signing_user_signing;
      Matrix_client.Secret_storage.secret_cross_signing_self_signing;
      Matrix_client.Secret_storage.secret_megolm_backup_v1;
    ]
    (List.filter
       (fun event_type -> List.mem event_type Recovery.known_secret_event_types)
       (event_types disabled));
  List.iter
    (fun write ->
      if List.mem write.Recovery.event_type Recovery.known_secret_event_types
      then
        check_string "known secret is valid and empty" {|{"encrypted":{}}|}
          (content_string write))
    disabled;
  let without_key = Recovery.disable_writes () in
  check_string "absent key id starts at the default marker"
    Matrix_client.Secrets.default_key_event_type
    (List.hd (event_types without_key));
  check_bool "unrelated account data is not targeted" true
    (not
       (List.exists (fun t -> String.equal t "m.direct") (event_types disabled)))

let () =
  Alcotest.run "recovery"
    [
      ( "account data",
        [
          Alcotest.test_case "markers and state" `Quick test_markers;
          Alcotest.test_case "write plans" `Quick test_write_plans;
          Alcotest.test_case "HTTP fetch and writes" `Quick test_http_helpers;
          Alcotest.test_case "disable lifecycle" `Quick test_disable_lifecycle;
          Alcotest.test_case "aggressive backup deletion" `Quick
            test_disable_and_delete_backups;
          Alcotest.test_case "recover order and failures" `Quick
            test_recover_order_and_failures;
          Alcotest.test_case "recover and fix backup order" `Quick
            test_recover_and_fix_backup_order;
          Alcotest.test_case "recover and fix user boundary" `Quick
            test_recover_and_fix_backup_mismatch_zero_io;
          Alcotest.test_case "reset mismatch makes no I/O" `Quick
            test_reset_key_mismatch_no_io;
          Alcotest.test_case "reset failure boundary" `Quick
            test_reset_key_failure_boundary;
          Alcotest.test_case "check state short circuits" `Quick
            test_check_state_short_circuits;
          Alcotest.test_case "manager local projection and subscriptions" `Quick
            test_manager_local_projection_and_subscriptions;
          Alcotest.test_case "manager remote refresh and failures" `Quick
            test_manager_remote_without_identity_and_error;
          Alcotest.test_case "manager identity reset boundaries" `Quick
            test_manager_reset_identity_boundaries;
          Alcotest.test_case "manager reset retry and cancellation" `Quick
            test_manager_reset_identity_no_auto_backup_and_retry_cancel;
          Alcotest.test_case "manager device upload retry" `Quick
            test_manager_reset_identity_device_upload_retry;
          Alcotest.test_case "enable boundaries" `Quick test_enable_boundaries;
        ] );
    ]
