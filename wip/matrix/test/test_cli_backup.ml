module M = Matrix_client
module Id = Matrix_proto.Id
module B = M.Backup
module Ck = M.Crypto_key

let ok = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%a" M.Error.pp error

let crypto_ok = function
  | Ok value -> value
  | Error (`Msg message) -> Alcotest.fail message

let random byte =
  M.Random.of_source (Eio.Flow.string_source (String.make 100_000 byte))

let key byte = B.Decryption_key.generate ~random:(random byte)
let user = Id.User_id.of_string_exn "@backup:example.org"
let device = Id.Device_id.of_string_exn "BACKUP"
let room = Id.Room_id.of_string_exn "!backup:example.org"
let json codec value = Result.get_ok (Jsont_bytesrw.encode_string codec value)
let jstr = Jsont.Json.string

let jobj members =
  Jsont.Json.object'
    (List.map (fun (name, value) -> ((name, Jsont.Meta.none), value)) members)

let binary = Unix.realpath (Sys.getenv "OMATRIX_EXE")

let contains text fragment =
  let rec loop index =
    index + String.length fragment <= String.length text
    && (String.sub text index (String.length fragment) = fragment
       || loop (index + 1))
  in
  loop 0

let with_fixture f =
  Eio_main.run @@ fun env ->
  let directory = Filename.temp_dir "matrix-cli-backup-" "" in
  let fs = Eio.Stdenv.fs env in
  let variables =
    [
      "XDG_DATA_HOME";
      "XDG_CONFIG_HOME";
      "XDG_CACHE_HOME";
      "XDG_STATE_HOME";
      "XDG_RUNTIME_DIR";
    ]
  in
  let previous = List.map (fun name -> (name, Sys.getenv_opt name)) variables in
  List.iter (fun name -> Unix.putenv name directory) variables;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (name, value) -> Unix.putenv name (Option.value value ~default:""))
        previous;
      Eio.Path.rmtree Eio.Path.(fs / directory))
    (fun () ->
      let xdg = Xdge.create fs "matrix" in
      let store = M.Crypto_store.create ~xdg ~profile:"default" in
      let machine =
        M.Encryption.create ~random:(random '\x12') ~user_id:user
          ~device_id:device ()
      in
      let old_key = key '\x23' in
      M.Encryption.enable_backup machine ~version:"old" ~decryption_key:old_key
        (B.Decryption_key.public old_key);
      ok (M.Crypto_store.save store (M.Encryption.snapshot machine));
      let profile = M.Profile_store.create ~xdg ~profile:"default" in
      f env directory xdg profile)

let snapshot profile =
  let dir = M.Profile_store.dir profile in
  Eio.Path.read_dir dir
  |> List.filter (fun name -> name <> ".profile.lock")
  |> List.map (fun name -> (name, Eio.Path.load Eio.Path.(dir / name)))

let run ?(command = [ "backup"; "restore" ]) env directory args =
  let output = Buffer.create 256 in
  let status =
    Eio.Switch.run @@ fun sw ->
    let child_env =
      Unix.environment () |> Array.to_list
      |> List.filter (fun entry ->
          not (String.starts_with ~prefix:"XDG_DATA_HOME=" entry))
      |> fun entries -> Array.of_list (("XDG_DATA_HOME=" ^ directory) :: entries)
    in
    let process =
      Eio.Process.spawn ~sw
        (Eio.Stdenv.process_mgr env)
        ~env:child_env
        ~stdin:(Eio.Flow.string_source "")
        ~stdout:(Eio.Flow.buffer_sink output)
        ~stderr:(Eio.Flow.buffer_sink output)
        ((binary :: command) @ args)
    in
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
        Eio.Process.await process)
  in
  (status, Buffer.contents output)

let with_server env handler f =
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen ~sw ~backlog:16 (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> Alcotest.fail "TCP expected"
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Net.accept_fork ~sw socket ~on_error:raise (fun flow _ ->
            let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
            let request = Eio.Buf_read.line reader in
            let rec headers () =
              if Eio.Buf_read.line reader <> "" then headers ()
            in
            headers ();
            let response = handler request in
            Eio.Flow.copy_string
              (Printf.sprintf
                 "HTTP/1.1 200 OK\r\n\
                  Content-Type: application/json\r\n\
                  Content-Length: %d\r\n\
                  Connection: close\r\n\
                  \r\n\
                  %s"
                 (String.length response) response)
              flow)
      done;
      `Stop_daemon);
  f (Uriz.of_string_exn (Printf.sprintf "http://127.0.0.1:%d" port))

let save_session profile homeserver =
  let time = Ptime.epoch in
  ok
    (M.Profile_store.save_session profile
       M.Session.Session_file.
         {
           server = { homeserver; user_id = user };
           auth =
             {
               access_token = "fixture-token";
               device_id = device;
               refresh_token = None;
               access_token_expires_at = None;
               method_ = Matrix;
             };
           sync = { next_batch = None; filter_id = None };
           metadata =
             { created_at = time; last_used_at = time; client_name = "test" };
         })

let version_json ?(algorithm = B.backup_algorithm) auth_data =
  json Matrix_proto.Json.Codec.json
    (jobj
       [
         ("version", jstr "current");
         ("algorithm", jstr algorithm);
         ("auth_data", auth_data);
       ])

let auth_data key =
  B.auth_data_to_json
    { public_key = B.Decryption_key.public key; signatures = [] }

let key_file env directory contents =
  let path = Filename.concat directory "recovery.key" in
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(Eio.Stdenv.fs env / path)
    contents;
  path

let reject_metadata name make_version =
  with_fixture @@ fun env directory _xdg profile ->
  let recovery = B.Recovery_key.encode (key '\x34') in
  let file = key_file env directory (recovery ^ "\r\n") in
  let requests = ref [] in
  with_server env (fun request ->
      requests := request :: !requests;
      if request = "GET /_matrix/client/v3/room_keys/version HTTP/1.1" then
        make_version (key '\x34')
      else Alcotest.fail "rejected backup must not be downloaded or uploaded")
  @@ fun homeserver ->
  save_session profile homeserver;
  let before = snapshot profile in
  let status, output = run env directory [ "--recovery-key-file"; file ] in
  Alcotest.(check bool) name true (status <> `Exited 0);
  Alcotest.(check bool)
    "key stays out of diagnostics" false (contains output recovery);
  Alcotest.(check (list (pair string string)))
    "profile unchanged" before (snapshot profile);
  Alcotest.(check int) "only metadata requested" 1 (List.length !requests)

let test_invalid_input () =
  List.iter
    (fun (name, contents) ->
      with_fixture @@ fun env directory _xdg profile ->
      let file =
        match contents with
        | None -> Filename.concat directory "missing"
        | Some value -> key_file env directory value
      in
      let before = snapshot profile in
      let status, output = run env directory [ "--recovery-key-file"; file ] in
      Alcotest.(check bool) name true (status <> `Exited 0);
      Alcotest.(check bool)
        "safe error" false
        (contains output "SENSITIVE_FIXTURE");
      Alcotest.(check (list (pair string string)))
        "profile unchanged" before (snapshot profile))
    [
      ("empty", Some " \r\n");
      ("unreadable", None);
      ("malformed", Some "SENSITIVE_FIXTURE");
    ]

let test_positional () =
  with_fixture @@ fun env directory _xdg profile ->
  let secret = B.Recovery_key.encode (key '\x45') in
  let file = key_file env directory secret in
  List.iter
    (fun args ->
      let before = snapshot profile in
      let status, output = run env directory args in
      Alcotest.(check bool) "positional rejected" true (status <> `Exited 0);
      Alcotest.(check bool)
        "positional secret not printed" false (contains output secret);
      Alcotest.(check (list (pair string string)))
        "profile unchanged" before (snapshot profile))
    [ [ secret ]; [ "--recovery-key-file"; file; secret ] ]

let test_restore populated =
  with_fixture @@ fun env directory xdg profile ->
  let recovery_key = key '\x56' in
  let file =
    key_file env directory (B.Recovery_key.encode recovery_key ^ "\r\n")
  in
  let outbound =
    M.Megolm.Outbound.create ~random:(random '\x67') ~room_id:room ()
  in
  let _, sender = Ck.Curve25519.generate ~random:(random '\x78') () in
  let session_data =
    crypto_ok
      (B.encrypt_room_key ~random:(random '\x89')
         (B.Decryption_key.public recovery_key)
         ~session_key:(M.Megolm.Outbound.exported_session_key outbound)
         ~sender_key:(Ck.Curve25519.Public.to_base64 sender))
  in
  let backup_data : B.key_backup_data =
    {
      first_message_index = 0;
      forwarded_count = 0;
      is_verified = false;
      session_data;
    }
  in
  let rooms =
    if not populated then jobj []
    else
      jobj
        [
          ( Id.Room_id.to_string room,
            jobj
              [
                ( "sessions",
                  jobj
                    [
                      ( Id.Session_id.to_string
                          (M.Megolm.Outbound.session_id outbound),
                        Result.get_ok
                          (Jsont.Json.encode B.key_backup_data_jsont backup_data)
                      );
                    ] );
              ] );
        ]
  in
  let response =
    json Matrix_proto.Json.Codec.json (jobj [ ("rooms", rooms) ])
  in
  let requests = ref [] in
  with_server env (fun request ->
      requests := request :: !requests;
      match request with
      | "GET /_matrix/client/v3/room_keys/version HTTP/1.1" ->
          version_json (auth_data recovery_key)
      | "GET /_matrix/client/v3/room_keys/keys?version=current HTTP/1.1" ->
          response
      | _ -> Alcotest.failf "unexpected request: %s" request)
  @@ fun homeserver ->
  save_session profile homeserver;
  let status, output = run env directory [ "--recovery-key-file"; file ] in
  Alcotest.(check bool) "success" true (status = `Exited 0);
  Alcotest.(check bool)
    "import count" true
    (contains output
       (if populated then "Imported 1 room key" else "Imported 0 room keys"));
  Alcotest.(check int)
    "one version and one fixed-version download" 2 (List.length !requests);
  let restored =
    Option.get
      (ok (M.Crypto_store.load (M.Crypto_store.create ~xdg ~profile:"default")))
  in
  Alcotest.(check (option string))
    "saved version" (Some "current") restored.state.backup.version;
  if populated then begin
    let plaintext = "recovered event" in
    let encrypted = M.Megolm.Outbound.encrypt outbound plaintext in
    let inbound =
      List.find
        (fun session ->
          Id.Session_id.equal
            (M.Megolm.Inbound.session_id session)
            (M.Megolm.Outbound.session_id outbound))
        restored.megolm_inbound
    in
    let decrypted =
      Result.get_ok
        (M.Megolm.Inbound.decrypt inbound ~ciphertext:encrypted.ciphertext)
    in
    Alcotest.(check string)
      "restored key decrypts" plaintext decrypted.plaintext
  end

let test_login_retires_corrupt_marker () =
  with_fixture @@ fun env directory _xdg profile ->
  let marker =
    Eio.Path.(M.Profile_store.dir profile / ".refresh_pending.json")
  in
  Eio.Path.save ~create:(`Exclusive 0o600) marker "{broken";
  let password = key_file env directory "fixture-password" in
  with_server env (fun request ->
      if request <> "POST /_matrix/client/v3/login HTTP/1.1" then
        Alcotest.failf "unexpected login request: %s" request;
      {|{"access_token":"fresh-login","refresh_token":"fresh-refresh","user_id":"@backup:example.org","device_id":"BACKUP"}|})
  @@ fun homeserver ->
  let status, _ =
    run ~command:[ "login" ] env directory
      [
        "-s";
        Uriz.to_string homeserver;
        "-u";
        Id.User_id.to_string user;
        "--password-file";
        password;
      ]
  in
  Alcotest.(check bool) "login succeeded" true (status = `Exited 0);
  let saved = Option.get (ok (M.Profile_store.load_session profile)) in
  Alcotest.(check string)
    "fresh login persisted" "fresh-login" saved.auth.access_token;
  Alcotest.(check bool) "corrupt marker retired" false (Eio.Path.is_file marker);
  let refreshed =
    ok
      (M.Profile_store.refresh_session profile ~clock:env#clock ~expected:saved
         ~refresh:(fun latest ->
           Ok
             {
               latest.auth with
               access_token = "next-access";
               refresh_token = Some "next-refresh";
             }))
  in
  Alcotest.(check string)
    "fresh login can refresh" "next-access" refreshed.auth.access_token

let () =
  Alcotest.run "CLI backup restore"
    [
      ( "restore",
        [
          Alcotest.test_case "login retires corrupt refresh marker" `Quick
            test_login_retires_corrupt_marker;
          Alcotest.test_case "wrong key" `Quick (fun () ->
              reject_metadata "wrong key" (fun _ ->
                  version_json (auth_data (key '\xab'))));
          Alcotest.test_case "unsupported algorithm" `Quick (fun () ->
              reject_metadata "unsupported" (fun key ->
                  version_json ~algorithm:"unknown" (auth_data key)));
          Alcotest.test_case "malformed metadata" `Quick (fun () ->
              reject_metadata "malformed" (fun _ -> version_json (jobj [])));
          Alcotest.test_case "key file errors" `Quick test_invalid_input;
          Alcotest.test_case "positional rejection" `Quick test_positional;
          Alcotest.test_case "empty backup" `Quick (fun () ->
              test_restore false);
          Alcotest.test_case "populated backup and decryption" `Quick (fun () ->
              test_restore true);
        ] );
    ]
