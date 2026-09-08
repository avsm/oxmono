module M = Matrix_client
module P = M.Profile_store
module C = M.Crypto_store
module S = M.Session
module Id = Matrix_proto.Id

let ok = function Ok x -> x | Error e -> Alcotest.failf "%a" M.Error.pp e
let user = Id.User_id.of_string_exn "@recovery:example.org"
let device = Id.Device_id.of_string_exn "RECOVERY"

let random () =
  M.Random.of_source (Eio.Flow.string_source (String.make 100_000 '\x12'))

let executable = Unix.realpath Sys.executable_name

let components =
  [
    "device.json";
    "one_time_keys.json";
    "olm_sessions.json";
    "megolm_inbound.json";
    "megolm_outbound.json";
    "crypto_state.json";
  ]

let load path = Eio.Path.load path
let write path data = Eio.Path.save ~create:(`Or_truncate 0o600) path data

let session =
  S.Session_file.
    {
      server =
        {
          homeserver = Uriz.of_string_exn "https://example.org";
          user_id = user;
        };
      auth =
        {
          access_token = "old";
          refresh_token = Some "old-refresh";
          access_token_expires_at = None;
          device_id = device;
          method_ = OAuth { client_id = "test" };
        };
      sync = { next_batch = None; filter_id = None };
      metadata =
        {
          created_at = Ptime.epoch;
          last_used_at = Ptime.epoch;
          client_name = "test";
        };
    }

let rotated =
  S.Auth.
    {
      session.auth with
      access_token = "new";
      refresh_token = Some "new-refresh";
    }

let stores env directory =
  let xdg =
    Xdge.create Eio.Path.(Eio.Stdenv.fs env / directory) "matrix-recovery"
  in
  (* Explicit XDG_DATA_HOME is set only in these isolated test processes. *)
  (xdg, P.create ~xdg ~profile:"test", C.create ~xdg ~profile:"test")

let with_fixture fn =
  Eio_main.run @@ fun env ->
  let directory = Filename.temp_dir "matrix-profile-recovery-" "" in
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
      Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / directory))
    (fun () ->
      let xdg, profile, crypto = stores env directory in
      ok (P.save_session profile session);
      let machine =
        M.Encryption.create ~random:(random ()) ~user_id:user ~device_id:device
          ()
      in
      let snapshot = M.Encryption.snapshot machine in
      let room_id = Id.Room_id.of_string_exn "!recovery:example.org" in
      let outbound = M.Megolm.Outbound.create ~random:(random ()) ~room_id () in
      let inbound =
        Result.get_ok
          (M.Megolm.Inbound.of_session_key
             ~sender_key:(M.Olm.Account.curve25519_key snapshot.account)
             ~room_id
             ~session_key:(M.Megolm.Outbound.session_key outbound)
             ())
      in
      ok
        (C.save crypto
           {
             snapshot with
             megolm_inbound = [ inbound ];
             megolm_outbound = [ outbound ];
           });
      fn env directory xdg profile crypto)

let spawn ~sw env args =
  Eio.Process.spawn ~sw
    (Eio.Stdenv.process_mgr env)
    (executable :: "--child" :: args)

let await env process =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      Eio.Process.await process)

let wait_file env path =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      while not (Eio.Path.is_file path) do
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01
      done)

let child env directory mode component =
  let _, profile, crypto = stores env directory in
  let dir = P.dir profile in
  if mode = "save" || mode = "clear" then begin
    let snapshot = Option.get (ok (C.load crypto)) in
    M.Olm.Account.generate_one_time_keys ~random:(random ()) snapshot.account 3;
    let outbound = List.hd snapshot.megolm_outbound in
    let inbound = List.hd snapshot.megolm_inbound in
    for index = 0 to 2 do
      let encrypted =
        M.Megolm.Outbound.encrypt outbound (string_of_int index)
      in
      ignore
        (Result.get_ok
           (M.Megolm.Inbound.decrypt inbound ~ciphertext:encrypted.ciphertext))
    done;
    let snapshot =
      {
        snapshot with
        state =
          {
            snapshot.state with
            secrets = [ ("test-secret", "recovered") ];
            trust_requirement = M.Encryption.Cross_signed;
          };
      }
    in
    let target = Eio.Path.(dir / component) in
    Eio.Path.unlink target;
    Eio.Path.mkdir ~perm:0o700 target;
    try
      ignore
        (ok (if mode = "clear" then C.clear crypto else C.save crypto snapshot));
      Unix._exit 2
    with Eio.Io _ -> Unix._exit 42
  end
  else begin
    let result =
      P.refresh_session profile ~clock:env#clock ~expected:session
        ~refresh:(fun _ ->
          if mode = "die" then Unix._exit 42;
          let count_path = Eio.Path.(dir / "exchanges") in
          let count =
            if Eio.Path.is_file count_path then int_of_string (load count_path)
            else 0
          in
          write count_path (string_of_int (count + 1));
          write Eio.Path.(dir / "started") "yes";
          if mode = "hold" then wait_file env Eio.Path.(dir / "release");
          Ok rotated)
    in
    let refreshed = ok result in
    if refreshed.auth <> rotated then Unix._exit 3
  end

let journal_files codec =
  Jsont.Object.(
    map (fun files -> files) |> mem "files" (Jsont.list codec) |> finish)

let test_restart mode component () =
  with_fixture @@ fun env directory xdg profile crypto ->
  let before = Option.get (ok (C.load crypto)) in
  let identity =
    M.Crypto_key.Curve25519.Public.to_base64
      (M.Olm.Account.curve25519_key before.account)
  in
  let stale = C.create ~xdg ~profile:"test" in
  ignore (ok (C.load stale));
  Eio.Switch.run @@ fun sw ->
  let proc = spawn ~sw env [ directory; mode; component ] in
  Alcotest.(check bool)
    "writer died after journal commit" true
    (await env proc = `Exited 42);
  let dir = P.dir profile in
  let journal = Eio.Path.(dir / ".crypto_transaction.json") in
  let journal_data = load journal in
  let expected =
    Result.get_ok
      (Jsont_bytesrw.decode_string
         (journal_files Matrix_proto.Json.Codec.string)
         journal_data)
  in
  Alcotest.(check int)
    "private journal" 0o600
    (Unix.stat (Option.get (Eio.Path.native journal))).Unix.st_perm;
  Eio.Path.rmdir Eio.Path.(dir / component);
  let fresh = C.create ~xdg ~profile:"test" in
  let recovered = ok (C.load fresh) in
  Alcotest.(check bool) "journal removed" false (Eio.Path.is_file journal);
  if mode = "clear" then begin
    Alcotest.(check bool) "cleared crypto" true (recovered = None);
    Alcotest.(check bool) "cleared login" false (P.exists profile);
    List.iter
      (fun name ->
        Alcotest.(check bool)
          name false
          (Eio.Path.is_file Eio.Path.(dir / name)))
      components
  end
  else begin
    List.iter2
      (fun name data ->
        Alcotest.(check string)
          ("coherent " ^ name) data
          (load Eio.Path.(dir / name)))
      components expected;
    let recovered = Option.get recovered in
    Alcotest.(check int)
      "one-time keys survived" 3
      (M.Olm.Account.one_time_keys_count recovered.account);
    let outbound = List.hd recovered.megolm_outbound in
    Alcotest.(check int)
      "outbound ratchet recovered" 3
      (M.Megolm.Outbound.message_index outbound);
    let encrypted = M.Megolm.Outbound.encrypt outbound "after restart" in
    let decrypted =
      Result.get_ok
        (M.Megolm.Inbound.decrypt
           (List.hd recovered.megolm_inbound)
           ~ciphertext:encrypted.ciphertext)
    in
    Alcotest.(check string)
      "recovered ratchets decrypt" "after restart" decrypted.plaintext;
    Alcotest.(check string)
      "same device" identity
      (M.Crypto_key.Curve25519.Public.to_base64
         (M.Olm.Account.curve25519_key recovered.account));
    Alcotest.(check (list (pair string string)))
      "recovered secrets"
      [ ("test-secret", "recovered") ]
      recovered.state.secrets;
    Alcotest.(check bool)
      "recovered trust policy" true
      (recovered.state.trust_requirement = M.Encryption.Cross_signed);
    (* A completed but not yet unlinked journal must never replay old files. *)
    write journal journal_data;
    ok
      (P.update_session profile (fun latest ->
           { latest with sync = { latest.sync with next_batch = Some "later" } }));
    ignore (ok (C.load fresh));
    Alcotest.(check (option string))
      "later login metadata retained" (Some "later")
      (Option.get (ok (P.load_session profile))).sync.next_batch
  end;
  match C.save stale before with
  | Error (M.Error.Policy_denied _) -> ()
  | _ -> Alcotest.fail "stale handle must not overwrite recovered state"

let test_before_marker () =
  with_fixture @@ fun env directory xdg profile _ ->
  Eio.Switch.run @@ fun sw ->
  let dir = P.dir profile in
  let marker = Eio.Path.(dir / ".crypto_generation") in
  let previous = load marker in
  let proc = spawn ~sw env [ directory; "save"; "device.json" ] in
  Alcotest.(check bool) "interrupted save" true (await env proc = `Exited 42);
  Eio.Path.rmdir Eio.Path.(dir / "device.json");
  write marker previous;
  let recovered = Option.get (ok (C.load (C.create ~xdg ~profile:"test"))) in
  Alcotest.(check (list (pair string string)))
    "replay before odd marker"
    [ ("test-secret", "recovered") ]
    recovered.state.secrets

let test_bad_journal () =
  with_fixture @@ fun _env _directory _xdg profile crypto ->
  let dir = P.dir profile in
  let before = List.map (fun name -> load Eio.Path.(dir / name)) components in
  List.iter
    (fun data ->
      write Eio.Path.(dir / ".crypto_transaction.json") data;
      Alcotest.(check bool)
        "bad journal rejected" true
        (Result.is_error (C.load crypto));
      Alcotest.(check (list string))
        "files untouched" before
        (List.map (fun name -> load Eio.Path.(dir / name)) components))
    [
      "{bad";
      {|{"generation":4,"clear":false,"files":[]}|};
      {|{"generation":100,"clear":true,"files":[]}|};
    ]

let test_process_refresh () =
  with_fixture @@ fun env directory _xdg profile _ ->
  Eio.Switch.run @@ fun sw ->
  let dir = P.dir profile in
  let leader = spawn ~sw env [ directory; "hold"; "unused" ] in
  wait_file env Eio.Path.(dir / "started");
  let follower = spawn ~sw env [ directory; "refresh"; "unused" ] in
  ok
    (P.update_session profile (fun latest ->
         {
           latest with
           sync = { latest.sync with next_batch = Some "concurrent" };
         }));
  write Eio.Path.(dir / "release") "yes";
  List.iter
    (fun proc ->
      Alcotest.(check bool) "refresh process" true (await env proc = `Exited 0))
    [ leader; follower ];
  Alcotest.(check string)
    "one exchange across processes" "1"
    (load Eio.Path.(dir / "exchanges"));
  let saved = Option.get (ok (P.load_session profile)) in
  Alcotest.(check string) "rotated token saved" "new" saved.auth.access_token;
  Alcotest.(check (option string))
    "concurrent sync retained" (Some "concurrent") saved.sync.next_batch

let test_refresh_crash () =
  with_fixture @@ fun env directory _xdg profile _ ->
  Eio.Switch.run @@ fun sw ->
  let proc = spawn ~sw env [ directory; "die"; "unused" ] in
  Alcotest.(check bool) "refresh process died" true (await env proc = `Exited 42);
  let refresh _ = Alcotest.fail "must not reuse a possibly consumed token" in
  Alcotest.(check bool)
    "uncertain exchange fails closed" true
    (Result.is_error
       (P.refresh_session profile ~clock:env#clock ~expected:session ~refresh));
  ok (P.save_session profile { session with auth = rotated });
  let adopted =
    ok (P.refresh_session profile ~clock:env#clock ~expected:session ~refresh)
  in
  Alcotest.(check string)
    "new login supersedes marker" "new" adopted.auth.access_token

let test_refresh_logout () =
  with_fixture @@ fun env _directory _xdg profile _ ->
  let result =
    P.refresh_session profile ~clock:env#clock ~expected:session
      ~refresh:(fun _ ->
        ignore
          (ok
             (P.with_lock profile (fun () ->
                  Eio.Path.unlink Eio.Path.(P.dir profile / "session.json"))));
        Ok rotated)
  in
  Alcotest.(check bool) "logout interrupts commit" true (Result.is_error result);
  Alcotest.(check bool) "login not resurrected" false (P.exists profile);
  let no_refresh =
    { session with auth = { rotated with refresh_token = None } }
  in
  ok (P.save_session profile no_refresh);
  let result =
    P.refresh_session profile ~clock:env#clock ~expected:session
      ~refresh:(fun _ -> Alcotest.fail "revoked refresh token reused")
  in
  Alcotest.(check bool)
    "removed refresh token never reintroduced" true (Result.is_error result);
  let other =
    {
      session with
      auth = { rotated with device_id = Id.Device_id.of_string_exn "OTHER" };
    }
  in
  ok (P.save_session profile other);
  let result =
    P.refresh_session profile ~clock:env#clock ~expected:session
      ~refresh:(fun _ -> Alcotest.fail "different login refreshed")
  in
  Alcotest.(check bool)
    "replacement login not overwritten" true (Result.is_error result);
  Alcotest.(check string)
    "new login token retained" "new"
    (Option.get (ok (P.load_session profile))).auth.access_token

let test_refresh_cancel_waiter () =
  with_fixture @@ fun env directory _xdg profile _ ->
  Eio.Switch.run @@ fun sw ->
  let dir = P.dir profile in
  let leader = spawn ~sw env [ directory; "hold"; "unused" ] in
  wait_file env Eio.Path.(dir / "started");
  let result =
    Eio.Time.with_timeout env#clock 0.1 (fun () ->
        Ok
          (P.refresh_session profile ~clock:env#clock ~expected:session
             ~refresh:(fun _ -> Alcotest.fail "waiter exchanged tokens")))
  in
  Alcotest.(check bool)
    "native lock wait is cancellable" true (result = Error `Timeout);
  write Eio.Path.(dir / "release") "yes";
  Alcotest.(check bool) "leader completed" true (await env leader = `Exited 0);
  ignore
    (ok
       (P.refresh_session profile ~clock:env#clock ~expected:session
          ~refresh:(fun _ -> Alcotest.fail "completed tokens not reused")))

let test_refresh_cancel_exchange () =
  with_fixture @@ fun env _directory _xdg profile _ ->
  let result =
    Eio.Time.with_timeout env#clock 0.05 (fun () ->
        Ok
          (P.refresh_session profile ~clock:env#clock ~expected:session
             ~refresh:(fun _ -> Eio.Fiber.await_cancel ())))
  in
  Alcotest.(check bool) "exchange cancellable" true (result = Error `Timeout);
  let next =
    P.refresh_session profile ~clock:env#clock ~expected:session
      ~refresh:(fun _ -> Alcotest.fail "consumed token reused")
  in
  Alcotest.(check bool)
    "interrupted exchange blocks reuse" true (Result.is_error next)

let test_login_replaces_refresh_state () =
  with_fixture @@ fun env _directory _xdg profile _ ->
  let marker = Eio.Path.(P.dir profile / ".refresh_pending.json") in
  let fresh =
    { session with auth = { rotated with access_token = "fresh-login" } }
  in
  List.iter
    (fun contents ->
      write marker contents;
      (* Metadata-only saves must never unblock a possibly consumed token. *)
      ok (P.update_session profile (fun latest -> latest));
      Alcotest.(check string)
        "ordinary save retains marker" contents (load marker);
      ok (P.save_login profile ~clock:env#clock fresh);
      Alcotest.(check bool)
        "fresh login retires marker" false (Eio.Path.is_file marker);
      let refreshed =
        ok
          (P.refresh_session profile ~clock:env#clock ~expected:fresh
             ~refresh:(fun _ -> Ok rotated))
      in
      Alcotest.(check string)
        "refresh works after new login" "new" refreshed.auth.access_token)
    [
      "{corrupt";
      Result.get_ok (Jsont_bytesrw.encode_string S.Session_file.jsont session);
    ]

let test_failed_login_preserves_refresh_state () =
  with_fixture @@ fun env _directory _xdg profile _ ->
  let dir = P.dir profile in
  let marker = Eio.Path.(dir / ".refresh_pending.json") in
  let contents =
    Result.get_ok (Jsont_bytesrw.encode_string S.Session_file.jsont session)
  in
  write marker contents;
  let session_path = Eio.Path.(dir / "session.json") in
  let original = Eio.Path.(dir / "session-before.json") in
  Eio.Path.rename session_path original;
  Eio.Path.mkdir ~perm:0o700 session_path;
  Alcotest.(check bool)
    "login write failed" true
    (try
       Result.is_error
         (P.save_login profile ~clock:env#clock { session with auth = rotated })
     with Eio.Io _ -> true);
  Alcotest.(check string) "uncertainty marker retained" contents (load marker);
  Eio.Path.rmdir session_path;
  Eio.Path.rename original session_path;
  Alcotest.(check bool)
    "old token still quarantined" true
    (Result.is_error
       (P.refresh_session profile ~clock:env#clock ~expected:session
          ~refresh:(fun _ -> Alcotest.fail "possibly consumed token reused")))

let test_login_waits_for_refresh () =
  with_fixture @@ fun env directory _xdg profile _ ->
  Eio.Switch.run @@ fun sw ->
  let dir = P.dir profile in
  let leader = spawn ~sw env [ directory; "hold"; "unused" ] in
  wait_file env Eio.Path.(dir / "started");
  let done_, resolve = Eio.Promise.create () in
  let fresh =
    {
      session with
      auth =
        {
          rotated with
          access_token = "fresh-login";
          refresh_token = Some "fresh-refresh";
        };
    }
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve resolve (P.save_login profile ~clock:env#clock fresh));
  let waiting =
    Eio.Time.with_timeout env#clock 0.05 (fun () ->
        Ok (Eio.Promise.await done_))
  in
  Alcotest.(check bool)
    "new login waits for active exchange" true (waiting = Error `Timeout);
  write Eio.Path.(dir / "release") "yes";
  Alcotest.(check bool)
    "old refresh finished" true
    (await env leader = `Exited 0);
  ok (Eio.Promise.await done_);
  Alcotest.(check string)
    "fresh login wins after old exchange" "fresh-login"
    (Option.get (ok (P.load_session profile))).auth.access_token;
  Alcotest.(check bool)
    "old marker retired" false
    (Eio.Path.is_file Eio.Path.(dir / ".refresh_pending.json"))

let test_preparation_cancellation () =
  with_fixture @@ fun env _directory _xdg profile _ ->
  let marker = Eio.Path.(P.dir profile / ".refresh_pending.json") in
  let before = load Eio.Path.(P.dir profile / "session.json") in
  let result =
    Eio.Time.with_timeout env#clock 0.05 (fun () ->
        Ok
          (P.refresh_session_prepared profile ~clock:env#clock ~expected:session
             ~prepare:(fun _ -> Eio.Fiber.await_cancel ())))
  in
  Alcotest.(check bool) "preparation cancellable" true (result = Error `Timeout);
  Alcotest.(check bool)
    "cancelled preparation leaves no marker" false (Eio.Path.is_file marker);
  let failed =
    P.refresh_session_prepared profile ~clock:env#clock ~expected:session
      ~prepare:(fun _ -> Error (M.Error.Network_error "discovery unavailable"))
  in
  Alcotest.(check bool)
    "preparation error propagated" true (Result.is_error failed);
  Alcotest.(check bool)
    "failed preparation leaves no marker" false (Eio.Path.is_file marker);
  Alcotest.(check string)
    "preparation leaves credentials intact" before
    (load Eio.Path.(P.dir profile / "session.json"));
  let refreshed =
    ok
      (P.refresh_session_prepared profile ~clock:env#clock ~expected:session
         ~prepare:(fun _ ->
           Alcotest.(check bool)
             "preparation precedes marker" false (Eio.Path.is_file marker);
           Ok
             (fun () ->
               Alcotest.(check bool)
                 "exchange follows durable marker" true
                 (Eio.Path.is_file marker);
               Ok rotated)))
  in
  Alcotest.(check string) "retry succeeds" "new" refreshed.auth.access_token

let () =
  match Array.to_list Sys.argv with
  | [ _; "--child"; directory; mode; component ] ->
      Eio_main.run (fun env -> child env directory mode component)
  | _ ->
      Alcotest.run "profile recovery"
        [
          ( "crypto save restart",
            List.map
              (fun file ->
                Alcotest.test_case file `Quick (test_restart "save" file))
              components );
          ( "crypto clear restart",
            List.map
              (fun file ->
                Alcotest.test_case file `Quick (test_restart "clear" file))
              ("session.json" :: components) );
          ( "transaction boundaries",
            [
              Alcotest.test_case "journal before marker" `Quick
                test_before_marker;
              Alcotest.test_case "invalid journal" `Quick test_bad_journal;
            ] );
          ( "refresh",
            [
              Alcotest.test_case "login retires old refresh state" `Quick
                test_login_replaces_refresh_state;
              Alcotest.test_case "failed login retains uncertain token" `Quick
                test_failed_login_preserves_refresh_state;
              Alcotest.test_case "login waits for another process refreshing"
                `Quick test_login_waits_for_refresh;
              Alcotest.test_case "preparation cancellation and retry" `Quick
                test_preparation_cancellation;
              Alcotest.test_case "two processes and concurrent sync" `Quick
                test_process_refresh;
              Alcotest.test_case "process death and relogin" `Quick
                test_refresh_crash;
              Alcotest.test_case "logout during exchange" `Quick
                test_refresh_logout;
              Alcotest.test_case "cancel native lock waiter" `Quick
                test_refresh_cancel_waiter;
              Alcotest.test_case "cancel network exchange" `Quick
                test_refresh_cancel_exchange;
            ] );
        ]
