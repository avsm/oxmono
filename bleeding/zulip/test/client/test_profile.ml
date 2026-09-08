open Zulip_eio

let ok = function
  | Ok value -> value
  | Error error -> Alcotest.fail (Error.error_to_string error)

let expect_error = function
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Expected failure"

let make_auth key =
  Auth.create ~site:"https://zulip.test" ~email:"bot@zulip.test" ~api_key:key
  |> ok

let with_store f =
  let root = Filename.temp_file "ocaml-zulip-profile-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  Unix.mkdir (Filename.concat root "runtime") 0o700;
  List.iter
    (fun (name, subdir) -> Unix.putenv name (Filename.concat root subdir))
    [
      ("ZULIP_CONFIG_DIR", "config");
      ("ZULIP_DATA_DIR", "data");
      ("ZULIP_CACHE_DIR", "cache");
      ("ZULIP_STATE_DIR", "state");
      ("ZULIP_RUNTIME_DIR", "runtime");
      ("XDG_CONFIG_HOME", "config-home");
      ("XDG_DATA_HOME", "data-home");
      ("XDG_CACHE_HOME", "cache-home");
      ("XDG_STATE_HOME", "state-home");
    ];
  Unix.putenv "XDG_RUNTIME_DIR" root;
  Eio_main.run (fun env ->
      Fun.protect
        ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(env#fs / root))
        (fun () -> f env#fs root))

let test_roundtrip () =
  with_store (fun fs root ->
      let profile =
        Profile.create ~name:"test" ~auth:(make_auth "first") |> ok
      in
      Profile.save ~fs profile |> ok;
      let loaded = Profile.load ~fs "test" |> ok in
      Alcotest.(check string)
        "credential roundtrip" "first"
        (Auth.api_key (Profile.auth loaded));
      let path =
        Eio.Path.(fs / root / "config" / "zulip" / "profiles" / "test.json")
      in
      Alcotest.(check int)
        "private file" 0
        ((Eio.Path.stat ~follow:false path).perm land 0o077);
      let replacement =
        Profile.create ~name:"test" ~auth:(make_auth "second") |> ok
      in
      Profile.save ~fs replacement |> ok;
      Alcotest.(check string)
        "atomic replacement" "second"
        (Auth.api_key (Profile.auth (Profile.load ~fs "test" |> ok)));
      let huge =
        Profile.create ~name:"test" ~auth:(make_auth (String.make 70000 'a'))
        |> ok
      in
      Profile.save ~fs huge |> expect_error;
      Alcotest.(check string)
        "failed write preserves previous file" "second"
        (Auth.api_key (Profile.auth (Profile.load ~fs "test" |> ok)));
      Eio.Path.chmod ~follow:false ~perm:0o644 path;
      Profile.load ~fs "test" |> expect_error)

let test_validation () =
  List.iter
    (fun name -> Profile.create ~name ~auth:(make_auth "key") |> expect_error)
    [ ""; "../escape"; "/tmp/absolute"; ".hidden"; "x/y" ];
  with_store (fun fs root ->
      let dir = Eio.Path.(fs / root / "config" / "zulip" / "profiles") in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
      Eio.Path.save ~create:(`Exclusive 0o600)
        Eio.Path.(dir / "broken.json")
        "{";
      Profile.resolve ~fs ~site:"https://override.test" ~email:"override@test"
        ~api_key:"override" "broken"
      |> expect_error)

let test_import () =
  with_store (fun fs root ->
      let path = Eio.Path.(fs / root / "bot.zuliprc") in
      Eio.Path.save ~create:(`Exclusive 0o600) path
        "[api]\nemail=bot@zulip.test\nkey=downloaded\nsite=https://zulip.test\n";
      Profile.import_zuliprc ~fs ~name:"imported" path |> ok |> ignore;
      Alcotest.(check string)
        "import persisted" "downloaded"
        (Auth.api_key (Profile.auth (Profile.load ~fs "imported" |> ok))))

let test_data_directory_error () =
  with_store (fun fs _ ->
      let profile = Profile.create ~name:"test" ~auth:(make_auth "key") |> ok in
      let path = Profile.data_dir ~fs profile |> ok in
      Eio.Path.chmod ~follow:false ~perm:0o755 path;
      match Profile.data_dir ~fs profile with
      | Error (Error.Storage _) -> ()
      | _ -> Alcotest.fail "insecure data directory did not return Storage")

let test_redacted_printers () =
  let auth =
    Auth.create ~site:"zulip.test:8443" ~email:"bot@zulip.test"
      ~api_key:"must-not-print"
    |> ok
  in
  Alcotest.(check string)
    "host and port shorthand" "https://zulip.test:8443" (Auth.site auth);
  let profile = Profile.create ~name:"test" ~auth |> ok in
  Alcotest.(check string)
    "auth omits key" "Zulip(https://zulip.test:8443, bot@zulip.test)"
    (Format.asprintf "%a" Auth.pp auth);
  Alcotest.(check string)
    "profile omits key"
    "Profile(test, Zulip(https://zulip.test:8443, bot@zulip.test))"
    (Format.asprintf "%a" Profile.pp profile)

let () =
  Alcotest.run "Zulip profiles"
    [
      ( "profile",
        [
          Alcotest.test_case "private atomic roundtrip" `Quick test_roundtrip;
          Alcotest.test_case "validation and malformed input" `Quick
            test_validation;
          Alcotest.test_case "zuliprc import" `Quick test_import;
          Alcotest.test_case "recoverable data directory failure" `Quick
            test_data_directory_error;
          Alcotest.test_case "redacted printers" `Quick test_redacted_printers;
        ] );
    ]
