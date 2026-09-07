let check name value = if not value then failwith name
let invalid f = match f () with _ -> failwith "accepted unsafe name" | exception Invalid_argument _ -> ()

let () =
  List.iter (fun name -> invalid (fun () -> Apub_auth_session.validate_name name))
    ["";".";"..";"../outside";"/tmp";"a/b";"a\\b";"a\000b"];
  let root = Filename.temp_file "apub-session-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let previous = Sys.getenv_opt "XDG_CONFIG_HOME" in
  Unix.putenv "XDG_CONFIG_HOME" root;
  Fun.protect ~finally:(fun () -> Unix.putenv "XDG_CONFIG_HOME" (Option.value ~default:"" previous)) @@ fun () ->
  Eio_main.run @@ fun env ->
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(env#fs / root)) @@ fun () ->
  let session = Apub_auth_session.create ~actor_uri:"https://example.com/alice"
      ~key_id:"https://example.com/alice#key" ~private_key_pem:"fixture" in
  Apub_auth_session.save env#fs ~app_name:"apub-test" ~profile:"alice" session;
  Apub_auth_session.set_current_profile env#fs ~app_name:"apub-test" "alice";
  let dir = Apub_auth_session.config_dir env#fs ~app_name:"apub-test" () in
  check "current profile directory" (Eio.Path.is_file Eio.Path.(dir / "session.json"));
  let file = root ^ "/apub-test/profiles/alice/session.json" in
  Unix.chmod file 0o644;
  Apub_auth_session.save env#fs ~app_name:"apub-test" session;
  check "replacement repairs mode" ((Unix.stat file).Unix.st_perm = 0o600);
  check "session round trip" (Apub_auth_session.load env#fs ~app_name:"apub-test" () = Some session);
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(dir / "session.json") "invalid";
  (match Apub_auth_session.load env#fs ~app_name:"apub-test" () with
   | _ -> failwith "malformed session treated as missing"
   | exception Apub_auth_session.Invalid_session _ -> ());
  Apub_auth_session.clear env#fs ~app_name:"apub-test" ();
  check "missing session is None" (Apub_auth_session.load env#fs ~app_name:"apub-test" () = None);
  check "no temporary files" (Eio.Path.read_dir dir = []);
  check "changing actor cannot reuse saved credentials"
    (Result.is_error (Apub_auth_credentials.resolve ~actor_uri:"https://example.com/bob" (Some session)))
