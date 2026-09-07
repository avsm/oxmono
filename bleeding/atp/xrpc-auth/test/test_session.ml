let check name value = if not value then failwith name
let invalid f = match f () with _ -> failwith "accepted unsafe name" | exception Invalid_argument _ -> ()

let () =
  List.iter (fun name -> invalid (fun () -> Xrpc_auth.Session.validate_name name))
    ["";".";"..";"../outside";"/tmp";"a/b";"a\\b";"a\000b"];
  let root = Filename.temp_file "xrpc-session-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let previous = Sys.getenv_opt "XDG_CONFIG_HOME" in
  Unix.putenv "XDG_CONFIG_HOME" root;
  Fun.protect ~finally:(fun () -> Unix.putenv "XDG_CONFIG_HOME" (Option.value ~default:"" previous)) @@ fun () ->
  Eio_main.run @@ fun env ->
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(env#fs / root)) @@ fun () ->
  let session : Xrpc_auth.Session.t = {access_jwt = "access"; refresh_jwt = "refresh";
    did = "did:plc:alice"; handle = "alice.example"; pds = "https://example.com";
    created_at = "2026-01-01T00:00:00Z"} in
  Xrpc_auth.Session.save env#fs ~app_name:"xrpc-test" ~profile:"alice" session;
  Xrpc_auth.Session.set_current_profile env#fs ~app_name:"xrpc-test" "alice";
  let dir = Xrpc_auth.Session.config_dir env#fs ~app_name:"xrpc-test" () in
  check "current profile directory" (Eio.Path.is_file Eio.Path.(dir / "session.json"));
  let file = root ^ "/xrpc-test/profiles/alice/session.json" in
  Unix.chmod file 0o644;
  Xrpc_auth.Session.save env#fs ~app_name:"xrpc-test" session;
  check "replacement repairs mode" ((Unix.stat file).Unix.st_perm = 0o600);
  check "session round trip" (Xrpc_auth.Session.load env#fs ~app_name:"xrpc-test" () = Some session);
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(dir / "session.json") "invalid";
  (match Xrpc_auth.Session.load env#fs ~app_name:"xrpc-test" () with
   | _ -> failwith "malformed session treated as missing"
   | exception Xrpc_auth.Session.Invalid_session _ -> ());
  Xrpc_auth.Session.clear env#fs ~app_name:"xrpc-test" ();
  check "missing session is None" (Xrpc_auth.Session.load env#fs ~app_name:"xrpc-test" () = None);
  check "no temporary files" (Eio.Path.read_dir dir = []);
  Eio.Switch.run @@ fun sw ->
  Xrpc_auth.Session.set_current_profile env#fs ~app_name:"xrpc-test" "custom";
  let client = Xrpc_auth.Client.create ~sw ~env ~app_name:"xrpc-test" ~pds:"https://example.com"
      ~http:(Fetch_mock.client (fun _ -> failwith "unexpected network")) () in
  Xrpc_auth.Client.resume client ~session;
  check "resume retains current profile" (Xrpc_auth.Client.get_profile client = Some "custom");
  check "resume persists to current profile" (Xrpc_auth.Session.load env#fs ~app_name:"xrpc-test" ~profile:"custom" () <> None);
  check "resume does not silently select handle profile"
    (not (List.mem "alice.example" (Xrpc_auth.Session.list_profiles env#fs ~app_name:"xrpc-test")))
