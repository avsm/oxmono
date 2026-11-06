(* Session persistence and JWT expiry for the Immich CLI. *)

module Session = Immich_auth.Session

let failures = ref 0

let check name ok =
  if not ok then (
    incr failures;
    prerr_endline ("FAIL: " ^ name))

let jwt payload =
  let b64 = Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet in
  String.concat "." [ b64 {|{"alg":"HS256"}|}; b64 payload; "sig" ]

let now = Unix.gettimeofday ()
let exp offset = jwt (Printf.sprintf {|{"exp":%.0f}|} (now +. offset))

let test_jwt_expiry () =
  check "future token is live" (not (Session.is_jwt_expired (exp 3600.)));
  check "past token is expired" (Session.is_jwt_expired (exp (-3600.)));
  check "token inside the default leeway is expired"
    (Session.is_jwt_expired (exp 30.));
  check "leeway is configurable"
    (not (Session.is_jwt_expired ~leeway:0 (exp 30.)));
  check "token without exp is expired"
    (Session.is_jwt_expired (jwt {|{"sub":"u"}|}));
  check "non-JWT is expired" (Session.is_jwt_expired "not-a-jwt");
  check "bad base64 is expired" (Session.is_jwt_expired "a.!!!.c");
  check "non-JSON payload is expired"
    (Session.is_jwt_expired (jwt "not json"));
  let api_key =
    Session.create ~server_url:"https://immich.example"
      ~auth:(Api_key { key = "k"; name = None })
      ()
  in
  check "API keys never expire" (not (Session.is_expired api_key));
  let stale =
    Session.create ~server_url:"https://immich.example"
      ~auth:(Jwt { access_token = exp (-3600.); user_id = "u"; email = "e" })
      ()
  in
  check "stale JWT session is expired" (Session.is_expired stale)

let round_trip session =
  match Jsont_bytesrw.encode_string Session.jsont session with
  | Error e -> failwith e
  | Ok text -> Jsont_bytesrw.decode_string Session.jsont text

let test_codec () =
  List.iter
    (fun auth ->
      let session =
        Session.create ~server_url:"https://immich.example" ~auth ()
      in
      check "session JSON round trip"
        (round_trip session = Ok session))
    [
      Session.Jwt { access_token = "t"; user_id = "u"; email = "a@b" };
      Api_key { key = "k"; name = Some "cli" };
      Api_key { key = "k"; name = None };
    ];
  let rejects auth =
    let text =
      Printf.sprintf {|{"server_url":"s","created_at":"c","auth":%s}|} auth
    in
    check ("malformed auth is a decoding error: " ^ auth)
      (Result.is_error (Jsont_bytesrw.decode_string Session.jsont text))
  in
  rejects {|{"type":"other"}|};
  rejects {|{"type":"jwt"}|};
  rejects {|{"type":"api_key","jwt":{"access_token":"t","user_id":"u","email":"e"}}|}

(* The test is single-domain, so changing HOME is safe here. *)
let[@alert "-unsafe_multidomain"] with_home f =
  let home = Filename.temp_dir "immich-test-" "" in
  let previous = Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" home;
  Fun.protect
    ~finally:(fun () ->
      Option.iter (fun home -> Unix.putenv "HOME" home) previous;
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; home ])))
    (fun () -> Eio_main.run @@ fun env -> f home (Eio.Stdenv.fs env))

let test_profiles () =
  with_home @@ fun home fs ->
  let session =
    Session.create ~server_url:"https://immich.example"
      ~auth:(Api_key { key = "secret"; name = Some "cli" })
      ()
  in
  check "no session at first" (Session.load fs () = None);
  check "default profile at first"
    (Session.get_current_profile fs = Session.default_profile);
  Session.save fs ~profile:"home" session;
  Session.set_current_profile fs "home";
  check "current profile is saved" (Session.get_current_profile fs = "home");
  check "load uses the current profile" (Session.load fs () = Some session);
  check "other profiles stay empty"
    (Session.load fs ~profile:"work" () = None);
  check "only profiles with a session are listed"
    (Session.list_profiles fs = [ "home" ]);
  let file =
    Filename.concat home ".config/immich/profiles/home/session.json"
  in
  check "session file is private"
    ((Unix.stat file).st_perm land 0o077 = 0);
  check "config directory is private"
    ((Unix.stat (Filename.concat home ".config/immich")).st_perm land 0o077
    = 0);
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(fs / file)
    {|{"server_url":"s","created_at":"c","auth":{"type":"jwt"}}|};
  check "corrupt session loads as none" (Session.load fs () = None);
  Session.clear fs ();
  check "clear removes the current session" (Session.load fs () = None);
  Session.clear fs ();
  check "clearing twice is harmless" (Session.load fs () = None)

let () =
  test_jwt_expiry ();
  test_codec ();
  test_profiles ();
  if !failures > 0 then exit 1;
  print_endline "test_session: immich session tests passed"
