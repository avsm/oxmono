(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tests for {!Jmap_eio.Auth} and {!Jmap_eio.Transport}: what a credential puts
    on the wire, where it comes from, and how a deadline ends an exchange. *)

open Jmap_eio

(* {1 A mock server that records what it was asked with} *)

let well_known = "https://api.example.com/.well-known/jmap"

let session_json =
  {|{
  "capabilities": { "urn:ietf:params:jmap:core": {
    "maxSizeUpload": 50000000,
    "maxConcurrentUpload": 4,
    "maxSizeRequest": 10000000,
    "maxConcurrentRequests": 4,
    "maxCallsInRequest": 16,
    "maxObjectsInGet": 500,
    "maxObjectsInSet": 500,
    "collationAlgorithms": []
  } },
  "accounts": {
    "acc1": {
      "name": "Test Account",
      "isPersonal": true,
      "isReadOnly": false,
      "accountCapabilities": {}
    }
  },
  "primaryAccounts": { "urn:ietf:params:jmap:core": "acc1" },
  "username": "test@example.com",
  "apiUrl": "https://api.example.com/jmap/api/",
  "downloadUrl": "https://api.example.com/jmap/download/{accountId}/{blobId}/{name}?type={type}",
  "uploadUrl": "https://api.example.com/jmap/upload/{accountId}/",
  "eventSourceUrl": "https://api.example.com/jmap/eventsource/?types={types}&closeafter={closeafter}&ping={ping}",
  "state": "state-1"
}|}

let echo_response =
  {|{ "methodResponses": [ [ "Core/echo", {}, "c1" ] ], "sessionState": "state-1" }|}

let json_headers = Http.Header.of_list [ ("content-type", "application/json") ]

(* Every request's [Authorization], oldest first: what the credential
   actually put on the wire. *)
let server ?(delay = fun () -> ()) () =
  let log = ref [] in
  let handler (req : Fetch.Middleware.request) =
    log := !log @ [ Http.Header.get req.headers "authorization" ];
    delay ();
    let body =
      match Fetch.Middleware.Url.path_and_query req.url with
      | "/.well-known/jmap" -> session_json
      | _ -> echo_response
    in
    Fetch_mock.respond ~headers:json_headers body req
  in
  (log, Fetch_mock.client handler)

let echo_request =
  Jmap.Proto.Request.create
    ~using:[ Jmap.Proto.Capability.core ]
    ~method_calls:
      [
        Jmap.Proto.Invocation.create ~name:"Core/echo"
          ~arguments:(Jsont.Object ([], Jsont.Meta.none))
          ~method_call_id:"c1";
      ]
    ()

let connect ~sw ?auth ?timeout ?clock fetch =
  Client.connect ~sw ?auth ?timeout (Transport.of_fetch ?clock fetch) well_known

let connect_exn ~sw ?auth fetch =
  match connect ~sw ?auth fetch with
  | Ok client -> client
  | Error e -> Alcotest.failf "connect failed: %s" (Client.error_to_string e)

let request_exn client =
  match Client.request client echo_request with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "request failed: %s" (Client.error_to_string e)

let check_headers = Alcotest.(check (list (option string)))
let check_string = Alcotest.(check string)
let pp_auth t = Fmt.str "%a" Auth.pp t

(* {1 Schemes on the wire} *)

(* RFC 6750 2.1: the token goes in [Authorization] behind the "Bearer"
   scheme. *)
let test_bearer () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let log, fetch = server () in
  let client = connect_exn ~sw ~auth:(Auth.bearer "TOKEN") fetch in
  request_exn client;
  check_headers "bearer on every request"
    [ Some "Bearer TOKEN"; Some "Bearer TOKEN" ]
    !log

(* RFC 7617 2: "user:password", base64, behind the "Basic" scheme. *)
let test_basic () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let log, fetch = server () in
  let auth = Auth.basic ~user:"user1" ~password:"secret" in
  let _ = connect_exn ~sw ~auth fetch in
  check_headers "basic" [ Some "Basic dXNlcjE6c2VjcmV0" ] !log

let test_constant_credentials_are_validated () =
  Alcotest.check_raises "invalid bearer"
    (Invalid_argument
       "Fetch.Credential.bearer: token is not an RFC 6750 b64token") (fun () ->
      ignore (Auth.bearer "two words"));
  List.iter
    (fun (name, user, password) ->
      match Auth.basic ~user ~password with
      | exception Invalid_argument _ -> ()
      | _ -> Alcotest.failf "%s: expected Invalid_argument" name)
    [
      ("colon in Basic user-id", "wrong:user", "secret");
      ("control in user", "user\nname", "secret");
      ("control in password", "user", "sec\027ret");
      ("non-ASCII user", "us\xc3\xa9r", "secret");
      ("non-ASCII password", "user", "s\xc3\xa9cret");
    ]

let test_lazy_invalid_bearer_is_denied () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let _, fetch = server () in
  match
    connect ~sw ~auth:(Auth.refreshing ~refresh:(fun () -> "two words")) fetch
  with
  | Error (Client.Transport (Fetch.Denied _, _)) -> ()
  | Error e ->
      Alcotest.failf "expected Denied, got %s" (Client.error_to_string e)
  | Ok _ -> Alcotest.fail "expected a malformed lazy token to be denied"

(* A refreshing credential is consulted once per request, which is what an
   OAuth access token that expires between them needs. *)
let test_refreshing () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let log, fetch = server () in
  let calls = ref 0 in
  let auth =
    Auth.refreshing ~refresh:(fun () ->
        incr calls;
        Fmt.str "t%d" !calls)
  in
  check_string "unused thunk is not called by pp" "bearer <refreshing>"
    (pp_auth auth);
  let client = connect_exn ~sw ~auth fetch in
  request_exn client;
  request_exn client;
  Alcotest.(check int) "one call per request" 3 !calls;
  check_headers "a fresh token each time"
    [ Some "Bearer t1"; Some "Bearer t2"; Some "Bearer t3" ]
    !log;
  check_string "pp reports the last token seen" "bearer ***" (pp_auth auth)

type Eio.Exn.Backend.t += Credential_store_failure

let refreshing_thunk refresh =
  match Auth.to_credentials (Auth.refreshing ~refresh) with
  | [ Fetch.Credential.Bearer get ] -> get
  | _ -> Alcotest.fail "refreshing did not produce one bearer credential"

let contains ~substring text =
  let length = String.length substring in
  let rec search offset =
    offset + length <= String.length text
    && (String.equal (String.sub text offset length) substring
       || search (offset + 1))
  in
  search 0

let test_refreshing_io_context () =
  let secret = "TOKEN-MUST-NOT-APPEAR" in
  let underlying =
    Eio.Exn.create
      (Eio.Fs.E (Eio.Fs.Permission_denied Credential_store_failure))
  in
  let contextual = Eio.Exn.add_context underlying "reading OAuth token store" in
  let underlying_diagnostic = Fmt.str "%a" Eio.Exn.pp underlying in
  let calls = ref 0 and callback_backtrace = ref None in
  let get =
    refreshing_thunk (fun () ->
        incr calls;
        if !calls = 1 then secret
        else
          try raise contextual
          with exn ->
            let backtrace = Printexc.get_raw_backtrace () in
            callback_backtrace := Some backtrace;
            Printexc.raise_with_backtrace exn backtrace)
  in
  Alcotest.(check string) "first refresh returns the token" secret (get ());
  match get () with
  | exception
      (Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied Credential_store_failure), _)
       as caught) -> (
      let caught_backtrace = Printexc.get_raw_backtrace () in
      let diagnostic = Fmt.str "%a" Eio.Exn.pp caught in
      Alcotest.(check bool)
        "underlying error survives" true
        (contains ~substring:underlying_diagnostic diagnostic);
      Alcotest.(check bool)
        "existing context survives" true
        (contains ~substring:"reading OAuth token store" diagnostic);
      Alcotest.(check bool)
        "credential operation is present" true
        (contains ~substring:"refreshing the JMAP bearer credential" diagnostic);
      Alcotest.(check bool)
        "no token is added" false
        (contains ~substring:secret diagnostic);
      match !callback_backtrace with
      | Some callback_backtrace ->
          let original = Printexc.raw_backtrace_to_string callback_backtrace in
          let caught = Printexc.raw_backtrace_to_string caught_backtrace in
          Alcotest.(check bool)
            "callback backtrace is nonempty" true
            (String.length original > 0);
          Alcotest.(check bool)
            "callback backtrace is the caught trace prefix" true
            (String.starts_with ~prefix:original caught)
      | None -> Alcotest.fail "callback backtrace was not captured")
  | exception Eio.Io _ -> Alcotest.fail "underlying Eio error code changed"
  | exception caught ->
      Alcotest.failf "expected Eio.Io, got %s" (Printexc.to_string caught)
  | _ -> Alcotest.fail "expected credential refresh to fail"

let test_refreshing_other_exceptions_unchanged () =
  let failure = Failure "credential callback failed" in
  let get = refreshing_thunk (fun () -> raise failure) in
  (match get () with
  | exception caught when caught == failure -> ()
  | exception caught ->
      Alcotest.failf "non-I/O exception changed to %s"
        (Printexc.to_string caught)
  | _ -> Alcotest.fail "expected callback failure");
  Eio_mock.Backend.run @@ fun () ->
  match
    Eio.Cancel.sub @@ fun context ->
    let get =
      refreshing_thunk (fun () ->
          Eio.Cancel.cancel context Exit;
          Eio.Fiber.check ();
          "TOKEN")
    in
    get ()
  with
  | exception Eio.Cancel.Cancelled _ -> ()
  | exception caught ->
      Alcotest.failf "cancellation changed to %s" (Printexc.to_string caught)
  | _ -> Alcotest.fail "expected cancellation"

(* {1 Secrets in files} *)

(* The file is named relative to a directory capability, so the same name
   works with and without an [fs]. *)
let with_key_file env name contents f =
  let path = Eio.Path.(Eio.Stdenv.cwd env / name) in
  Eio.Path.save ~create:(`Or_truncate 0o600) path contents;
  Fun.protect ~finally:(fun () -> Eio.Path.unlink path) (fun () -> f name)

(* The first line only, trimmed: a key file usually ends in a newline. *)
let test_bearer_from_file () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_key_file env "test-auth-key.txt" "  FILE-TOKEN  \nignored second line\n"
  @@ fun name ->
  let fs = Eio.Stdenv.cwd env in
  let log, fetch = server () in
  let client = connect_exn ~sw ~auth:(Auth.bearer_from_file ~fs name) fetch in
  request_exn client;
  check_headers "first line, trimmed"
    [ Some "Bearer FILE-TOKEN"; Some "Bearer FILE-TOKEN" ]
    !log

(* Without an [fs] the standard library reads the same file. *)
let test_bearer_from_file_no_fs () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_key_file env "test-auth-native.txt" "NATIVE-TOKEN\n" @@ fun name ->
  let log, fetch = server () in
  let client = connect_exn ~sw ~auth:(Auth.bearer_from_file name) fetch in
  request_exn client;
  check_headers "read with the standard library"
    [ Some "Bearer NATIVE-TOKEN"; Some "Bearer NATIVE-TOKEN" ]
    !log

let test_invalid_bearer_from_file_is_denied () =
  let contains ~needle s =
    let n = String.length needle and length = String.length s in
    let rec at i =
      i + n <= length && (String.sub s i n = needle || at (i + 1))
    in
    at 0
  in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_key_file env "test-auth-invalid-token.txt" "two words\n" @@ fun name ->
  let _, fetch = server () in
  match
    connect ~sw
      ~auth:(Auth.bearer_from_file ~fs:(Eio.Stdenv.cwd env) name)
      fetch
  with
  | Error (Client.Transport (Fetch.Denied msg, _)) ->
      Alcotest.(check bool)
        ("invalid token is named without being exposed: " ^ msg)
        true
        (contains ~needle:name msg && not (contains ~needle:"two words" msg))
  | Error e ->
      Alcotest.failf "expected Denied, got %s" (Client.error_to_string e)
  | Ok _ -> Alcotest.fail "expected a malformed Bearer file to be denied"

let test_basic_from_file () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_key_file env "test-auth-basic.txt" "user1:secret\n" @@ fun name ->
  let fs = Eio.Stdenv.cwd env in
  let log, fetch = server () in
  let auth = Auth.basic_from_file ~fs name in
  let _ = connect_exn ~sw ~auth fetch in
  check_headers "basic from file" [ Some "Basic dXNlcjE6c2VjcmV0" ] !log;
  check_string "the file is named, never printed"
    "basic <file:test-auth-basic.txt>" (pp_auth auth)

let test_basic_from_file_needs_separator () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_key_file env "test-auth-basic-invalid.txt" "user-only\n" @@ fun name ->
  let _, fetch = server () in
  match
    connect ~sw ~auth:(Auth.basic_from_file ~fs:(Eio.Stdenv.cwd env) name) fetch
  with
  | Error (Client.Transport (Fetch.Denied msg, _)) ->
      check_string "separator is named"
        ("the basic credential in " ^ name
       ^ " must contain a user and password separated by ':'")
        msg
  | Error e ->
      Alcotest.failf "expected Denied, got %s" (Client.error_to_string e)
  | Ok _ -> Alcotest.fail "expected a malformed Basic file to be denied"

let test_invalid_basic_from_file_is_denied () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let check name contents =
    with_key_file env name contents @@ fun name ->
    let _, fetch = server () in
    match
      connect ~sw
        ~auth:(Auth.basic_from_file ~fs:(Eio.Stdenv.cwd env) name)
        fetch
    with
    | Error (Client.Transport (Fetch.Denied _, _)) -> ()
    | Error e ->
        Alcotest.failf "expected Denied, got %s" (Client.error_to_string e)
    | Ok _ -> Alcotest.fail "expected a malformed Basic file to be denied"
  in
  check "test-auth-basic-control.txt" "user:sec\027ret\n";
  check "test-auth-basic-nonascii.txt" "user:s\xc3\xa9cret\n"

let test_fifo_is_rejected () =
  Eio_main.run @@ fun env ->
  let name = "test-auth-fifo" in
  Unix.mkfifo name 0o600;
  Fun.protect ~finally:(fun () -> Unix.unlink name) @@ fun () ->
  let rejects source = function
    | Error message ->
        Alcotest.(check bool)
          (source ^ " identifies a non-regular file")
          true
          (String.ends_with ~suffix:"is not a regular file" message)
    | Ok _ -> Alcotest.failf "%s accepted a FIFO" source
  in
  rejects "native" (Auth.read_secret_file name);
  rejects "Eio" (Auth.read_secret_file ~fs:(Eio.Stdenv.cwd env) name)

(* The file is read once and kept, so a token rotated on disk is picked up
   only when the client is told to look again. *)
let test_file_refresh () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let name = "test-auth-rotate.txt" in
  let fs = Eio.Stdenv.cwd env in
  let path = Eio.Path.(fs / name) in
  Eio.Path.save ~create:(`Or_truncate 0o600) path "FIRST\n";
  Fun.protect ~finally:(fun () -> Eio.Path.unlink path) @@ fun () ->
  let log, fetch = server () in
  let auth = Auth.bearer_from_file ~fs name in
  let client = connect_exn ~sw ~auth fetch in
  Eio.Path.save ~create:(`Or_truncate 0o600) path "SECOND\n";
  request_exn client;
  Auth.refresh auth;
  request_exn client;
  check_headers "re-read only after refresh"
    [ Some "Bearer FIRST"; Some "Bearer FIRST"; Some "Bearer SECOND" ]
    !log

(* An unreadable file is a failure of the request that needed it, reported
   like any other refusal to let a request out. *)
let test_missing_file () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.cwd env in
  let _, fetch = server () in
  match
    connect ~sw ~auth:(Auth.bearer_from_file ~fs "test-auth-absent.txt") fetch
  with
  | Ok _ -> Alcotest.fail "expected the missing key file to fail the request"
  | Error (Client.Transport (Fetch.Denied _, _)) -> ()
  | Error e ->
      Alcotest.failf "expected Denied, got %s" (Client.error_to_string e)

let test_loose_file_permissions () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_key_file env "test-auth-loose.txt" "SECRET\n" @@ fun name ->
  Unix.chmod name 0o644;
  let _, fetch = server () in
  match
    connect ~sw
      ~auth:(Auth.bearer_from_file ~fs:(Eio.Stdenv.cwd env) name)
      fetch
  with
  | Error (Client.Transport (Fetch.Denied msg, _)) ->
      check_string "permissions are named"
        "the credential file test-auth-loose.txt has permissions 644; remove \
         group and other access (for example, chmod 600)"
        msg
  | Error e ->
      Alcotest.failf "expected Denied, got %s" (Client.error_to_string e)
  | Ok _ -> Alcotest.fail "expected a group/world-readable key to be denied"

let test_secret_file_bounds () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.cwd env in
  let maximum = String.make (64 * 1024) 'A' ^ "\nignored" in
  with_key_file env "test-auth-maximum.txt" maximum @@ fun name ->
  let check_maximum source = function
    | Ok secret ->
        Alcotest.(check int)
          (source ^ " boundary accepted")
          (64 * 1024) (String.length secret)
    | Error message -> Alcotest.failf "%s boundary rejected: %s" source message
  in
  check_maximum "Eio" (Auth.read_secret_file ~fs name);
  check_maximum "native" (Auth.read_secret_file name);
  let oversized = String.make ((64 * 1024) + 1) 'A' ^ "\n" in
  with_key_file env "test-auth-oversized.txt" oversized @@ fun name ->
  let check_too_large = function
    | Error message ->
        Alcotest.(check bool)
          ("size is reported: " ^ message)
          true
          (String.starts_with ~prefix:"the first line" message)
    | Ok _ -> Alcotest.fail "expected an oversized credential to be refused"
  in
  check_too_large (Auth.read_secret_file ~fs name);
  check_too_large (Auth.read_secret_file name)

(* {!Auth.pp} never opens the file, so it is neither a cancellation point nor
   a way for an unreadable file to fail a log line. *)
let test_pp_file () =
  Eio_main.run @@ fun env ->
  with_key_file env "test-auth-print.txt" "SECRET-TOKEN\n" @@ fun name ->
  let fs = Eio.Stdenv.cwd env in
  let auth = Auth.bearer_from_file ~fs name in
  check_string "a readable file is named rather than read"
    "bearer <file:test-auth-print.txt>" (pp_auth auth);
  check_string "and so is an unreadable one"
    "bearer <file:test-auth-no-such-file.txt>"
    (pp_auth (Auth.bearer_from_file ~fs "test-auth-no-such-file.txt"));
  check_string "reading it first does not change what pp says"
    "bearer <file:test-auth-print.txt>"
    (Eio.Switch.run (fun sw ->
         let _, fetch = server () in
         let _ = connect_exn ~sw ~auth fetch in
         pp_auth auth));
  (* Printing inside a cancelled context is not a cancellation point. *)
  match
    Eio.Cancel.sub (fun cc ->
        Eio.Cancel.cancel cc Exit;
        pp_auth auth)
  with
  | s ->
      check_string "even in a cancelled fiber"
        "bearer <file:test-auth-print.txt>" s
  | exception Eio.Cancel.Cancelled _ ->
      Alcotest.fail "pp must not be a cancellation point"

let test_pp_file_escapes_controls () =
  let printed = pp_auth (Auth.bearer_from_file "key\n\027[2J") in
  check_string "control bytes are visible" "bearer <file:key\\x0A\\x1B[2J>"
    printed;
  Alcotest.(check bool) "no newline" false (String.contains printed '\n');
  Alcotest.(check bool) "no escape byte" false (String.contains printed '\027')

(* {1 Redaction} *)

let test_pp () =
  check_string "none" "none" (pp_auth Auth.none);
  check_string "long token" "bearer TOKE***"
    (pp_auth (Auth.bearer "TOKEN-abcdef"));
  check_string "short token" "bearer ***" (pp_auth (Auth.bearer "abc"));
  check_string "basic keeps the user" "basic user1:secr***"
    (pp_auth (Auth.basic ~user:"user1" ~password:"secret"));
  check_string "short password" "basic user1:***"
    (pp_auth (Auth.basic ~user:"user1" ~password:"x"))

(* {1 The environment} *)

let vars =
  [
    "JMAP_API_KEY";
    "JMAP_API_KEY_FILE";
    "JMAP_AUTH";
    "JMAP_SESSION_URL";
    "JMAP_PROFILE";
    "ORACLE_API_KEY";
  ]

(* [Unix.putenv] cannot unset, and {!Auth.of_env} treats an empty setting as
   absent, so "" is how a variable is cleared here. *)
let with_env bindings f =
  let saved =
    List.map (fun v -> (v, Option.value ~default:"" (Sys.getenv_opt v))) vars
  in
  let set = List.iter (fun (name, value) -> Unix.putenv name value) in
  set (List.map (fun v -> (v, "")) vars);
  set bindings;
  Fun.protect ~finally:(fun () -> set saved) f

let of_env_exn ?prefix ?fs () =
  match Auth.of_env ?prefix ?fs () with
  | Ok (Some auth) -> auth
  | Ok None -> Alcotest.fail "expected a credential"
  | Error msg -> Alcotest.failf "expected a credential, got %S" msg

let of_env_error ?prefix () =
  match Auth.of_env ?prefix () with
  | Error msg -> msg
  | Ok None -> Alcotest.fail "expected an error, got no credential"
  | Ok (Some auth) -> Alcotest.failf "expected an error, got %s" (pp_auth auth)

let test_basic_settings_are_validated () =
  let check_scheme key =
    match
      Auth.of_scheme ~key_name:"KEY" ~auth_name:"AUTH=basic" Auth.Basic
        (`Key key)
    with
    | Error _ -> ()
    | Ok _ -> Alcotest.failf "of_scheme accepted %S" key
  in
  check_scheme "user:sec\nret";
  check_scheme "us\xc3\xa9r:secret";
  List.iter
    (fun key ->
      with_env
        [ ("JMAP_AUTH", "basic"); ("JMAP_API_KEY", key) ]
        (fun () -> ignore (of_env_error ())))
    [ "user:sec\027ret"; "user:s\xc3\xa9cret" ]

let test_of_env () =
  with_env [] (fun () ->
      Alcotest.(check bool) "nothing set" true (Auth.of_env () = Ok None));
  with_env
    [ ("JMAP_API_KEY", "TOKEN-abcdef") ]
    (fun () ->
      check_string "bearer by default" "bearer TOKE***"
        (pp_auth (of_env_exn ())));
  with_env
    [ ("JMAP_API_KEY", "user1:secret"); ("JMAP_AUTH", "basic") ]
    (fun () ->
      check_string "basic" "basic user1:secr***" (pp_auth (of_env_exn ())));
  with_env
    [ ("ORACLE_API_KEY", "TOKEN-abcdef") ]
    (fun () ->
      check_string "prefix" "bearer TOKE***"
        (pp_auth (of_env_exn ~prefix:"ORACLE" ())));
  with_env
    [ ("JMAP_API_KEY", "TOKEN-abcdef"); ("JMAP_AUTH", "digest") ]
    (fun () ->
      check_string "unknown scheme"
        "JMAP_AUTH: expected \"bearer\" or \"basic\", got \"digest\""
        (of_env_error ()));
  with_env
    [ ("JMAP_API_KEY", "nocolon"); ("JMAP_AUTH", "basic") ]
    (fun () ->
      Alcotest.(check bool)
        "basic needs a colon" true
        (String.length (of_env_error ()) > 0));
  with_env
    [ ("JMAP_API_KEY", "two words") ]
    (fun () ->
      check_string "bearer is validated"
        "JMAP_API_KEY: with JMAP_AUTH=bearer the key is not a valid Bearer \
         token"
        (of_env_error ()));
  with_env
    [ ("JMAP_API_KEY", "user:sec\027ret"); ("JMAP_AUTH", "basic") ]
    (fun () ->
      check_string "the scheme names itself"
        "JMAP_API_KEY: with JMAP_AUTH=basic the key is not a valid Basic \
         credential"
        (of_env_error ()))

(* The key file wins over a direct key, and is resolved against [fs]. *)
let test_of_env_file () =
  Eio_main.run @@ fun env ->
  with_key_file env "test-auth-env.txt" "FILE-TOKEN\n" @@ fun name ->
  with_env
    [ ("JMAP_API_KEY", "ignored"); ("JMAP_API_KEY_FILE", name) ]
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      let auth = of_env_exn ~fs:(Eio.Stdenv.cwd env) () in
      check_string "the file is named rather than read"
        ("bearer <file:" ^ name ^ ">")
        (pp_auth auth);
      let log, fetch = server () in
      let _ = connect_exn ~sw ~auth fetch in
      check_headers "and read for the request" [ Some "Bearer FILE-TOKEN" ] !log;
      let log, fetch = server () in
      let _ = connect_exn ~sw ~auth:(of_env_exn ()) fetch in
      check_headers "the same without an fs capability"
        [ Some "Bearer FILE-TOKEN" ]
        !log)

(* {1 The command line reads the same settings} *)

(* [Cmdliner] wraps and indents what it prints, so the message is compared
   with its whitespace collapsed. *)
let normalise s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      let c = match c with '\n' | '\t' -> ' ' | c -> c in
      let doubled =
        c = ' '
        && (Buffer.length b = 0 || Buffer.nth b (Buffer.length b - 1) = ' ')
      in
      if not doubled then Buffer.add_char b c)
    s;
  String.trim (Buffer.contents b)

let contains ~needle s =
  let n = String.length needle and len = String.length s in
  let rec at i = i + n <= len && (String.sub s i n = needle || at (i + 1)) in
  at 0

let cli_error () =
  let buf = Buffer.create 256 in
  let err = Format.formatter_of_buffer buf in
  let cmd = Cmdliner.Cmd.v (Cmdliner.Cmd.info "jmap") Cli.config_term in
  ignore (Cmdliner.Cmd.eval_value ~argv:[| "jmap" |] ~err ~help:err cmd);
  Format.pp_print_flush err ();
  normalise (Buffer.contents buf)

(* A malformed setting is refused in the same words whether {!Auth.of_env} or
   {!Cli.config_term} read it. *)
let test_cli_agrees_with_of_env () =
  let check name bindings =
    with_env (("JMAP_SESSION_URL", well_known) :: bindings) (fun () ->
        let expected = normalise (of_env_error ()) in
        let got = cli_error () in
        Alcotest.(check bool)
          (Fmt.str "%s: %S in %S" name expected got)
          true
          (contains ~needle:expected got))
  in
  check "unknown scheme" [ ("JMAP_API_KEY", "TOKEN"); ("JMAP_AUTH", "digest") ];
  check "basic needs a colon"
    [ ("JMAP_API_KEY", "nocolon"); ("JMAP_AUTH", "basic") ]

(* {1 Transport and deadlines} *)

(* A transport over a mock backend carries no clock, so asking for a
   deadline on it is a programming error rather than a silent no-op. *)
let test_timeout_needs_a_clock () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let _, fetch = server () in
  let transport = Transport.of_fetch fetch in
  Alcotest.(check bool) "no clock" true (Transport.clock transport = None);
  Alcotest.check_raises "timeout without a clock"
    (Invalid_argument
       "Client.connect: ?timeout needs a transport that carries a clock")
    (fun () -> ignore (Client.connect ~sw ~timeout:5. transport well_known))

let test_client_bounds_are_validated () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let _, fetch = server () in
  let transport = Transport.of_fetch fetch in
  let invalid_timeout value =
    Alcotest.check_raises "invalid timeout"
      (Invalid_argument
         "Client.connect: ?timeout must be finite and non-negative") (fun () ->
        ignore (Client.connect ~sw ~timeout:value transport well_known))
  in
  invalid_timeout (-1.);
  invalid_timeout Float.nan;
  invalid_timeout Float.infinity;
  let invalid_body value =
    Alcotest.check_raises "invalid body limit"
      (Invalid_argument "Client.connect: ?max_body must be positive") (fun () ->
        ignore (Client.connect ~sw ~max_body:value transport well_known))
  in
  invalid_body (-1);
  invalid_body 0

(* A server that never answers is cancelled at the deadline and reported as
   {!Client.Timeout}. The mock clock advances on its own once every fiber is
   blocked, so the test does not wait. *)
let test_timeout () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#clock in
  let _, fetch = server ~delay:(fun () -> Eio.Time.sleep clock 3600.) () in
  match connect ~sw ~timeout:5. ~clock fetch with
  | Ok _ -> Alcotest.fail "expected the session fetch to time out"
  | Error (Client.Timeout seconds) ->
      Alcotest.(check (float 1e-9)) "deadline" 5. seconds
  | Error e ->
      Alcotest.failf "expected Timeout, got %s" (Client.error_to_string e)

(* One deadline covers the whole session fetch, redirects included: three
   hops of three seconds must not be allowed nine. *)
let test_timeout_spans_redirects () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#clock in
  let hops = ref 0 in
  let handler (req : Fetch.Middleware.request) =
    Eio.Time.sleep clock 3.;
    match Fetch.Middleware.Url.path_and_query req.url with
    | "/hop3" -> Fetch_mock.respond ~headers:json_headers session_json req
    | _ ->
        incr hops;
        Fetch_mock.respond ~status:301
          ~headers:
            (Http.Header.of_list
               [ ("location", Printf.sprintf "/hop%d" !hops) ])
          "" req
  in
  match connect ~sw ~timeout:5. ~clock (Fetch_mock.client handler) with
  | Ok _ -> Alcotest.fail "expected the redirect walk to time out"
  | Error (Client.Timeout seconds) ->
      Alcotest.(check (float 1e-9)) "deadline" 5. seconds;
      (* A per-hop deadline would have allowed all three hops. *)
      Alcotest.(check int) "the walk stopped part way" 1 !hops
  | Error e ->
      Alcotest.failf "expected Timeout, got %s" (Client.error_to_string e)

(* The deadline is per exchange, so a client that answered once still has
   its full allowance for the next request. *)
let test_timeout_per_exchange () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#clock in
  let stall = ref false in
  let _, fetch =
    server ~delay:(fun () -> if !stall then Eio.Time.sleep clock 3600.) ()
  in
  let client =
    match connect ~sw ~timeout:5. ~clock fetch with
    | Ok client -> client
    | Error e -> Alcotest.failf "connect failed: %s" (Client.error_to_string e)
  in
  request_exn client;
  stall := true;
  match Client.request client echo_request with
  | Error (Client.Timeout 5.) -> ()
  | Error e ->
      Alcotest.failf "expected Timeout, got %s" (Client.error_to_string e)
  | Ok _ -> Alcotest.fail "expected the request to time out"

let () =
  Alcotest.run "jmap-eio-auth"
    [
      ( "auth",
        [
          Alcotest.test_case "bearer" `Quick test_bearer;
          Alcotest.test_case "basic" `Quick test_basic;
          Alcotest.test_case "constant credential validation" `Quick
            test_constant_credentials_are_validated;
          Alcotest.test_case "Basic settings are validated" `Quick
            test_basic_settings_are_validated;
          Alcotest.test_case "lazy invalid bearer is denied" `Quick
            test_lazy_invalid_bearer_is_denied;
          Alcotest.test_case "refreshing" `Quick test_refreshing;
          Alcotest.test_case "refreshing I/O context" `Quick
            test_refreshing_io_context;
          Alcotest.test_case "refreshing exception propagation" `Quick
            test_refreshing_other_exceptions_unchanged;
          Alcotest.test_case "bearer from file" `Quick test_bearer_from_file;
          Alcotest.test_case "bearer from a native file" `Quick
            test_bearer_from_file_no_fs;
          Alcotest.test_case "invalid bearer file is denied" `Quick
            test_invalid_bearer_from_file_is_denied;
          Alcotest.test_case "basic from file" `Quick test_basic_from_file;
          Alcotest.test_case "basic file needs a separator" `Quick
            test_basic_from_file_needs_separator;
          Alcotest.test_case "invalid Basic file is denied" `Quick
            test_invalid_basic_from_file_is_denied;
          Alcotest.test_case "FIFOs are rejected" `Quick test_fifo_is_rejected;
          Alcotest.test_case "file refresh" `Quick test_file_refresh;
          Alcotest.test_case "missing file" `Quick test_missing_file;
          Alcotest.test_case "loose file permissions" `Quick
            test_loose_file_permissions;
          Alcotest.test_case "secret file bounds" `Quick test_secret_file_bounds;
          Alcotest.test_case "pp of a file credential" `Quick test_pp_file;
          Alcotest.test_case "pp escapes file controls" `Quick
            test_pp_file_escapes_controls;
          Alcotest.test_case "redaction" `Quick test_pp;
          Alcotest.test_case "of_env" `Quick test_of_env;
          Alcotest.test_case "of_env key file" `Quick test_of_env_file;
          Alcotest.test_case "the command line agrees with of_env" `Quick
            test_cli_agrees_with_of_env;
        ] );
      ( "transport",
        [
          Alcotest.test_case "client bounds are validated" `Quick
            test_client_bounds_are_validated;
          Alcotest.test_case "timeout needs a clock" `Quick
            test_timeout_needs_a_clock;
          Alcotest.test_case "one deadline spans the redirects" `Quick
            test_timeout_spans_redirects;
          Alcotest.test_case "timeout" `Quick test_timeout;
          Alcotest.test_case "timeout per exchange" `Quick
            test_timeout_per_exchange;
        ] );
    ]
