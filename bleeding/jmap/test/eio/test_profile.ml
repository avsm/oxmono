(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Jmap_eio

let fail_error label error =
  Alcotest.failf "%s: %s" label (Profile.error_to_string error)

let get_ok label = function
  | Ok value -> value
  | Error error -> fail_error label error

let bearer ?(name = "work") ?(token = "TOKEN") () =
  Profile.v ~name ~session_url:"https://api.example.com/.well-known/jmap"
    (Profile.Bearer token)
  |> get_ok "construct bearer profile"

let with_store f =
  Eio_main.run @@ fun env ->
  let root = Filename.temp_dir "jmap-profile-" "-store" in
  let root_path = Eio.Path.(Eio.Stdenv.fs env / root) in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root_path)
    (fun () ->
      let directory = Filename.concat root "profiles" in
      let store = Profile.of_directory ~fs:(Eio.Stdenv.fs env) directory in
      f env store directory)

let permissions path = (Unix.stat path).Unix.st_perm land 0o777

let contains ~substring value =
  let substring_length = String.length substring in
  let rec search offset =
    offset + substring_length <= String.length value
    && (String.sub value offset substring_length = substring
       || search (offset + 1))
  in
  search 0

let check_bearer expected profile =
  match Profile.credential profile with
  | Profile.Bearer token ->
      Alcotest.(check string) "bearer token" expected token
  | Profile.Basic _ -> Alcotest.fail "loaded a basic profile"

let test_round_trip () =
  with_store @@ fun env store directory ->
  Alcotest.(check int)
    "missing store is empty" 0
    (List.length (Profile.list store |> get_ok "list missing store"));
  let original = bearer () in
  Profile.save store original |> get_ok "save profile";
  let file = Filename.concat directory "work" in
  Alcotest.(check int) "private directory" 0o700 (permissions directory);
  Alcotest.(check int) "private file" 0o600 (permissions file);
  let loaded = Profile.load store "work" |> get_ok "load profile" in
  Alcotest.(check string) "name" "work" (Profile.name loaded);
  Alcotest.(check string)
    "session URL" "https://api.example.com/.well-known/jmap"
    (Profile.session_url loaded);
  check_bearer "TOKEN" loaded;
  let shown = Fmt.str "%a" Profile.pp loaded in
  Alcotest.(check bool)
    "printer redacts the token" true
    (not (String.contains shown 'N'));
  let profiles = Profile.list store |> get_ok "list profiles" in
  Alcotest.(check (list string))
    "listed names" [ "work" ]
    (List.map Profile.name profiles);
  Profile.save store (bearer ~name:"alpha" ()) |> get_ok "save alpha profile";
  Profile.save store (bearer ~name:"zulu" ()) |> get_ok "save zulu profile";
  Alcotest.(check (list string))
    "profiles are sorted"
    [ "alpha"; "work"; "zulu" ]
    (Profile.list store
    |> get_ok "list sorted profiles"
    |> List.map Profile.name);
  let replacement = bearer ~token:"SECOND" () in
  Profile.save store replacement |> get_ok "replace profile";
  Profile.load store "work" |> get_ok "load replacement"
  |> check_bearer "SECOND";
  Alcotest.(check (list string))
    "no temporary file remains"
    [ "alpha"; "work"; "zulu" ]
    (Sys.readdir directory |> Array.to_list |> List.sort String.compare);
  let oversized = bearer ~token:(String.make (64 * 1024) 'A') () in
  (match Profile.save store oversized with
  | Error (Profile.Invalid_profile _) -> ()
  | Error error -> fail_error "wrong oversized error" error
  | Ok () -> Alcotest.fail "saved an oversized profile");
  Profile.load store "work"
  |> get_ok "old profile survived"
  |> check_bearer "SECOND";
  Unix.chmod file 0o644;
  (match Profile.load store "work" with
  | Error (Profile.Storage_error _) -> ()
  | Error error -> fail_error "wrong permissions error" error
  | Ok _ -> Alcotest.fail "loaded a publicly readable profile");
  Alcotest.(check (list string))
    "unsafe profile omitted from list" [ "alpha"; "zulu" ]
    (Profile.list store |> get_ok "list unsafe store" |> List.map Profile.name);
  let too_large = Filename.concat directory "too-large" in
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(Eio.Stdenv.fs env / too_large)
    (String.make ((64 * 1024) + 1) 'A');
  match Profile.load store "too-large" with
  | Error (Profile.Invalid_profile _) -> ()
  | Error error -> fail_error "wrong file-bound error" error
  | Ok _ -> Alcotest.fail "loaded a profile larger than 64 KiB"

let test_public_store_is_rejected () =
  with_store @@ fun env store directory ->
  let path = Eio.Path.(Eio.Stdenv.fs env / directory) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 path;
  Unix.chmod directory 0o755;
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(path / "work")
    "url=https://example.test\nauth=bearer\nuser=\nsecret=TOKEN\n";
  let rejects label action = function
    | Error (Profile.Storage_error { path; message }) ->
        Alcotest.(check string) (label ^ " identifies store") directory path;
        Alcotest.(check bool)
          (label ^ " explains permissions")
          true
          (String.starts_with ~prefix:"profile store has permissions 755;"
             message);
        Alcotest.(check bool)
          (label ^ " identifies action")
          true
          (contains ~substring:action message)
    | Error error -> fail_error (label ^ " wrong error") error
    | Ok _ -> Alcotest.failf "%s accepted a public store" label
  in
  rejects "load" "loading profile work" (Profile.load store "work");
  rejects "list" "listing JMAP profiles" (Profile.list store);
  rejects "save" "saving profile new"
    (Profile.save store (bearer ~name:"new" ()));
  Alcotest.(check int) "mode unchanged" 0o755 (permissions directory);
  Alcotest.(check bool)
    "no profile written" false
    (Sys.file_exists (Filename.concat directory "new"))

let test_storage_error_context () =
  with_store @@ fun env store directory ->
  let store_path = Eio.Path.(Eio.Stdenv.fs env / directory) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 store_path;
  (match Profile.load store "missing" with
  | Error (Profile.Storage_error { path; message }) ->
      Alcotest.(check string)
        "missing profile path"
        (Filename.concat directory "missing")
        path;
      Alcotest.(check bool)
        "keeps open cause" true
        (contains ~substring:"open" message);
      Alcotest.(check bool)
        "adds load action" true
        (contains ~substring:"loading profile missing" message)
  | Error error -> fail_error "wrong missing-profile error" error
  | Ok _ -> Alcotest.fail "loaded a missing profile");
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(store_path / "blocked");
  (match Profile.save store (bearer ~name:"blocked" ()) with
  | Error (Profile.Storage_error { path; message }) ->
      Alcotest.(check string)
        "failed destination path"
        (Filename.concat directory "blocked")
        path;
      Alcotest.(check bool)
        "keeps rename cause" true
        (contains ~substring:"rename" message);
      Alcotest.(check bool)
        "adds save action" true
        (contains ~substring:"saving profile blocked" message)
  | Error error -> fail_error "wrong blocked-save error" error
  | Ok () -> Alcotest.fail "replaced a directory with a profile");
  Alcotest.(check (list string))
    "temporary profile cleaned up" [ "blocked" ]
    (Sys.readdir directory |> Array.to_list |> List.sort String.compare)

let test_basic_and_validation () =
  with_store @@ fun _env store _directory ->
  let basic =
    Profile.v ~name:"personal" ~session_url:"  https://example.test/jmap  "
      (Profile.Basic { user = "  alice  "; password = "p:a ss" })
    |> get_ok "construct basic profile"
  in
  Profile.save store basic |> get_ok "save basic profile";
  let loaded = Profile.load store "personal" |> get_ok "load basic profile" in
  Alcotest.(check string)
    "trimmed URL" "https://example.test/jmap"
    (Profile.session_url loaded);
  (match Profile.credential loaded with
  | Profile.Basic { user; password } ->
      Alcotest.(check string) "basic user" "alice" user;
      Alcotest.(check string) "basic password" "p:a ss" password
  | Profile.Bearer _ -> Alcotest.fail "loaded a bearer profile");
  let rejects label result =
    match result with
    | Error (Profile.Invalid_profile _) -> ()
    | Error error -> fail_error ("wrong validation error for " ^ label) error
    | Ok _ -> Alcotest.failf "accepted %s" label
  in
  rejects "a path as a name"
    (Profile.v ~name:"../work" ~session_url:"https://example.test"
       (Profile.Bearer "TOKEN"));
  rejects "a multiline password"
    (Profile.v ~name:"work" ~session_url:"https://example.test"
       (Profile.Basic { user = "alice"; password = "one\ntwo" }));
  rejects "a control in the session URL"
    (Profile.v ~name:"work" ~session_url:"https://example.test/\027[2J"
       (Profile.Bearer "TOKEN"));
  rejects "DEL in a password"
    (Profile.v ~name:"work" ~session_url:"https://example.test"
       (Profile.Basic { user = "alice"; password = "one\127two" }));
  rejects "a Unicode C1 control in a password"
    (Profile.v ~name:"work" ~session_url:"https://example.test"
       (Profile.Basic { user = "alice"; password = "one\u{0085}two" }));
  rejects "an invalid bearer token"
    (Profile.v ~name:"work" ~session_url:"https://example.test"
       (Profile.Bearer "two words"))

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
  "accounts": {},
  "primaryAccounts": {},
  "username": "profile@example.com",
  "apiUrl": "https://api.example.com/jmap/api/",
  "downloadUrl": "https://api.example.com/jmap/download/{accountId}/{blobId}/{name}?type={type}",
  "uploadUrl": "https://api.example.com/jmap/upload/{accountId}/",
  "eventSourceUrl": "https://api.example.com/jmap/events/?types={types}&closeafter={closeafter}&ping={ping}",
  "state": "one"
}|}

let test_connect_name () =
  with_store @@ fun env store _directory ->
  Profile.save store (bearer ()) |> get_ok "save connection profile";
  let authorization = ref None in
  let handler (request : Fetch.Middleware.request) =
    authorization := Http.Header.get request.headers "authorization";
    Fetch_mock.respond
      ~headers:(Http.Header.of_list [ ("content-type", "application/json") ])
      session_json request
  in
  let transport = Transport.of_fetch (Fetch_mock.client handler) in
  Eio.Switch.run @@ fun sw ->
  let client =
    Profile.connect_name ~sw ~store ~transport env "work"
    |> get_ok "connect selected profile"
  in
  Alcotest.(check string)
    "fetched session" "profile@example.com" (Client.session client).username;
  Alcotest.(check (option string))
    "profile credential" (Some "Bearer TOKEN") !authorization

let test_connect_error_context () =
  with_store @@ fun env _store _directory ->
  let profile = bearer ~name:"safe-name" ~token:"SUPERSECRET" () in
  let handler _request = raise (Fetch.err (Fetch.Denied "fixture denial")) in
  let transport = Transport.of_fetch (Fetch_mock.client handler) in
  Eio.Switch.run @@ fun sw ->
  match Profile.connect ~sw ~transport env profile with
  | Error
      (Profile.Connection_error
         (Client.Transport (Fetch.Denied reason, message))) ->
      Alcotest.(check string) "typed transport cause" "fixture denial" reason;
      Alcotest.(check bool)
        "connection names profile" true
        (contains ~substring:"connecting profile safe-name" message);
      Alcotest.(check bool)
        "connection omits secret" false
        (contains ~substring:"SUPERSECRET" message)
  | Error error -> fail_error "wrong connection error" error
  | Ok _ -> Alcotest.fail "connected through a denied transport"

let test_xdg_store () =
  Eio_main.run @@ fun env ->
  let root = Filename.temp_dir "jmap-profile-" "-xdg" in
  let previous = Sys.getenv_opt "XDG_CONFIG_HOME" in
  let restore () =
    (match previous with
    | Some value -> Unix.putenv "XDG_CONFIG_HOME" value
    | None -> Unix.putenv "XDG_CONFIG_HOME" "");
    Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)
  in
  Fun.protect ~finally:restore @@ fun () ->
  Unix.putenv "XDG_CONFIG_HOME" root;
  let store = Profile.xdg_store env |> get_ok "resolve XDG store" in
  Alcotest.(check string)
    "shared directory"
    (List.fold_left Filename.concat root [ "jmap"; "profiles" ])
    (Profile.directory store)

let cli_eval args =
  let output = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer output in
  let command =
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "profile-test") Cli.config_term
  in
  let result =
    Cmdliner.Cmd.eval_value
      ~argv:(Array.of_list ("profile-test" :: args))
      ~err:ppf ~help:ppf command
  in
  Format.pp_print_flush ppf ();
  (result, Buffer.contents output)

let cli_config label args =
  match cli_eval args with
  | Ok (`Ok config), _ -> config
  | _, output -> Alcotest.failf "%s: expected a config, got %S" label output

let test_cli_profile () =
  Eio_main.run @@ fun env ->
  let root = Filename.temp_dir "jmap-profile-" "-cli" in
  let vars =
    [
      "XDG_CONFIG_HOME";
      "JMAP_SESSION_URL";
      "JMAP_API_KEY";
      "JMAP_API_KEY_FILE";
      "JMAP_AUTH";
      "JMAP_ACCOUNT_ID";
      "JMAP_PROFILE";
    ]
  in
  let saved =
    List.map
      (fun name -> (name, Option.value ~default:"" (Sys.getenv_opt name)))
      vars
  in
  let set = List.iter (fun (name, value) -> Unix.putenv name value) in
  let root_path = Eio.Path.(Eio.Stdenv.fs env / root) in
  Fun.protect
    ~finally:(fun () ->
      set saved;
      Eio.Path.rmtree ~missing_ok:true root_path)
    (fun () ->
      set (List.map (fun name -> (name, "")) vars);
      Unix.putenv "XDG_CONFIG_HOME" root;
      let store = Profile.xdg_store env |> get_ok "CLI XDG store" in
      let profile =
        Profile.v ~name:"shared" ~session_url:"https://profile.example/jmap"
          (Profile.Basic { user = "alice"; password = "secret" })
        |> get_ok "construct CLI profile"
      in
      Profile.save store profile |> get_ok "save CLI profile";
      let raw = cli_config "profile-only command" [ "--profile"; "shared" ] in
      Alcotest.(check (option string))
        "selected profile" (Some "shared") raw.Cli.profile;
      Alcotest.(check bool)
        "command-line profile source" true
        (raw.profile_source = Cli.Cmdline);
      Alcotest.(check string) "URL deferred" "" raw.session_url;
      let resolved =
        match Cli.resolve env raw with
        | Ok config -> config
        | Error message -> Alcotest.failf "resolve profile: %s" message
      in
      Alcotest.(check string)
        "profile URL" "https://profile.example/jmap" resolved.session_url;
      Alcotest.(check string) "profile key" "alice:secret" resolved.api_key;
      Alcotest.(check bool) "profile auth" true (resolved.auth = Cli.Basic);
      Alcotest.(check bool)
        "profile URL source" true
        (resolved.session_url_source = Cli.Profile "shared");
      Alcotest.(check bool)
        "profile key source" true
        (resolved.api_key_source = Cli.Profile "shared");
      let auth =
        match Cli.auth_value resolved with
        | Ok auth -> auth
        | Error message -> Alcotest.failf "profile auth: %s" message
      in
      Alcotest.(check string)
        "resolved authentication" "basic alice:secr***"
        (Fmt.str "%a" Auth.pp auth);
      let direct =
        cli_config "direct override"
          [
            "--profile";
            "shared";
            "--url";
            "https://direct.example/jmap";
            "--api-key";
            "DIRECT";
          ]
      in
      let direct =
        match Cli.resolve env direct with
        | Ok config -> config
        | Error message -> Alcotest.failf "resolve override: %s" message
      in
      Alcotest.(check string)
        "direct URL wins" "https://direct.example/jmap" direct.session_url;
      Alcotest.(check string) "direct key wins" "DIRECT" direct.api_key;
      Alcotest.(check bool) "direct auth default" true (direct.auth = Cli.Bearer);
      Unix.putenv "JMAP_PROFILE" "shared";
      let from_env = cli_config "environment profile" [] in
      Alcotest.(check bool)
        "environment source" true
        (from_env.profile_source = Cli.Env "JMAP_PROFILE");
      match cli_eval [ "--profile"; "../escape" ] with
      | Error `Term, _ -> ()
      | _, output ->
          Alcotest.failf "unsafe profile name was accepted: %S" output)

let () =
  Alcotest.run "jmap profiles"
    [
      ( "store",
        [
          Alcotest.test_case "round trip and safety" `Quick test_round_trip;
          Alcotest.test_case "public store is rejected" `Quick
            test_public_store_is_rejected;
          Alcotest.test_case "storage error context" `Quick
            test_storage_error_context;
          Alcotest.test_case "basic and validation" `Quick
            test_basic_and_validation;
          Alcotest.test_case "XDG location" `Quick test_xdg_store;
          Alcotest.test_case "CLI selection" `Quick test_cli_profile;
        ] );
      ( "connection",
        [
          Alcotest.test_case "selected profile" `Quick test_connect_name;
          Alcotest.test_case "error context" `Quick test_connect_error_context;
        ] );
    ]
