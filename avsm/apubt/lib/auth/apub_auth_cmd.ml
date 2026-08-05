(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Common Arguments *)

let actor_uri_arg =
  let doc = "Actor URI (e.g., https://mastodon.social/users/alice)." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"ACTOR_URI" ~doc)

let key_file_arg =
  let doc = "Path to PEM file containing the private RSA key." in
  Arg.(
    value & opt (some file) None & info [ "key-file"; "k" ] ~docv:"FILE" ~doc)

let key_id_arg =
  let doc =
    "Key ID (default: <actor_uri>#main-key). Usually the actor's publicKey.id."
  in
  Arg.(
    value & opt (some string) None & info [ "key-id"; "K" ] ~docv:"URI" ~doc)

let profile_arg =
  let doc =
    "Profile name (default: derived from actor URI, e.g., alice@example.com)."
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "profile"; "P" ] ~docv:"PROFILE" ~doc)

(* Setup command - import a key for an actor *)

let setup_action ~app_name ~actor_uri ~key_file ~key_id ~profile env =
  let fs = env#fs in
  let key_file =
    match key_file with
    | Some f -> f
    | None ->
        Fmt.pr "Key file path: @?";
        read_line ()
  in
  (* Read the private key *)
  let private_key_pem =
    try In_channel.with_open_bin key_file In_channel.input_all
    with Sys_error e ->
      Fmt.epr "Error reading key file: %s@." e;
      exit 1
  in
  (* Validate it's a valid PEM key *)
  (match X509.Private_key.decode_pem private_key_pem with
  | Ok _ -> ()
  | Error (`Msg e) ->
      Fmt.epr "Error: Invalid PEM key: %s@." e;
      exit 1);
  (* Derive key_id if not provided *)
  let key_id =
    match key_id with Some k -> k | None -> actor_uri ^ "#main-key"
  in
  (* Derive profile name if not provided *)
  let profile_name =
    match profile with
    | Some p -> p
    | None -> Apub_auth_session.profile_name_of_actor_uri actor_uri
  in
  (* Create and save session *)
  let session =
    Apub_auth_session.create ~actor_uri ~key_id ~private_key_pem
  in
  Apub_auth_session.save fs ~app_name ~profile:profile_name session;
  (* Set as current profile if first setup or explicitly requested *)
  let profiles = Apub_auth_session.list_profiles fs ~app_name in
  if List.length profiles <= 1 || Option.is_some profile then
    Apub_auth_session.set_current_profile fs ~app_name profile_name;
  Fmt.pr "Saved actor credentials (profile: %s)@." profile_name;
  Fmt.pr "  Actor: %s@." actor_uri;
  Fmt.pr "  Key ID: %s@." key_id

let setup_cmd ~app_name () =
  let doc = "Setup actor credentials from a PEM key file." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Import an existing RSA private key for an ActivityPub actor. The key \
         is stored locally and used for HTTP signature authentication.";
      `S Manpage.s_examples;
      `Pre
        "  apub auth setup https://example.com/users/alice -k \
         ~/.config/apub/key.pem";
      `Pre
        "  apub auth setup https://mastodon.social/users/bob --profile work";
    ]
  in
  let info = Cmd.info "setup" ~doc ~man in
  let setup' actor_uri key_file key_id profile =
    Eio_main.run @@ fun env ->
    setup_action ~app_name ~actor_uri ~key_file ~key_id ~profile env
  in
  Cmd.v info
    Term.(const setup' $ actor_uri_arg $ key_file_arg $ key_id_arg $ profile_arg)

(* Login command - OAuth login with Mastodon instance *)

let account_arg =
  let doc = "Account handle (e.g., user@mastodon.social)." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"ACCOUNT" ~doc)

let login_action ~app_name ~account ~profile env =
  Mirage_crypto_rng_unix.use_default ();
  let fs = env#fs in
  (* Extract instance from account *)
  let instance = match Apub_mastodon_oauth.instance_of_account account with
    | Some i -> i
    | None ->
        Fmt.epr "Error: Invalid account format. Use user@instance.social@.";
        exit 1
  in
  Fmt.pr "Authenticating with %s...@." instance;
  (* Create HTTP client *)
  Eio.Switch.run @@ fun sw ->
  let timeout_config = Requests.Timeout.create ~connect:30.0 ~read:30.0 () in
  let requests = Requests.create ~sw ~timeout:timeout_config env in
  (* Step 1: Register OAuth app *)
  Fmt.pr "Registering OAuth app...@.";
  let app = match Apub_mastodon_oauth.register_app requests ~instance with
    | Ok app -> app
    | Error msg ->
        Fmt.epr "Error: %s@." msg;
        exit 1
  in
  (* Step 2: Generate PKCE *)
  let (code_verifier, code_challenge) = Apub_mastodon_oauth.Pkce.generate () in
  (* Step 3: Display authorization URL *)
  let auth_url = Apub_mastodon_oauth.authorization_url
    ~instance
    ~client_id:app.client_id
    ~code_challenge
  in
  Fmt.pr "@.Please visit this URL to authorize:@.";
  Fmt.pr "@.  %s@.@." auth_url;
  Fmt.pr "After authorizing, paste the authorization code here.@.";
  Fmt.pr "Authorization code: @?";
  let code = read_line () |> String.trim in
  if code = "" then begin
    Fmt.epr "Error: No authorization code provided.@.";
    exit 1
  end;
  (* Step 4: Exchange code for token *)
  Fmt.pr "Exchanging authorization code...@.";
  let token = match Apub_mastodon_oauth.exchange_code requests
    ~instance
    ~client_id:app.client_id
    ~client_secret:app.client_secret
    ~code
    ~code_verifier
  with
    | Ok t -> t
    | Error msg ->
        Fmt.epr "Error: %s@." msg;
        exit 1
  in
  (* Step 5: Verify credentials *)
  Fmt.pr "Verifying credentials...@.";
  let account_info = match Apub_mastodon_oauth.verify_credentials requests
    ~instance
    ~access_token:token.access_token
  with
    | Ok a -> a
    | Error msg ->
        Fmt.epr "Error: %s@." msg;
        exit 1
  in
  (* Step 6: Save session *)
  let actor_uri = Apub_mastodon_oauth.actor_uri_of_account_url account_info.url in
  let profile_name = match profile with
    | Some p -> p
    | None -> account_info.acct ^ "@" ^ instance
  in
  let session = Apub_auth_session.create_oauth
    ~actor_uri
    ~instance
    ~access_token:token.access_token
    ~client_id:app.client_id
    ~client_secret:app.client_secret
  in
  Apub_auth_session.save fs ~app_name ~profile:profile_name session;
  (* Set as current profile if first setup or explicitly requested *)
  let profiles = Apub_auth_session.list_profiles fs ~app_name in
  if List.length profiles <= 1 || Option.is_some profile then
    Apub_auth_session.set_current_profile fs ~app_name profile_name;
  Fmt.pr "@.Successfully logged in!@.";
  Fmt.pr "  Account: %s@." account_info.acct;
  Fmt.pr "  Profile: %s@." profile_name;
  Fmt.pr "  Actor URI: %s@." actor_uri

let login_cmd ~app_name () =
  let doc = "Login to a Mastodon instance via OAuth." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Authenticate with a Mastodon-compatible instance using OAuth 2.0. \
         This enables access to the Mastodon REST API for posting, following, \
         liking, and other social actions.";
      `P
        "The login flow will open a URL in your browser for authorization. \
         After authorizing, copy the code and paste it back here.";
      `S Manpage.s_examples;
      `Pre "  apub auth login alice@mastodon.social";
      `Pre "  apub auth login bob@fosstodon.org --profile work";
    ]
  in
  let info = Cmd.info "login" ~doc ~man in
  let login' account profile =
    Eio_main.run @@ fun env ->
    login_action ~app_name ~account ~profile env
  in
  Cmd.v info Term.(const login' $ account_arg $ profile_arg)

(* Logout command - clear saved session *)

let logout_action ~app_name ~profile env =
  let fs = env#fs in
  let profile =
    match profile with
    | Some p -> p
    | None -> Apub_auth_session.get_current_profile fs ~app_name
  in
  match Apub_auth_session.load fs ~app_name ~profile () with
  | None -> Fmt.pr "No session found for profile '%s'.@." profile
  | Some session ->
      Apub_auth_session.clear fs ~app_name ~profile ();
      Fmt.pr "Cleared session for %s (profile: %s).@." session.actor_uri profile

let logout_cmd ~app_name () =
  let doc = "Clear saved actor credentials." in
  let info = Cmd.info "logout" ~doc in
  let logout' profile =
    Eio_main.run @@ fun env -> logout_action ~app_name ~profile env
  in
  Cmd.v info Term.(const logout' $ profile_arg)

(* Status command *)

let status_action ~app_name ~profile env =
  let fs = env#fs in
  let home = Sys.getenv "HOME" in
  Fmt.pr "Config directory: %s/.config/%s@." home app_name;
  let current = Apub_auth_session.get_current_profile fs ~app_name in
  Fmt.pr "Current profile: %s@." current;
  let profiles = Apub_auth_session.list_profiles fs ~app_name in
  if profiles <> [] then
    Fmt.pr "Available profiles: %s@." (String.concat ", " profiles);
  Fmt.pr "@.";
  let profile = Option.value ~default:current profile in
  match Apub_auth_session.load fs ~app_name ~profile () with
  | None -> Fmt.pr "Profile '%s': Not configured.@." profile
  | Some session ->
      Fmt.pr "Profile '%s':@." profile;
      Fmt.pr "  Actor: %s@." session.actor_uri;
      (* Show signature auth if present *)
      Option.iter (fun key_id ->
        Fmt.pr "  Key ID: %s@." key_id
      ) session.key_id;
      (* Show OAuth auth if present *)
      Option.iter (fun instance ->
        Fmt.pr "  OAuth Instance: %s@." instance
      ) session.oauth_instance;
      (match session.oauth_access_token with
       | Some _ -> Fmt.pr "  OAuth Token: Configured@."
       | None -> ());
      (* Show auth type summary *)
      let auth_types = List.filter_map (fun x -> x) [
        (if Apub_auth_session.has_signature session then Some "HTTP Signatures" else None);
        (if Apub_auth_session.has_oauth session then Some "OAuth" else None);
      ] in
      if auth_types <> [] then
        Fmt.pr "  Auth: %s@." (String.concat ", " auth_types);
      Fmt.pr "  Created: %s@." session.created_at

let status_cmd ~app_name () =
  let doc = "Show authentication status." in
  let info = Cmd.info "status" ~doc in
  let status' profile =
    Eio_main.run @@ fun env -> status_action ~app_name ~profile env
  in
  Cmd.v info Term.(const status' $ profile_arg)

(* Profile list command *)

let profile_list_action ~app_name env =
  let fs = env#fs in
  let current = Apub_auth_session.get_current_profile fs ~app_name in
  let profiles = Apub_auth_session.list_profiles fs ~app_name in
  if profiles = [] then
    Fmt.pr "No profiles found. Use '%s auth setup' to create one.@." app_name
  else begin
    Fmt.pr "Profiles:@.";
    List.iter
      (fun p ->
        let marker = if p = current then " (current)" else "" in
        match Apub_auth_session.load fs ~app_name ~profile:p () with
        | Some session -> Fmt.pr "  %s%s - %s@." p marker session.actor_uri
        | None -> Fmt.pr "  %s%s@." p marker)
      profiles
  end

let profile_list_cmd ~app_name () =
  let doc = "List available profiles." in
  let info = Cmd.info "list" ~doc in
  let list' () = Eio_main.run @@ fun env -> profile_list_action ~app_name env in
  Cmd.v info Term.(const list' $ const ())

(* Profile switch command *)

let profile_name_arg =
  let doc = "Profile name to switch to." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PROFILE" ~doc)

let profile_switch_action ~app_name ~profile env =
  let fs = env#fs in
  let profiles = Apub_auth_session.list_profiles fs ~app_name in
  if List.mem profile profiles then begin
    Apub_auth_session.set_current_profile fs ~app_name profile;
    Fmt.pr "Switched to profile: %s@." profile
  end
  else begin
    Fmt.epr "Profile '%s' not found.@." profile;
    if profiles <> [] then
      Fmt.epr "Available profiles: %s@." (String.concat ", " profiles);
    exit 1
  end

let profile_switch_cmd ~app_name () =
  let doc = "Switch to a different profile." in
  let info = Cmd.info "switch" ~doc in
  let switch' profile =
    Eio_main.run @@ fun env -> profile_switch_action ~app_name ~profile env
  in
  Cmd.v info Term.(const switch' $ profile_name_arg)

(* Profile current command *)

let profile_current_action ~app_name env =
  let fs = env#fs in
  let current = Apub_auth_session.get_current_profile fs ~app_name in
  Fmt.pr "%s@." current

let profile_current_cmd ~app_name () =
  let doc = "Show current profile name." in
  let info = Cmd.info "current" ~doc in
  let current' () =
    Eio_main.run @@ fun env -> profile_current_action ~app_name env
  in
  Cmd.v info Term.(const current' $ const ())

(* Profile command group *)

let profile_cmd ~app_name () =
  let doc = "Profile management commands." in
  let info = Cmd.info "profile" ~doc in
  Cmd.group info
    [
      profile_list_cmd ~app_name ();
      profile_switch_cmd ~app_name ();
      profile_current_cmd ~app_name ();
    ]

(* Auth command group *)

let auth_cmd ~app_name () =
  let doc = "Authentication commands." in
  let info = Cmd.info "auth" ~doc in
  Cmd.group info
    [
      setup_cmd ~app_name ();
      login_cmd ~app_name ();
      logout_cmd ~app_name ();
      status_cmd ~app_name ();
      profile_cmd ~app_name ();
    ]

(* Helper to load session or exit with error *)

let with_session ~app_name ?profile f env =
  let fs = env#fs in
  match Apub_auth_session.load fs ~app_name ?profile () with
  | None ->
      let profile_msg =
        match profile with
        | Some p -> Printf.sprintf " (profile: %s)" p
        | None ->
            let current =
              Apub_auth_session.get_current_profile fs ~app_name
            in
            Printf.sprintf " (profile: %s)" current
      in
      Fmt.epr "Not configured%s. Use '%s auth setup' first.@." profile_msg
        app_name;
      exit 1
  | Some session -> f fs session
