(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Common Arguments *)

let identifier_arg =
  let doc = "Handle or DID (e.g., alice.bsky.social)." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"IDENTIFIER" ~doc)

let password_arg =
  let doc = "App password (will prompt if not provided)." in
  Arg.(
    value
    & opt (some string) None
    & info [ "password"; "p" ] ~docv:"PASSWORD" ~doc)

let pds_arg ?(default = "https://bsky.social") () =
  let doc = "PDS URL." in
  Arg.(value & opt string default & info [ "pds" ] ~docv:"URL" ~doc)

let profile_arg =
  let doc = "Profile name (default: current profile, or handle on login)." in
  Arg.(
    value
    & opt (some string) None
    & info [ "profile"; "P" ] ~docv:"PROFILE" ~doc)

let user_arg =
  let doc = "User handle or DID (default: logged-in user)." in
  Arg.(value & opt (some string) None & info [ "user"; "u" ] ~docv:"USER" ~doc)

(* Helper to resolve handle to DID *)

type resolve_handle_response = { did : string }

let resolve_handle_response_jsont =
  Jsont.Object.map ~kind:"resolve_handle_response" (fun did -> { did })
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.finish

let resolve_did client user_opt =
  match user_opt with
  | Some u when String.starts_with ~prefix:"did:" u -> u
  | Some handle ->
      let xrpc_client = Xrpc_auth_client.get_client client in
      let resp =
        Xrpc.Client.query xrpc_client ~nsid:"com.atproto.identity.resolveHandle"
          ~params:[ ("handle", handle) ]
          ~decoder:resolve_handle_response_jsont
      in
      resp.did
  | None -> Xrpc_auth_client.get_did client

(* Session helper *)

let with_session ~app_name ?profile f env =
  let fs = env#fs in
  match Xrpc_auth_session.load fs ~app_name ?profile () with
  | None ->
      let profile_msg =
        match profile with
        | Some p -> Printf.sprintf " (profile: %s)" p
        | None ->
            let current = Xrpc_auth_session.get_current_profile fs ~app_name in
            Printf.sprintf " (profile: %s)" current
      in
      Fmt.epr "Not logged in%s. Use '%s auth login' first.@." profile_msg
        app_name;
      exit 1
  | Some session -> f fs session

(* Login command *)

let login_action ~app_name ~identifier ~password ~pds ~profile env =
  let password =
    match password with
    | Some p -> p
    | None ->
        Fmt.pr "Password: @?";
        read_line ()
  in
  Eio.Switch.run @@ fun sw ->
  let fs = env#fs in
  let client = Xrpc_auth_client.create ~sw ~env ~app_name ?profile ~pds () in
  Xrpc_auth_client.login client ~identifier ~password;
  match Xrpc_auth_client.get_session client with
  | Some session ->
      (* Set this as current profile if it's the first login or explicitly requested *)
      let profile_name = Option.value ~default:session.handle profile in
      let profiles = Xrpc_auth_session.list_profiles fs ~app_name in
      if profiles = [] || Option.is_some profile then
        Xrpc_auth_session.set_current_profile fs ~app_name profile_name;
      Fmt.pr "Logged in as %s (profile: %s)@." session.handle profile_name
  | None -> Fmt.pr "Logged in as %s@." identifier

let login_cmd ~app_name ?(default_pds = "https://bsky.social") () =
  let doc = "Login to your PDS." in
  let info = Cmd.info "login" ~doc in
  let login' identifier password pds profile =
    Eio_main.run @@ fun env ->
    login_action ~app_name ~identifier ~password ~pds ~profile env
  in
  Cmd.v info
    Term.(
      const login' $ identifier_arg $ password_arg
      $ pds_arg ~default:default_pds ()
      $ profile_arg)

(* Logout command *)

let logout_action ~app_name ~profile env =
  Eio.Switch.run @@ fun sw ->
  let fs = env#fs in
  match Xrpc_auth_session.load fs ~app_name ?profile () with
  | None -> Fmt.pr "Not logged in.@."
  | Some session ->
      let client =
        Xrpc_auth_client.create ~sw ~env ~app_name ?profile ~pds:session.pds ()
      in
      Xrpc_auth_client.resume client ~session;
      Xrpc_auth_client.logout client;
      let profile_name = Option.value ~default:session.handle profile in
      Fmt.pr "Logged out (profile: %s).@." profile_name

let logout_cmd ~app_name () =
  let doc = "Logout and clear saved session." in
  let info = Cmd.info "logout" ~doc in
  let logout' profile =
    Eio_main.run @@ fun env -> logout_action ~app_name ~profile env
  in
  Cmd.v info Term.(const logout' $ profile_arg)

(* Status command *)

let status_action ~app_name ~profile env =
  Eio.Switch.run @@ fun _sw ->
  let fs = env#fs in
  let home = Sys.getenv "HOME" in
  Fmt.pr "Config directory: %s/.config/%s@." home app_name;
  let current = Xrpc_auth_session.get_current_profile fs ~app_name in
  Fmt.pr "Current profile: %s@." current;
  let profiles = Xrpc_auth_session.list_profiles fs ~app_name in
  if profiles <> [] then begin
    Fmt.pr "Available profiles: %s@." (String.concat ", " profiles)
  end;
  Fmt.pr "@.";
  let profile = Option.value ~default:current profile in
  match Xrpc_auth_session.load fs ~app_name ~profile () with
  | None -> Fmt.pr "Profile '%s': Not logged in.@." profile
  | Some session ->
      Fmt.pr "Profile '%s':@." profile;
      Fmt.pr "  Handle: %s@." session.handle;
      Fmt.pr "  DID: %s@." session.did;
      Fmt.pr "  PDS: %s@." session.pds;
      Fmt.pr "  Created: %s@." session.created_at;
      if Xrpc_auth_session.is_expired session then
        Fmt.pr "  (token expired, will refresh on next use)@."

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
  let current = Xrpc_auth_session.get_current_profile fs ~app_name in
  let profiles = Xrpc_auth_session.list_profiles fs ~app_name in
  if profiles = [] then
    Fmt.pr "No profiles found. Use '%s auth login' to create one.@." app_name
  else begin
    Fmt.pr "Profiles:@.";
    List.iter
      (fun p ->
        let marker = if p = current then " (current)" else "" in
        match Xrpc_auth_session.load fs ~app_name ~profile:p () with
        | Some session -> Fmt.pr "  %s%s - %s@." p marker session.did
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
  let profiles = Xrpc_auth_session.list_profiles fs ~app_name in
  if List.mem profile profiles then begin
    Xrpc_auth_session.set_current_profile fs ~app_name profile;
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
  let current = Xrpc_auth_session.get_current_profile fs ~app_name in
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

let auth_cmd ~app_name ?default_pds () =
  let doc = "Authentication commands." in
  let info = Cmd.info "auth" ~doc in
  Cmd.group info
    [
      login_cmd ~app_name ?default_pds ();
      logout_cmd ~app_name ();
      status_cmd ~app_name ();
      profile_cmd ~app_name ();
    ]
