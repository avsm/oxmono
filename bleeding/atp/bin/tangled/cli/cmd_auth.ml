(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let app_name = "tangled"

(* Common options *)

let pds =
  let doc = "PDS base URL." in
  let env = Cmd.Env.info "TANGLED_PDS" in
  Arg.(
    value
    & opt string "https://bsky.social"
    & info [ "pds" ] ~env ~docv:"URL" ~doc)

(* Auth login command *)

let handle_arg =
  let doc = "Handle or DID to login as." in
  Arg.(
    value & opt (some string) None & info [ "handle"; "u" ] ~docv:"HANDLE" ~doc)

let password_arg =
  let doc = "Password or app password." in
  let env = Cmd.Env.info "TANGLED_PASSWORD" in
  Arg.(
    value
    & opt (some string) None
    & info [ "password"; "p" ] ~env ~docv:"PASSWORD" ~doc)

let save_password_arg =
  let doc =
    "Save password to config (stored in separate file with 600 permissions)."
  in
  Arg.(value & flag & info [ "save-password" ] ~doc)

let prompt_or label = function
  | Some v -> v
  | None ->
      Fmt.pr "%s: @?" label;
      read_line ()

let login_action ~pds ~handle ~password ~save_password env =
  Eio.Switch.run @@ fun sw ->
  let fs = env#fs in
  let xdg = Xdge.create fs app_name in
  let api = Tangled.Api.create ~sw ~env ~app_name ~pds () in

  (* Try to get credentials from config if not provided *)
  let config = Tangled.Config.load fs xdg in
  let saved_password = Tangled.Config.load_password fs xdg in

  let handle =
    match handle with
    | Some h -> h
    | None -> (
        match config with Some c -> c.handle | None -> prompt_or "Handle" None)
  in
  let password =
    match password with
    | Some p -> p
    | None -> (
        match saved_password with
        | Some p -> p
        | None -> prompt_or "Password" None)
  in

  (* Login *)
  Tangled.Api.login api ~identifier:handle ~password;

  match Tangled.Api.get_session api with
  | Some session ->
      (* Save config *)
      let config : Tangled.Config.t = { handle = session.handle; pds } in
      Tangled.Config.save fs xdg config;
      if save_password then Tangled.Config.save_password fs xdg password;
      Fmt.pr "Logged in as %s (%s)@." session.handle session.did;
      if save_password then Fmt.pr "Password saved to config.@."
  | None -> Fmt.pr "Login failed@."

let login_cmd =
  let doc = "Login to Tangled." in
  let info = Cmd.info "login" ~doc in
  let login' pds handle password save_password =
    Eio_main.run @@ fun env ->
    login_action ~pds ~handle ~password ~save_password env
  in
  Cmd.v info
    Term.(const login' $ pds $ handle_arg $ password_arg $ save_password_arg)

(* Auth logout command *)

let logout_action env =
  Eio.Switch.run @@ fun sw ->
  let fs = env#fs in
  let xdg = Xdge.create fs app_name in
  match Xrpc_auth.Session.load fs ~app_name () with
  | None -> Fmt.pr "Not logged in.@."
  | Some session ->
      let pds = session.pds in
      let api = Tangled.Api.create ~sw ~env ~app_name ~pds () in
      Tangled.Api.resume api ~session;
      Tangled.Api.logout api;
      Tangled.Config.clear_password fs xdg;
      Fmt.pr "Logged out.@."

let logout_cmd =
  let doc = "Logout from Tangled." in
  let info = Cmd.info "logout" ~doc in
  let logout' () = Eio_main.run @@ fun env -> logout_action env in
  Cmd.v info Term.(const logout' $ const ())

(* Auth status command *)

let status_action env =
  let fs = env#fs in
  let xdg = Xdge.create fs app_name in
  Fmt.pr "@[<v>Config directory: %a@,@]" Eio.Path.pp (Xdge.config_dir xdg);
  (match Tangled.Config.load fs xdg with
  | Some config -> Fmt.pr "@[<v>Config:@,  %a@]@." Tangled.Config.pp config
  | None -> Fmt.pr "No config file found.@.");
  (match Tangled.Config.load_password fs xdg with
  | Some _ -> Fmt.pr "Password: (saved)@."
  | None -> Fmt.pr "Password: (not saved)@.");
  Fmt.pr "@.";
  match Xrpc_auth.Session.load fs ~app_name () with
  | None ->
      Fmt.pr "Session: Not logged in.@.";
      Fmt.pr "Use 'tangled auth login' to authenticate.@."
  | Some session ->
      Fmt.pr "Session:@.";
      Fmt.pr "  @[<v>%a@]@." Xrpc_auth.Session.pp session;
      if Xrpc_auth.Session.is_expired session then
        Fmt.pr
          "@[<v>@,\
           Note: Access token is expired, will refresh on next request.@]@."

let status_cmd =
  let doc = "Show current authentication status." in
  let info = Cmd.info "status" ~doc in
  let status' () = Eio_main.run @@ fun env -> status_action env in
  Cmd.v info Term.(const status' $ const ())

(* Auth command group *)

let cmd =
  let doc = "Authentication commands." in
  let info = Cmd.info "auth" ~doc in
  Cmd.group info
    [
      login_cmd; logout_cmd; status_cmd; Xrpc_auth.Cmd.profile_cmd ~app_name ();
    ]
