(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let app_name = "typesense"

(* Styled output helpers *)
let header_style = Fmt.(styled `Bold string)
let label_style = Fmt.(styled `Faint string)
let value_style = Fmt.(styled (`Fg `Cyan) string)
let success_style = Fmt.(styled (`Fg `Green) string)
let warning_style = Fmt.(styled (`Fg `Yellow) string)
let error_style = Fmt.(styled (`Fg `Red) string)
let profile_style = Fmt.(styled (`Fg `Magenta) string)
let current_style = Fmt.(styled (`Fg `Green) (styled `Bold string))

(* Common Arguments *)

let server_arg =
  let doc = "Typesense server URL." in
  let env = Cmd.Env.info "TYPESENSE_SERVER" ~doc in
  Arg.(required & pos 0 (some string) None & info [] ~env ~docv:"SERVER" ~doc)

let server_opt =
  let doc = "Typesense server URL." in
  let env = Cmd.Env.info "TYPESENSE_SERVER" ~doc in
  Arg.(value & opt (some string) None & info ["server"; "s"] ~env ~docv:"URL" ~doc)

let api_key_arg =
  let doc = "API key (will prompt if not provided)." in
  let env = Cmd.Env.info "TYPESENSE_API_KEY" ~doc in
  Arg.(value & opt (some string) None & info ["api-key"; "k"] ~env ~docv:"KEY" ~doc)

let profile_arg =
  let doc = "Profile name (default: current profile)." in
  Arg.(value & opt (some string) None & info ["profile"; "P"] ~docv:"PROFILE" ~doc)

(* Logging setup - takes requests_config to extract verbose_http *)

let setup_logging_with_config style_renderer level (requests_config : Requests.Cmd.config) =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  Requests.Cmd.setup_log_sources ~verbose_http:requests_config.verbose_http.value level

let setup_logging_simple style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let setup_logging =
  Term.(const (fun style_renderer level -> (style_renderer, level))
  $ Fmt_cli.style_renderer ()
  $ Logs_cli.level ())

(* Requests config term *)

let requests_config_term fs =
  Requests.Cmd.config_term app_name fs

(* Session helper *)

let with_session ?profile f env =
  let fs = env#fs in
  match Session.load fs ?profile () with
  | None ->
      let profile_name =
        match profile with
        | Some p -> p
        | None -> Session.get_current_profile fs
      in
      Fmt.epr "%a Not logged in (profile: %a). Use '%a' first.@."
        error_style "Error:"
        profile_style profile_name
        Fmt.(styled `Bold string) "typesense-cli auth login";
      raise (Error.Exit_code 1)
  | Some session -> f fs session

let with_client ?requests_config ?profile f env =
  with_session ?profile (fun fs session ->
    Eio.Switch.run @@ fun sw ->
    let client = Client.resume ~sw ~env ?requests_config ?profile ~session () in
    f fs client
  ) env

(* Profile configuration for external programs *)

module Profile_config = struct
  type t = {
    style_renderer : Fmt.style_renderer option;
    log_level : Logs.level option;
    requests_config : Requests.Cmd.config;
    profile : string option;
  }

  let style_renderer t = t.style_renderer
  let log_level t = t.log_level
  let requests_config t = t.requests_config
  let profile t = t.profile

  let setup_logging t =
    setup_logging_with_config t.style_renderer t.log_level t.requests_config
end

let profile_config_term fs =
  let make (sr, ll) rc p =
    Profile_config.{ style_renderer = sr; log_level = ll; requests_config = rc; profile = p }
  in
  Term.(const make $ setup_logging $ requests_config_term fs $ profile_arg)

(* Login command *)

let login_action ~requests_config ~server ~api_key ~profile env =
  let api_key = match api_key with
    | Some k -> k
    | None ->
        Fmt.pr "%a @?" label_style "API Key:";
        read_line ()
  in
  Eio.Switch.run @@ fun sw ->
  let client = Client.login ~sw ~env ~requests_config ?profile ~server_url:server ~api_key () in
  let profile_name = Option.value ~default:Session.default_profile profile in
  Fmt.pr "%a Logged in to %a (profile: %a)@."
    success_style "Success:" value_style server profile_style profile_name;
  ignore client

let login_cmd env fs =
  let doc = "Login to a Typesense server." in
  let info = Cmd.info "login" ~doc in
  let login' (style_renderer, level) requests_config server api_key profile =
    setup_logging_with_config style_renderer level requests_config;
    Error.wrap (fun () ->
      login_action ~requests_config ~server ~api_key ~profile env)
  in
  Cmd.v info
    Term.(const login' $ setup_logging $ requests_config_term fs $ server_arg $ api_key_arg $ profile_arg)

(* Logout command *)

let logout_action ~profile env =
  let fs = env#fs in
  match Session.load fs ?profile () with
  | None -> Fmt.pr "%a Not logged in.@." warning_style "Note:"
  | Some session ->
      Session.clear fs ?profile ();
      let profile_name =
        match profile with
        | Some p -> p
        | None -> Session.get_current_profile fs
      in
      Fmt.pr "%a Logged out from %a (profile: %a).@."
        success_style "Success:"
        value_style (Session.server_url session)
        profile_style profile_name

let logout_cmd env =
  let doc = "Logout and clear saved session." in
  let info = Cmd.info "logout" ~doc in
  let logout' (style_renderer, level) profile =
    setup_logging_simple style_renderer level;
    logout_action ~profile env
  in
  Cmd.v info Term.(const logout' $ setup_logging $ profile_arg)

(* Status command *)

let status_action ~profile env =
  let fs = env#fs in
  let home = Sys.getenv "HOME" in
  Fmt.pr "%a %a@." label_style "Config directory:" value_style (home ^ "/.config/typesense");
  let current = Session.get_current_profile fs in
  Fmt.pr "%a %a@." label_style "Current profile:" current_style current;
  let profiles = Session.list_profiles fs in
  if profiles <> [] then begin
    Fmt.pr "%a %a@." label_style "Available profiles:"
      Fmt.(list ~sep:(any ", ") profile_style) profiles
  end;
  Fmt.pr "@.";
  let profile = Option.value ~default:current profile in
  match Session.load fs ~profile () with
  | None ->
      Fmt.pr "%a %a: %a@."
        header_style "Profile"
        profile_style profile
        warning_style "Not logged in"
  | Some session ->
      Fmt.pr "%a %a:@." header_style "Profile" profile_style profile;
      Fmt.pr "  %a@." Session.pp session

let auth_status_cmd env =
  let doc = "Show authentication status." in
  let info = Cmd.info "status" ~doc in
  let status' (style_renderer, level) profile =
    setup_logging_simple style_renderer level;
    status_action ~profile env
  in
  Cmd.v info Term.(const status' $ setup_logging $ profile_arg)

(* Profile list command *)

let profile_list_action env =
  let fs = env#fs in
  let current = Session.get_current_profile fs in
  let profiles = Session.list_profiles fs in
  if profiles = [] then
    Fmt.pr "%a No profiles found. Use '%a' to create one.@."
      warning_style "Note:"
      Fmt.(styled `Bold string) "typesense-cli auth login"
  else begin
    Fmt.pr "%a@." header_style "Profiles:";
    List.iter
      (fun p ->
        let is_current = p = current in
        match Session.load fs ~profile:p () with
        | Some session ->
            if is_current then
              Fmt.pr "  %a %a - %a@."
                current_style p
                success_style "(current)"
                value_style (Session.server_url session)
            else
              Fmt.pr "  %a - %a@."
                profile_style p
                value_style (Session.server_url session)
        | None ->
            if is_current then
              Fmt.pr "  %a %a@." current_style p success_style "(current)"
            else
              Fmt.pr "  %a@." profile_style p)
      profiles
  end

let profile_list_cmd env =
  let doc = "List available profiles." in
  let info = Cmd.info "list" ~doc in
  let list' (style_renderer, level) () =
    setup_logging_simple style_renderer level;
    profile_list_action env
  in
  Cmd.v info Term.(const list' $ setup_logging $ const ())

(* Profile switch command *)

let profile_name_arg =
  let doc = "Profile name to switch to." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PROFILE" ~doc)

let profile_switch_action ~profile env =
  let fs = env#fs in
  let profiles = Session.list_profiles fs in
  if List.mem profile profiles then begin
    Session.set_current_profile fs profile;
    Fmt.pr "%a Switched to profile: %a@."
      success_style "Success:"
      profile_style profile
  end
  else begin
    Fmt.epr "%a Profile '%a' not found.@."
      error_style "Error:"
      profile_style profile;
    if profiles <> [] then
      Fmt.epr "%a %a@."
        label_style "Available profiles:"
        Fmt.(list ~sep:(any ", ") profile_style) profiles;
    raise (Error.Exit_code 1)
  end

let profile_switch_cmd env =
  let doc = "Switch to a different profile." in
  let info = Cmd.info "switch" ~doc in
  let switch' (style_renderer, level) profile =
    setup_logging_simple style_renderer level;
    profile_switch_action ~profile env
  in
  Cmd.v info Term.(const switch' $ setup_logging $ profile_name_arg)

(* Profile current command *)

let profile_current_action env =
  let fs = env#fs in
  let current = Session.get_current_profile fs in
  Fmt.pr "%a@." current_style current

let profile_current_cmd env =
  let doc = "Show current profile name." in
  let info = Cmd.info "current" ~doc in
  let current' (style_renderer, level) () =
    setup_logging_simple style_renderer level;
    profile_current_action env
  in
  Cmd.v info Term.(const current' $ setup_logging $ const ())

(* Profile command group *)

let profile_cmd env =
  let doc = "Profile management commands." in
  let info = Cmd.info "profile" ~doc in
  Cmd.group info
    [ profile_list_cmd env
    ; profile_switch_cmd env
    ; profile_current_cmd env
    ]

(* Auth command group *)

let auth_cmd env fs =
  let doc = "Authentication commands." in
  let info = Cmd.info "auth" ~doc in
  Cmd.group info
    [ login_cmd env fs
    ; logout_cmd env
    ; auth_status_cmd env
    ; profile_cmd env
    ]
