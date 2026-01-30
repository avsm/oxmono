(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let app_name = "peertube"

(** {1 Common Options} *)

let profile_arg =
  let doc = "Profile name to use for authentication." in
  Arg.(value & opt (some string) None & info ["profile"; "p"] ~docv:"PROFILE" ~doc)

let server_url_arg =
  let doc = "PeerTube server URL (e.g., https://peertube.example.com)." in
  Arg.(required & opt (some string) None & info ["server"; "s"] ~docv:"URL" ~doc)

let username_arg =
  let doc = "Username for authentication." in
  Arg.(required & opt (some string) None & info ["username"; "u"] ~docv:"USER" ~doc)

let password_arg =
  let doc = "Password for authentication. For security, prefer using the environment variable PEERTUBE_PASSWORD." in
  Arg.(value & opt (some string) None & info ["password"] ~docv:"PASS" ~doc)

(* Requests config term *)
let requests_config_term fs =
  Requests.Cmd.config_term app_name fs

(** {1 Login Command} *)

let login_cmd env fs =
  let login_action profile server_url username password requests_config =
    Error.wrap @@ fun () ->
    Eio.Switch.run @@ fun sw ->
    let password = match password with
      | Some p -> p
      | None ->
          match Sys.getenv_opt "PEERTUBE_PASSWORD" with
          | Some p -> p
          | None -> Error.fail "Password required. Use --password or set PEERTUBE_PASSWORD environment variable."
    in
    let requests_config = Some requests_config in
    let client = Client.login_password ~sw ~env ?requests_config ?profile ~server_url ~username ~password () in
    Fmt.pr "@[<v>%a Logged in successfully@,%a@]@."
      Fmt.(styled (`Fg `Green) string) "[OK]"
      Session.pp (Client.session client);
    0
  in
  let term = Term.(const login_action $ profile_arg $ server_url_arg $ username_arg $ password_arg $ requests_config_term fs) in
  let info = Cmd.info "login"
    ~doc:"Login to a PeerTube server"
    ~man:[
      `S Manpage.s_description;
      `P "Authenticate with a PeerTube server using username and password.";
      `P "The OAuth client credentials are automatically retrieved from the server.";
      `S Manpage.s_examples;
      `Pre "  peertube auth login -s https://peertube.example.com -u myuser";
      `P "You can also set the password via environment variable:";
      `Pre "  PEERTUBE_PASSWORD=mypass peertube auth login -s https://peertube.example.com -u myuser";
    ]
  in
  Cmd.v info term

(** {1 Logout Command} *)

let logout_cmd _env fs =
  let logout_action profile =
    Error.wrap @@ fun () ->
    Session.clear fs ?profile ();
    Fmt.pr "@[%a Logged out@]@." Fmt.(styled (`Fg `Green) string) "[OK]";
    0
  in
  let term = Term.(const logout_action $ profile_arg) in
  let info = Cmd.info "logout"
    ~doc:"Logout from PeerTube"
    ~man:[
      `S Manpage.s_description;
      `P "Remove saved authentication credentials.";
    ]
  in
  Cmd.v info term

(** {1 Status Command} *)

let status_cmd _env fs =
  let status_action profile =
    Error.wrap @@ fun () ->
    match Session.load fs ?profile () with
    | None ->
        Fmt.pr "@[Not logged in@]@.";
        1
    | Some session ->
        Fmt.pr "@[<v>%a@]@." Session.pp session;
        0
  in
  let term = Term.(const status_action $ profile_arg) in
  let info = Cmd.info "status"
    ~doc:"Show current authentication status"
    ~man:[
      `S Manpage.s_description;
      `P "Display information about the current session.";
    ]
  in
  Cmd.v info term

(** {1 Profiles Command} *)

let profiles_cmd _env fs =
  let profiles_action () =
    Error.wrap @@ fun () ->
    let profiles = Session.list_profiles fs in
    let current = Session.get_current_profile fs in
    if profiles = [] then begin
      Fmt.pr "@[No profiles found. Use 'peertube auth login' to create one.@]@.";
      0
    end else begin
      Fmt.pr "@[<v>%a@]@."
        Fmt.(list ~sep:(any "@,") (fun ppf p ->
          if p = current then
            pf ppf "* %a (current)" (styled (`Fg `Green) string) p
          else
            pf ppf "  %s" p)) profiles;
      0
    end
  in
  let term = Term.(const profiles_action $ const ()) in
  let info = Cmd.info "profiles"
    ~doc:"List available profiles"
    ~man:[
      `S Manpage.s_description;
      `P "List all saved authentication profiles.";
    ]
  in
  Cmd.v info term

(** {1 Auth Command Group} *)

let auth_cmd env fs =
  let info = Cmd.info "auth"
    ~doc:"Authentication commands"
    ~man:[
      `S Manpage.s_description;
      `P "Commands for managing PeerTube authentication.";
    ]
  in
  Cmd.group info [
    login_cmd env fs;
    logout_cmd env fs;
    status_cmd env fs;
    profiles_cmd env fs;
  ]

(** {1 Client Helper} *)

(** [with_client ~sw ~env ~fs ?profile f] loads a session and creates a client,
    then calls [f] with the client. Exits with an error if not logged in. *)
let with_client ~sw ~env ~fs ?requests_config ?profile f =
  match Session.load fs ?profile () with
  | None ->
      Error.fail "Not logged in. Use 'peertube auth login' first."
  | Some session ->
      let client = Client.resume ~sw ~env ?requests_config ?profile ~session () in
      f client
