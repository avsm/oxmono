(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Cli = Jmap_eio.Cli
module Io = Jmap_mosaic.Io
module Login = Jmap_mosaic.Login
module Model = Jmap_mosaic.Model
module View = Jmap_mosaic.View

let doc = "Read, label, file and reply to mail over JMAP"

let man =
  [
    `S Cmdliner.Manpage.s_description;
    `P
      "$(tname) is a mail client for the terminal. It opens on a login screen \
       unless a saved profile or a session URL and key are chosen, and then \
       lists smart searches and mailboxes on the left and the newest hundred \
       matching messages on the right. Enter opens a message, $(b,r) replies \
       to it, $(b,u) and $(b,f) toggle the \\$seen and \\$flagged keywords, \
       and $(b,m) files it into another mailbox.";
    `P
      "A login that succeeds is saved as a named profile under \
       $(b,\\$XDG_CONFIG_HOME/jmap/profiles). Profile files include the bearer \
       token or password and are created with mode 0600.";
    `S "KEYS";
    `Pre
      "Tab      move the focus between the two panes, and between the login \
       fields\n\
       S-Tab    move back a login field\n\
       Ctrl-A   switch the login between basic and bearer\n\
       n e      create or edit a profile, on the profile picker\n\
       1 2      open the Unread or Unanswered >30d smart search\n\
       j k      move down and up in the focused pane, as do the arrows\n\
       Enter    open the selected mailbox or message, or log in\n\
       Esc      go back one screen, or quit the profile/login screen\n\
       R        reload the mailboxes and the current list\n\
       u        toggle \\$seen on the current message\n\
       f        toggle \\$flagged on the current message\n\
       m        file the current message into another mailbox\n\
       r        reply to the open message\n\
       Ctrl-S   send the reply\n\
       q        quit";
    `S Cmdliner.Manpage.s_environment;
    `Pre Cli.env_docs;
  ]

let env_opt var =
  match Sys.getenv_opt var with Some "" | None -> None | v -> v

let given cmdline var =
  match cmdline with Some _ -> cmdline | None -> env_opt var

type secret_source = [ `Key of string | `File of string ]

let resolve_secret key key_file =
  match (key, key_file) with
  | Some _, Some _ ->
      Error "--api-key and --api-key-file cannot be used together"
  | Some key, None -> Ok (Some (`Key key))
  | None, Some path -> Ok (Some (`File path))
  | None, None -> (
      match env_opt "JMAP_API_KEY_FILE" with
      | Some path -> Ok (Some (`File path))
      | None -> Ok (Option.map (fun key -> `Key key) (env_opt "JMAP_API_KEY")))

let resolve_auth = function
  | Some scheme -> Ok (Some scheme)
  | None -> (
      match env_opt "JMAP_AUTH" with
      | None -> Ok None
      | Some value -> (
          match Model.scheme_of_string value with
          | Some scheme -> Ok (Some scheme)
          | None ->
              Error
                (Printf.sprintf
                   "JMAP_AUTH: expected \"bearer\" or \"basic\", got %S" value))
      )

let supplied_secret env = function
  | None -> Ok None
  | Some (`Key secret) -> Ok (Some secret)
  | Some (`File path) ->
      Result.map Option.some
        (Jmap_eio.Auth.read_secret_file ~fs:(Eio.Stdenv.fs env) path)

let split_supplied scheme value =
  match (scheme, String.index_opt value ':') with
  | Model.Bearer, _ -> Ok ("", value)
  | Model.Basic, Some i ->
      Ok
        ( String.sub value 0 i,
          String.sub value (i + 1) (String.length value - i - 1) )
  | Model.Basic, None ->
      Error "a Basic --api-key or key file must be USER:PASSWORD"

let form env ~(remembered : Model.login) ~url ~auth ~secret_source =
  let scheme = Option.value auth ~default:remembered.scheme in
  let user, secret, error =
    match supplied_secret env secret_source with
    | Error error -> (remembered.user, "", error)
    | Ok None -> (remembered.user, remembered.secret, remembered.error)
    | Ok (Some value) -> (
        match split_supplied scheme value with
        | Ok (user, secret) -> (user, secret, "")
        | Error error -> (remembered.user, "", error))
  in
  {
    remembered with
    url = Option.value (given url "JMAP_SESSION_URL") ~default:remembered.url;
    scheme;
    user;
    secret;
    error;
  }

let find_profile name profiles =
  List.find_opt (fun (login : Model.login) -> login.profile = name) profiles

let run (profile, url, secret_source, auth, account, allow_insecure, debug) =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let account = given account "JMAP_ACCOUNT_ID" in
  let profiles = Login.profiles env in
  let remembered =
    match profile with
    | None -> Model.blank
    | Some name -> (
        match find_profile name profiles with
        | Some login -> login
        | None ->
            {
              Model.blank with
              profile = name;
              error = "no saved profile named " ^ name;
            })
  in
  let form = form env ~remembered ~url ~auth ~secret_source in
  let choose =
    Option.is_none profile && Option.is_none url && Option.is_none auth
    && Option.is_none secret_source
    && profiles <> []
  in
  if debug then Fmt.epr "profiles stored in %s@." (Login.profiles_path env);
  let io = Io.create ~sw ?account ~allow_insecure env in
  let matrix =
    Matrix_eio.create ~sw ~clock:(Eio.Stdenv.clock env) ~stdin:env#stdin
      ~stdout:env#stdout ()
  in
  let process_perform thunk =
    Eio.Fiber.fork_daemon ~sw (fun () ->
        thunk ();
        `Stop_daemon)
  in
  let perform action = Mosaic.Cmd.perform (Io.perform io action) in
  let commands actions = Mosaic.Cmd.batch (List.map perform actions) in
  let init () =
    let model, actions =
      Model.init ~profiles:(if choose then profiles else []) form
    in
    (model, commands actions)
  in
  let remember msg (model : Model.t) =
    match (msg, model.screen) with
    | Model.Connected _, (Model.Connecting l | Model.Login l) ->
        Login.write_profile env l
    | _ -> ()
  in
  let update msg model =
    remember msg model;
    let model, actions = Model.update msg model in
    (model, if model.Model.quit then Mosaic.Cmd.quit else commands actions)
  in
  Mosaic.run ~matrix ~process_perform
    { init; update; view = View.render; subscriptions = View.subscriptions }

open Cmdliner

let auth_term =
  let doc =
    "Authentication scheme, $(b,bearer) or $(b,basic). Can also be set with \
     the JMAP_AUTH environment variable. It prefills the login screen and is \
     saved with a profile after a successful login."
  in
  let schemes = [ ("bearer", Model.Bearer); ("basic", Model.Basic) ] in
  Arg.(
    value & opt (some (enum schemes)) None & info [ "auth" ] ~docv:"SCHEME" ~doc)

let settings_term =
  let make profile url key key_file auth account allow_insecure debug =
    let ( let* ) = Result.bind in
    let* secret = resolve_secret key key_file in
    let* auth = resolve_auth auth in
    Ok (profile, url, secret, auth, account, allow_insecure, debug)
  in
  Term.(
    term_result' ~usage:true
      (const make $ Cli.profile_term $ Cli.session_url_term $ Cli.api_key_term
     $ Cli.api_key_file_term $ auth_term $ Cli.account_id_term
     $ Cli.allow_insecure_term $ Cli.debug_term))

let cmd =
  Cmd.v (Cmd.info "jmap-mosaic" ~doc ~man) Term.(const run $ settings_term)

let () = exit (Cmd.eval cmd)
