module M = Matrix_eio
module Cli = Matrix_cli
module Profile_store = Matrix_client.Profile_store
module Session = Matrix_client.Session

let client_of_session (session : Session.Session_file.t) ~sw ~env =
  let client =
    M.Client.create ~sw ~env ~homeserver:session.server.homeserver ()
  in
  M.Client.with_session client
    {
      user_id = session.server.user_id;
      device_id = session.auth.device_id;
      access_token = session.auth.access_token;
      refresh_token = session.auth.refresh_token;
    }

let login_and_save ~sw ~env ~profile ~homeserver ~username ~password store =
  match (username, password) with
  | Some user, Some password ->
      let client = M.login_password ~sw ~env ~homeserver ~user ~password () in
      let session = Option.get (M.Client.session client) in
      let now = Ptime_clock.now () in
      let file : Session.Session_file.t =
        {
          server = { homeserver; user_id = session.user_id };
          auth =
            {
              access_token = session.access_token;
              device_id = session.device_id;
              refresh_token = session.refresh_token;
              access_token_expires_at = None;
              method_ = Session.Auth.Matrix;
            };
          sync = { next_batch = None; filter_id = None };
          metadata =
            { created_at = now; last_used_at = now; client_name = "8-cli" };
        }
      in
      (match Profile_store.save_session store file with
      | Ok () -> ()
      | Error e ->
          Logs.err (fun m ->
              m "Cannot save the session: %a" Matrix_client.Error.pp e);
          exit Cli.exit_internal);
      Logs.app (fun m -> m "Logged in and saved session to profile %S" profile);
      client
  | _ ->
      Logs.err (fun m ->
          m "No session for profile %S; pass --username and set MATRIX_PASSWORD"
            profile);
      exit Cli.exit_usage

let run () homeserver username password profile =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix" in
  let store = Profile_store.create ~xdg ~profile in
  let client =
    match Profile_store.load_session store with
    | Ok (Some session) ->
        Logs.app (fun m -> m "Reusing session for profile %S" profile);
        client_of_session session ~sw ~env
    | Ok None ->
        login_and_save ~sw ~env ~profile ~homeserver ~username ~password store
    | Error e ->
        Logs.err (fun m ->
            m "The session of profile %S is unreadable: %a" profile
              Matrix_client.Error.pp e);
        exit Cli.exit_internal
  in
  let who = M.Auth.whoami client in
  Logs.app (fun m ->
      m "Logged in as %s" (Matrix_proto.Id.User_id.to_string who))

let term =
  Cmdliner.Term.(
    const run $ Cli.verbosity_term $ Cli.homeserver_term $ Cli.username_opt_term
    $ Cli.password_opt_term $ Cli.profile_term)

let cmd =
  let doc = "keep a Matrix session on disk, logging in only when needed" in
  let man =
    [
      `S Cmdliner.Manpage.s_description;
      `P
        "Loads the session stored under $(b,--profile). If none is stored, \
         logs in with $(b,--username) and $(b,MATRIX_PASSWORD) and stores it \
         for the next run.";
    ]
  in
  let exits =
    Cmdliner.Cmd.Exit.defaults
    @ [
        Cmdliner.Cmd.Exit.info Cli.exit_usage
          ~doc:"there is no session, and no password to create one";
        Cmdliner.Cmd.Exit.info Cli.exit_internal ~doc:"the profile is damaged";
      ]
  in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "cli" ~doc ~man ~exits) term

let () = exit (Cmdliner.Cmd.eval cmd)
