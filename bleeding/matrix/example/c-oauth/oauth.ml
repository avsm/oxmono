module M = Matrix_eio
module Cli = Matrix_cli

let run () homeserver =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = M.connect ~sw ~env ~homeserver () in
  let open_url url =
    Logs.app (fun m -> m "Open this URL in a browser to authorise this client:");
    Logs.app (fun m -> m "");
    Logs.app (fun m -> m "  %s" url);
    Logs.app (fun m -> m "");
    Logs.app (fun m -> m "Waiting for the browser to come back...");
    M.Oauth.browser_opener ~sw ~env url
  in
  let session = M.Oauth.login_with_browser ~env client ~open_url () in
  let client = M.Client.with_session client session in
  Logs.app (fun m ->
      m "Logged in as %s on device %s"
        (Matrix_proto.Id.User_id.to_string (M.Auth.whoami client))
        (Matrix_proto.Id.Device_id.to_string session.device_id))

let term = Cmdliner.Term.(const run $ Cli.verbosity_term $ Cli.homeserver_term)

let cmd =
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "oauth" ~doc:"Log in with OAuth 2.0 in a browser")
    term

let () = exit (Cmdliner.Cmd.eval cmd)
