let setup_logs level =
  Logs.set_level (Some level);
  Logs.set_reporter (Logs_fmt.reporter ())

let run_mode ~name ?version ?(doc = "Run a Zulip bot") action () =
  let open Cmdliner in
  let profile =
    Arg.(
      value & opt string "default"
      & info [ "profile" ] ~docv:"NAME" ~doc:"XDG Zulip profile to use.")
  in
  let zuliprc =
    Arg.(
      value
      & opt (some string) None
      & info [ "zuliprc" ] ~docv:"FILE"
          ~doc:
            "Import this standard zuliprc into the selected profile before \
             connecting.")
  in
  let site =
    Arg.(
      value
      & opt (some string) None
      & info [ "site" ] ~docv:"URL" ~doc:"Override the profile's Zulip site.")
  in
  let email =
    Arg.(
      value
      & opt (some string) None
      & info [ "email" ] ~docv:"EMAIL" ~doc:"Override the profile's API email.")
  in
  let api_key =
    Arg.(
      value
      & opt (some string) None
      & info [ "api-key" ] ~docv:"KEY" ~doc:"Override the profile's API key.")
  in
  let allow_insecure =
    Arg.(
      value & flag
      & info [ "allow-insecure-http" ]
          ~doc:
            "Allow HTTP credentials for a local test server. HTTPS certificate \
             checking is unchanged.")
  in
  let start profile zuliprc site email api_key allow_insecure =
    setup_logs Logs.Info;
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let profile =
      match zuliprc with
      | None -> profile
      | Some path -> (
          match
            Zulip_eio.Profile.import_zuliprc ~fs:env#fs ~name:profile
              Eio.Path.(env#fs / path)
          with
          | Ok profile -> Zulip_eio.Profile.name profile
          | Error error ->
              raise (Failure (Zulip_eio.Error.error_to_string error)))
    in
    match
      Zulip_bot.Context.connect ~sw ~env ~profile ?site ?email ?api_key
        ~allow_insecure ()
    with
    | Error error -> raise (Failure (Zulip_eio.Error.error_to_string error))
    | Ok context -> action sw context
  in
  let term =
    Term.(
      const start $ profile $ zuliprc $ site $ email $ api_key $ allow_insecure)
  in
  exit (Cmd.eval (Cmd.v (Cmd.info name ?version ~doc) term))

let run ~name ?version ?doc spec () =
  run_mode ~name ?version ?doc
    (fun sw context ->
      let running = ref None in
      let stop_requested = Atomic.make false in
      let stopped = Eio.Condition.create () in
      let signal _ =
        Atomic.set stop_requested true;
        Eio.Condition.broadcast stopped
      in
      let previous_int = Sys.signal Sys.sigint (Sys.Signal_handle signal) in
      let previous_term = Sys.signal Sys.sigterm (Sys.Signal_handle signal) in
      Fun.protect ~finally:(fun () ->
          Sys.set_signal Sys.sigint previous_int;
          Sys.set_signal Sys.sigterm previous_term)
      @@ fun () ->
      Eio.Fiber.fork_daemon ~sw (fun () ->
          Eio.Condition.await_no_mutex stopped;
          Option.iter Zulip_bot.Bot.stop !running;
          `Stop_daemon);
      Zulip_bot.Bot.run
        ~on_start:(fun bot ->
          running := Some bot;
          if Atomic.get stop_requested then Zulip_bot.Bot.stop bot)
        context spec)
    ()

let run_once ~name ?version ?doc action () =
  run_mode ~name ?version ?doc (fun _sw context -> action context) ()
