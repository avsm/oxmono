module Cli = Matrix_cli
module Log = (val Logs.src_log Logging.src : Logs.LOG)

type login = {
  homeserver : Uriz.t option;
  username : string option;
  password : string option;
  profile : string;
}

type mode =
  | Run_bot of Bot.spec
  | Run_once of (Context.t -> Cmdliner.Cmd.Exit.code)

let login_term =
  let build homeserver username password profile =
    { homeserver; username; password; profile }
  in
  Cmdliner.Term.(
    const build $ Cli.homeserver_opt_term $ Cli.username_opt_term
    $ Cli.password_opt_term $ Cli.profile_term)

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

(* A signal handler runs between two instructions of whatever fiber the
   scheduler happened to be in, so the only thing it may touch is a
   primitive that takes no lock: [Eio.Condition.broadcast]. Everything the
   shutdown actually does happens in the fiber waiting on it. *)
let on_signals stop =
  let condition = Eio.Condition.create () in
  let handler =
    Sys.Signal_handle (fun _ -> Eio.Condition.broadcast condition)
  in
  List.iter
    (fun signal -> Sys.set_signal signal handler)
    [ Sys.sigint; Sys.sigterm ];
  fun () ->
    Eio.Condition.await_no_mutex condition;
    stop ()

(* [with_context] is the common command-line envelope.  In particular, keep
   the profile and login handling here so a command that does not run a bot
   gets exactly the same authentication behaviour as [run]. *)
let with_context ~encrypt ~persist_events login style_renderer level action =
  setup_logs style_renderer level;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match
    Context.connect ~env ~sw ~profile:login.profile ?homeserver:login.homeserver
      ?username:login.username ?password:login.password ~encrypt ~persist_events
      ()
  with
  | Error error ->
      Log.err (fun m -> m "%a" Context.pp_error error);
      Cli.exit_auth
  | Ok ctx -> action ctx

let run_bot ctx spec =
  let running = ref None in
  let stop_requested = ref false in
  let request_stop () =
    stop_requested := true;
    Option.iter (fun bot -> Bot.stop bot) !running
  in
  let wait = on_signals request_stop in
  Eio.Fiber.fork_daemon ~sw:(Context.switch ctx) (fun () ->
      wait ();
      `Stop_daemon);
  Bot.run
    ~on_start:(fun bot ->
      running := Some bot;
      if !stop_requested then Bot.stop bot)
    ctx spec;
  Cli.exit_ok

(* [Context.save] may touch the filesystem and must be allowed to finish even
   when the action is cancelled.  The [finally] runs while the context's
   switch is still alive, before [Eio.Switch.run] starts tearing it down. *)
let with_saved_context ctx action =
  Fun.protect
    ~finally:(fun () -> Eio.Cancel.protect (fun () -> Context.save ctx))
    (fun () -> action ctx)

let run_once_with_context ctx action = with_saved_context ctx action

let run_mode_with_context ctx = function
  | Run_bot spec -> run_bot ctx spec
  | Run_once action -> run_once_with_context ctx action

let start ~encrypt ~persist_events mode login style_renderer level =
  with_context ~encrypt ~persist_events login style_renderer level (fun ctx ->
      run_mode_with_context ctx mode)

let command_info name version doc man =
  let open Cmdliner in
  Cmd.info name ?version ?doc ?man
    ~exits:
      [
        Cmd.Exit.info Cli.exit_ok ~doc:"on success.";
        Cmd.Exit.info Cli.exit_auth
          ~doc:"when there is nothing to log in with, or the login fails.";
        Cmd.Exit.info Cmd.Exit.some_error
          ~doc:"when a one-shot action reports that it failed.";
      ]

let command ~name ?version ?doc ?man term =
  let open Cmdliner in
  Cmd.v (command_info name version doc man) term

let run_mode ~name ?version ?doc ?man ?(encrypt = true) ?(persist_events = true)
    mode =
  let open Cmdliner in
  let term =
    Term.(
      const (start ~encrypt ~persist_events)
      $ mode $ login_term $ Fmt_cli.style_renderer () $ Logs_cli.level ())
  in
  exit (Cmd.eval' (command ~name ?version ?doc ?man term))

let run ~name ?version ?doc ?man ?encrypt ?persist_events spec =
  let mode = Cmdliner.Term.(const (fun spec -> Run_bot spec) $ spec) in
  run_mode ~name ?version ?doc ?man ?encrypt ?persist_events mode

let run_once ~name ?version ?doc ?man ?encrypt ?persist_events action =
  let mode = Cmdliner.Term.(const (fun action -> Run_once action) $ action) in
  run_mode ~name ?version ?doc ?man ?encrypt ?persist_events mode

let plugin_flag ~name ~doc plugin =
  let open Cmdliner in
  Term.(
    const (fun plugin enabled -> if enabled then plugin else Fun.id)
    $ plugin
    $ Arg.(value & flag & info [ name ] ~doc))

let compose plugins base =
  Cmdliner.Term.(
    const (fun plugins ->
        List.fold_left (fun spec plugin -> plugin spec) base plugins)
    $ plugins)
