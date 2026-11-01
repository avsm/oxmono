open! Core
open Async

let running = ref []
let jobs = ref []
let stopping = ref false

let stop_helpers () =
  stopping := true;
  List.iter !running ~f:(fun process -> Process.send_signal process Signal.kill);
  let%map () = Deferred.List.iter !jobs ~how:`Parallel ~f:Ivar.read in
  stopping := false

let collect_helper ?timeout ~prog ~args ~stdin () =
  let%bind created = Process.create ~prog ~args ~stdin () in
  match created with
  | Error _ -> return (Or_error.error_string "Helper could not start")
  | Ok process ->
      running := process :: !running;
      if !stopping then Process.send_signal process Signal.kill;
      Monitor.protect
        (fun () ->
          let output = Process.collect_stdout_and_wait process in
          match timeout with
          | None -> output
          | Some seconds -> (
              let%bind result =
                Clock_ns.with_timeout (Time_ns.Span.of_sec seconds) output
              in
              match result with
              | `Result result -> return result
              | `Timeout ->
                  Process.send_signal process Signal.kill;
                  let%map _ = output in
                  Or_error.error_string "Helper timed out"))
        ~finally:(fun () ->
          Process.send_signal process Signal.kill;
          let%map _ = Process.wait process in
          running :=
            List.filter !running ~f:(fun p -> not (phys_equal p process)))

let run_helper ?timeout ~prog ~args ~stdin () =
  (* Include processes still being created when the terminal exits. *)
  let finished = Ivar.create () in
  jobs := finished :: !jobs;
  Monitor.protect
    (fun () -> collect_helper ?timeout ~prog ~args ~stdin ())
    ~finally:(fun () ->
      jobs := List.filter !jobs ~f:(fun job -> not (phys_equal job finished));
      Ivar.fill_exn finished ();
      return ())

let command =
  Command.async_or_error ~summary:"Review Siftbot's local corpus"
    (let%map_open.Command profile =
       flag "--profile"
         (optional_with_default "personal" string)
         ~doc:"NAME Local profile"
     and demo = flag "--demo" no_arg ~doc:" Use synthetic mail only"
     and setup = flag "--setup" no_arg ~doc:" Open connection profiles" in
     fun () ->
       let exe = Filename_unix.realpath Sys.executable_name in
       let sibling source installed =
         Filename.concat (Filename.dirname exe)
           (if String.is_suffix exe ~suffix:".exe" then source else installed)
       in
       let worker = sibling "siftbot_worker.exe" "siftbot-worker" in
       let config = sibling "siftbot_config.exe" "siftbot-config" in
       let configure request =
         let%map result =
           run_helper ~timeout:10. ~prog:config ~args:[]
             ~stdin:(Siftbot_setup.request request)
             ()
         in
         match result with
         | Error _ ->
             Siftbot_setup.Error
               "Setup helper failed or timed out. Ctrl-C quits. Reopen setup \
                to retry."
         | Ok raw -> Siftbot_setup.parse_response raw
       in
       let rec run profile ~demo ~setup =
         let next = ref None in
         let%bind result =
           if setup then
             Bonsai_term.start_with_exit ~dispose:true
               ~mouse:Bonsai_term.Mouse_reporting.No_mouse_events
               (fun ~exit ~dimensions graph ->
                 let select name =
                   next := Some (name, false, false);
                   exit ()
                 in
                 Siftbot_ui.Settings.app ~default_name:profile
                   ~execute:(fun r ->
                     Bonsai_term.Effect.of_deferred_thunk (fun () ->
                         configure r))
                   ~select ~exit ~dimensions graph)
           else
             let execute request =
               Bonsai_term.Effect.of_deferred_thunk (fun () ->
                   let%map result =
                     run_helper ~prog:worker
                       ~args:
                         ([ "--profile"; profile ]
                         @ if demo then [ "--demo" ] else [])
                       ~stdin:(Siftbot_protocol.request request)
                       ()
                   in
                   match result with
                   | Error _ ->
                       Error "Worker failed. Check profile and installation."
                   | Ok raw -> (
                       match Siftbot_protocol.parse_response raw with
                       | Ok r -> r
                       | Error e -> Error e))
             in
             Bonsai_term.start_with_exit ~dispose:true
               ~mouse:Bonsai_term.Mouse_reporting.No_mouse_events
               (fun ~exit ~dimensions graph ->
                 let configure () =
                   next := Some (profile, demo, true);
                   exit ()
                 in
                 Siftbot_ui.app
                   ~profile_name:(if demo then "synthetic-demo" else profile)
                   ~configure ~execute ~exit ~dimensions graph)
         in
         let%bind () = stop_helpers () in
         match (result, !next) with
         | Ok (), Some (profile, demo, setup) -> run profile ~demo ~setup
         | _ -> return result
       in
       let%bind setup =
         if setup || demo then return setup
         else
           let%map result = configure Siftbot_setup.List in
           match result with
           | Profiles profiles ->
               not
                 (List.exists profiles ~f:(fun p ->
                      String.equal p.Siftbot_setup.name profile))
           | _ -> true
       in
       Monitor.protect
         (fun () -> run profile ~demo ~setup)
         ~finally:stop_helpers)

let () = Command_unix.run command
