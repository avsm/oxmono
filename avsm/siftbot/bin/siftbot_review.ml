open! Core
open Async

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
           Process.run ~prog:config ~args:[]
             ~stdin:(Siftbot_setup.request request)
             ()
         in
         match result with
         | Error _ ->
             Siftbot_setup.Error "Setup helper failed. Check installation."
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
                     Process.run ~prog:worker
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
       run profile ~demo ~setup)

let () = Command_unix.run command
