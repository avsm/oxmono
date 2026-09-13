(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open! Core
open Async

let worker_path explicit =
  match explicit with
  | Some path -> path
  | None ->
      let exe = Filename_unix.realpath Sys.executable_name in
      let name =
        if String.is_suffix exe ~suffix:".exe" then "termanil_worker.exe"
        else "termanil-worker"
      in
      Filename.concat (Filename.dirname exe) name

let run_worker ~worker ~config ~args ?stdin () =
  let args =
    Option.value_map config ~default:args ~f:(fun path ->
        [ "--config"; path ] @ args)
  in
  Process.run ~prog:worker ~args ?stdin ()

let command =
  Command.async_or_error ~summary:"JMAP mail, Sortal/CardDAV contacts and Dooit"
    (let%map_open.Command demo =
       flag "--demo" no_arg ~doc:" Run the persistent synthetic JMAP server"
     and demo_dir =
       flag "--demo-dir" (optional string)
         ~doc:"DIR Isolated demo state directory (implies --demo)"
     and config =
       flag "--config" (optional string) ~doc:"FILE XDG TOML configuration"
     and init =
       flag "--init-config" no_arg ~doc:" Create an example XDG configuration"
     and check =
       flag "--check-worker" no_arg ~doc:" Check the native worker protocol"
     and worker =
       flag "--worker" (optional string) ~doc:"PATH Override native worker path"
     in
     fun () ->
       let worker = worker_path worker in
       if init || check then
         let%map result =
           run_worker ~worker ~config
             ~args:[ (if init then "--init-config" else "--check") ]
             ()
         in
         Result.map result ~f:(fun text -> printf "%s" text)
       else
         let demo_args =
           Option.value_map demo_dir
             ~default:(if demo then [ "--demo" ] else [])
             ~f:(fun path -> [ "--demo-dir"; path ])
         in
         let execute request =
           Bonsai_term.Effect.of_deferred_thunk (fun () ->
               let%map result =
                 run_worker ~worker ~config ~args:demo_args
                   ~stdin:(Termanil_protocol.request request)
                   ()
               in
               match result with
               | Error _ ->
                   Error
                     "Native worker failed. Check --check-worker and --config."
               | Ok raw -> (
                   match Termanil_protocol.parse_response raw with
                   | Ok result -> result
                   | Error s -> Error s))
         in
         Bonsai_term.start_with_exit ~dispose:true
           ~mouse:Bonsai_term.Mouse_reporting.No_mouse_events
           (Termanil_ui.app
              ~initial:
                {
                  Termanil_model.initial with
                  demo = demo || Option.is_some demo_dir;
                }
              ~autoload:true ~execute))

let () = Command_unix.run command
