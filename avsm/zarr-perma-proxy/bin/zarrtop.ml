open! Core
open Async

let command =
  Command.async_or_error
    ~summary:"Watch Zarr proxy connections, traffic and cache effectiveness"
    (let%map_open.Command cache_dir =
       flag "--cache-dir"
         (optional_with_default "./perma-cache" string)
         ~doc:"DIR Proxy cache"
     and interval =
       flag "--interval"
         (optional_with_default 2. float)
         ~doc:"SECONDS Poll interval (at least 0.5)"
     in
     fun () ->
       if (not (Float.is_finite interval)) || Float.(interval < 0.5) then
         return (Or_error.error_string "Interval must be at least 0.5 seconds")
       else
         let exe = Filename_unix.realpath Sys.executable_name in
         let worker =
           Filename.concat (Filename.dirname exe)
             (if String.is_suffix exe ~suffix:".exe" then "main.exe"
              else "zarr-perma-proxy")
         in
         let running = ref None and stopping = ref false and job = ref None in
         let poll () =
           Bonsai_term.Effect.of_deferred_thunk (fun () ->
               let done_ = Ivar.create () in
               job := Some done_;
               Monitor.protect
                 (fun () ->
                   let%bind result =
                     Process.create ~prog:worker
                       ~args:[ "snapshot"; "--cache-dir"; cache_dir ]
                       ()
                   in
                   match result with
                   | Error _ ->
                       return
                         (Error
                            "Cannot start telemetry helper. Check installation.")
                   | Ok process ->
                       running := Some process;
                       if !stopping then Process.send_signal process Signal.kill;
                       Monitor.protect
                         (fun () ->
                           let output =
                             Process.collect_stdout_and_wait process
                           in
                           let%bind result =
                             Clock_ns.with_timeout (Time_ns.Span.of_sec 5.)
                               output
                           in
                           match result with
                           | `Timeout ->
                               Process.send_signal process Signal.kill;
                               let%map _ = output in
                               Error "Telemetry query timed out."
                           | `Result (Error _) ->
                               return
                                 (Error
                                    "Cannot read telemetry. Check cache \
                                     directory and proxy version.")
                           | `Result (Ok text) -> return (Ok text))
                         ~finally:(fun () ->
                           Process.send_signal process Signal.kill;
                           let%map _ = Process.wait process in
                           running := None))
                 ~finally:(fun () ->
                   job := None;
                   Ivar.fill_exn done_ ();
                   return ()))
         in
         Monitor.protect
           (fun () ->
             Bonsai_term.start_with_exit ~dispose:true
               ~mouse:Bonsai_term.Mouse_reporting.No_mouse_events
               (Zarrtop_ui.app ~poll ~interval))
           ~finally:(fun () ->
             stopping := true;
             Option.iter !running ~f:(fun process ->
                 Process.send_signal process Signal.kill);
             match !job with
             | None -> return ()
             | Some finished -> Ivar.read finished))

let () = Command_unix.run command
