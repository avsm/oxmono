open! Core
open! Async_kernel
open! Import
include Async_log_kernel.Log

module Private = struct
  include Private

  let logging_shutdown_handlers = ref []

  let register_logging_shutdown_handler f =
    logging_shutdown_handlers := f :: !logging_shutdown_handlers
  ;;

  let () =
    Shutdown.Private.run_after_other_shutdown_handlers (fun () ->
      let%bind () = Private.all_live_logs_flushed () in
      Deferred.List.iter !logging_shutdown_handlers ~how:`Parallel ~f:(fun f -> f ()))
  ;;
end
