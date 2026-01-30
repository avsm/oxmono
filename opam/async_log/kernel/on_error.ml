open! Core
open Async_kernel
open! Import

type t =
  [ `Raise
  | `Call of Error.t -> unit
  ]

let send_to_top_level_monitor =
  `Call
    (fun e ->
      let e =
        try Error.raise e with
        | e -> e
      in
      Monitor.send_exn Monitor.main ~backtrace:`Get e)
;;

let handle_error = function
  | `Raise -> fun x -> raise x
  | `Call f -> Fn.compose f Error.of_exn
;;
