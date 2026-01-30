open! Core
open! Import

type t =
  [ `Raise
  | `Call of Error.t -> unit
  ]

val send_to_top_level_monitor : t
val handle_error : t -> exn -> unit
