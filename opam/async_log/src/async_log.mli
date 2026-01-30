open! Core
open! Async_kernel
open! Import
include module type of Log
module Blocking = Blocking
module Level = Async_log_kernel.Level
module Message = Message
module Message_event = Async_log_kernel.Message_event
module Output = Output
module Reader = Message_reader
module Rotation = Rotation
module Rotation_id = Rotation_id

(*_ The below are not direct [module X = Async_log_kernel.X] statements because of some
    sneaky behaviour with top-level effects.

    Top-level effects in this library (as in [Assign_top_level_logs]) are only run if the
    library is actually used. If someone uses [Async_log.Global] and we had
    [module Global = Async_log_kernel.Global], then the top-level effects are not run,
    probably because the compiler inferred only [Async_log_kernel] was used. Declaring
    that their signatures are the same however seems to preserve the effects.

    (The specific top-level effect we want is for the stderr output used by the global log
    to use [Writer.stderr] instead of [Core.prerr_endline]. So it makes sense to de-alias
    [Global] and [Ppx_log_syntax] specifically.)

    (This change would break type equalities, but there's no [type Global.t] or
    [type Ppx_log_syntax.t], so this seems OK.)
*)
module Global : module type of Async_log_kernel.Global
module Ppx_log_syntax : module type of Async_log_kernel.Ppx_log_syntax
