open! Core
open Async

type t =
  { stdin_reader : Reader.t
  ; stdin_writer : string Pipe.Writer.t
  ; stdout_reader : string Pipe.Reader.t
  ; stdout_writer : Writer.t
  }

val create_default_for_testing : unit -> t Deferred.t
