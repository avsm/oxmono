open! Core
open Async

type t =
  { stdin_reader : Reader.t
  ; stdin_writer : string Pipe.Writer.t
  ; stdout_reader : string Pipe.Reader.t
  ; stdout_writer : Writer.t
  }

let create_default_for_testing () =
  let stdin_reader, stdin_writer = Pipe.create () in
  let%bind stdin_reader =
    Reader.of_pipe (Info.of_string "Bonsai_term fake STDIN") stdin_reader
  in
  let stdout_reader, stdout_writer = Pipe.create () in
  let%bind stdout_writer, `Closed_and_flushed_downstream _stdout_is_closed =
    Writer.of_pipe (Info.of_string "Bonsai_term fake STDOUT") stdout_writer
  in
  let out = { stdin_reader; stdin_writer; stdout_reader; stdout_writer } in
  return out
;;
