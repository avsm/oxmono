open! Core

val run
  :  ?separator:char
  -> field:string
  -> start:Time_float.t
  -> stop:Time_float.t
  -> step:Time_float.Span.t
  -> Csv_common.Or_file.t
  -> unit
