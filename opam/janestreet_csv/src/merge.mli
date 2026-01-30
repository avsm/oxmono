open! Core

val merge : Csv_common.t list -> Csv_common.t
val run : ?separator:char -> string list -> unit
