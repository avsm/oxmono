open! Core

val run : ?separator:char -> ?one_row_at_a_time:bool -> Csv_common.Or_file.t -> unit
