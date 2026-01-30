open! Core

val field_names : sep:char -> Csv_common.Or_file.t -> string list

(* [row_processor input ~skip_header ~sep ~f headers] reads rows from [input], applies
   some preprocessing to the rows based on [headers] and finally applies [f] to the new
   rows *)

type row_processor =
  Csv_common.Or_file.t
  -> skip_header:bool
  -> sep:char
  -> f:(string array -> unit)
  -> [ `Limit_to of string list | `All_but of string list ]
  -> unit

(* cut the rows down to just the columns matching [headers] and apply [f] to each new row *)

val cut_by_field_names : row_processor
val cut_by_field_indices : row_processor

val regex_match
  :  Csv_common.Or_file.t
  -> sep:char
  -> f:(string array -> unit)
  -> regexp:Pcre.regexp
  -> unit

(* apply [f] to rows in which _all_ columns matching [headers] are populated *)

val fully_populated_rows : row_processor

(* apply [f] to rows in which _at_least_one_ column matching [headers] is not populated *)

val not_fully_populated_rows : row_processor
