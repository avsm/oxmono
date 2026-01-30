open Core
open Csv_common

let row_count csv = List.length csv.lines

let run ?separator file =
  Or_file.with_all file ?separator ~no_header:false ~f:(fun csv ->
    row_count csv |> Int.to_string |> print_endline)
;;
