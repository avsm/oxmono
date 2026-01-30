open Core
open Csv_common

let sum csv =
  let line =
    List.fold
      csv.lines
      ~init:(List.map csv.header ~f:(fun _ -> 0.))
      ~f:
        (List.map2_exn ~f:(fun sum x ->
           sum
           +.
           try Float.of_string x with
           | _ -> 0.))
    |> List.map ~f:Float.to_string_12
  in
  { header = csv.header; lines = [ line ] }
;;

let run ?separator file =
  Or_file.with_all file ?separator ~no_header:false ~f:(fun csv ->
    sum csv |> print_csv ?separator)
;;
