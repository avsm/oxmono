open Core
open Poly
open Csv_common
module Time = Time_float_unix

let grid ~field ~start ~stop ~step csv =
  match List.findi csv.header ~f:(fun _idx elem -> elem = field) with
  | None -> failwithf "unable to find csv field %s" field ()
  | Some (field_idx, _) ->
    let num_times =
      Time.Span.( // ) (Time.diff stop start) step |> Float.iround_down_exn
    in
    let grid_times =
      List.init num_times ~f:(fun i ->
        Time.add start (Time.Span.scale step (Float.of_int (i + 1))))
    in
    let dummy_line = Array.create ~len:(List.length csv.header) "" in
    let old_lines = List.map csv.lines ~f:Array.of_list |> Array.of_list in
    let old_times =
      Array.map old_lines ~f:(fun old_line -> Time.of_string old_line.(field_idx))
    in
    let rec increment_to_max_on_or_before_grid_time i grid_time =
      if Time.( < ) grid_time old_times.(i)
      then i - 1
      else if i = Array.length old_times - 1
      then i
      else increment_to_max_on_or_before_grid_time (i + 1) grid_time
    in
    let lines =
      List.fold grid_times ~init:(0, []) ~f:(fun (prior_line_idx, l) grid_time ->
        let new_idx = increment_to_max_on_or_before_grid_time prior_line_idx grid_time in
        if new_idx < 0
        then (
          let add_line = Array.copy dummy_line in
          add_line.(field_idx) <- Time.to_string grid_time;
          0, add_line :: l)
        else (
          let add_line = Array.copy old_lines.(new_idx) in
          add_line.(field_idx) <- Time.to_string grid_time;
          new_idx, add_line :: l))
      |> snd
      |> List.map ~f:Array.to_list
      |> List.rev
    in
    { header = csv.header; lines }
;;

let run ?separator ~field ~start ~stop ~step file =
  Or_file.with_all ?separator file ~no_header:false ~f:(fun csv ->
    csv
    |> Csv_sort.sort_on_fields
         Csv_sort.Csv_kind.has_header
         [ { field; order = Ascending; sort_type = Time } ]
    |> grid ~field ~start ~stop ~step
    |> print_csv ?separator)
;;
