open Core

let run ?separator ?(one_row_at_a_time = false) file =
  Csv_common.Or_file.with_all
    file
    ?separator
    ~no_header:false
    ~f:(fun (csv : Csv_common.t) ->
      if one_row_at_a_time
      then
        List.iter csv.lines ~f:(fun line ->
          Csvlib.Csv.print ?separator (List.transpose_exn (csv.header :: [ line ])))
      else Csvlib.Csv.print ?separator (List.transpose_exn (csv.header :: csv.lines)))
;;
