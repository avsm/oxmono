open Csv_common

let run ?separator ?limit_width_to ?(prefer_split_on_spaces = true) file =
  Or_file.with_all file ?separator ~no_header:false ~f:(fun csv ->
    let open Ascii_table in
    simple_list_table
      ~display:Display.short_box
      ?limit_width_to
      ~prefer_split_on_spaces
      csv.header
      csv.lines)
;;
