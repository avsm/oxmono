open Core
open Csv_common

let run ?separator ~no_header ~suppress_header file =
  let row =
    let buffer = Buffer.create 1000 (* probably longer than a legible table row? *) in
    fun ~delimiter cells ->
      Buffer.clear buffer;
      Buffer.add_string buffer delimiter;
      (match cells with
       | [] -> raise_s [%message "Expected at least one cell."]
       | _ :: _ ->
         List.iter cells ~f:(fun cell ->
           let cell = if String.is_empty cell then " " else cell in
           Buffer.add_string buffer cell;
           Buffer.add_string buffer delimiter));
      Buffer.contents buffer
  in
  Or_file.with_all file ?separator ~no_header ~f:(fun csv ->
    (match no_header || suppress_header with
     | true -> ()
     | false -> print_endline (row ~delimiter:"||" csv.header));
    List.iter csv.lines ~f:(fun cells -> print_endline (row cells ~delimiter:"|")))
;;
