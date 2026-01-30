open! Core
open! Async

let summary = "Changes the delimiter, respecting quoting"

let main =
  let%map_open.Csv_param () = return ()
  and in_sep =
    flag
      "-input-separator"
      (optional_with_default ',' sep_arg)
      ~doc:"CHAR separator expected in input"
  and out_sep =
    flag "-output-separator" (required sep_arg) ~doc:"CHAR separator desired in output"
  and filename = anon (maybe ("FILE" %: Filename_unix.arg_type)) in
  fun () ->
    let%bind r =
      match filename with
      | None ->
        return
          (Delimited.Read.pipe_of_reader
             Delimited.Read.Row.builder
             ~sep:in_sep
             ~header:`No
             (force Reader.stdin))
      | Some filename ->
        Delimited.Read.create_reader
          Delimited.Read.Row.builder
          ~sep:in_sep
          ~header:`No
          filename
    in
    let w =
      Delimited.Write.Expert.By_row.of_writer_and_close ~sep:out_sep (force Writer.stdout)
    in
    let%map () =
      Pipe.iter r ~f:(fun row -> Pipe.write_if_open w (Delimited.Read.Row.to_list row))
    in
    Pipe.close_read r;
    Pipe.close w
;;

let command = Command.async main ~summary ~behave_nicely_in_pipeline:false
