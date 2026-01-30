open Core
open Async

module Target_fields = struct
  module T = struct
    type t =
      | All
      | Field_names of String.Set.t
    [@@deriving compare, sexp_of]
  end

  include T

  include struct
    open Command.Param

    let arg_type =
      Arg_type.comma_separated ~strip_whitespace:true string ~allow_empty:true
      |> Arg_type.map ~f:(function
        | [] -> All
        | list -> Field_names (String.Set.of_list list))
    ;;

    let param =
      flag
        "-grep-fields"
        (optional_with_default All arg_type)
        ~doc:"_ comma separated fieldnames to grep in, defaults to all"
        ~aliases:[ "--grep-fields" ]
    ;;
  end
end

let the_headers = ref []

let header =
  `Transform
    (fun headers ->
      the_headers := headers;
      headers)
;;

let run ?separator ?skip_lines ~invert ~always_print_header ~grep_fields ~regexp file =
  let delimited_reader_pipe reader =
    Delimited.Read.pipe_of_reader
      Delimited.Read.Row.builder
      ?skip_lines
      ~header
      ?sep:separator
      reader
  in
  let csv_pipe ({ header; lines } : Csv_common.t) =
    let header =
      List.mapi header ~f:(fun i header -> header, i)
      |> Hashtbl.of_alist_exn (module String)
    in
    Pipe.of_list
      (List.map lines ~f:(fun line ->
         Delimited.Read.Row.create header (Array.of_list line)))
  in
  let run rows_pipe =
    Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
      let writer =
        Delimited.Write.Expert.By_row.of_writer_and_close
          ?sep:separator
          (Lazy.force Writer.stdout)
      in
      Pipe.fold rows_pipe ~init:`Haven't_printed_header ~f:(fun is_header_printed row ->
        (match is_header_printed with
         | `Header_printed -> Deferred.return `Header_printed
         | `Haven't_printed_header ->
           Pipe.write_if_open writer !the_headers >>| fun () -> `Header_printed)
        >>= fun is_header_printed ->
        let matches_grep =
          Delimited.Read.Row.fold row ~init:false ~f:(fun print_it ~header ~data ->
            print_it
            ||
            let possible_to_check =
              match grep_fields with
              | Target_fields.All -> true
              | Target_fields.Field_names field_name_set -> Set.mem field_name_set header
            in
            if possible_to_check then Re2.matches regexp data else false)
        in
        (if Bool.( <> ) matches_grep invert
         then Pipe.write_if_open writer (Delimited.Read.Row.to_list row)
         else Deferred.unit)
        >>| fun () -> is_header_printed)
      >>= function
      | `Haven't_printed_header ->
        if always_print_header
        then Pipe.write_if_open writer !the_headers
        else Deferred.unit
      | `Header_printed -> Deferred.unit)
  in
  match (file : Csv_common.Or_file.t) with
  | Csv csv -> run (csv_pipe csv)
  | Stdin | File "-" -> run (delimited_reader_pipe (Lazy.force Reader.stdin))
  | File x -> Reader.with_file x ~f:(fun reader -> run (delimited_reader_pipe reader))
;;
