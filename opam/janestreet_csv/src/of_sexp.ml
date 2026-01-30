open! Core
open! Async
open! Composition_infix

let parse_sexp of_sexp sexp =
  match of_sexp sexp with
  | x -> x
  | exception exn -> raise_s [%message "Malformed row sexp." (exn : exn) (sexp : Sexp.t)]
;;

module Row = struct
  type t = string String.Map.t [@@deriving sexp]

  let parse_sexp = parse_sexp [%of_sexp: t]

  let to_csv_row t ~header ~row_number:row =
    if Map.length t <> List.length header
    then
      raise_s
        [%message
          "Row has wrong number of fields." (header : string list) (row : int) ~_:(t : t)]
    else
      List.map header ~f:(fun field ->
        match Map.find t field with
        | None ->
          raise_s
            [%message "Missing field in a row." (field : string) (row : int) ~_:(t : t)]
        | Some value -> value)
  ;;
end

let get_header sexps =
  match%bind Pipe.values_available sexps with
  | `Eof -> return `Eof
  | `Ok ->
    (match Pipe.peek sexps with
     (* In general this is valid. Here we know nobody else is reading from the pipe. *)
     | None -> raise_s [%message "Pipe has no values despite [values_available]."]
     | Some row ->
       return (`Ok (List.map (parse_sexp [%of_sexp: (string * string) list] row) ~f:fst)))
;;

let command =
  let summary = "of sexp" in
  Async.Command.async
    ~summary
    (let%map_open.Csv_param separator = sep
     and include_header = map ~f:not no_header in
     fun () ->
       let sexps = Reader.read_sexps (Lazy.force Reader.stdin) in
       match%bind get_header sexps with
       | `Eof -> return ()
       | `Ok header ->
         if include_header then Csvlib.Csv.print ~separator [ header ];
         Pipe.folding_map sexps ~init:1 ~f:(fun row_number row ->
           let row = Row.parse_sexp row in
           row_number + 1, Row.to_csv_row row ~header ~row_number)
         |> Pipe.iter' ~f:(fun rows ->
           Csvlib.Csv.print ~separator (Queue.to_list rows);
           return ()))
    ~behave_nicely_in_pipeline:false
;;
