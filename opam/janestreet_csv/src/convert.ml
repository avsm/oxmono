open Core
open Async

module On_error = struct
  type t =
    | Skip
    | Raise
    | Raise_unless_empty

  let arg_type =
    Command.Arg_type.of_alist_exn
      ~list_values_in_help:true
      [ "skip", Skip; "fail", Raise; "only-fail-if-nonempty", Raise_unless_empty ]
  ;;
end

let convert_some_columns convert_value_exn (columns_to_map : bool array) ~on_error =
  stage
    (Array.mapi ~f:(fun i value ->
       match columns_to_map.(i) with
       | false -> value
       | true ->
         (match convert_value_exn value with
          | converted -> converted
          | exception parse_failure ->
            (match (on_error : On_error.t), value with
             | Skip, _ | Raise_unless_empty, "" -> value
             | Raise, _ | Raise_unless_empty, _ ->
               raise_s
                 [%message
                   "Failed to convert value" (value : string) (parse_failure : exn)]))
       | exception Invalid_argument _ ->
         (* we just pass through extra columns we don’t have a name for. Alternatively, we
            could raise here. *)
         value))
;;

let convert_all_columns convert_value_exn =
  stage
    (Array.map ~f:(fun value ->
       (* when converting all columns we assume [on_error = Skip] *)
       try convert_value_exn value with
       | _ -> value))
;;

let convert_pipe reader writer ~convert_row =
  Pipe.iter' reader ~f:(fun queue ->
    Queue.iter queue ~f:(fun row ->
      Delimited.Read.Row.to_array row
      |> unstage convert_row
      |> Array.to_list
      |> Pipe.write_without_pushback_if_open writer);
    Pipe.pushback writer)
;;

let run convert_value_exn reader ~only_these_columns ~sep =
  let the_headers = ref [] in
  let reader ~get_headers =
    Delimited.Read.pipe_of_reader
      ~sep
      Delimited.Read.Row.builder
      ~header:
        (if get_headers
         then
           `Transform
             (fun headers ->
               the_headers := headers;
               headers)
         else `No)
      reader
  in
  let writer =
    Delimited.Write.Expert.By_row.of_writer_and_close ~sep (Lazy.force Writer.stdout)
  in
  match only_these_columns with
  | `All_columns_including_first_row ->
    (* We don’t need headers and can safely process every column *)
    let convert_row = convert_all_columns convert_value_exn in
    convert_pipe (reader ~get_headers:false) writer ~convert_row
  | `Only (column_names, on_error) ->
    let reader = reader ~get_headers:true in
    (* We need to read at least one row for [the_headers] to be set. *)
    (match%bind Pipe.values_available reader with
     | `Eof ->
       (match !the_headers with
        | [] -> return ()
        | header -> Pipe.write writer header)
     | `Ok ->
       Pipe.write_without_pushback writer !the_headers;
       let columns_to_map =
         Array.of_list !the_headers
         |> Array.map ~f:(List.mem column_names ~equal:String.equal)
       in
       (match
          List.filter column_names ~f:(Fn.non (List.mem !the_headers ~equal:String.equal))
        with
        | [] -> ()
        | _ :: _ as missing ->
          raise_s [%message "Missing requested headers" ~_:(missing : string list)]);
       let convert_row =
         convert_some_columns convert_value_exn columns_to_map ~on_error
       in
       convert_pipe reader writer ~convert_row)
;;

let make_command convert_value_exn ~can_raise ~summary ~name =
  let command =
    Command.async
      ~summary
      (let%map_open.Csv_param sep
       and file = file_stdin_anon
       and fields = fields_gen ~doc:"field,... Fields to convert (default: all of them)"
       and on_error =
         if can_raise
         then
           flag
             "-on-error"
             (optional On_error.arg_type)
             ~doc:
               "ACTION What to do if a named field doesn’t have the expected type \
                (default: skip)"
         else return None
       and convert_value_exn in
       fun () ->
         let only_these_columns =
           match fields, on_error with
           | [], Some _ ->
             (* This was requested because it's empirically useful to assume that people
                converting all columns are dealing with headerless csv files.

                People can list out all the columns if they want to convert all fields and
                fail on conversions errors. *)
             failwith "-on-error is only allowed if -fields are specified."
           | [], None -> `All_columns_including_first_row
           | headers, on_error -> `Only (headers, Option.value on_error ~default:Skip)
         in
         let%bind reader =
           match file with
           | Csv _ -> failwith "BUG: expected a file and got a csv directly"
           | File f -> Reader.open_file f
           | Stdin -> force Reader.stdin |> return
         in
         run convert_value_exn reader ~only_these_columns ~sep)
      ~behave_nicely_in_pipeline:true
  in
  name, command
;;

module Span_unit = struct
  type t =
    | Second
    | Ms
    | Us
    | Ns
    | Minute
    | Hour
    | Day
  [@@deriving compare, enumerate, sexp_of]

  let arg =
    Enum.make_param_optional_with_default_doc_sexp
      "-unit"
      (module struct
        type nonrec t = t [@@deriving enumerate, sexp_of]
      end)
      ~default:Second
      ~doc:"The units to convert the span into"
  ;;
end

module Time_format = struct
  module T = struct
    type t =
      | To_ISO8601_UTC (* ‘T’ after date *)
      | To_UTC (* space after date *)
      | To_unix_fractional_seconds
        (* seconds since epoch, fractional seconds as decimal *)
      | To_unix_seconds (* seconds since epoch, truncated to whole seconds *)
      | To_unix_ns (* ns since epoch *)
    [@@deriving compare, enumerate, equal, sexp_of]
  end

  include T

  let format_span_ns_as_seconds t =
    let n = Time_ns.Span.to_int_ns t |> Int.to_int64 in
    let open Int64.O in
    if n < 0L
    then sprintf "-%Ld.%09Ld" (-n / 1_000_000_000L) (Int64.rem (-n) 1_000_000_000L)
    else sprintf "%Ld.%09Ld" (n / 1_000_000_000L) (Int64.rem n 1_000_000_000L)
  ;;

  let format : t -> (Time_ns.t -> string) = function
    | To_ISO8601_UTC -> Time_ns.to_string_iso8601_basic ~zone:Time_ns_unix.Zone.utc
    | To_UTC -> Time_ns.to_string_utc
    | To_unix_fractional_seconds ->
      fun time_ns ->
        let span = Time_ns.to_span_since_epoch time_ns in
        format_span_ns_as_seconds span
    | To_unix_seconds ->
      fun time_ns ->
        let ns = time_ns |> Time_ns.to_int63_ns_since_epoch |> Int63.to_int64 in
        let open Int64 in
        sprintf "%Ld" (ns / 1_000_000_000L)
    | To_unix_ns ->
      fun time_ns ->
        let ns = time_ns |> Time_ns.to_int63_ns_since_epoch |> Int63.to_int64 in
        sprintf "%Ld" ns
  ;;

  let param =
    let default = To_ISO8601_UTC in
    let one_ns_after_epoch = Time_ns.add Time_ns.epoch (Time_ns.Span.of_int_ns 1) in
    Enum.make_param_optional_one_of_flags
      (module T)
      ~aliases:(function
        | To_ISO8601_UTC | To_UTC | To_unix_fractional_seconds | To_unix_seconds -> []
        | To_unix_ns -> [ "to-unix-nanos" ])
      ~doc:(fun t ->
        let example =
          [%string {|Show "epoch + 1ns" as "%{format t one_ns_after_epoch}"|}]
        in
        match [%compare.equal: t] t default with
        | false -> example
        | true -> [%string "%{example} (default)"])
    |> Command.Param.map ~f:(Option.value ~default)
  ;;
end

let span_ns_command () =
  let parse = Time_ns.Span.of_string in
  let sign n = if n < 0 then "-" else "" in
  let nanos = Time_ns.Span.to_int_ns in
  let convert_value =
    let%map.Csv_param unit = Span_unit.arg in
    match unit with
    | Second -> fun s -> Time_format.format_span_ns_as_seconds (parse s)
    | Ms ->
      fun s ->
        let n = nanos (parse s) in
        sprintf "%s%d.%06d" (sign n) (abs (n / 1_000_000)) (abs (n mod 1_000_000))
    | Us ->
      fun s ->
        let n = nanos (parse s) in
        sprintf "%s%d.%03d" (sign n) (abs (n / 1_000)) (abs (n mod 1_000))
    | Ns -> fun s -> Int.to_string (nanos (parse s))
    | Minute -> fun s -> s |> parse |> Time_ns.Span.to_min |> Float.to_string
    | Hour -> fun s -> s |> parse |> Time_ns.Span.to_hr |> Float.to_string
    | Day -> fun s -> s |> parse |> Time_ns.Span.to_day |> Float.to_string
  in
  make_command
    convert_value
    ~can_raise:true
    ~summary:"Convert Time_ns.Span.t into a number"
    ~name:"span"
;;

let time_ns_command () =
  let parse str =
    if String.(str <> "") && Char.(str.[0] = '(')
    then Sexp.of_string str |> Time_ns_unix.t_of_sexp
    else Time_ns_unix.of_string str
  in
  let convert_value =
    let%map.Csv_param time_format = Time_format.param in
    let format = Time_format.format time_format in
    fun s -> parse s |> format
  in
  make_command
    convert_value
    ~can_raise:true
    ~summary:
      "Convert Time_ns.t, including the list-of-two-atoms sexp format, into a simpler \
       format"
    ~name:"time"
;;

let command () =
  let open Csv_param in
  [ make_command
      (return (function
        | "()" -> ""
        | s -> s))
      ~can_raise:false
      ~summary:"Replace () with the empty string"
      ~name:"unit"
  ; span_ns_command ()
  ; time_ns_command ()
  ; (let error = Failure "Percentages must end in bp, x, or %." in
     make_command
       (return (fun str ->
          (* We check the last char here to avoid allocating lots of error messages in the
             typical case where we don’t have a string *)
          match if String.is_empty str then '1' else str.[String.length str - 1] with
          | 'p' | 'x' | '%' ->
            (* All percentages end in % or bp or x *)
            Percent.of_string str |> Percent.to_mult |> Float.to_string
          | _ -> raise error))
       ~can_raise:true
       ~summary:
         "Convert percentage values like 3x or 25bp into multipliers like 3 or 0.0025."
       ~name:"percent")
  ]
  |> Command.group
       ~summary:"Convert from structured string formats of OCaml types to simpler formats"
;;
