open! Core
module Cut = Csv_tool_lib.Cut
module Diff = Csv_tool_lib.Diff
module Join = Csv_tool_lib.Join
open Csv_tool_lib.Csv_param.Let_syntax

let headers_wanted wanted_fields =
  let%map_open fields = wanted_fields
  and exclude = exclude_fields in
  if exclude then `All_but fields else `Limit_to fields
;;

let pretty_command ?alias_for () =
  let summary =
    "pretty printer for csv files"
    ^
    match alias_for with
    | None -> ""
    | Some other_command -> [%string " (alias for %{other_command} subcommand)"]
  in
  Command.basic
    ~summary
    (let%map_open separator = sep
     and suppress_header
     and space
     and file = anon (maybe ("FILE" %: Filename_unix.arg_type)) in
     fun () ->
       let chan =
         match file with
         | None -> In_channel.stdin
         | Some file -> In_channel.create file
       in
       let csv = Csvlib.Csv.load_in ~separator chan in
       match Csv_tool_lib.Pretty.prettify ~space ~suppress_header csv with
       | Ok res ->
         Csv_tool_lib.Pretty.print res;
         In_channel.close chan
       | Error msg ->
         prerr_endline msg;
         In_channel.close chan;
         exit 1)
;;

let cut_command ~deprecated =
  let summary = "cut for csv files" ^ if deprecated then " (deprecated)" else "" in
  Command.basic
    ~summary
    (let%map_open sep
     and headers_wanted = headers_wanted fields
     and suppress_header
     and no_header = no_headers_use_indices_instead
     and files = anon (sequence ("FILE" %: Filename_unix.arg_type)) in
     fun () ->
       if no_header && suppress_header
       then failwith "-suppress-header implies headers, but provided -no-headers too";
       let cutfn =
         match no_header with
         | true -> Cut.cut_by_field_indices
         | false -> Cut.cut_by_field_names
       in
       let handle_file ~skip_header file =
         cutfn file ~sep headers_wanted ~skip_header ~f:(fun row ->
           Csvlib.Csv.print [ Array.to_list row ])
       in
       match files with
       | [] -> handle_file ~skip_header:suppress_header Stdin
       | first :: rest ->
         (* If we process more than one file we should not emit multiple copies of the
            header. *)
         handle_file ~skip_header:suppress_header (File first);
         List.iter rest ~f:(fun x -> handle_file ~skip_header:true (File x)))
;;

let join_command =
  let summary = "join for csv files" in
  Command.basic
    ~summary
    (let%map_open sep
     and key_fields =
       flag "-field" (listed string) ~doc:"FIELD field(s) on which to join"
       |> map ~f:(function
         | [] -> failwith "must pass at least one -field"
         | fields -> Array.of_list fields)
     and join = Join.param
     and files = anon (sequence ("FILE" %: Filename_unix.arg_type)) in
     fun () ->
       Join.join join files ~key_fields ~sep
       |> Sequence.iter ~f:(fun row -> Csvlib.Csv.print [ row ]))
;;

let diff_command =
  let summary = "diff for csv files" in
  Command.basic
    ~summary
    (let%map_open no_header
     and key_spec = key_specifier
     and file1 = anon ("FILE1" %: Filename_unix.arg_type)
     and file2 = anon ("FILE2" %: Filename_unix.arg_type) in
     fun () ->
       let output =
         Diff.diff_from_files ~header:(not no_header) ~key:key_spec file1 file2
       in
       Csvlib.Csv.print output)
;;

let pop_or_unpop_command ~summary pop_type =
  Command.basic
    ~summary
    (let%map_open sep
     and skip_header = suppress_header
     and headers_wanted = headers_wanted pop_fields
     and files = anon (maybe ("FILE" %: Filename_unix.arg_type)) in
     fun () ->
       let handle_file file =
         match pop_type with
         | `pop ->
           Cut.fully_populated_rows file ~skip_header ~sep headers_wanted ~f:(fun row ->
             Csvlib.Csv.print [ Array.to_list row ])
         | `unpop ->
           Cut.not_fully_populated_rows
             file
             ~skip_header
             ~sep
             headers_wanted
             ~f:(fun row -> Csvlib.Csv.print [ Array.to_list row ])
       in
       match files with
       | None -> handle_file Stdin
       | Some file -> handle_file (File file))
;;

let pop_command =
  pop_or_unpop_command
    `pop
    ~summary:"filter to rows which have certain fields fully populated"
;;

let unpop_command =
  pop_or_unpop_command
    `unpop
    ~summary:"filter to rows which do not have certain fields fully populated"
;;

let fields_command =
  let summary = "list csv field names" in
  Command.basic
    ~summary
    (let%map_open sep
     and files = anon (sequence ("FILE" %: Filename_unix.arg_type)) in
     fun () ->
       let handle_file file = List.iter ~f:print_endline (Cut.field_names file ~sep) in
       match files with
       | [] -> handle_file Stdin
       | hd :: _ -> handle_file (File hd))
;;

let to_sexp_command =
  let summary = "to sexp" in
  Command.basic
    ~summary
    (let%map_open separator = sep
     and utf8
     and include_header = map ~f:not no_header in
     fun () ->
       let list x = Sexp.List x in
       let atom x = Sexp.Atom x in
       let rows = Csvlib.Csv.load_in ~separator In_channel.stdin in
       let to_string =
         match utf8 with
         | true -> (Sexp.Utf8.to_string :> Sexp.t -> string)
         | false -> Sexp.to_string
       in
       if include_header
       then (
         match rows with
         | [] -> failwith "no rows"
         | header :: rows ->
           List.iter rows ~f:(fun row ->
             List.zip_exn header row
             |> List.map ~f:(fun (h, v) -> list [ atom h; atom v ])
             |> list
             |> to_string
             |> print_endline))
       else
         List.iter rows ~f:(fun row ->
           List.map row ~f:atom |> list |> to_string |> print_endline))
;;

let command =
  Command.group
    ~summary:"CSV tool"
    [ "add-column", Csv_new.Add_column.command
    ; "change-separator", Csv_tool_lib.Change_separator.command
    ; "cat", Csv_new.Merge.command
    ; "cut2", cut_command ~deprecated:true
    ; "cut", cut_command ~deprecated:false
    ; "join", join_command
    ; "diff", diff_command
    ; "pop", pop_command
    ; "pp", pretty_command ~alias_for:"pretty" ()
    ; "pretty", pretty_command ()
    ; "unpop", unpop_command
    ; "fields", fields_command
    ; "to-sexp", to_sexp_command
    ; "of-sexp", Csv_tool_lib.Of_sexp.command
    ; "merge", Csv_new.Merge.command
    ; "sort", Csv_new.Sort.command
    ; "transpose", Csv_new.Transpose.command
    ; "grid", Csv_new.Grid.command
    ; "to-ascii-table", Csv_new.To_ascii_table.command
    ; "to-html-table", Csv_new.To_html_table.command
    ; "to-pipe-table", Csv_new.To_pipe_table.command
    ; "grep", Csv_new.Grep.command
    ; "sum", Csv_new.Sum.command
    ; "sum-group", Csv_new.Sum_group.command
    ; "id", Csv_new.Id.command
    ; "validate", Csv_new.Validate.command
    ; "enhead", Csv_new.Header.command
    ; "count-rows", Csv_new.Count_rows.command
    ; "destructure", Csv_tool_lib.Convert.command ()
    ]
;;

let main () =
  Async.Writer.behave_nicely_in_pipeline ();
  Command_unix.run command
;;

let () = main ()
