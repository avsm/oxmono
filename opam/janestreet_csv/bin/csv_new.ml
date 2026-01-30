open Core
open Async
module Lib = Csv_tool_lib
module Csv_param = Lib.Csv_param

module Merge = struct
  let command =
    Command.basic
      ~summary:"concatenate multiple CSV files into one"
      (let%map_open.Csv_param separator = sep
       and files in
       fun () ->
         if List.length files < 2
         then failwith "must specify at least two files to merge"
         else Lib.Merge.run files ~separator)
  ;;
end

module Sort = struct
  let command =
    Command.basic
      ~summary:"stably sort a csv file on a given column(s)"
      (let%map_open.Csv_param separator = sep
       and file = file_stdin_anon
       and sort_columns = Lib.Csv_sort.Sort_columns.param
       and no_header = Csv_param.no_headers_use_indices_instead in
       fun () -> Lib.Csv_sort.run sort_columns file ~separator ~no_header)
  ;;
end

module Transpose = struct
  let command =
    Command.basic
      ~summary:"transpose csv rows to columns for easier command-line viewing"
      (let%map_open.Csv_param separator = sep
       and one_row_at_a_time =
         flag
           "-one-row-at-a-time"
           no_arg
           ~doc:
             " transpose one row at a time.  Output will have ROWS*COLS number of rows."
       and file = file_stdin_anon in
       fun () -> Lib.Csv_transpose.run ~separator ~one_row_at_a_time file)
  ;;
end

module Grep = struct
  let command =
    Command.async_or_error
      ~summary:
        "grep in csv fields (using the re2 regular expression engine), while keeping the \
         header"
      (let%map_open.Csv_param
         (* The default behavior is to print header only if there are output rows. It is
            less good a behavior, but it is historically what this command did. *)
         separator
         =
         sep
       and skip_lines
       and grep_fields = Lib.Grep.Target_fields.param
       and file = file_stdin_anon
       and regexp
       and invert
       and always_print_header =
         flag "-always-print-header" no_arg ~doc:" print headers even if no rows match"
       in
       fun () ->
         Lib.Grep.run
           ~separator
           ?skip_lines
           ~invert
           ~always_print_header
           ~grep_fields
           ~regexp
           file)
  ;;
end

module Grid = struct
  let command =
    Command.basic
      ~summary:"grid based on specified column, start time, stop time and span"
      (let%map_open.Csv_param separator = sep
       and file = file_stdin_anon
       and field = time_field
       and start = start_time
       and stop = stop_time
       and step = grid_step in
       fun () -> Lib.Csv_grid.run ~separator ~field ~start ~stop ~step file)
  ;;
end

module To_ascii_table = struct
  let command =
    Command.basic
      ~summary:"print a csv as an ascii-table"
      (let%map_open.Csv_param separator = sep
       and file = file_stdin_anon
       and limit_width_to = max_width
       and prefer_split_on_spaces in
       fun () ->
         Lib.To_ascii_table.run ~separator ?limit_width_to file ~prefer_split_on_spaces)
  ;;
end

module To_html_table = struct
  let command =
    Command.basic
      ~summary:"print a csv as an HTML table"
      (let%map_open.Csv_param separator = sep
       and file = file_stdin_anon
       and no_header
       and suppress_header
       and table_attrs
       and th_attrs
       and tr_attrs
       and td_attrs
       and border
       and unescaped_html =
         flag
           "-unescaped-html"
           no_arg
           ~doc:
             " if input file contains HTML snippets, don't escape them and include as-is"
       in
       fun () ->
         Lib.To_html_table.run
           ~separator
           ~no_header
           ~suppress_header
           ~table_attrs
           ~th_attrs
           ~tr_attrs
           ~td_attrs
           ~border
           ~unescaped_html
           file)
  ;;
end

module To_pipe_table = struct
  let command =
    Command.basic
      ~summary:"print a csv as a pipe-delimited table"
      (let%map_open.Csv_param separator = sep
       and file = file_stdin_anon
       and no_header
       and suppress_header in
       fun () -> Lib.To_pipe_table.run ~separator ~no_header ~suppress_header file)
  ;;
end

module Sum = struct
  let command =
    Command.basic
      ~summary:
        "sum columns (use 0 for a cell if Float.of_string fails). All rows must have the \
         same length"
      (let%map_open.Csv_param separator = sep
       and file = file_stdin_anon in
       fun () -> Lib.Csv_sum.run ~separator file)
  ;;
end

module Sum_group = struct
  let command = Lib.Sum_group.command
end

module Id = struct
  let command =
    Command.async
      ~summary:"transfer input to output, canonicalising quoting"
      (let%map_open.Csv_param sep
       and file = file_stdin_anon
       and suppress_header in
       fun () ->
         let%bind reader =
           match file with
           | Csv _ -> failwith "BUG: expected a file and got a csv directly"
           | File f -> Reader.open_file f
           | Stdin -> force Reader.stdin |> return
         in
         Lib.Id.run ~sep ~suppress_header reader (force Writer.stdout))
  ;;
end

module Validate = struct
  let command =
    Command.async
      ~summary:"raise if the csv has ragged rows"
      (let%map_open.Csv_param sep
       and file = file_stdin_anon in
       fun () ->
         let%bind reader =
           match file with
           | Csv _ -> failwith "BUG: expected a file and got a csv directly"
           | File f -> Reader.open_file f
           | Stdin -> force Reader.stdin |> return
         in
         match%bind Lib.Validate.run ~sep reader with
         | Ok () -> return ()
         | Error msg ->
           prerr_endline msg;
           exit 1)
  ;;
end

module Header = struct
  let command =
    Command.async
      ~summary:"Add the given column headers to a csv"
      (let%map_open.Csv_param sep
       and file = file_stdin_flag
       and add_header = anon (non_empty_sequence_as_list ("COLUMN-HEADER" %: string)) in
       fun () ->
         let%bind reader =
           match file with
           | Csv _ -> failwith "BUG: expected a file and got a csv directly"
           | File f -> Reader.open_file f
           | Stdin -> force Reader.stdin |> return
         in
         Lib.Id.run ~sep ~add_header reader (force Writer.stdout))
  ;;
end

module Count_rows = struct
  let command =
    Command.basic
      ~summary:"print the number of rows of data (excluding header) in a csv"
      (let%map_open.Csv_param separator = sep
       and file = file_stdin_anon in
       fun () -> Lib.Csv_count_rows.run ~separator file)
  ;;
end

module Add_column = struct
  let command =
    Command.async
      ~summary:"add a new column to a csv"
      (let%map_open.Csv_param sep
       and file = file_stdin_anon
       and column = flag "-column" (required string) ~doc:" name of new column"
       and value =
         flag
           "-value"
           (optional_with_default "" string)
           ~doc:" value of new column (defaults to blank)"
       and after =
         flag
           "-after"
           (optional string)
           ~doc:"COLUMN insert new column after this column (defaults to end)"
       and allow_duplicate_column =
         flag
           "-allow-duplicate-column"
           no_arg
           ~doc:" continue even if the new column's name is already in use"
       in
       fun () ->
         Lib.Add_column.run ~sep ?after file ~column ~value ~allow_duplicate_column)
  ;;
end
