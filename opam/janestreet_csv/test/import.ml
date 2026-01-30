open! Core
open! Async
module Time = Time_float_unix
include Expect_test_helpers_core
include Expect_test_helpers_async
include Csv_tool_lib

let do_test f = within_temp_dir f ~links:[ "../bin/csv.exe", `In_path_as, "csv" ]

let make_input_csv ?sep filename rows =
  Writer.with_file filename ~f:(fun writer ->
    let pipe = Delimited.Write.Expert.By_row.of_writer_and_close ?sep writer in
    let%map () = Deferred.List.iter ~how:`Sequential rows ~f:(Pipe.write pipe) in
    Pipe.close pipe)
;;
