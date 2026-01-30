open! Core
open Async
open Import

let write_test_csv ~filename csv =
  Writer.with_file filename ~f:(fun writer ->
    Writer.write writer csv;
    Writer.flushed writer)
;;

let%expect_test "valid" =
  do_test (fun () ->
    let valid_csv =
      {|"col1","col2","col3"
"1","2","3"
"x","y","z"|}
    in
    let%bind () = write_test_csv ~filename:"input.csv" valid_csv in
    let%bind () = run "csv" [ "validate"; "input.csv" ] in
    [%expect {| |}];
    return ())
;;

let%expect_test "ragged" =
  do_test (fun () ->
    let test ragged_csv =
      let%bind () = write_test_csv ~filename:"input.csv" ragged_csv in
      run "csv" [ "validate"; "input.csv" ]
    in
    (* Row has fewer columns than header *)
    let%bind () =
      test
        {|"col1","col2","col3"
"1","2","3"
"x","y"|}
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      |}];
    (* Row has more columns than header *)
    let%bind () =
      test
        {|"col1","col2","col3"
"1","2","3"
"x","y","z","a"|}
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      |}];
    (* All rows have more columns than header, but by different amounts *)
    let%bind () =
      test
        {|"col1","col3"
"1","2","3"
"x","y","z","a"|}
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      |}];
    return ())
;;
