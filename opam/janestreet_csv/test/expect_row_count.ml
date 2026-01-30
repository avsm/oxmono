open! Core
open! Async
open! Import

let example_csv =
  { Csv_common.header = [ "fruit"; "quantity"; "owner" ]
  ; lines =
      [ [ "apple"; "4"; "Abraham\nJackson" ]
      ; [ "apple"; "6"; "Bathsheba" ]
      ; [ "orange"; "2"; "Cyrus" ]
      ]
  }
;;

let header_only_csv = { Csv_common.header = [ "fruit"; "quantity"; "owner" ]; lines = [] }

let make_input_csv ?sep filename =
  let csv_lines = example_csv.header :: example_csv.lines in
  make_input_csv ?sep filename csv_lines
;;

(* Generic test with default separator *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    (* notice how counting with cat and wc-l does gives the WRONG row count due to the
       newline and header *)
    let%bind () = system "cat input.csv | wc -l" in
    [%expect {| 5 |}];
    (* using the count-rows command we can correctly count the 3 rows *)
    let%bind () = run "csv" [ "count-rows"; "input.csv" ] in
    [%expect {| 3 |}];
    return ())
;;

(* Using an unusual delimeter *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () = run "csv" [ "count-rows"; "-d"; "."; "input.csv" ] in
    [%expect {| 3 |}];
    return ())
;;

(* header only csv *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = Import.make_input_csv "input.csv" [ header_only_csv.header ] in
    let%bind () = run "csv" [ "count-rows"; "input.csv" ] in
    [%expect {| 0 |}];
    return ())
;;
