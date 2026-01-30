open! Core
open! Async
open! Import

let make_ragged_csv filename =
  make_input_csv
    filename
    [ [ "int"; "float"; "string" ]; [ "0"; "0.0"; "foo" ]; [ "1"; "1.1"; "bar"; "1" ] ]
;;

let make_input_csv ?sep filename =
  make_input_csv
    ?sep
    filename
    [ [ "int"; "float"; "string" ]; [ "0"; "0.0"; "foo" ]; [ "1"; "1.1"; "bar" ] ]
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "sum"; "input.csv" ] in
    [%expect
      {|
      int,float,string
      1.,1.1,0.
      |}];
    return ())
;;

(* Unusual delimiter. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'/' "input.csv" in
    let%bind () = run "csv" [ "sum"; "input.csv"; "-sep"; "/" ] in
    [%expect
      {|
      int/float/string
      1./1.1/0.
      |}];
    return ())
;;

(* Rows have different lengths. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_ragged_csv "input.csv" in
    let%bind () = run ~enable_ocaml_backtraces:false "csv" [ "sum"; "input.csv" ] in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        (Invalid_argument "length mismatch in map2_exn: 3 <> 4")
      |}];
    return ())
;;
