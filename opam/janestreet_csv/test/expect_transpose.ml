open! Core
open! Async
open! Import

let make_ragged_csv ?sep filename =
  make_input_csv
    ?sep
    filename
    [ [ "fruit"; "quantity"; "owner" ]
    ; [ "apple"; "4"; "Abraham" ]
    ; [ "apple"; "6"; "Bathsheba" ]
    ; [ "orange"; "2"; "Cyrus"; "baz" ]
    ]
;;

let make_input_csv ?sep filename =
  make_input_csv
    ?sep
    filename
    [ [ "fruit"; "quantity"; "owner" ]
    ; [ "apple"; "4"; "Abraham" ]
    ; [ "apple"; "6"; "Bathsheba" ]
    ; [ "orange"; "2"; "Cyrus" ]
    ]
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "transpose"; "input.csv" ] in
    [%expect
      {|
      fruit,apple,apple,orange
      quantity,4,6,2
      owner,Abraham,Bathsheba,Cyrus
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () = run "csv" [ "transpose"; "-sep"; "."; "input.csv" ] in
    [%expect
      {|
      fruit.apple.apple.orange
      quantity.4.6.2
      owner.Abraham.Bathsheba.Cyrus
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () =
      run "csv" [ "transpose"; "-sep"; "."; "-one-row-at-a-time"; "input.csv" ]
    in
    [%expect
      {|
      fruit.apple
      quantity.4
      owner.Abraham
      fruit.apple
      quantity.6
      owner.Bathsheba
      fruit.orange
      quantity.2
      owner.Cyrus
      |}];
    return ())
;;

(* Rows have different lengths. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_ragged_csv "input.csv" in
    let%bind () = run ~enable_ocaml_backtraces:false "csv" [ "transpose"; "input.csv" ] in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        (list.ml.Transpose_got_lists_of_different_lengths (3 3 3 4))
      |}];
    return ())
;;

(* Single row, just a header. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = Import.make_input_csv "input.csv" [ [ "foo"; "bar"; "baz" ] ] in
    let%bind () = run "csv" [ "transpose"; "input.csv" ] in
    [%expect
      {|
      foo
      bar
      baz
      |}];
    return ())
;;
