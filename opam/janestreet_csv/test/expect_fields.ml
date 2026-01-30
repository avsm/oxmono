open! Core
open! Async
open! Import

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
    let%bind () = run "csv" [ "fields"; "input.csv" ] in
    [%expect
      {|
      fruit
      quantity
      owner
      |}];
    return ())
;;

(* Incorrect delimiter flag. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "fields"; "-d"; "."; "input.csv" ] in
    [%expect {| fruit,quantity,owner |}];
    return ())
;;

(* Unusual delimiter, and flag to match. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () = run "csv" [ "fields"; "-d"; "."; "input.csv" ] in
    [%expect
      {|
      fruit
      quantity
      owner
      |}];
    return ())
;;

(* Unusual delimiter, and no flag. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () = run "csv" [ "fields"; "input.csv" ] in
    [%expect {| fruit.quantity.owner |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = make_input_csv ~sep:'.' "input-with-dots.csv" in
    let%bind () = run "csv" [ "fields"; "input-with-dots.csv"; "input.csv" ] in
    [%expect {| fruit.quantity.owner |}];
    return ())
;;
