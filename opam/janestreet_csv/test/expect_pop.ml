open! Core
open! Async
open! Import

let make_input_csv ?sep filename =
  make_input_csv
    ?sep
    filename
    [ [ "fruit"; "quantity"; "owner" ]
    ; [ "apple"; ""; "Abraham" ]
    ; [ ""; "6"; "Bathsheba" ]
    ; [ "orange"; "2"; "" ]
    ]
;;

(* Default is to require no fields to be fully populated. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "pop"; "input.csv" ] in
    [%expect
      {|
      fruit,quantity,owner
      apple,,Abraham
      ,6,Bathsheba
      orange,2,
      |}];
    return ())
;;

(* But we can specify one field. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "pop"; "input.csv"; "-fields"; "fruit" ] in
    [%expect
      {|
      fruit,quantity,owner
      apple,,Abraham
      orange,2,
      |}];
    return ())
;;

(* Specifying two fields selects rows where both are populated. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "pop"; "input.csv"; "-fields"; "fruit,owner" ] in
    [%expect
      {|
      fruit,quantity,owner
      apple,,Abraham
      |}];
    return ())
;;

(* Specifying all but one field selects rows where both other fields are populated (same
   as above). *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "pop"; "input.csv"; "-v"; "-fields"; "quantity" ] in
    [%expect
      {|
      fruit,quantity,owner
      apple,,Abraham
      |}];
    return ())
;;

(* Passing -sh. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "pop"; "input.csv"; "-fields"; "fruit,owner"; "-sh" ] in
    [%expect {| apple,,Abraham |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () =
      run "csv" [ "pop"; "input.csv"; "-fields"; "fruit,quantity"; "-d"; "." ]
    in
    [%expect
      {|
      fruit,quantity,owner
      orange,2,
      |}];
    return ())
;;
