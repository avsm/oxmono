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

(* Default is to require all fields to be unpopulated? *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "unpop"; "input.csv" ] in
    [%expect {| fruit,quantity,owner |}];
    return ())
;;

(* But we can specify one field. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "unpop"; "input.csv"; "-fields"; "fruit" ] in
    [%expect
      {|
      fruit,quantity,owner
      ,6,Bathsheba
      |}];
    return ())
;;

(* Specifying two fields selects for rows where either is unpopulated. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "unpop"; "input.csv"; "-fields"; "fruit,owner" ] in
    [%expect
      {|
      fruit,quantity,owner
      ,6,Bathsheba
      orange,2,
      |}];
    return ())
;;

(* Specifying all but one field selects for rows where either field is unpopulated (same
   as above). *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "unpop"; "input.csv"; "-v"; "-fields"; "quantity" ] in
    [%expect
      {|
      fruit,quantity,owner
      ,6,Bathsheba
      orange,2,
      |}];
    return ())
;;

(* Passing -sh. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "unpop"; "input.csv"; "-fields"; "fruit,owner"; "-sh" ] in
    [%expect
      {|
      ,6,Bathsheba
      orange,2,
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () =
      run "csv" [ "unpop"; "input.csv"; "-fields"; "fruit,owner"; "-d"; "." ]
    in
    [%expect
      {|
      fruit,quantity,owner
      ,6,Bathsheba
      orange,2,
      |}];
    return ())
;;
