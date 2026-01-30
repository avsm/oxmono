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

(* Default is to search in all fields. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    (* We're not trying to test the regex implementation here. *)
    let%bind () = run "csv" [ "grep"; "input.csv"; "-regexp"; "r" ] in
    [%expect " \n fruit,quantity,owner\n apple,4,Abraham\n orange,2,Cyrus\n "];
    return ())
;;

(* Restrict to fields with no matches. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      let fields = "quantity" in
      run "csv" [ "grep"; "input.csv"; "-regexp"; "r"; "-grep-fields"; fields ]
    in
    [%expect "fruit,quantity,owner"];
    return ())
;;

(* Restrict to field with matches. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      let fields = "fruit" in
      run "csv" [ "grep"; "input.csv"; "-regexp"; "r"; "-grep-fields"; fields ]
    in
    [%expect " \n fruit,quantity,owner\n orange,2,Cyrus\n "];
    return ())
;;

(* -grep-fields on multiple fields means return rows that match in any field. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      let fields = "quantity,fruit" in
      run "csv" [ "grep"; "input.csv"; "-regexp"; "r"; "-grep-fields"; fields ]
    in
    [%expect " \n fruit,quantity,owner\n orange,2,Cyrus\n "];
    return ())
;;

(* Unusual delimiter, and flag to match. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () =
      let fields = "quantity,fruit" in
      run
        "csv"
        [ "grep"; "input.csv"; "-regexp"; "r"; "-grep-fields"; fields; "-sep"; "." ]
    in
    [%expect " \n fruit.quantity.owner\n orange.2.Cyrus\n "];
    return ())
;;

(* The header is printed even when no rows follow. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      let fields = "vegetable" in
      run "csv" [ "grep"; "input.csv"; "-regexp"; "r"; "-grep-fields"; fields ]
    in
    [%expect "fruit,quantity,owner"];
    return ())
;;

(* Fields with commas are not quoted if the delimiter is not a comma. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      let fields = "fruit,quantity,owner" in
      run
        "csv"
        [ "grep"; "input.csv"; "-regexp"; "r"; "-grep-fields"; fields; "-sep"; "." ]
    in
    [%expect "fruit,quantity,owner"];
    return ())
;;

(* Read from stdin when no arguments are given. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = system "csv grep -regexp r < input.csv" in
    [%expect " \n fruit,quantity,owner\n apple,4,Abraham\n orange,2,Cyrus\n "];
    return ())
;;

(* Read from stdin when - is given. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = system "csv grep -regexp r - < input.csv" in
    [%expect " \n fruit,quantity,owner\n apple,4,Abraham\n orange,2,Cyrus\n "];
    return ())
;;

(* Skip lines. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "grep"; "input.csv"; "-regexp"; "r"; "-skip-lines"; "1" ] in
    [%expect " \n apple,4,Abraham\n orange,2,Cyrus\n "];
    return ())
;;

(* Behave nicely in pipeline. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () =
      Import.make_input_csv
        "input.csv"
        (List.init 10_000 ~f:(const [ "foo"; "bar"; "baz" ]))
    in
    let%bind () = system "csv grep -grep-fields foo -regex foo input.csv | head" in
    [%expect
      " \n\
      \ foo,bar,baz\n\
      \ foo,bar,baz\n\
      \ foo,bar,baz\n\
      \ foo,bar,baz\n\
      \ foo,bar,baz\n\
      \ foo,bar,baz\n\
      \ foo,bar,baz\n\
      \ foo,bar,baz\n\
      \ foo,bar,baz\n\
      \ foo,bar,baz\n\
      \ "];
    return ())
;;

(* Files with only a header, no records. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = Import.make_input_csv "input.csv" [ [ "foo"; "bar"; "baz" ] ] in
    let%bind () = run "csv" [ "grep"; "-regex"; "foo"; "input.csv" ] in
    [%expect ""];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = Import.make_input_csv "input.csv" [ [ "foo"; "bar"; "baz" ] ] in
    let%bind () =
      run "csv" [ "grep"; "-regex"; "foo"; "input.csv"; "-always-print-header" ]
    in
    [%expect "foo,bar,baz"];
    return ())
;;

(* Test -invert flag *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    (* We're not trying to test the regex implementation here. *)
    let%bind () = run "csv" [ "grep"; "input.csv"; "-invert-match"; "-regexp"; "r" ] in
    [%expect " \n fruit,quantity,owner\n apple,6,Bathsheba\n "];
    return ())
;;
