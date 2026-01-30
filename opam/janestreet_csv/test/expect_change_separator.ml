open! Core
open! Async
open! Import

let make_input_csv ?sep filename =
  make_input_csv
    ?sep
    filename
    [ [ "fruit"; "quantity"; "owner" ]
    ; [ "apple"; "4"; "Lincoln, Abraham" ]
    ; [ "apple"; "6"; "Bathsheba" ]
    ; [ "orange"; "2"; "Cyrus|Foo" ]
    ]
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run
        "csv"
        [ "change-separator"
        ; "-input-separator"
        ; ","
        ; "-output-separator"
        ; "|"
        ; "input.csv"
        ]
    in
    [%expect
      " \n\
      \ fruit|quantity|owner\n\
      \ apple|4|Lincoln, Abraham\n\
      \ apple|6|Bathsheba\n\
      \ orange|2|\"Cyrus|Foo\"\n\
      \ "];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" ~sep:'|' in
    let%bind () =
      run
        "csv"
        [ "change-separator"
        ; "-input-separator"
        ; "|"
        ; "-output-separator"
        ; ","
        ; "input.csv"
        ]
    in
    [%expect
      " \n\
      \ fruit,quantity,owner\n\
      \ apple,4,\"Lincoln, Abraham\"\n\
      \ apple,6,Bathsheba\n\
      \ orange,2,Cyrus|Foo\n\
      \ "];
    return ())
;;

let%expect_test "tab separated" =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" ~sep:'\t' in
    let%bind () =
      run
        "csv"
        [ "change-separator"
        ; "-input-separator"
        ; "\\t"
        ; "-output-separator"
        ; ","
        ; "input.csv"
        ]
    in
    [%expect
      {|
      fruit,quantity,owner
      apple,4,"Lincoln, Abraham"
      apple,6,Bathsheba
      orange,2,Cyrus|Foo
      |}];
    return ())
;;
