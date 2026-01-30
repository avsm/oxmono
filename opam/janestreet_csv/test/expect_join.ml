open! Core
open! Async
open! Import

let%expect_test _ =
  do_test (fun () ->
    let%bind () =
      make_input_csv
        "numbers.csv"
        [ [ "name"; "number" ]
        ; [ "Abraham"; "1" ]
        ; [ "Bathsheba"; "2" ]
        ; [ "Cyrus"; "3" ]
        ; [ "Deborah"; "04" ]
        ; [ "Elijah"; "5" ]
        ]
    and () =
      make_input_csv
        "fruit.csv"
        [ [ "fruit"; "quantity"; "name" ]
        ; [ "apple"; "4"; "Abraham" ]
        ; [ "apple"; "6"; "Bathsheba" ]
        ; [ "orange"; "2"; "Cyrus" ]
        ]
    in
    let%bind () = run "csv" [ "join"; "-field"; "name"; "numbers.csv"; "fruit.csv" ] in
    [%expect
      {|
      name,number,fruit,quantity
      Abraham,1,apple,4
      Bathsheba,2,apple,6
      Cyrus,3,orange,2
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () =
      make_input_csv
        "numbers.csv"
        [ [ "name"; "number" ]
        ; [ "Abraham"; "1" ]
        ; [ "Abraham"; "1.2" ]
        ; [ "Bathsheba"; "2" ]
        ; [ "Cyrus"; "3" ]
        ; [ "Cyrus"; "3.5" ]
        ; [ "Deborah"; "04" ]
        ; [ "Elijah"; "5" ]
        ]
    and () =
      make_input_csv
        "fruit.csv"
        [ [ "fruit"; "quantity"; "name" ]
        ; [ "apple"; "4"; "Abraham" ]
        ; [ "apple"; "6"; "Bathsheba" ]
        ; [ "orange"; "2"; "Cyrus" ]
        ; [ "orange"; "2"; "Felix" ]
        ]
    in
    let%bind () =
      run "csv" [ "join"; "-join"; "left"; "-field"; "name"; "numbers.csv"; "fruit.csv" ]
    in
    [%expect
      {|
      name,number,fruit,quantity
      Abraham,1,apple,4
      Abraham,1.2,apple,4
      Bathsheba,2,apple,6
      Cyrus,3,orange,2
      Cyrus,3.5,orange,2
      Deborah,04,,
      Elijah,5,,
      |}];
    return ())
;;

(* Can only join on column that appears in all files. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () =
      make_input_csv
        "numbers.csv"
        [ [ "name"; "number" ]
        ; [ "Abraham"; "1" ]
        ; [ "Bathsheba"; "2" ]
        ; [ "Cyrus"; "3" ]
        ; [ "Deborah"; "04" ]
        ; [ "Elijah"; "5" ]
        ]
    and () =
      make_input_csv
        "fruit.csv"
        [ [ "fruit"; "quantity"; "name" ]
        ; [ "apple"; "4"; "Abraham" ]
        ; [ "apple"; "6"; "Bathsheba" ]
        ; [ "orange"; "2"; "Cyrus" ]
        ]
    in
    let%bind () =
      run
        ~enable_ocaml_backtraces:false
        "csv"
        [ "join"; "-field"; "quantity"; "numbers.csv"; "fruit.csv" ]
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        (Failure "No quantity column in numbers.csv")
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () =
      make_input_csv
        "numbers.csv"
        [ [ "name"; "number" ]
        ; [ "Abraham"; "1" ]
        ; [ "Bathsheba"; "2" ]
        ; [ "Cyrus"; "3" ]
        ; [ "Deborah"; "04" ]
        ; [ "Elijah"; "5" ]
        ]
    and () =
      make_input_csv
        "fruit.csv"
        [ [ "fruit"; "quantity"; "name" ]
        ; [ "apple"; "4"; "Abraham" ]
        ; [ "apple"; "6"; "Bathsheba" ]
        ; [ "orange"; "2"; "Cyrus" ]
        ]
    in
    let%bind () =
      run
        "csv"
        [ "join"
        ; "-field"
        ; "name"
        ; "-keys-need-not-occur-in-all-files"
        ; "numbers.csv"
        ; "fruit.csv"
        ]
    in
    [%expect
      {|
      name,number,fruit,quantity
      Abraham,1,apple,4
      Bathsheba,2,apple,6
      Cyrus,3,orange,2
      Deborah,04,,
      Elijah,5,,
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () =
      make_input_csv
        "numbers.csv"
        [ [ "name"; "number" ]
        ; [ "Abraham"; "1" ]
        ; [ "Bathsheba"; "2" ]
        ; [ "Cyrus"; "3" ]
        ; [ "Deborah"; "04" ]
        ; [ ""; "5" ]
        ]
    and () =
      make_input_csv
        "fruit.csv"
        [ [ "fruit"; "quantity"; "name" ]
        ; [ "apple"; "4"; "Abraham" ]
        ; [ "apple"; "6"; "Bathsheba" ]
        ; [ "orange"; "2"; "Cyrus" ]
        ]
    in
    let%bind () =
      run "csv" [ "join"; "-field"; "name"; "-join"; "full"; "numbers.csv"; "fruit.csv" ]
    in
    [%expect
      {|
      name,number,fruit,quantity
      ,5,,
      Abraham,1,apple,4
      Bathsheba,2,apple,6
      Cyrus,3,orange,2
      Deborah,04,,
      |}];
    return ())
;;

(* Doesn't distinguish between "" and None. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () =
      make_input_csv
        "numbers.csv"
        [ [ "name"; "number" ]
        ; [ "Abraham"; "1" ]
        ; [ "Bathsheba"; "2" ]
        ; [ "Cyrus"; "3" ]
        ; [ "Deborah"; "04" ]
        ; [ ""; "5" ]
        ; [ ""; "6" ]
        ]
    and () =
      make_input_csv
        "fruit.csv"
        [ [ "fruit"; "quantity"; "name" ]
        ; [ "apple"; "4"; "Abraham" ]
        ; [ "apple"; "6"; "Bathsheba" ]
        ; [ "orange"; "2"; "Cyrus" ]
        ]
    in
    let%bind () =
      run
        ~enable_ocaml_backtraces:false
        "csv"
        [ "join"; "-field"; "name"; "-join"; "full"; "numbers.csv"; "fruit.csv" ]
    in
    [%expect
      {|
      name,number,fruit,quantity
      ,5,,
      ,6,,
      Abraham,1,apple,4
      Bathsheba,2,apple,6
      Cyrus,3,orange,2
      Deborah,04,,
      |}];
    return ())
;;

(* The only column that can be appear in both files is the one being joined on. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () =
      make_input_csv
        "fruit.csv"
        [ [ "fruit"; "quantity"; "name" ]
        ; [ "apple"; "4"; "Abraham" ]
        ; [ "apple"; "6"; "Bathsheba" ]
        ; [ "orange"; "2"; "Cyrus" ]
        ]
    in
    let%bind () =
      run
        ~enable_ocaml_backtraces:false
        "csv"
        [ "join"; "-field"; "quantity"; "fruit.csv"; "fruit.csv" ]
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        ("Only key fields may appear in multiple files." (duplicate fruit)
         (combined_header (quantity fruit name fruit name)))
      |}];
    return ())
;;

(* Unusual delimiter. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () =
      make_input_csv
        ~sep:'.'
        "numbers.csv"
        [ [ "name"; "number" ]
        ; [ "Abraham"; "1" ]
        ; [ "Bathsheba"; "2" ]
        ; [ "Cyrus"; "3" ]
        ; [ "Deborah"; "04" ]
        ; [ "Elijah"; "5" ]
        ]
    and () =
      make_input_csv
        ~sep:'.'
        "fruit.csv"
        [ [ "fruit"; "quantity"; "name" ]
        ; [ "apple"; "4"; "Abraham" ]
        ; [ "apple"; "6"; "Bathsheba" ]
        ; [ "orange"; "2"; "Cyrus" ]
        ]
    in
    let%bind () =
      run "csv" [ "join"; "-field"; "name"; "-d"; "."; "numbers.csv"; "fruit.csv" ]
    in
    [%expect
      {|
      name,number,fruit,quantity
      Abraham,1,apple,4
      Bathsheba,2,apple,6
      Cyrus,3,orange,2
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () =
      make_input_csv
        "numbers.csv"
        [ [ "name"; "number"; "fruit" ]
        ; [ "Abraham"; "1"; "apple" ]
        ; [ "Bathsheba"; "2"; "apple" ]
        ; [ "Cyrus"; "3"; "pomelo" ]
        ; [ "Deborah"; "04"; "kumquat" ]
        ; [ "Elijah"; "5"; "citron" ]
        ]
    and () =
      make_input_csv
        "fruit.csv"
        [ [ "fruit"; "quantity"; "name" ]
        ; [ "apple"; "4"; "Abraham" ]
        ; [ "apple"; "6"; "Bathsheba" ]
        ; [ "orange"; "2"; "Cyrus" ]
        ]
    in
    let%bind () =
      run
        "csv"
        [ "join"; "-field"; "name"; "-field"; "fruit"; "numbers.csv"; "fruit.csv" ]
    in
    [%expect
      {|
      name,fruit,number,quantity
      Abraham,apple,1,4
      Bathsheba,apple,2,6
      |}];
    return ())
;;
