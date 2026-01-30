open! Core
open! Async
open! Import

let make_before filename =
  make_input_csv
    filename
    [ [ "fruit"; "quantity"; "owner" ]
    ; [ "apple"; "4"; "Abraham" ]
    ; [ "apple"; "6"; "Bathsheba" ]
    ; [ "orange"; "3"; "Cyrus" ]
    ]
;;

let make_after filename =
  make_input_csv
    filename
    [ [ "fruit"; "quantity"; "owner" ]
    ; [ "apple"; "3"; "Abraham" ]
    ; [ "apple"; "7"; "Bathsheba" ]
    ; [ "orange"; "2"; "Cyrus" ]
    ; [ "orange"; "1"; "Deborah" ]
    ]
;;

(* The straightforward case. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_before "before.csv" in
    let%bind () = make_after "after.csv" in
    let%bind () = run "csv" [ "diff"; "-k"; "owner"; "before.csv"; "after.csv" ] in
    [%expect
      {|
      fruit,quantity,owner
      apple,4 --> 3,Abraham
      apple,6 --> 7,Bathsheba
      orange,3 --> 2,Cyrus
      NONE --> orange,NONE --> 1,Deborah
      |}];
    return ())
;;

(* Passing a column name when there is no header to name the columns. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_before "before.csv" in
    let%bind () = make_after "after.csv" in
    let%bind () =
      system
        "csv diff -nh -k owner before.csv after.csv 2> >(grep Invalid_specifier > \
         /dev/stderr)"
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
        Csv_tool_lib__Key_specifier.Invalid_specifier("Column specifier formatted incorrectly")
      |}];
    return ())
;;

(* Choosing a column, by number, that is not a key. Projected down to the fields named in
   [key], the input must contain no duplicates. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_before "before.csv" in
    let%bind () = make_after "after.csv" in
    let%bind () =
      run
        ~enable_ocaml_backtraces:false
        "csv"
        [ "diff"; "-nh"; "-k"; "1"; "before.csv"; "after.csv" ]
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        ("Map.of_alist_exn: duplicate key" ((1 apple)))
      |}];
    return ())
;;

(* Using zero-indexed columns instead of one-indexed. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_before "before.csv" in
    let%bind () = make_after "after.csv" in
    let%bind () =
      run
        ~enable_ocaml_backtraces:false
        "csv"
        [ "diff"; "-nh"; "-k"; "0"; "before.csv"; "after.csv" ]
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        (Invalid_argument "Key column 0 does not exist")
      |}];
    return ())
;;

(* Correct usage of column index. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_before "before.csv" in
    let%bind () = make_after "after.csv" in
    let%bind () = run "csv" [ "diff"; "-nh"; "-k"; "3"; "before.csv"; "after.csv" ] in
    [%expect
      {|
      1,2,3
      apple,4 --> 3,Abraham
      apple,6 --> 7,Bathsheba
      orange,3 --> 2,Cyrus
      NONE --> orange,NONE --> 1,Deborah
      |}];
    return ())
;;

(* Cannot use column indices when names are available. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_before "before.csv" in
    let%bind () = make_after "after.csv" in
    let%bind () =
      run
        ~enable_ocaml_backtraces:false
        "csv"
        [ "diff"; "-k"; "3"; "before.csv"; "after.csv" ]
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        (Invalid_argument "Key column 3 does not exist")
      |}];
    return ())
;;

(* Invalid key, chosen by name. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_before "before.csv" in
    let%bind () = make_after "after.csv" in
    let%bind () =
      run
        ~enable_ocaml_backtraces:false
        "csv"
        [ "diff"; "-k"; "fruit"; "before.csv"; "after.csv" ]
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        ("Map.of_alist_exn: duplicate key" ((fruit apple)))
      |}];
    return ())
;;

(* A valid key of multiple columns, albeit a nonsensical one. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_before "before.csv" in
    let%bind () = make_after "after.csv" in
    let%bind () =
      run "csv" [ "diff"; "-k"; "fruit,quantity"; "before.csv"; "after.csv" ]
    in
    [%expect
      {|
      fruit,quantity,owner
      apple,4,Abraham --> NONE
      apple,6,Bathsheba --> NONE
      orange,3,Cyrus --> NONE
      apple,3,NONE --> Abraham
      apple,7,NONE --> Bathsheba
      orange,2,NONE --> Cyrus
      orange,1,NONE --> Deborah
      |}];
    return ())
;;

(* An invalid key of multiple columns. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () =
      make_input_csv
        "before.csv"
        [ [ "fruit"; "quantity"; "owner" ]; [ "orange"; "2"; "Cyrus" ] ]
    in
    let%bind () =
      make_input_csv
        "after.csv"
        [ [ "fruit"; "quantity"; "owner" ]
        ; [ "orange"; "1"; "Cyrus" ]
        ; [ "orange"; "1"; "Deborah" ]
        ]
    in
    let%bind () =
      run
        ~enable_ocaml_backtraces:false
        "csv"
        [ "diff"; "-k"; "fruit,quantity"; "before.csv"; "after.csv" ]
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        ("Map.of_alist_exn: duplicate key" ((fruit orange) (quantity 1)))
      |}];
    return ())
;;
