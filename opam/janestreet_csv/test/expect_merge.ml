open! Core
open! Async
open! Import

(* Shared columns are not used as a key. Absent columns are assumed to be blank. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () =
      make_input_csv
        "left.csv"
        [ [ "left-handed"; "name" ]
        ; [ "true"; "Abraham" ]
        ; [ "false"; "Bathsheba" ]
        ; [ "true"; "Cyrus" ]
        ]
    and () =
      make_input_csv
        "right.csv"
        [ [ "right-handed"; "name" ]
        ; [ "false"; "Cyrus" ]
        ; [ "true"; "Bathsheba" ]
        ; [ "false"; "Abraham" ]
        ]
    in
    let%bind () = run "csv" [ "merge"; "left.csv"; "right.csv" ] in
    [%expect
      {|
      left-handed,name,right-handed
      true,Abraham,
      false,Bathsheba,
      true,Cyrus,
      ,Cyrus,false
      ,Bathsheba,true
      ,Abraham,false
      |}];
    return ())
;;

(* Delimiters work as expected. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () =
      make_input_csv
        ~sep:'.'
        "left.csv"
        [ [ "left-handed"; "name" ]
        ; [ "true"; "Abraham" ]
        ; [ "false"; "Bathsheba" ]
        ; [ "true"; "Cyrus" ]
        ]
    and () =
      make_input_csv
        ~sep:'.'
        "right.csv"
        [ [ "right-handed"; "name" ]
        ; [ "false"; "Cyrus" ]
        ; [ "true"; "Bathsheba" ]
        ; [ "false"; "Abraham" ]
        ]
    in
    let%bind () = run "csv" [ "merge"; "left.csv"; "right.csv"; "-sep"; "." ] in
    [%expect
      {|
      left-handed.name.right-handed
      true.Abraham.
      false.Bathsheba.
      true.Cyrus.
      .Cyrus.false
      .Bathsheba.true
      .Abraham.false
      |}];
    return ())
;;

let%expect_test "passing - for stdin" =
  do_test (fun () ->
    let%bind () =
      make_input_csv "left.csv" [ [ "left-handed"; "name" ]; [ "true"; "Abraham" ] ]
    in
    let%bind () = run "csv" [ "merge"; "left.csv"; "-" ] in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        (Sys_error "-: No such file or directory")
      |}];
    return ())
;;

let%expect_test "merge csv files that are loaded into memory" =
  let csv1 =
    { Csv_common.header = [ "d"; "c"; "b"; "a" ]
    ; lines =
        [ [ "1d1"; "1c1"; "1b1"; "1a1" ]
        ; [ "1d2"; "1c2"; "1b2"; "1a2" ]
        ; [ "1d3"; "1c3"; "1b3"; "1a3" ]
        ]
    }
  in
  let csv2 =
    { Csv_common.header = [ "e"; "d"; "c"; "b" ]
    ; lines =
        [ [ "2e1"; "2d1"; "2c1"; "2b1" ]
        ; [ "2e2"; "2d2"; "2c2"; "2b2" ]
        ; [ "2e3"; "2d3"; "2c3"; "2b3" ]
        ]
    }
  in
  print_s [%sexp (Merge.merge [ csv1; csv2 ] : Csv_common.t)];
  [%expect
    {|
    ((header (d c b a e))
     (lines (
       (1d1 1c1 1b1 1a1 "")
       (1d2 1c2 1b2 1a2 "")
       (1d3 1c3 1b3 1a3 "")
       (2d1 2c1 2b1 ""  2e1)
       (2d2 2c2 2b2 ""  2e2)
       (2d3 2c3 2b3 ""  2e3))))
    |}];
  return ()
;;
