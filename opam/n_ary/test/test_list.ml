open! Base
open Expect_test_helpers_base

let partition_enum = N_ary.List3.partition_enum

let%expect_test _ =
  let list = String.to_list "98p6p4pp10" in
  let f char : N_ary.Enum3.t =
    if Char.is_digit char
    then if Char.get_digit_exn char % 3 = 0 then Case0 else Case1
    else Case2
  in
  print_s [%sexp (partition_enum list ~f : char list * char list * char list)];
  [%expect
    {|
    ((9 6 0)
     (8 4 1)
     (p p p p))
    |}]
;;

let partition_enumi = N_ary.List3.partition_enumi

let%expect_test _ =
  let list = String.to_list "98p6p4pp10" in
  let f i char : N_ary.Enum3.t =
    if Char.is_digit char
    then if Char.get_digit_exn char % 3 = i % 2 then Case0 else Case1
    else Case2
  in
  print_s [%sexp (partition_enumi list ~f : char list * char list * char list)];
  [%expect
    {|
    ((9 4)
     (8 6 1 0)
     (p p p p))
    |}]
;;

let partition_map = N_ary.List3.partition_map

let%expect_test _ =
  let list = String.to_list "98p6p4pp10" in
  let f char : _ N_ary.Variant3.t =
    if Char.is_digit char
    then (
      let n = Char.get_digit_exn char in
      if n % 3 = 0 then Case0 (10 * n) else Case1 ((10 * n) + 5))
    else Case2 char
  in
  print_s [%sexp (partition_map list ~f : int list * int list * char list)];
  [%expect
    {|
    ((90 60 0)
     (85 45 15)
     (p p p p))
    |}]
;;

let partition_mapi = N_ary.List3.partition_mapi

let%expect_test _ =
  let list = String.to_list "98p6p4pp10" in
  let f i char : _ N_ary.Variant3.t =
    if Char.is_digit char
    then (
      let n = Char.get_digit_exn char in
      if n % 3 = i % 2 then Case0 (10 * n) else Case1 ((10 * n) + 5))
    else Case2 char
  in
  print_s [%sexp (partition_mapi list ~f : int list * int list * char list)];
  [%expect
    {|
    ((90 40)
     (85 65 15 5)
     (p  p  p  p))
    |}]
;;

let unzip = N_ary.List3.unzip

let%expect_test _ =
  print_s
    [%sexp
      (unzip [ 3, 2, 1; 1, 7, 6; 4, 1, 1; 1, 8, 8; 5, 2, 0; 9, 8, 3 ]
       : int list * int list * int list)];
  [%expect
    {|
    ((3 1 4 1 5 9)
     (2 7 1 8 2 8)
     (1 6 1 8 0 3))
    |}]
;;

let zip = N_ary.List3.zip

let%expect_test _ =
  let test list1 list2 list3 =
    print_s
      [%sexp (zip list1 list2 list3 : (int * int * int) list List.Or_unequal_lengths.t)]
  in
  test [] [] [];
  [%expect {| (Ok ()) |}];
  test [ 1; 2; 3 ] [ 1; 2 ] [ 1; 2 ];
  [%expect {| Unequal_lengths |}];
  test [ 1; 2 ] [ 1; 2; 3 ] [ 1; 2 ];
  [%expect {| Unequal_lengths |}];
  test [ 1; 2 ] [ 1; 2 ] [ 1; 2; 3 ];
  [%expect {| Unequal_lengths |}];
  test [ 3; 1; 4; 1; 5; 9 ] [ 2; 7; 1; 8; 2; 8 ] [ 1; 6; 1; 8; 0; 3 ];
  [%expect
    {|
    (Ok (
      (3 2 1)
      (1 7 6)
      (4 1 1)
      (1 8 8)
      (5 2 0)
      (9 8 3)))
    |}]
;;

let zip_exn = N_ary.List3.zip_exn

let%expect_test _ =
  let test list1 list2 list3 =
    print_s
      [%sexp
        (Or_error.try_with (fun () -> zip_exn list1 list2 list3)
         : (int * int * int) list Or_error.t)]
  in
  test [] [] [];
  [%expect {| (Ok ()) |}];
  test [ 1; 2; 3 ] [ 1; 2 ] [ 1; 2 ];
  [%expect {| (Error "N_ary.List3.zip_exn: lists have unequal lengths") |}];
  test [ 1; 2 ] [ 1; 2; 3 ] [ 1; 2 ];
  [%expect {| (Error "N_ary.List3.zip_exn: lists have unequal lengths") |}];
  test [ 1; 2 ] [ 1; 2 ] [ 1; 2; 3 ];
  [%expect {| (Error "N_ary.List3.zip_exn: lists have unequal lengths") |}];
  test [ 3; 1; 4; 1; 5; 9 ] [ 2; 7; 1; 8; 2; 8 ] [ 1; 6; 1; 8; 0; 3 ];
  [%expect
    {|
    (Ok (
      (3 2 1)
      (1 7 6)
      (4 1 1)
      (1 8 8)
      (5 2 0)
      (9 8 3)))
    |}]
;;

let print_triple x y z = print_s [%message "" ~_:(x : int) ~_:(y : int) ~_:(z : int)]

let print_itriple index x y z =
  print_s [%message "" ~_:(index : int) ~_:(x : int) ~_:(y : int) ~_:(z : int)]
;;

let print_return_triple x y z =
  print_triple x y z;
  x, y, z
;;

let print_return_itriple index x y z =
  print_itriple index x y z;
  index, x, y, z
;;

let map = N_ary.List3.map

let%expect_test _ =
  let test list1 list2 list3 =
    print_s
      [%sexp
        (map list1 list2 list3 ~f:print_return_triple
         : (int * int * int) list List.Or_unequal_lengths.t)]
  in
  test [] [] [];
  [%expect {| (Ok ()) |}];
  test [ 1; 2; 3 ] [ 1; 2 ] [ 1; 2 ];
  [%expect {| Unequal_lengths |}];
  test [ 1; 2 ] [ 1; 2; 3 ] [ 1; 2 ];
  [%expect {| Unequal_lengths |}];
  test [ 1; 2 ] [ 1; 2 ] [ 1; 2; 3 ];
  [%expect {| Unequal_lengths |}];
  test [ 3; 1; 4; 1; 5; 9 ] [ 2; 7; 1; 8; 2; 8 ] [ 1; 6; 1; 8; 0; 3 ];
  [%expect
    {|
    (3 2 1)
    (1 7 6)
    (4 1 1)
    (1 8 8)
    (5 2 0)
    (9 8 3)
    (Ok (
      (3 2 1)
      (1 7 6)
      (4 1 1)
      (1 8 8)
      (5 2 0)
      (9 8 3)))
    |}]
;;

let map_exn = N_ary.List3.map_exn

let%expect_test _ =
  let test list1 list2 list3 =
    print_s
      [%sexp
        (Or_error.try_with (fun () -> map_exn list1 list2 list3 ~f:print_return_triple)
         : (int * int * int) list Or_error.t)]
  in
  test [] [] [];
  [%expect {| (Ok ()) |}];
  test [ 1; 2; 3 ] [ 1; 2 ] [ 1; 2 ];
  [%expect
    {|
    (1 1 1)
    (2 2 2)
    (Error "N_ary.List3.map_exn: lists have unequal lengths")
    |}];
  test [ 1; 2 ] [ 1; 2; 3 ] [ 1; 2 ];
  [%expect
    {|
    (1 1 1)
    (2 2 2)
    (Error "N_ary.List3.map_exn: lists have unequal lengths")
    |}];
  test [ 1; 2 ] [ 1; 2 ] [ 1; 2; 3 ];
  [%expect
    {|
    (1 1 1)
    (2 2 2)
    (Error "N_ary.List3.map_exn: lists have unequal lengths")
    |}];
  test [ 3; 1; 4; 1; 5; 9 ] [ 2; 7; 1; 8; 2; 8 ] [ 1; 6; 1; 8; 0; 3 ];
  [%expect
    {|
    (3 2 1)
    (1 7 6)
    (4 1 1)
    (1 8 8)
    (5 2 0)
    (9 8 3)
    (Ok (
      (3 2 1)
      (1 7 6)
      (4 1 1)
      (1 8 8)
      (5 2 0)
      (9 8 3)))
    |}]
;;

let mapi = N_ary.List3.mapi

let%expect_test _ =
  let test list1 list2 list3 =
    print_s
      [%sexp
        (mapi list1 list2 list3 ~f:print_return_itriple
         : (int * int * int * int) list List.Or_unequal_lengths.t)]
  in
  test [] [] [];
  [%expect {| (Ok ()) |}];
  test [ 1; 2; 3 ] [ 1; 2 ] [ 1; 2 ];
  [%expect {| Unequal_lengths |}];
  test [ 1; 2 ] [ 1; 2; 3 ] [ 1; 2 ];
  [%expect {| Unequal_lengths |}];
  test [ 1; 2 ] [ 1; 2 ] [ 1; 2; 3 ];
  [%expect {| Unequal_lengths |}];
  test [ 3; 1; 4; 1; 5; 9 ] [ 2; 7; 1; 8; 2; 8 ] [ 1; 6; 1; 8; 0; 3 ];
  [%expect
    {|
    (0 3 2 1)
    (1 1 7 6)
    (2 4 1 1)
    (3 1 8 8)
    (4 5 2 0)
    (5 9 8 3)
    (Ok (
      (0 3 2 1)
      (1 1 7 6)
      (2 4 1 1)
      (3 1 8 8)
      (4 5 2 0)
      (5 9 8 3)))
    |}]
;;

let mapi_exn = N_ary.List3.mapi_exn

let%expect_test _ =
  let test list1 list2 list3 =
    print_s
      [%sexp
        (Or_error.try_with (fun () -> mapi_exn list1 list2 list3 ~f:print_return_itriple)
         : (int * int * int * int) list Or_error.t)]
  in
  test [] [] [];
  [%expect {| (Ok ()) |}];
  test [ 1; 2; 3 ] [ 1; 2 ] [ 1; 2 ];
  [%expect
    {|
    (0 1 1 1)
    (1 2 2 2)
    (Error "N_ary.List3.mapi_exn: lists have unequal lengths")
    |}];
  test [ 1; 2 ] [ 1; 2; 3 ] [ 1; 2 ];
  [%expect
    {|
    (0 1 1 1)
    (1 2 2 2)
    (Error "N_ary.List3.mapi_exn: lists have unequal lengths")
    |}];
  test [ 1; 2 ] [ 1; 2 ] [ 1; 2; 3 ];
  [%expect
    {|
    (0 1 1 1)
    (1 2 2 2)
    (Error "N_ary.List3.mapi_exn: lists have unequal lengths")
    |}];
  test [ 3; 1; 4; 1; 5; 9 ] [ 2; 7; 1; 8; 2; 8 ] [ 1; 6; 1; 8; 0; 3 ];
  [%expect
    {|
    (0 3 2 1)
    (1 1 7 6)
    (2 4 1 1)
    (3 1 8 8)
    (4 5 2 0)
    (5 9 8 3)
    (Ok (
      (0 3 2 1)
      (1 1 7 6)
      (2 4 1 1)
      (3 1 8 8)
      (4 5 2 0)
      (5 9 8 3)))
    |}]
;;

let iter = N_ary.List3.iter

let%expect_test _ =
  let test list1 list2 list3 =
    print_s
      [%sexp (iter list1 list2 list3 ~f:print_triple : unit List.Or_unequal_lengths.t)]
  in
  test [] [] [];
  [%expect {| (Ok ()) |}];
  test [ 1; 2; 3 ] [ 1; 2 ] [ 1; 2 ];
  [%expect {| Unequal_lengths |}];
  test [ 1; 2 ] [ 1; 2; 3 ] [ 1; 2 ];
  [%expect {| Unequal_lengths |}];
  test [ 1; 2 ] [ 1; 2 ] [ 1; 2; 3 ];
  [%expect {| Unequal_lengths |}];
  test [ 3; 1; 4; 1; 5; 9 ] [ 2; 7; 1; 8; 2; 8 ] [ 1; 6; 1; 8; 0; 3 ];
  [%expect
    {|
    (3 2 1)
    (1 7 6)
    (4 1 1)
    (1 8 8)
    (5 2 0)
    (9 8 3)
    (Ok ())
    |}]
;;

let iter_exn = N_ary.List3.iter_exn

let%expect_test _ =
  let test list1 list2 list3 =
    print_s
      [%sexp
        (Or_error.try_with (fun () -> iter_exn list1 list2 list3 ~f:print_triple)
         : unit Or_error.t)]
  in
  test [] [] [];
  [%expect {| (Ok ()) |}];
  test [ 1; 2; 3 ] [ 1; 2 ] [ 1; 2 ];
  [%expect
    {|
    (1 1 1)
    (2 2 2)
    (Error "N_ary.List3.iter_exn: lists have unequal lengths")
    |}];
  test [ 1; 2 ] [ 1; 2; 3 ] [ 1; 2 ];
  [%expect
    {|
    (1 1 1)
    (2 2 2)
    (Error "N_ary.List3.iter_exn: lists have unequal lengths")
    |}];
  test [ 1; 2 ] [ 1; 2 ] [ 1; 2; 3 ];
  [%expect
    {|
    (1 1 1)
    (2 2 2)
    (Error "N_ary.List3.iter_exn: lists have unequal lengths")
    |}];
  test [ 3; 1; 4; 1; 5; 9 ] [ 2; 7; 1; 8; 2; 8 ] [ 1; 6; 1; 8; 0; 3 ];
  [%expect
    {|
    (3 2 1)
    (1 7 6)
    (4 1 1)
    (1 8 8)
    (5 2 0)
    (9 8 3)
    (Ok ())
    |}]
;;

let iteri = N_ary.List3.iteri

let%expect_test _ =
  let test list1 list2 list3 =
    print_s
      [%sexp (iteri list1 list2 list3 ~f:print_itriple : unit List.Or_unequal_lengths.t)]
  in
  test [] [] [];
  [%expect {| (Ok ()) |}];
  test [ 1; 2; 3 ] [ 1; 2 ] [ 1; 2 ];
  [%expect {| Unequal_lengths |}];
  test [ 1; 2 ] [ 1; 2; 3 ] [ 1; 2 ];
  [%expect {| Unequal_lengths |}];
  test [ 1; 2 ] [ 1; 2 ] [ 1; 2; 3 ];
  [%expect {| Unequal_lengths |}];
  test [ 3; 1; 4; 1; 5; 9 ] [ 2; 7; 1; 8; 2; 8 ] [ 1; 6; 1; 8; 0; 3 ];
  [%expect
    {|
    (0 3 2 1)
    (1 1 7 6)
    (2 4 1 1)
    (3 1 8 8)
    (4 5 2 0)
    (5 9 8 3)
    (Ok ())
    |}]
;;

let iteri_exn = N_ary.List3.iteri_exn

let%expect_test _ =
  let test list1 list2 list3 =
    print_s
      [%sexp
        (Or_error.try_with (fun () -> iteri_exn list1 list2 list3 ~f:print_itriple)
         : unit Or_error.t)]
  in
  test [] [] [];
  [%expect {| (Ok ()) |}];
  test [ 1; 2; 3 ] [ 1; 2 ] [ 1; 2 ];
  [%expect
    {|
    (0 1 1 1)
    (1 2 2 2)
    (Error "N_ary.List3.iteri_exn: lists have unequal lengths")
    |}];
  test [ 1; 2 ] [ 1; 2; 3 ] [ 1; 2 ];
  [%expect
    {|
    (0 1 1 1)
    (1 2 2 2)
    (Error "N_ary.List3.iteri_exn: lists have unequal lengths")
    |}];
  test [ 1; 2 ] [ 1; 2 ] [ 1; 2; 3 ];
  [%expect
    {|
    (0 1 1 1)
    (1 2 2 2)
    (Error "N_ary.List3.iteri_exn: lists have unequal lengths")
    |}];
  test [ 3; 1; 4; 1; 5; 9 ] [ 2; 7; 1; 8; 2; 8 ] [ 1; 6; 1; 8; 0; 3 ];
  [%expect
    {|
    (0 3 2 1)
    (1 1 7 6)
    (2 4 1 1)
    (3 1 8 8)
    (4 5 2 0)
    (5 9 8 3)
    (Ok ())
    |}]
;;

let check_equal_lengths = N_ary.List3.check_equal_lengths

let%expect_test _ =
  let test list1 list2 list3 =
    print_s
      [%sexp (check_equal_lengths list1 list2 list3 : unit List.Or_unequal_lengths.t)]
  in
  test [] [] [];
  [%expect {| (Ok ()) |}];
  test [ 1; 2; 3 ] [ 1; 2 ] [ 1; 2 ];
  [%expect {| Unequal_lengths |}];
  test [ 1; 2 ] [ 1; 2; 3 ] [ 1; 2 ];
  [%expect {| Unequal_lengths |}];
  test [ 1; 2 ] [ 1; 2 ] [ 1; 2; 3 ];
  [%expect {| Unequal_lengths |}];
  test [ 3; 1; 4; 1; 5; 9 ] [ 2; 7; 1; 8; 2; 8 ] [ 1; 6; 1; 8; 0; 3 ];
  [%expect {| (Ok ()) |}]
;;
