open! Base
open Expect_test_helpers_base

type ('a, 'b, 'c) t = ('a, 'b, 'c) N_ary.Variant3.t =
  | Case0 of 'a
  | Case1 of 'b
  | Case2 of 'c
[@@deriving compare, equal, hash, sexp_of]

let case0 = N_ary.Variant3.case0

let%expect_test _ =
  print_s [%sexp (case0 1 : (int, int, int) t)];
  [%expect {| (Case0 1) |}]
;;

let case1 = N_ary.Variant3.case1

let%expect_test _ =
  print_s [%sexp (case1 2 : (int, int, int) t)];
  [%expect {| (Case1 2) |}]
;;

let case2 = N_ary.Variant3.case2

let%expect_test _ =
  print_s [%sexp (case2 2 : (int, int, int) t)];
  [%expect {| (Case2 2) |}]
;;

let is_case0 = N_ary.Variant3.is_case0

let%expect_test _ =
  print_s
    [%sexp
      (List.filter [ Case0 1; Case1 2; Case2 3 ] ~f:is_case0 : (int, int, int) t list)];
  [%expect {| ((Case0 1)) |}]
;;

let is_case1 = N_ary.Variant3.is_case1

let%expect_test _ =
  print_s
    [%sexp
      (List.filter [ Case0 1; Case1 2; Case2 3 ] ~f:is_case1 : (int, int, int) t list)];
  [%expect {| ((Case1 2)) |}]
;;

let is_case2 = N_ary.Variant3.is_case2

let%expect_test _ =
  print_s
    [%sexp
      (List.filter [ Case0 1; Case1 2; Case2 3 ] ~f:is_case2 : (int, int, int) t list)];
  [%expect {| ((Case2 3)) |}]
;;

let get_case0 = N_ary.Variant3.get_case0

let%expect_test _ =
  print_s [%sexp (List.filter_map [ Case0 1; Case1 2; Case2 3 ] ~f:get_case0 : int list)];
  [%expect {| (1) |}]
;;

let get_case1 = N_ary.Variant3.get_case1

let%expect_test _ =
  print_s [%sexp (List.filter_map [ Case0 1; Case1 2; Case2 3 ] ~f:get_case1 : int list)];
  [%expect {| (2) |}]
;;

let get_case2 = N_ary.Variant3.get_case2

let%expect_test _ =
  print_s [%sexp (List.filter_map [ Case0 1; Case1 2; Case2 3 ] ~f:get_case2 : int list)];
  [%expect {| (3) |}]
;;

let get_case0_exn = N_ary.Variant3.get_case0_exn

let%expect_test _ =
  print_s [%sexp (List.map [ Case0 1; Case0 2 ] ~f:get_case0_exn : int list)];
  [%expect {| (1 2) |}]
;;

let get_case1_exn = N_ary.Variant3.get_case1_exn

let%expect_test _ =
  print_s [%sexp (List.map [ Case1 1; Case1 2 ] ~f:get_case1_exn : int list)];
  [%expect {| (1 2) |}]
;;

let get_case2_exn = N_ary.Variant3.get_case2_exn

let%expect_test _ =
  print_s [%sexp (List.map [ Case2 1; Case2 2 ] ~f:get_case2_exn : int list)];
  [%expect {| (1 2) |}]
;;

let map = N_ary.Variant3.map

let%expect_test _ =
  print_s
    [%sexp
      (List.map
         [ Case0 1; Case1 2; Case2 3 ]
         ~f:(map ~f0:(( + ) 10) ~f1:(( + ) 20) ~f2:(( + ) 30))
       : (int, int, int) t list)];
  [%expect
    {|
    ((Case0 11)
     (Case1 22)
     (Case2 33))
    |}]
;;

let map_case0 = N_ary.Variant3.map_case0

let%expect_test _ =
  print_s
    [%sexp
      (List.map [ Case0 1; Case1 2; Case2 3 ] ~f:(map_case0 ~f:(( + ) 10))
       : (int, int, int) t list)];
  [%expect
    {|
    ((Case0 11)
     (Case1 2)
     (Case2 3))
    |}]
;;

let map_case1 = N_ary.Variant3.map_case1

let%expect_test _ =
  print_s
    [%sexp
      (List.map [ Case0 1; Case1 2; Case2 3 ] ~f:(map_case1 ~f:(( + ) 20))
       : (int, int, int) t list)];
  [%expect
    {|
    ((Case0 1)
     (Case1 22)
     (Case2 3))
    |}]
;;

let map_case2 = N_ary.Variant3.map_case2

let%expect_test _ =
  print_s
    [%sexp
      (List.map [ Case0 1; Case1 2; Case2 3 ] ~f:(map_case2 ~f:(( + ) 20))
       : (int, int, int) t list)];
  [%expect
    {|
    ((Case0 1)
     (Case1 2)
     (Case2 23))
    |}]
;;
