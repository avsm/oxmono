open! Base
open Expect_test_helpers_base

type t = N_ary.Enum3.t =
  | Case0
  | Case1
  | Case2
[@@deriving compare, enumerate, equal, hash, sexp_of]

let to_int = N_ary.Enum3.to_int

let%expect_test _ =
  List.iter all ~f:(fun t -> print_s [%sexp (t : t), (to_int t : int)]);
  [%expect
    {|
    (Case0 0)
    (Case1 1)
    (Case2 2)
    |}]
;;

let of_int = N_ary.Enum3.of_int

let%expect_test _ =
  for i = -1 to 2 do
    print_s [%sexp (i : int), (of_int i : t option)]
  done;
  [%expect
    {|
    (-1 ())
    (0 (Case0))
    (1 (Case1))
    (2 (Case2))
    |}]
;;

let of_int_exn = N_ary.Enum3.of_int_exn

let%expect_test _ =
  for i = -1 to 3 do
    print_s [%sexp (i : int), (Or_error.try_with (fun () -> of_int_exn i) : t Or_error.t)]
  done;
  [%expect
    {|
    (-1 (Error ("N_ary.Enum3.of_int_exn: invalid input" -1)))
    (0 (Ok Case0))
    (1 (Ok Case1))
    (2 (Ok Case2))
    (3 (Error ("N_ary.Enum3.of_int_exn: invalid input" 3)))
    |}]
;;

let case0 = N_ary.Enum3.case0

let%expect_test _ =
  print_s [%sexp (case0 : t)];
  [%expect {| Case0 |}]
;;

let case1 = N_ary.Enum3.case1

let%expect_test _ =
  print_s [%sexp (case1 : t)];
  [%expect {| Case1 |}]
;;

let case2 = N_ary.Enum3.case2

let%expect_test _ =
  print_s [%sexp (case2 : t)];
  [%expect {| Case2 |}]
;;

let is_case0 = N_ary.Enum3.is_case0

let%expect_test _ =
  print_s [%sexp (List.filter all ~f:is_case0 : t list)];
  [%expect {| (Case0) |}]
;;

let is_case1 = N_ary.Enum3.is_case1

let%expect_test _ =
  print_s [%sexp (List.filter all ~f:is_case1 : t list)];
  [%expect {| (Case1) |}]
;;

let is_case2 = N_ary.Enum3.is_case2

let%expect_test _ =
  print_s [%sexp (List.filter all ~f:is_case2 : t list)];
  [%expect {| (Case2) |}]
;;
