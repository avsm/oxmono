open! Base
open Expect_test_helpers_base

type ('a, 'b, 'c) t = ('a, 'b, 'c) N_ary.Tuple3.t
[@@deriving compare, equal, hash, sexp_of]

let create = N_ary.Tuple3.create

let%expect_test _ =
  print_s [%sexp (create 1 2 3 : (int, int, int) t)];
  [%expect {| (1 2 3) |}]
;;

let part0 = N_ary.Tuple3.part0

let%expect_test _ =
  print_s [%sexp (part0 (1, 2, 3) : int)];
  [%expect {| 1 |}]
;;

let part1 = N_ary.Tuple3.part1

let%expect_test _ =
  print_s [%sexp (part1 (1, 2, 3) : int)];
  [%expect {| 2 |}]
;;

let part2 = N_ary.Tuple3.part2

let%expect_test _ =
  print_s [%sexp (part2 (1, 2, 3) : int)];
  [%expect {| 3 |}]
;;

let set_part0 = N_ary.Tuple3.set_part0

let%expect_test _ =
  print_s [%sexp (set_part0 (1, 2, 3) 10 : (int, int, int) t)];
  [%expect {| (10 2 3) |}]
;;

let set_part1 = N_ary.Tuple3.set_part1

let%expect_test _ =
  print_s [%sexp (set_part1 (1, 2, 3) 20 : (int, int, int) t)];
  [%expect {| (1 20 3) |}]
;;

let set_part2 = N_ary.Tuple3.set_part2

let%expect_test _ =
  print_s [%sexp (set_part2 (1, 2, 3) 30 : (int, int, int) t)];
  [%expect {| (1 2 30) |}]
;;

let map = N_ary.Tuple3.map

let%expect_test _ =
  print_s
    [%sexp
      (map (1, 2, 3) ~f0:(( + ) 10) ~f1:(( + ) 20) ~f2:(( + ) 30) : (int, int, int) t)];
  [%expect {| (11 22 33) |}]
;;

let map_part0 = N_ary.Tuple3.map_part0

let%expect_test _ =
  print_s [%sexp (map_part0 (1, 2, 3) ~f:(( + ) 10) : (int, int, int) t)];
  [%expect {| (11 2 3) |}]
;;

let map_part1 = N_ary.Tuple3.map_part1

let%expect_test _ =
  print_s [%sexp (map_part1 (1, 2, 3) ~f:(( + ) 20) : (int, int, int) t)];
  [%expect {| (1 22 3) |}]
;;

let map_part2 = N_ary.Tuple3.map_part2

let%expect_test _ =
  print_s [%sexp (map_part2 (1, 2, 3) ~f:(( + ) 30) : (int, int, int) t)];
  [%expect {| (1 2 33) |}]
;;
