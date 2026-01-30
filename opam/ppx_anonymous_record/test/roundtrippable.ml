open! Core

let%test_unit "pattern expands to expression" =
  let [%anon? { a; b; c }] = [%anon { a = 1; b = 1.; c = "1" }] in
  [%test_result: int * float * string] (a, b, c) ~expect:(1, 1.0, "1")
;;

let%test_unit "a single field" =
  let [%anon? { a }] = [%anon { a = 1 }] in
  [%test_result: int] a ~expect:1
;;

let%test_unit "order does not matter" =
  let [%anon? { b; a; c }] = [%anon { c = "1"; b = 1.; a = 1 }] in
  [%test_result: int * float * string] (a, b, c) ~expect:(1, 1.0, "1")
;;

let%test_unit "rename fields" =
  let [%anon? { a = a'; b = b'; c = c' }] = [%anon { a = 1; b = 1.; c = "1" }] in
  [%test_result: int * float * string] (a', b', c') ~expect:(1, 1.0, "1")
;;

let%test_unit "nested anonymous records" =
  let [%anon?
        { int = [%anon? { min = min_int; max = max_int }]
        ; float = [%anon? { min = min_float; max = max_float }]
        }]
    =
    [%anon
      { int = [%anon { min = Int.min_value; max = Int.max_value }]
      ; float = [%anon { min = Float.neg_infinity; max = Float.infinity }]
      }]
  in
  [%test_result: (int * int) * (float * float)]
    ((min_int, max_int), (min_float, max_float))
    ~expect:((Int.min_value, Int.max_value), (Float.neg_infinity, Float.infinity))
;;
