[@@@disable_unused_warnings]

open! Core

(* $MDX part-begin=example-1 *)
let two_kinds_of_sums ints =
  let init = 0, 0 in
  List.fold ints ~init ~f:(fun (normal_sum, absolute_value_sum) elem ->
    let normal_sum = elem + normal_sum in
    let absolute_value_sum = abs elem + absolute_value_sum in
    normal_sum, absolute_value_sum)
;;

(* $MDX part-end *)

(* $MDX part-begin=example-1-failure *)
let diff =
  let absolute_value_sum, normal_sum = two_kinds_of_sums [ 1; -1; 2; -3; 5 ] in
  absolute_value_sum - normal_sum
;;

(* $MDX part-end *)

let%test _ = diff = -8

(* $MDX part-begin=example-2 *)
let two_kinds_of_sums ints =
  let init = [%anon { normal_sum = 0; absolute_value_sum = 0 }] in
  List.fold ints ~init ~f:(fun [%anon? { normal_sum; absolute_value_sum }] elem ->
    let normal_sum = elem + normal_sum in
    let absolute_value_sum = abs elem + absolute_value_sum in
    [%anon { normal_sum; absolute_value_sum }])
;;

(* $MDX part-end *)

let (_ : int list -> [%anon { normal_sum : int; absolute_value_sum : int }]) =
  two_kinds_of_sums
;;

(* $MDX part-begin=example-2-success *)
let diff =
  let [%anon? { normal_sum; absolute_value_sum }] =
    two_kinds_of_sums [ 1; -1; 2; -3; 5 ]
  in
  absolute_value_sum - normal_sum
;;

(* $MDX part-end *)

let%test _ = diff = 8

(* $MDX part-begin=example-2-almost-a-record *)
let diff =
  let [%anon? { absolute_value_sum = _; normal_sum = sum }] =
    two_kinds_of_sums [ 1; -1; 2; -3; 5 ]
  in
  sum
;;

(* $MDX part-end *)

(* $MDX part-begin=example-3 *)
let ints_vs_floats =
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
  max_int - min_int - int_of_float (max_float -. min_float)
;;

(* $MDX part-end *)
