open! Core
open! Expect_test_helpers_core

let%test "[interval] and [nonempty_interval] equal when [interval] is not empty" =
  Interval.equal
    Interval.zero_to_one
    (Interval.Nonempty.to_interval Interval.Nonempty.zero_to_one)
;;

let%test "[interval] and [nonempty_interval] equal when [interval] is not empty" =
  Interval.Nonempty.equal
    (Interval.Nonempty.of_interval_exn Interval.zero_to_one)
    Interval.Nonempty.zero_to_one
;;

let%test "[empty_interval] is [None]" =
  Interval.Nonempty.create Int.one Int.zero |> Option.is_none
;;

let%expect_test "[empty_interval] raises in [create_exn]" =
  show_raise (fun () -> Interval.Nonempty.create_exn Int.one Int.zero);
  [%expect
    {|
    (raised (
      Invalid_argument
      "(\"Lower bound strictly greater than upper bound:\" (lbound 1) (ubound 0))"))
    |}]
;;

let%test "[of_interval] is [None] when input is empty" =
  Interval.create Int.one Int.zero |> Interval.Nonempty.of_interval |> Option.is_none
;;

let%test "[is_singleton] correctly identifies a singleton" =
  Interval.Nonempty.create_exn Int.one Int.one |> Interval.Nonempty.is_singleton
;;

let%test "check [lbound]" =
  let lbound = Interval.Nonempty.lbound Interval.Nonempty.neg_one_to_one in
  lbound = Int.(neg one)
;;

let%test "check [ubound]" =
  let ubound = Interval.Nonempty.ubound Interval.Nonempty.neg_one_to_one in
  ubound = Int.one
;;

let%test "check [bounds]" =
  let lbound, ubound = Interval.Nonempty.bounds Interval.Nonempty.neg_one_to_one in
  Int.(lbound = neg one) && Int.(ubound = one)
;;

let%test "[map] is [None] when mapping to an empty interval" =
  Interval.Nonempty.map Interval.Nonempty.neg_one_to_one ~f:Int.neg |> Option.is_none
;;

let%expect_test "empty mapping raises in [map_exn]" =
  show_raise (fun () ->
    Interval.Nonempty.map_exn Interval.Nonempty.neg_one_to_one ~f:Int.neg);
  [%expect
    {|
    (raised (
      "Mapping a [Nonempty_interval.t] to an empty interval is disallowed:"
      (t (-1 1))))
    |}]
;;

let%test "check mapped [bounds]" =
  let mapped_interval =
    Interval.Nonempty.map_exn Interval.Nonempty.neg_one_to_one ~f:(Int.( * ) 1_000)
  in
  let lbound, ubound = Interval.Nonempty.bounds mapped_interval in
  (lbound = Int.(neg 1_000)) && ubound = 1_000
;;
