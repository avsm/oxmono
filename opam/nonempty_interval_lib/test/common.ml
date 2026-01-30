open! Core
open! Quickcheck.Generator.Let_syntax

let interval_and_nonempty_interval_of_length ~length =
  let%map lbound = Int.gen_incl Int.min_value (Int.max_value - length) in
  let rbound = lbound + length in
  Interval.create lbound rbound, Interval.Nonempty.create_exn lbound rbound
;;

let interval_and_nonempty_interval =
  let%bind length = Quickcheck.Generator.small_non_negative_int in
  interval_and_nonempty_interval_of_length ~length
;;
