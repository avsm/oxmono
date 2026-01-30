open! Core

module Nonempty = struct
  include Nonempty_interval_lib.Nonempty_interval.Make (Int)

  let zero_to_one = create_exn Int.zero Int.one
  let neg_one_to_one = create_exn Int.(neg one) Int.one
  let equal t t' = is_subset t ~of_:t' && is_superset t ~of_:t'

  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind length = Quickcheck.Generator.small_non_negative_int in
    let%map lbound = Int.gen_incl Int.min_value (Int.max_value - length) in
    let rbound = lbound + length in
    create_exn lbound rbound
  ;;
end

include Interval_lib.Interval.Make (Int)

let zero_to_one = create Int.zero Int.one
let equal t t' = is_subset t ~of_:t' && is_superset t ~of_:t'

let gen_nonempty =
  let open Quickcheck.Generator.Let_syntax in
  let%bind length = Quickcheck.Generator.small_non_negative_int in
  let%map lbound = Int.gen_incl Int.min_value (Int.max_value - length) in
  let rbound = lbound + length in
  let interval = create lbound rbound in
  if is_empty interval
  then
    raise_s
      [%message
        "[Quickcheck.Generator] created an empty interval where a nonempty interval was \
         expected."];
  interval
;;
