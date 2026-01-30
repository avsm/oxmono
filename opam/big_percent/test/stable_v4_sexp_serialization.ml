open! Core
open! Expect_test_helpers_core

let print_unstable_and_stable_v4_sexp_representations x =
  print_s ([%sexp_of: Big_percent.Stable.V4.t] x);
  print_s ([%sexp_of: Big_percent.Unstable.t] x)
;;

let%expect_test "x" =
  let x = Helpers.of_mult 2. in
  print_unstable_and_stable_v4_sexp_representations x;
  [%expect
    {|
    2x
    2x
    |}]
;;

let%expect_test "bp" =
  let x = Helpers.of_mult 0.0001 in
  print_unstable_and_stable_v4_sexp_representations x;
  [%expect
    {|
    1bp
    1bp
    |}]
;;

let%expect_test "%" =
  let x = Helpers.of_mult 0.53 in
  print_unstable_and_stable_v4_sexp_representations x;
  [%expect
    {|
    53%
    53%
    |}]
;;

let%expect_test "fractions" =
  let x = Helpers.of_mult 0.53 in
  let y = Helpers.of_mult 0.42 in
  let z = Big_percent.(x / y) in
  print_unstable_and_stable_v4_sexp_representations z;
  [%expect
    {|
    "(1.261904761 + 19/21000000000)x"
    "(1.261904761 + 19/21000000000)x"
    |}]
;;

let%expect_test "real constants" =
  let zero = Big_percent.zero in
  let one_hundred_percent = Big_percent.one_hundred_percent in
  print_unstable_and_stable_v4_sexp_representations zero;
  print_unstable_and_stable_v4_sexp_representations one_hundred_percent;
  [%expect
    {|
    0x
    0x
    1x
    1x
    |}]
;;

let%expect_test "non-real constants" =
  let infinity = "1/0%" |> Big_percent.of_string_allow_nan_and_inf in
  let neg_infinity = "-1/0%" |> Big_percent.of_string_allow_nan_and_inf in
  let nan = "nan%" |> Big_percent.of_string_allow_nan_and_inf in
  print_unstable_and_stable_v4_sexp_representations infinity;
  print_unstable_and_stable_v4_sexp_representations neg_infinity;
  print_unstable_and_stable_v4_sexp_representations nan;
  [%expect
    {|
    INFx
    INFx
    -INFx
    -INFx
    NANbp
    NANbp
    |}]
;;

let%test_unit "roundtrip with [to_sexp] finite" =
  Quickcheck.test
    Bignum.gen_finite
    ~sexp_of:[%sexp_of: Bignum.t]
    ~trials:10_000
    ~f:(fun bignum ->
      let big_percent = Big_percent.of_mult bignum in
      let roundtripped_big_percent =
        big_percent |> Big_percent.Stable.V4.sexp_of_t |> Big_percent.Stable.V4.t_of_sexp
      in
      [%test_eq: Big_percent.t] big_percent roundtripped_big_percent)
;;

let%test_unit "roundtrip with [to_sexp] +infinity" =
  let big_percent = Big_percent.of_string_allow_nan_and_inf "INFx" in
  let roundtripped_big_percent =
    big_percent |> Big_percent.Stable.V4.sexp_of_t |> Big_percent.Stable.V4.t_of_sexp
  in
  [%test_eq: Big_percent.t] big_percent roundtripped_big_percent
;;

let%test_unit "roundtrip with [to_sexp] -infinity" =
  let big_percent = Big_percent.of_string_allow_nan_and_inf "-INFx" in
  let roundtripped_big_percent =
    big_percent |> Big_percent.Stable.V4.sexp_of_t |> Big_percent.Stable.V4.t_of_sexp
  in
  [%test_eq: Big_percent.t] big_percent roundtripped_big_percent
;;

let%test_unit "roundtrip with [to_sexp] nan" =
  let big_percent = Big_percent.of_string_allow_nan_and_inf "NANx" in
  let roundtripped_big_percent =
    big_percent |> Big_percent.Stable.V4.sexp_of_t |> Big_percent.Stable.V4.t_of_sexp
  in
  [%test_eq: Big_percent.t] big_percent roundtripped_big_percent
;;

let%expect_test "failed [t_of_sexp] when there isn't a suffix" =
  Expect_test_helpers_core.show_raise (fun () ->
    Big_percent.Stable.V4.t_of_sexp (Sexp.of_string "1234"));
  [%expect
    {|
    (raised (
      Of_sexp_error
      "Big_percent.Stable.V4.t_of_sexp: must end in x, %, or bp: 1234"
      (invalid_sexp 1234)))
    |}]
;;

let%expect_test "failed [t_of_sexp] when the bignum is malformed" =
  Expect_test_helpers_core.show_raise (fun () ->
    Big_percent.Stable.V4.t_of_sexp (Sexp.of_string "12a34x"));
  [%expect
    {|
    (raised (
      Of_sexp_error
      "unable to parse as Big_percent.Stable.V4.t. Expected a plain Bignum without suffixes, but received \"12a34\""
      (invalid_sexp 12a34x)))
    |}]
;;

let%expect_test "sexp grammar validation" =
  Sexp_grammar_validation.validate_grammar
    (module struct
      include Big_percent.Stable.V4

      let quickcheck_generator = Big_percent.quickcheck_generator
      let quickcheck_shrinker = Big_percent.quickcheck_shrinker
    end)
  |> ok_exn;
  [%expect {| String |}]
;;
