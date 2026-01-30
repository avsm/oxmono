open! Core
open! Expect_test_helpers_core

let%expect_test "canonical example" =
  let with_percent = Percent.(of_mult 0.1 + of_mult 0.2) |> Percent.to_mult in
  let with_big_percent =
    Big_percent.(
      of_mult (Bignum.of_float_decimal 0.1) + of_mult (Bignum.of_float_decimal 0.2)
      |> to_mult)
    |> Bignum.to_float
  in
  print_s [%message (with_percent : float)];
  print_s [%message (with_big_percent : float)];
  [%expect
    {|
    (with_percent 0.30000000000000004)
    (with_big_percent 0.3)
    |}]
;;

let%expect_test "[round_decimal_]" =
  let big_percent' = "0.0123456%" |> Big_percent.of_string_exn in
  let mult =
    Big_percent.round_decimal_mult big_percent' ~digits:4
    |> Big_percent.to_string_accurate
  in
  let percentage =
    Big_percent.round_decimal_percentage big_percent' ~digits:4
    |> Big_percent.to_string_accurate
  in
  let bp =
    Big_percent.round_decimal_bp big_percent' ~digits:4 |> Big_percent.to_string_accurate
  in
  print_s [%message mult];
  print_s [%message percentage];
  print_s [%message bp];
  [%expect
    {|
    1bp
    1.23bp
    1.2346bp
    |}]
;;

module%test [@name "expressions"] _ = struct
  let mult = Helpers.of_mult 0.53
  let mult' = Helpers.of_mult 0.42

  let%expect_test "inline" =
    let multiply = Big_percent.(mult * mult') in
    let add = Big_percent.(mult + mult') in
    let subtract = Big_percent.(mult - mult') in
    let divide = Big_percent.(mult / mult') in
    let divide_bignum = Big_percent.(mult // mult') in
    print_s [%message (multiply : Big_percent.Unstable.t)];
    print_s [%message (add : Big_percent.Unstable.t)];
    print_s [%message (subtract : Big_percent.Unstable.t)];
    print_s [%message (divide : Big_percent.Unstable.t)];
    print_s [%message (divide_bignum : Bignum.t)];
    [%expect
      {|
      (multiply 22.26%)
      (add 95%)
      (subtract 11%)
      (divide "(1.261904761 + 19/21000000000)x")
      (divide_bignum (1.261904761 + 19/21000000000))
      |}]
  ;;

  let%expect_test "[scale] and [apply]" =
    let bignum = 2. |> Bignum.of_float_decimal in
    let scaled = Big_percent.scale mult bignum in
    let applied = Big_percent.apply mult bignum in
    print_s [%message (scaled : Big_percent.Unstable.t)];
    print_s [%message (applied : Bignum.t)];
    [%expect
      {|
      (scaled 1.06x)
      (applied 1.06)
      |}]
  ;;

  let%expect_test "real constants" =
    let zero = Big_percent.zero in
    let one_hundred_percent = Big_percent.one_hundred_percent in
    print_s [%message (zero : Big_percent.Unstable.t) (Big_percent.is_zero zero : bool)];
    print_s [%message (one_hundred_percent : Big_percent.Unstable.t)];
    [%expect
      {|
      ((zero                       0x)
       ("Big_percent.is_zero zero" true))
      (one_hundred_percent 1x)
      |}]
  ;;

  let%expect_test "non-real constants" =
    let infinity = "1/0%" |> Big_percent.of_string_allow_nan_and_inf in
    let nan = "nan%" |> Big_percent.of_string_allow_nan_and_inf in
    print_s
      [%message (infinity : Big_percent.Unstable.t) (Big_percent.is_inf infinity : bool)];
    print_s [%message (nan : Big_percent.Unstable.t) (Big_percent.is_nan nan : bool)];
    [%expect
      {|
      ((infinity                      INFx)
       ("Big_percent.is_inf infinity" true))
      ((nan                      NANbp)
       ("Big_percent.is_nan nan" true))
      |}]
  ;;
end

module%test [@name "strings"] _ = struct
  let mult = Helpers.of_mult 53.1
  let percentage = Helpers.of_mult 0.531
  let bp = Helpers.of_mult 0.00531
  let nan = Helpers.of_mult Float.nan
  let infinity = Helpers.of_mult Float.infinity
  let neg_infinity = Helpers.of_mult Float.neg_infinity

  let%expect_test "[to_string_accurate] real" =
    let mult = Big_percent.to_string_accurate mult in
    let percentage = Big_percent.to_string_accurate percentage in
    let bp = Big_percent.to_string_accurate bp in
    print_s [%message mult];
    print_s [%message percentage];
    print_s [%message bp];
    [%expect
      {|
      53.1x
      53.1%
      53.1bp
      |}]
  ;;

  let%expect_test "[to_string_accurate] non-real" =
    let nan = Big_percent.to_string_accurate nan in
    let infinity = Big_percent.to_string_accurate infinity in
    let neg_infinity = Big_percent.to_string_accurate neg_infinity in
    print_s [%message nan];
    print_s [%message infinity];
    print_s [%message neg_infinity];
    [%expect
      {|
      NANbp
      INFx
      -INFx
      |}]
  ;;

  let%expect_test "[to_string_hum] real" =
    let mult = Big_percent.to_string_accurate mult in
    let percentage = Big_percent.to_string_accurate percentage in
    let bp = Big_percent.to_string_accurate bp in
    print_s [%message mult];
    print_s [%message percentage];
    print_s [%message bp];
    [%expect
      {|
      53.1x
      53.1%
      53.1bp
      |}]
  ;;

  let%expect_test "[of_string_exn] real" =
    let mult = Big_percent.of_string_exn "53x" in
    let percentage = Big_percent.of_string_exn "53%" in
    let bp = Big_percent.of_string_exn "53bp" in
    print_s [%message (mult : Big_percent.Unstable.t)];
    print_s [%message (percentage : Big_percent.Unstable.t)];
    print_s [%message (bp : Big_percent.Unstable.t)];
    [%expect
      {|
      (mult 53x)
      (percentage 53%)
      (bp 53bp)
      |}]
  ;;

  let%expect_test "[of_string_allow_nan_and_inf]" =
    let nan = Big_percent.of_string_allow_nan_and_inf "nanx" in
    let positive_infinity = Big_percent.of_string_allow_nan_and_inf "inf%" in
    let negative_infinity = Big_percent.of_string_allow_nan_and_inf "-inf%" in
    print_s [%message (nan : Big_percent.Unstable.t)];
    print_s [%message (positive_infinity : Big_percent.Unstable.t)];
    print_s [%message (negative_infinity : Big_percent.Unstable.t)];
    [%expect
      {|
      (nan NANbp)
      (positive_infinity INFx)
      (negative_infinity -INFx)
      |}]
  ;;

  let%expect_test "raise [Nan_or_inf]" =
    show_raise (fun () -> "nan%" |> Big_percent.of_string_exn);
    show_raise (fun () -> "1/0x" |> Big_percent.of_string_exn);
    [%expect
      {|
      (raised big_percent.ml.Nan_or_inf)
      (raised big_percent.ml.Nan_or_inf)
      |}]
  ;;

  let%expect_test "raise `must end in'" =
    show_raise (fun () -> Big_percent.of_string_exn "0.53");
    show_raise (fun () -> Big_percent.of_string_allow_nan_and_inf "inf");
    [%expect
      {|
      (raised (Failure "Big_percent.of_string_exn: must end in x, %, or bp: 0.53"))
      (raised (
        Failure
        "Big_percent.of_string_allow_nan_and_inf: must end in x, %, or bp: inf"))
      |}]
  ;;

  let%expect_test "[Always_percentage] real" =
    let mult = Big_percent.Always_percentage.to_string_accurate mult in
    let percentage = Big_percent.Always_percentage.to_string_accurate percentage in
    let bp = Big_percent.Always_percentage.to_string_accurate bp in
    print_s [%message mult];
    print_s [%message percentage];
    print_s [%message bp];
    [%expect
      {|
      5310%
      53.1%
      0.531%
      |}]
  ;;

  let%expect_test "[Always_percentage] infinite" =
    let infinity = Big_percent.Always_percentage.to_string_accurate infinity in
    let neg_infinity = Big_percent.Always_percentage.to_string_accurate neg_infinity in
    print_s [%message infinity];
    print_s [%message neg_infinity];
    [%expect
      {|
      INF%
      -INF%
      |}]
  ;;
end

let%expect_test "raise if using Stable.t_of_sexp on Unstable.sexp_of_t" =
  let test t =
    require_does_raise (fun () ->
      let unstable_sexp = Big_percent.Unstable.sexp_of_t t in
      Big_percent.Stable.V3.t_of_sexp unstable_sexp)
  in
  test (Helpers.of_mult 1.23);
  [%expect
    {|
    (Failure
     "unable to parse as Big_percent.Stable.V3.t. Expected a plain Bignum without suffixes, but received \"1.23x\"")
    |}];
  test (Helpers.of_percentage 1.23);
  [%expect
    {|
    (Failure
     "unable to parse as Big_percent.Stable.V3.t. Expected a plain Bignum without suffixes, but received \"1.23%\"")
    |}];
  test (Helpers.of_bp 1.23);
  [%expect
    {|
    (Failure
     "unable to parse as Big_percent.Stable.V3.t. Expected a plain Bignum without suffixes, but received \"1.23bp\"")
    |}];
  test (Helpers.of_mult Float.nan);
  [%expect
    {|
    (Failure
     "unable to parse as Big_percent.Stable.V3.t. Expected a plain Bignum without suffixes, but received \"NANbp\"")
    |}];
  test (Helpers.of_mult Float.infinity);
  [%expect
    {|
    (Failure
     "unable to parse as Big_percent.Stable.V3.t. Expected a plain Bignum without suffixes, but received \"INFx\"")
    |}];
  test (Helpers.of_mult Float.neg_infinity);
  [%expect
    {|
    (Failure
     "unable to parse as Big_percent.Stable.V3.t. Expected a plain Bignum without suffixes, but received \"-INFx\"")
    |}]
;;

let%expect_test "raise if using Unstable.t_of_sexp on Stable.sexp_of_t" =
  let test t =
    require_does_raise (fun () ->
      let stable_sexp = Big_percent.Stable.V3.sexp_of_t t in
      Big_percent.Unstable.t_of_sexp stable_sexp)
  in
  test (Helpers.of_mult 1.23);
  [%expect
    {|
    (Of_sexp_error
     "Big_percent.Unstable.t_of_sexp: must end in x, %, or bp: 1.23"
     (invalid_sexp 1.23))
    |}];
  test (Helpers.of_percentage 1.23);
  [%expect
    {|
    (Of_sexp_error
     "Big_percent.Unstable.t_of_sexp: must end in x, %, or bp: 0.0123"
     (invalid_sexp 0.0123))
    |}];
  test (Helpers.of_bp 1.23);
  [%expect
    {|
    (Of_sexp_error
     "Big_percent.Unstable.t_of_sexp: must end in x, %, or bp: 0.000123"
     (invalid_sexp 0.000123))
    |}];
  test (Helpers.of_mult Float.nan);
  [%expect
    {|
    (Of_sexp_error
     "Big_percent.Unstable.t_of_sexp: must end in x, %, or bp: nan"
     (invalid_sexp nan))
    |}];
  test (Helpers.of_mult Float.infinity);
  [%expect
    {|
    (Of_sexp_error
     "Big_percent.Unstable.t_of_sexp: must end in x, %, or bp: inf"
     (invalid_sexp inf))
    |}];
  test (Helpers.of_mult Float.neg_infinity);
  [%expect
    {|
    (Of_sexp_error
     "Big_percent.Unstable.t_of_sexp: must end in x, %, or bp: -inf"
     (invalid_sexp -inf))
    |}]
;;
