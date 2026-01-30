open! Core

let%expect_test "human-readable strings at [1x] threshold" =
  let threshold_use_percent =
    "0.999999999999999999999999999999999999999999999999"
    |> Bignum.of_string
    |> Big_percent.of_mult
    |> Big_percent.to_string_accurate
  in
  let threshold_use_x =
    "1" |> Bignum.of_string |> Big_percent.of_mult |> Big_percent.to_string_accurate
  in
  print_s [%message threshold_use_percent];
  print_s [%message threshold_use_x];
  [%expect
    {|
    99.9999999999999999999999999999999999999999999999%
    1x
    |}]
;;

let%expect_test "human-readable strings at [1%] threshold" =
  let threshold_use_bps =
    "0.00999999999999999999999999999999999999999999999999"
    |> Bignum.of_string
    |> Big_percent.of_mult
    |> Big_percent.to_string_accurate
  in
  let threshold_use_percent =
    "0.01" |> Bignum.of_string |> Big_percent.of_mult |> Big_percent.to_string_accurate
  in
  print_s [%message threshold_use_bps];
  print_s [%message threshold_use_percent];
  [%expect
    {|
    99.9999999999999999999999999999999999999999999999bp
    1%
    |}]
;;

let%expect_test "threshold to_string_hum rounding" =
  let threshold_use_percent =
    "0.999999999999999999999999999999999999999999999999"
    |> Bignum.of_string
    |> Big_percent.of_mult
    |> Big_percent.to_string_hum
  in
  let threshold_use_bps =
    "0.00999999999999999999999999999999999999999999999999"
    |> Bignum.of_string
    |> Big_percent.of_mult
    |> Big_percent.to_string_hum
  in
  print_s [%message threshold_use_percent];
  print_s [%message threshold_use_bps];
  [%expect
    {|
    100%
    100bp
    |}]
;;
