open! Core

let%test_unit "roundtrip to [Percent.t] finite decimal" =
  Quickcheck.test
    Float.gen_finite
    ~sexp_of:[%sexp_of: float]
    ~trials:10_000
    ~f:(fun float ->
      let percent = Percent.of_mult float in
      let roundtripped_percent =
        percent |> Big_percent.of_percent_decimal |> Big_percent.to_percent
      in
      [%test_eq: Percent.t] percent roundtripped_percent)
;;

let%test_unit "roundtrip to [Percent.t] finite dyadic" =
  Quickcheck.test
    Float.gen_finite
    ~sexp_of:[%sexp_of: float]
    ~trials:10_000
    ~f:(fun float ->
      let percent = Percent.of_mult float in
      let roundtripped_percent =
        percent |> Big_percent.of_percent_dyadic |> Big_percent.to_percent
      in
      [%test_eq: Percent.t] percent roundtripped_percent)
;;

let%test_unit "roundtrip to [Percent.t] +infinity" =
  let percent = Percent.of_mult Float.infinity in
  let roundtripped_percent =
    percent |> Big_percent.of_percent_decimal |> Big_percent.to_percent
  in
  [%test_eq: Percent.t] percent roundtripped_percent
;;

let%test_unit "roundtrip to [Percent.t] -infinity" =
  let percent = Percent.of_mult Float.neg_infinity in
  let roundtripped_percent =
    percent |> Big_percent.of_percent_decimal |> Big_percent.to_percent
  in
  [%test_eq: Percent.t] percent roundtripped_percent
;;

let%test_unit "roundtrip to [Percent.t] nan" =
  let percent = Percent.of_mult Float.nan in
  let roundtripped_percent =
    percent |> Big_percent.of_percent_decimal |> Big_percent.to_percent
  in
  [%test_eq: Percent.t] percent roundtripped_percent
;;

let%test_unit "roundtrip with [to_string_accurate] finite" =
  Quickcheck.test
    Bignum.gen_finite
    ~sexp_of:[%sexp_of: Bignum.t]
    ~trials:10_000
    ~f:(fun bignum ->
      let big_percent = Big_percent.of_mult bignum in
      let roundtripped_big_percent =
        big_percent
        |> Big_percent.to_string_accurate
        |> Big_percent.of_string_allow_nan_and_inf
      in
      [%test_eq: Big_percent.Unstable.t] big_percent roundtripped_big_percent)
;;

let%test_unit "roundtrip with [to_string_accurate] +infinity" =
  let big_percent = Big_percent.of_string_allow_nan_and_inf "INFx" in
  let roundtripped_big_percent =
    big_percent
    |> Big_percent.to_string_accurate
    |> Big_percent.of_string_allow_nan_and_inf
  in
  [%test_eq: Big_percent.Unstable.t] big_percent roundtripped_big_percent
;;

let%test_unit "roundtrip with [to_string_accurate] -infinity" =
  let big_percent = Big_percent.of_string_allow_nan_and_inf "-INFx" in
  let roundtripped_big_percent =
    big_percent
    |> Big_percent.to_string_accurate
    |> Big_percent.of_string_allow_nan_and_inf
  in
  [%test_eq: Big_percent.Unstable.t] big_percent roundtripped_big_percent
;;

let%test_unit "roundtrip with [to_string_accurate] nan" =
  let big_percent = Big_percent.of_string_allow_nan_and_inf "NANx" in
  let roundtripped_big_percent =
    big_percent
    |> Big_percent.to_string_accurate
    |> Big_percent.of_string_allow_nan_and_inf
  in
  [%test_eq: Big_percent.Unstable.t] big_percent roundtripped_big_percent
;;

let%test_unit "roundtrip with [to_sexp] finite" =
  Quickcheck.test
    Bignum.gen_finite
    ~sexp_of:[%sexp_of: Bignum.t]
    ~trials:10_000
    ~f:(fun bignum ->
      let big_percent = Big_percent.of_mult bignum in
      let roundtripped_big_percent =
        big_percent |> Big_percent.Unstable.sexp_of_t |> Big_percent.Unstable.t_of_sexp
      in
      [%test_eq: Big_percent.Unstable.t] big_percent roundtripped_big_percent)
;;

let%test_unit "roundtrip with [to_sexp] +infinity" =
  let big_percent = Big_percent.of_string_allow_nan_and_inf "INFx" in
  let roundtripped_big_percent =
    big_percent |> Big_percent.Unstable.sexp_of_t |> Big_percent.Unstable.t_of_sexp
  in
  [%test_eq: Big_percent.t] big_percent roundtripped_big_percent
;;

let%test_unit "roundtrip with [to_sexp] -infinity" =
  let big_percent = Big_percent.of_string_allow_nan_and_inf "-INFx" in
  let roundtripped_big_percent =
    big_percent |> Big_percent.Unstable.sexp_of_t |> Big_percent.Unstable.t_of_sexp
  in
  [%test_eq: Big_percent.t] big_percent roundtripped_big_percent
;;

let%test_unit "roundtrip with [to_sexp] nan" =
  let big_percent = Big_percent.of_string_allow_nan_and_inf "NANx" in
  let roundtripped_big_percent =
    big_percent |> Big_percent.Unstable.sexp_of_t |> Big_percent.Unstable.t_of_sexp
  in
  [%test_eq: Big_percent.t] big_percent roundtripped_big_percent
;;
