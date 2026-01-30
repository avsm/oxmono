open! Core

let make ~f float = Bignum.of_float_decimal float |> f
let of_mult = make ~f:Big_percent.of_mult
let of_percentage = make ~f:Big_percent.of_percentage
let of_bp = make ~f:Big_percent.of_bp
