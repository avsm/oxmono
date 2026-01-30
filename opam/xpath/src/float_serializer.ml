open! Core

(* NaN is converted to the string NaN

   positive zero is converted to the string 0

   negative zero is converted to the string 0

   positive infinity is converted to the string Infinity

   negative infinity is converted to the string -Infinity

   if the number is an integer, the number is represented in decimal form as a Number with
   no decimal point and no leading zeros, preceded by a minus sign (-) if the number is
   negative

   otherwise, the number is represented in decimal form as a Number including a decimal
   point with at least one digit before the decimal point and at least one digit after the
   decimal point, preceded by a minus sign (-) if the number is negative; there must be no
   leading zeros before the decimal point apart possibly from the one required digit
   immediately before the decimal point; beyond the one required digit after the decimal
   point there must be as many, but only as many, more digits as are needed to uniquely
   distinguish the number from all other IEEE 754 numeric values.
*)
let serialize f =
  match Float.classify f with
  | Infinite ->
    if Float.Replace_polymorphic_compare.(f > 0.) then "Infinity" else "-Infinity"
  | Nan -> "NaN"
  | Zero -> "0"
  | Normal | Subnormal ->
    let stringified = Printf.sprintf "%f" f in
    let rec extra_zeros pos =
      match stringified.[pos] with
      | '0' -> extra_zeros (pos - 1)
      | '.' -> pos
      | _ -> pos + 1
    in
    String.prefix stringified (extra_zeros (String.length stringified - 1))
;;
