(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Make (B : sig
  val kind : string
  val min_value : int64
end) =
struct
  type t = int64

  let min_value = B.min_value
  let max_value = 9007199254740991L

  let of_int64 n =
    if n >= min_value && n <= max_value then Ok n
    else
      Error
        (Printf.sprintf "%s out of range [%Ld, %Ld]: %Ld" B.kind min_value
           max_value n)

  let of_int n = of_int64 (Int64.of_int n)

  let to_int n =
    if n >= Int64.of_int min_int && n <= Int64.of_int max_int then
      Some (Int64.to_int n)
    else None

  let jsont =
    let kind = B.kind in
    (* RFC 8620 Section 1.3: an "Int" is an integer in the range an IEEE 754
       double represents exactly.  Non-integral numbers, NaN (which is what a
       JSON null decodes to here) and out of range values are rejected before
       the conversion, since [Int64.of_float] is unspecified on NaN and
       outside the [int64] range. *)
    let dec meta f =
      if not (Float.is_integer f) then
        Jsont.Error.msgf meta "%s: %.17g is not an integer" kind f
      else if f < Int64.to_float min_value || f > Int64.to_float max_value then
        Jsont.Error.msgf meta "%s: value %.17g out of range [%Ld, %Ld]" kind f
          min_value max_value
      else Int64.of_float f
    in
    let enc n =
      match of_int64 n with
      | Ok n -> Int64.to_float n
      | Error msg -> Jsont.Error.msg Jsont.Meta.none msg
    in
    Jsont.Base.number (Jsont.Base.map ~kind ~dec ~enc ())
end

module Signed = Make (struct
  let kind = "Int53"
  let min_value = -9007199254740991L
end)

module Unsigned = Make (struct
  let kind = "UnsignedInt53"
  let min_value = 0L
end)
