(* Httpz accepts every HTTP-date form required by RFC 9110 section 5.6.7. It
   works in [float#] over a span, which is what conditional processing wants. *)

module F64 = Stdlib_upstream_compatible.Float_u
module I16 = Stdlib_stable.Int16_u

(* These bounds are the years 0001 through 9999 representable by IMF-fixdate. *)
let min_time = -62135596800.
let max_time = 253402300799.

(* [Float.compare] orders NaN below every number and infinities outside the
   bounds, so both are rejected. *)
let[@zero_alloc] representable (t : float @ local) =
  let t = F64.of_float t in
  F64.compare t (F64.of_float min_time) >= 0
  && F64.compare t (F64.of_float max_time) <= 0
;;

(* The time is unspecified when the result is [false]. *)
let[@zero_alloc] parse_imf ~has_now (now : float#) (s : string @ local)
    : #(bool * float#) =
  let #(status, t) =
    let buf = Bytes.unsafe_of_string s in
    let span =
      Httpz.Span.make ~off:(I16.of_int 0) ~len:(I16.of_int (String.length s))
    in
    Httpz.Date.parse_unboxed ~has_now now buf span
  in
  match status with
  | Httpz.Date.Valid -> #(true, t)
  | Httpz.Date.Invalid -> #(false, #0.)
;;
