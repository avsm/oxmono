(* HTTP dates are httpz's. It formats an IMF-fixdate and parses all three
   forms RFC 9110 requires a recipient to accept, so a server layer that kept
   its own copy would be a second implementation of the same grammar, and the
   narrower one: this module used to accept IMF-fixdate alone.

   httpz works in [float#] over a span, because that is what a parser wants.
   The two wrappers here are the whole of the difference: a described response
   carries a boxed [float], and a request's If-Modified-Since has already been
   copied out of the read buffer by the time proffer sees it. *)

module F64 = Stdlib_upstream_compatible.Float_u
module I16 = Stdlib_stable.Int16_u

(* The ends of the range httpz can spell. The upper one is what a fixed
   29-byte IMF-fixdate holds. The lower one is 0001-01-01 rather than the
   proleptic 0000-01-01: httpz clamps below year 1, as most date libraries do,
   and a [representable] that promised more than it could spell would let
   [Resp.v] emit a field naming the wrong year. *)
let min_time = -62135596800.
let max_time = 253402300799.
let representable t = Float.is_finite t && t >= min_time && t <= max_time
let to_imf t = Httpz.Date.format (F64.of_float t)

let of_imf s =
  let #(status, t) =
    Httpz.Date.parse
      (Bytes.unsafe_of_string s)
      (Httpz.Span.make ~off:(I16.of_int 0) ~len:(I16.of_int (String.length s)))
  in
  match status with
  | Httpz.Date.Valid -> Some (F64.to_float t)
  | Httpz.Date.Invalid -> None
