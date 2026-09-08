(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = Ptime.t

(* RFC 8620 Section 1.4: "the [RFC3339] time-secfrac MUST always be omitted if
   zero".  A non-zero fraction is legal and must survive a decode and encode
   round trip, so render it with the least number of decimal digits that
   represents it exactly.  [Ptime] has picosecond precision, hence at most 12
   digits. *)
let frac_s_digits t =
  let _d, ps = Ptime.Span.to_d_ps (Ptime.frac_s t) in
  if Int64.equal ps 0L then 0
  else
    let rec loop digits scale =
      (* [scale] is 10^(12 - digits). *)
      if digits >= 12 then 12
      else if Int64.equal (Int64.rem ps scale) 0L then digits
      else loop (digits + 1) (Int64.div scale 10L)
    in
    loop 1 100_000_000_000L

let to_string t = Ptime.to_rfc3339 ~frac_s:(frac_s_digits t) ~tz_offset_s:0 t
let to_utc_string = to_string

(* RFC 8620 Section 1.4 requires a normalised form in which any letter is
   upper case.  [~strict:true] rejects a lowercase "t" or "z", a space as the
   date and time separator, and the "hhmm" and "hh" time zone offset forms. *)
let of_string s =
  match Ptime.rfc3339_string_error (Ptime.of_rfc3339 ~strict:true s) with
  | Ok (t, _, _) -> Ok t
  | Error msg -> Error (Printf.sprintf "invalid RFC 3339 date %S: %s" s msg)

let of_utc_string s =
  match of_string s with
  | Error _ as error -> error
  | Ok t ->
      if String.ends_with ~suffix:"Z" s then Ok t
      else Error (Printf.sprintf "%S does not use the 'Z' time zone offset" s)

let jsont = Jsont.of_of_string ~kind:"Date" ~enc:to_string of_string

let utc_jsont =
  Jsont.of_of_string ~kind:"UTCDate" ~enc:to_utc_string of_utc_string
