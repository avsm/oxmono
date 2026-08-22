(* test_date_format.ml - differential check of IMF-fixdate formatting.

   {!Httpz.Date.format} no longer calls [Unix.gmtime]; it derives the civil
   date arithmetically and writes through [String.unsafe_get] into fixed
   tables. A wrong day-of-week, a month outside [1, 12], or an overflow in the
   era arithmetic would therefore write out of bounds rather than merely
   produce a wrong string. This checks the output against [Unix.gmtime] across
   the representable range, and checks that out-of-range and non-finite inputs
   are clamped rather than escaping. *)

open Base

module F64 = Stdlib_upstream_compatible.Float_u

let failures = ref 0

let check name cond detail =
  if not cond
  then begin
    Int.incr failures;
    if !failures <= 20 then Stdio.printf "FAIL [%s] %s\n" name (detail ())
  end
;;

let day_names = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

let month_names =
  [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
;;

(* The previous implementation, kept as the reference. *)
let ref_format (ts : float) =
  let tm = Unix.gmtime ts in
  Printf.sprintf
    "%s, %02d %s %04d %02d:%02d:%02d GMT"
    day_names.(tm.Unix.tm_wday)
    tm.Unix.tm_mday
    month_names.(tm.Unix.tm_mon)
    (tm.Unix.tm_year + 1900)
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec
;;

(* "Www, DD Mmm YYYY hh:mm:ss GMT" with a real day and month name. A table
   index that escaped its bounds would show up here as a stray byte. *)
let well_formed s =
  String.length s = 29
  && Array.mem day_names (String.sub s ~pos:0 ~len:3) ~equal:String.equal
  && Array.mem month_names (String.sub s ~pos:8 ~len:3) ~equal:String.equal
  && String.equal (String.sub s ~pos:3 ~len:2) ", "
  && Char.equal s.[7] ' '
  && Char.equal s.[11] ' '
  && Char.equal s.[16] ' '
  && Char.equal s.[19] ':'
  && Char.equal s.[22] ':'
  && String.equal (String.sub s ~pos:25 ~len:4) " GMT"
  && List.for_all [ 5; 6; 12; 13; 14; 15; 17; 18; 20; 21; 23; 24 ] ~f:(fun i ->
       Char.is_digit s.[i])
;;

let cmp ts =
  let got = Httpz.Date.format (F64.of_float ts) in
  let expect = ref_format ts in
  check "format" (String.equal got expect) (fun () ->
    Printf.sprintf "ts=%.0f got=%S expect=%S" ts got expect)
;;

(* Dates that exercise leap years, century rules and the March-shifted era. *)
let test_landmarks () =
  List.iter
    ~f:cmp
    [ 0.0 (* 1970-01-01, a Thursday *)
    ; 1.0
    ; 86_399.0 (* last second of the epoch day *)
    ; 86_400.0
    ; 951_782_400.0 (* 2000-02-29, leap year via the 400 rule *)
    ; 1_709_164_800.0 (* 2024-02-29 *)
    ; 4_107_542_400.0 (* 2100-03-01, NOT a leap year: the 100 rule *)
    ; 1_735_689_599.0 (* 2024-12-31 23:59:59 *)
    ; 1_735_689_600.0 (* 2025-01-01 00:00:00 *)
    ; 2_147_483_647.0 (* signed 32-bit boundary *)
    ; 2_147_483_648.0
    ; -1.0 (* 1969-12-31 23:59:59 *)
    ; -86_400.0
    ; -2_208_988_800.0 (* 1900-01-01 *)
    ]
;;

(* Dense sweep across the range a server realistically emits, plus a random
   sweep over the whole representable span including pre-epoch values. *)
let test_sweeps rng =
  let day = 86_400.0 in
  (* Every day for ~30 years from 2000, at a non-midnight time of day. *)
  let base = 946_684_800.0 in
  for i = 0 to 11_000 do
    cmp (base +. (Float.of_int i *. day) +. 45_296.0)
  done;
  (* Every second across two full days, to cover the time-of-day fields. *)
  for i = 0 to 172_800 do
    cmp (base +. Float.of_int i)
  done;
  (* Random, including pre-epoch, out to years 1 and 9999. *)
  for _ = 1 to 200_000 do
    let lo = -62_135_596_800.0 in
    let hi = 253_402_300_799.0 in
    cmp (Float.round_down (lo +. (Random.State.float rng (hi -. lo))))
  done
;;

(* Out-of-range and non-finite inputs must be clamped, not written out of
   bounds. [Unix.gmtime] raises on these, so there is no reference to compare
   against: the requirement is that the call returns a well-formed 29-byte
   string. *)
let test_clamped () =
  let extremes =
    [ Float.infinity
    ; Float.neg_infinity
    ; Float.nan
    ; 1e300
    ; -1e300
    ; 1e18
    ; -1e18
    ; Float.of_int Int.max_value
    ; Float.of_int Int.min_value
    ]
  in
  List.iter extremes ~f:(fun ts ->
    let got = Httpz.Date.format (F64.of_float ts) in
    check "clamp/length" (String.length got = 29) (fun () ->
      Printf.sprintf "ts=%g got=%S" ts got);
    (* The exact instant is unspecified — [int_of_float] gives 0 for the
       non-finite cases here and an unspecified value at the [int] limits, and
       the clamp maps the rest to a range endpoint. What must hold is that the
       output is structurally a well-formed IMF-fixdate: that is what shows
       the table indices stayed in bounds. *)
    check "clamp/well_formed" (well_formed got) (fun () ->
      Printf.sprintf "ts=%g got=%S" ts got))
;;

(* Normal dates must survive a format/parse round trip. *)
let test_roundtrip () =
  let base = 946_684_800.0 in
  for i = 0 to 2_000 do
    let ts = base +. (Float.of_int i *. 86_400.0) +. 3_661.0 in
    let s = Httpz.Date.format (F64.of_float ts) in
    let buf = Bytes.of_string s in
    let sp = Httpz.Span.make ~off:(Httpz.Buf_read.i16 0) ~len:(Httpz.Buf_read.i16 29) in
    let #(status, back) = Httpz.Date.parse buf sp in
    check
      "roundtrip/valid"
      (match status with
       | Httpz.Date.Valid -> true
       | Httpz.Date.Invalid -> false)
      (fun () -> Printf.sprintf "ts=%.0f s=%S" ts s);
    check "roundtrip/value" (Float.equal (F64.to_float back) ts) (fun () ->
      Printf.sprintf "ts=%.0f back=%.0f s=%S" ts (F64.to_float back) s)
  done
;;

(* The sweep above starts at the year 2000 and runs forward, which is how
   [parse] came to reject every instant before the epoch without anything
   noticing: [format] wrote those correctly and [parse] would not read them
   back, so an If-Modified-Since naming a pre-epoch date was silently dropped.
   These are the landmarks below 1970, down to the year [format] clamps at. *)
let test_roundtrip_pre_epoch () =
  let landmarks =
    [ -62_135_596_800.0 (* 0001-01-01, the clamp *)
    ; -2_208_988_800.0 (* 1900-01-01 *)
    ; -1_234_567_890.0
    ; -86_400.0 (* 1969-12-31 *)
    ; -1.0
    ; 0.0
    ]
  in
  List.iter landmarks ~f:(fun ts ->
    let s = Httpz.Date.format (F64.of_float ts) in
    let buf = Bytes.of_string s in
    let sp =
      Httpz.Span.make ~off:(Httpz.Buf_read.i16 0) ~len:(Httpz.Buf_read.i16 29)
    in
    let #(status, back) = Httpz.Date.parse buf sp in
    check
      "pre_epoch/valid"
      (match status with
       | Httpz.Date.Valid -> true
       | Httpz.Date.Invalid -> false)
      (fun () -> Printf.sprintf "ts=%.0f s=%S" ts s);
    check "pre_epoch/value" (Float.equal (F64.to_float back) ts) (fun () ->
      Printf.sprintf "ts=%.0f back=%.0f s=%S" ts (F64.to_float back) s))
;;

let () =
  test_landmarks ();
  test_sweeps (Random.State.make [| 20260805 |]);
  test_clamped ();
  test_roundtrip ();
  test_roundtrip_pre_epoch ();
  if !failures > 0
  then begin
    Stdio.printf "%d date-format failures\n" !failures;
    Stdlib.exit 1
  end;
  Stdio.printf "test_date_format: matches Unix.gmtime; extremes clamped\n"
;;
