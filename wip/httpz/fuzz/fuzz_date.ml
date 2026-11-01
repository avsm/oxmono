(* Crowbar fuzz testing for Httpz.Date.parse (which dispatches internally to
   the IMF-fixdate, RFC 850, and asctime forms depending on the span's
   length and shape): random bytes at random in-bounds spans must never
   raise (see A2 in the security audit, about parse_rfc850's length guard).
   To run: dune exec httpz/fuzz/fuzz_date.exe
   With AFL: afl-fuzz -i fuzz/corpus -o fuzz/findings -- ./_build/default/httpz/fuzz/fuzz_date.exe @@ *)
open Crowbar

(* A buffer together with an in-bounds span [off, off + len), so a crash can
   only be attributed to the date parser reading past its own span, never to
   reading past memory it was never given. *)
let windowed_bytes =
  dynamic_bind bytes (fun s ->
    let n = String.length s in
    map [ range (n + 1); range (n + 1) ] (fun a b ->
      let off = min a b
      and len = max a b in
      s, off, len))
;;

(* Truncations of real dates in all three accepted forms, at every length
   from 0 to just past the full string, to specifically target the length
   guards each form's parser must apply before peeking ahead. *)
let imf_fixdate = "Sun, 06 Nov 1994 08:49:37 GMT"
let rfc850_date = "Sunday, 06-Nov-94 08:49:37 GMT"
let asctime_date = "Sun Nov  6 08:49:37 1994"

let truncation_gen full =
  map [ range (String.length full + 2) ] (fun n -> String.sub full 0 (min n (String.length full)))
;;

let truncated_date_gen =
  choose [ truncation_gen imf_fixdate; truncation_gen rfc850_date; truncation_gen asctime_date ]
;;

let now_gen = option (map [ range 4_000_000_000 ] float_of_int)

let i16 = Httpz.Buf_read.i16

let test_span now (s, off, len) =
  let buf = Bytes.of_string s in
  let span = Httpz.Span.make ~off:(i16 off) ~len:(i16 len) in
  try
    let #(_status, _ts) = Httpz.Date.parse ?now buf span in
    check true
  with
  | e -> failf "Httpz.Date.parse raised: %s" (Printexc.to_string e)
;;

let test_truncated now s =
  let buf = Bytes.of_string s in
  let span = Httpz.Span.make ~off:(i16 0) ~len:(i16 (String.length s)) in
  try
    let #(_status, _ts) = Httpz.Date.parse ?now buf span in
    check true
  with
  | e -> failf "Httpz.Date.parse raised on truncated date %S: %s" s (Printexc.to_string e)
;;

let () =
  add_test
    ~name:"Httpz.Date.parse: random bytes at random spans never raise"
    [ now_gen; windowed_bytes ]
    test_span;
  add_test
    ~name:"Httpz.Date.parse: truncated real dates never raise"
    [ now_gen; truncated_date_gen ]
    test_truncated
;;
