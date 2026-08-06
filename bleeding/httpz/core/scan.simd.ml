(* scan.simd.ml - SSE2 byte-class scans, sixteen bytes at a time.

   Selected by dune when [ocaml_simd] is available; see [scan.portable.ml] for
   the fallback. Sub-16-byte remainders are handed to [Scan_portable], which
   finishes them eight bytes at a time. *)

module I64 = Stdlib_upstream_compatible.Int64_u
module V = Ocaml_simd_sse.Int8x16
module V16 = Ocaml_simd_sse.Int16x8
module Bits = Ocaml_intrinsics_kernel.Int64.Unboxed

(* [pcmpeqb] against a broadcast byte, then [pmovmskb]: bit [i] of the result
   is set iff byte [i] of the vector matched. *)
let[@inline always] match_mask (local_ buf : bytes) p (needle : int8x16#) : int =
  I64.to_int (V.movemask V.(V.Bytes.unsafe_get buf ~byte:p = needle))
;;

let[@inline always] lowest_bit (m : int) : int =
  I64.to_int (Bits.count_trailing_zeros (I64.of_int m))
;;

let find_cr (local_ buf : bytes) ~pos ~limit =
  let cr = V.const1 #13s in
  let mutable p = pos in
  let mutable found = -1 in
  while found < 0 && p + 16 <= limit do
    let m = match_mask buf p cr in
    if m = 0 then p <- p + 16 else found <- p + lowest_bit m
  done;
  if found >= 0 then found else Scan_portable.find_cr buf ~pos:p ~limit
;;

let find_sp_or_cr (local_ buf : bytes) ~pos ~limit =
  let cr = V.const1 #13s in
  let sp = V.const1 #32s in
  let mutable p = pos in
  let mutable found = -1 in
  while found < 0 && p + 16 <= limit do
    let v = V.Bytes.unsafe_get buf ~byte:p in
    let m = I64.to_int (V.movemask V.(V.(v = cr) lor V.(v = sp))) in
    if m = 0 then p <- p + 16 else found <- p + lowest_bit m
  done;
  if found >= 0 then found else Scan_portable.find_sp_or_cr buf ~pos:p ~limit
;;

(* ----- Token-character classification (Muła/Langdale nibble tables) -----

   A byte is an RFC 7230 [tchar] iff two [pshufb] lookups intersect.
   [lo_tbl], indexed by the low nibble, holds the set of high nibbles that
   form a token character with it, one bit per high nibble (bit [h] is
   [1 lsl h]). Token characters occupy only high nibbles 2..7, so the set
   fits in a byte and [hi_tbl] can select it with a single bit.

   [pshufb] zeroes any lane whose index byte has bit 7 set, so bytes >= 0x80
   are classified as non-token characters for free. *)

let lo_tbl : int8x16# =
  V.const
    #0xE8s #0xFCs #0xF8s #0xFCs #0xFCs #0xFCs #0xFCs #0xFCs
    #0xF8s #0xF8s #0xF4s #0x54s #0xD0s #0x54s #0xF4s #0x70s
;;

let hi_tbl : int8x16# =
  V.const #1s #2s #4s #8s #16s #32s #64s #128s #0s #0s #0s #0s #0s #0s #0s #0s
;;

let nibble_mask : int8x16# = V.const1 #0x0Fs
let zero_v : int8x16# = V.const1 #0s

(* The high nibble of each byte, as a lane index for [hi_tbl]. There is no
   byte-granularity shift in SSE2, so this shifts 16-bit lanes and masks the
   bits that bleed across the byte boundary. *)
let[@inline always] nibble_hi (v : int8x16#) : int8x16# =
  V.( land )
    (V.of_int16x8_bits (V16.shift_right_logical (V16.of_int8x16_bits v) #4L))
    nibble_mask
;;

(* The length test precedes anything vector-valued, so a short token — a
   3-byte method, or the tail of a header name — does not pay for
   materialising the four table constants. *)
let skip_token (local_ buf : bytes) ~pos ~limit =
  if pos + 16 > limit
  then Scan_portable.skip_token buf ~pos ~limit
  else (
    let mutable p = pos in
    let mutable found = -1 in
    while found < 0 && p + 16 <= limit do
      let v = V.Bytes.unsafe_get buf ~byte:p in
      let m =
        V.( land ) (V.shuffle ~pattern:v lo_tbl) (V.shuffle ~pattern:(nibble_hi v) hi_tbl)
      in
      (* A zero lane is a non-token character; the lowest one ends the token. *)
      let mask = I64.to_int (V.movemask V.(m = zero_v)) in
      if mask = 0 then p <- p + 16 else found <- p + lowest_bit mask
    done;
    if found >= 0 then found else Scan_portable.skip_token buf ~pos:p ~limit)
;;
