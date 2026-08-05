(* scan_portable.ml 

   Used directly on platforms without [ocaml_simd], and as the sub-16-byte tail
   of the SSE2 implementation in [scan.simd.ml]. *)

open Base

module I64 = Stdlib_upstream_compatible.Int64_u
module Bits = Ocaml_intrinsics_kernel.Int64.Unboxed
module Char_u = Stdlib_stable.Char_u

let ones : int64# = I64.of_int64 0x0101010101010101L
let highs : int64# = I64.of_int64 0x8080808080808080L
let cr_rep : int64# = I64.of_int64 0x0D0D0D0D0D0D0D0DL
let sp_rep : int64# = I64.of_int64 0x2020202020202020L

(* Sets 0x80 in every zero byte of [w] (Bit Twiddling Hacks' [haszero]).

   A byte equal to 0x01 that is immediately preceded by a zero byte is also
   marked, because of the borrow out of that byte. Every caller below resolves
   the *lowest* marked byte, and such a false positive always has a genuine
   zero byte below it, so the resolved index is always a true match. *)
let[@inline always] zero_bytes (w : int64#) : int64# =
  I64.logand (I64.logand (I64.sub w ones) (I64.lognot w)) highs
;;

(* Byte index of the lowest marked byte of a non-zero [zero_bytes] result. *)
let[@inline always] lowest_marked (m : int64#) : int =
  I64.to_int (Bits.count_trailing_zeros m) lsr 3
;;

let[@inline always] word (local_ buf : bytes) p : int64# =
  I64.of_int64 (Bytes.unsafe_get_int64 buf p)
;;

let find_cr (local_ buf : bytes) ~pos ~limit =
  let mutable p = pos in
  let mutable found = -1 in
  while found < 0 && p + 8 <= limit do
    let m = zero_bytes (I64.logxor (word buf p) cr_rep) in
    if I64.equal m #0L then p <- p + 8 else found <- p + lowest_marked m
  done;
  if found >= 0
  then found
  else (
    while p < limit && Char.( <> ) (Bytes.unsafe_get buf p) '\r' do
      p <- p + 1
    done;
    p)
;;

let find_sp_or_cr (local_ buf : bytes) ~pos ~limit =
  let mutable p = pos in
  let mutable found = -1 in
  while found < 0 && p + 8 <= limit do
    let w = word buf p in
    let m =
      I64.logor (zero_bytes (I64.logxor w cr_rep)) (zero_bytes (I64.logxor w sp_rep))
    in
    if I64.equal m #0L then p <- p + 8 else found <- p + lowest_marked m
  done;
  if found >= 0
  then found
  else (
    let mutable stop = false in
    while (not stop) && p < limit do
      let c = Bytes.unsafe_get buf p in
      if Char.( = ) c ' ' || Char.( = ) c '\r' then stop <- true else p <- p + 1
    done;
    p)
;;

(* ----- Token characters (RFC 7230 tchar) -----

   Defined here rather than in {!Buf_read} so that {!Scan}'s vectorised
   [skip_token] can share this scalar tail; [Buf_read] re-exports both. *)

let[@inline always] is_token_char (c : char#) =
  match c with
  | #'a' .. #'z' | #'A' .. #'Z' | #'0' .. #'9' -> true
  | #'!' | #'#' | #'$' | #'%' | #'&' | #'\'' | #'*' | #'+' | #'-' | #'.' -> true
  | #'^' | #'_' | #'`' | #'|' | #'~' -> true
  | _ -> false
;;

(* One byte per code point, non-zero for the token characters accepted above.
   The pattern match compiles to a decision tree whose branches depend on the
   input byte and so mispredict; indexing a table instead makes {!skip_token}
   roughly 1.5x faster on typical header names. Derived from [is_token_char]
   so the two cannot drift apart. *)
let tchar_table =
  String.init 256 ~f:(fun i ->
    if is_token_char (Char_u.of_char (Char.of_int_exn i)) then '\001' else '\000')
;;

(* Index of the first byte in [pos, limit) that is not a token character, or
   [limit] if there is none. No bounds checking is performed. *)
let skip_token (local_ buf : bytes) ~pos ~limit =
  let mutable p = pos in
  while
    p < limit
    && Char.to_int (String.unsafe_get tchar_table (Char.to_int (Bytes.unsafe_get buf p)))
       <> 0
  do
    p <- p + 1
  done;
  p
;;
