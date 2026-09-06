open Base
module I64 = Stdlib_upstream_compatible.Int64_u
module Bits = Ocaml_intrinsics_kernel.Int64.Unboxed
module Char_u = Stdlib_stable.Char_u

let ones : int64# = I64.of_int64 0x0101010101010101L
let highs : int64# = I64.of_int64 0x8080808080808080L
let cr_rep : int64# = I64.of_int64 0x0D0D0D0D0D0D0D0DL
let sp_rep : int64# = I64.of_int64 0x2020202020202020L

(* Sets 0x80 in every zero byte of [w] (Bit Twiddling Hacks' [haszero]).

   A byte equal to 0x01 is also marked as a false positive when a genuine zero byte sits
   below it, because of the borrow out of that zero byte; that borrow propagates through a
   run of 0x01 bytes, so a marked byte may itself be immediately preceded by another
   marked byte rather than by the zero byte. Every caller below resolves the *lowest*
   marked byte, which is always the genuine zero byte that started the borrow, so the
   resolved index is always a true match. *)
let[@inline always] zero_bytes (w : int64#) : int64# =
  I64.logand (I64.logand (I64.sub w ones) (I64.lognot w)) highs
;;

let[@inline always] lowest_marked (m : int64#) : int =
  I64.to_int (Bits.count_trailing_zeros m) lsr 3
;;

(* This native-endian load assumes a little-endian target: [lowest_marked] resolves the
   lowest-addressed byte only when the word's byte 0 is its least significant byte. *)
let[@inline always] word (local_ (buf : bytes)) p : int64# =
  I64.of_int64 (Bytes.unsafe_get_int64 buf p)
;;

let find_cr (local_ (buf : bytes)) ~pos ~limit =
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

let find_sp_or_cr (local_ (buf : bytes)) ~pos ~limit =
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

let[@inline always] is_token_char c = Httpz_syntax.is_token_char c

(* Derive the table from [is_token_char] so the two classifiers stay aligned. *)
let tchar_table =
  String.init 256 ~f:(fun i ->
    if is_token_char (Char_u.of_char (Char.of_int_exn i)) then '\001' else '\000')
;;

let skip_token (local_ (buf : bytes)) ~pos ~limit =
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
