(* span.ml - Unboxed spans into the parse buffer *)

open Base

module I16 = Stdlib_stable.Int16_u
module I64 = Stdlib_upstream_compatible.Int64_u
module Char_u = Stdlib_stable.Char_u
module Bits = Ocaml_intrinsics_kernel.Int64.Unboxed

let[@inline always] i16 x = I16.of_int x

(* Unboxed char helpers *)
let[@inline always] peek buf pos = Buf_read.peek buf (i16 pos)
let[@inline always] peek_str s i = Char_u.of_char (String.unsafe_get s i)
let ( =. ) = Buf_read.( =. )

(* Span with int16# fields - sufficient for 32KB max buffer. *)
type t =
  #{ off : int16#
   ; len : int16#
   }

let[@inline] make ~off:(off : int16#) ~len:(len : int16#) : t = #{ off; len }

(* Accessors - return int for compatibility with stdlib *)
let[@inline] off (sp : t) = I16.to_int sp.#off
let[@inline] len (sp : t) = I16.to_int sp.#len

(* ----- Word-at-a-time comparison -----

   Both comparisons below step eight bytes at a time while a whole word of each
   side remains, then finish byte-wise. *)

external str_word : (string[@local_opt]) -> int -> int64 @@ portable = "%caml_string_get64u"
external str_half : (string[@local_opt]) -> int -> int @@ portable = "%caml_string_get16u"

let[@inline always] sword (local_ s) i : int64# = I64.of_int64 (str_word s i)

let[@inline always] bword (local_ buf : bytes) i : int64# =
  I64.of_int64 (Bytes.unsafe_get_int64 buf i)
;;

(* Two- and four-byte reads for the tail below. Both sides are masked to the
   width being compared, so the signedness of the underlying primitives
   cannot make two equal byte sequences compare unequal. *)
let[@inline always] shalf2 (local_ s) i = str_half s i land 0xFFFF
let[@inline always] bhalf2 (local_ buf : bytes) i = Bytes.unsafe_get_int16 buf i land 0xFFFF
let[@inline always] shalf4 (local_ s) i = shalf2 s i lor (shalf2 s (i + 2) lsl 16)

let[@inline always] bhalf4 (local_ buf : bytes) i =
  bhalf2 buf i lor (bhalf2 buf (i + 2) lsl 16)
;;

(* [to_lower_word] at 32 and 16 bits; same trick, same ASCII precondition. *)
let[@inline always] to_lower32 (w : int) =
  w lor (((w + 0x3F3F3F3F) land (0xDADADADA - w) land 0x80808080) lsr 2)
;;

let[@inline always] to_lower16 (w : int) =
  w lor (((w + 0x3F3F) land (0xDADA - w) land 0x8080) lsr 2)
;;

let highs : int64# = I64.of_int64 0x8080808080808080L
let ge_bias : int64# = I64.of_int64 0x3F3F3F3F3F3F3F3FL (* 0x80 - 'A' *)
let le_bias : int64# = I64.of_int64 0xDADADADADADADADAL (* 0x80 lor 'Z' *)

(* [ascii w] holds when no byte of [w] has its high bit set. [upper_mask]
   depends on it: with a byte above 0x7F its two range tests would carry into
   the neighbouring byte and give a wrong answer. *)
let[@inline always] ascii (w : int64#) = I64.equal (I64.logand w highs) #0L

(* 0x20 in each byte of [w] that holds an ASCII uppercase letter, 0 elsewhere.
   Requires [ascii w]. *)
let[@inline always] upper_mask (w : int64#) : int64# =
  let ge_a = I64.add w ge_bias in (* high bit set iff byte >= 'A' *)
  let le_z = I64.sub le_bias w in (* high bit set iff byte <= 'Z' *)
  I64.shift_right_logical (I64.logand (I64.logand ge_a le_z) highs) 2
;;

(* ASCII-lowercase every byte of [w]. Requires [ascii w]. *)
let[@inline always] to_lower_word (w : int64#) = I64.logor w (upper_mask w)

let[@inline] equal (local_ buf : bytes) (sp : t) s =
  let slen = String.length s in
  let sp_len = len sp in
  if sp_len <> slen then false
  else (
    let sp_off = off sp in
    let mutable i = 0 in
    let mutable eq = true in
    while eq && i + 8 <= slen do
      if not (I64.equal (bword buf (sp_off + i)) (sword s i))
      then eq <- false
      else i <- i + 8
    done;
    (* Header names are typically 4-17 bytes, so the remainder after the word
       loop dominates. Step it 4 then 2 bytes at a time rather than one:
       [String.length s] is constant at every call site, so flambda2 collapses
       this ladder to a single compare of the right width. *)
    if eq && slen - i >= 4
    then if bhalf4 buf (sp_off + i) <> shalf4 s i then eq <- false else i <- i + 4;
    if eq && slen - i >= 2
    then if bhalf2 buf (sp_off + i) <> shalf2 s i then eq <- false else i <- i + 2;
    while eq && i < slen do
      if not (peek buf (sp_off + i) =. peek_str s i)
      then eq <- false
      else i <- i + 1
    done;
    eq)

(* Case-insensitive comparison. Assumes s is lowercase. *)
let[@inline] equal_caseless (local_ buf : bytes) (sp : t) s =
  let slen = String.length s in
  let sp_len = len sp in
  if sp_len <> slen then false
  else (
    let mutable i = 0 in
    let mutable eq = true in
    let mutable word_wise = true in
    let sp_off = off sp in
    (* Only the buffer side is folded, matching the byte loop below: a literal
       with an uppercase letter in it is never matched by either path. *)
    while eq && word_wise && i + 8 <= slen do
      let x = bword buf (sp_off + i) in
      if not (ascii x)
      then word_wise <- false (* non-ASCII byte: finish this word byte-wise *)
      else if not (I64.equal (to_lower_word x) (sword s i))
      then eq <- false
      else i <- i + 8
    done;
    (* Same 4/2 ladder as [equal]. The ASCII guard is repeated at each width
       and is NOT redundant: [s] is caller-supplied and may itself contain a
       byte above 0x7F, in which case the range tests in [to_lower32] and
       [to_lower16] carry into the neighbouring byte and report a false match.
       [test_scan.ml] fails loudly if this guard is dropped. *)
    if eq && word_wise && slen - i >= 4
    then (
      let x = bhalf4 buf (sp_off + i) in
      if x land 0x80808080 <> 0
      then word_wise <- false
      else if to_lower32 x <> shalf4 s i
      then eq <- false
      else i <- i + 4);
    if eq && word_wise && slen - i >= 2
    then (
      let x = bhalf2 buf (sp_off + i) in
      if x land 0x8080 <> 0
      then word_wise <- false
      else if to_lower16 x <> shalf2 s i
      then eq <- false
      else i <- i + 2);
    while eq && i < slen do
      let b1 = Char_u.code (peek buf (sp_off + i)) in
      let b2 = Char_u.code (peek_str s i) in
      let lower_b1 = if b1 >= 65 && b1 <= 90 then b1 + 32 else b1 in
      if lower_b1 <> b2 then eq <- false
      else i <- i + 1
    done;
    eq)

let[@inline] is_empty (sp : t) = I16.compare sp.#len (I16.of_int 0) = 0

(* Internal: find first occurrence of character, returns -1 if not found.

   Steps eight bytes at a time using the same "haszero" trick as
   {!Scan_portable}: XOR against a broadcast of [c] turns a match into a zero
   byte. As there, a byte whose XOR is 0x01 and which directly follows a match
   is also marked, but resolving the *lowest* marked byte always lands on a
   genuine match. *)
let ones : int64# = I64.of_int64 0x0101010101010101L

let[@inline always] zero_bytes (w : int64#) : int64# =
  I64.logand (I64.logand (I64.sub w ones) (I64.lognot w)) highs
;;

let[@inline always] lowest_marked (m : int64#) : int =
  I64.to_int (Bits.count_trailing_zeros m) lsr 3
;;

let[@inline] find_char_internal (local_ buf : bytes) (sp : t) (c : char#) : int =
  let sp_off = off sp in
  let sp_len = len sp in
  let rep = I64.mul ones (I64.of_int (Char_u.code c)) in
  let mutable i = 0 in
  let mutable found = -1 in
  while found < 0 && i + 8 <= sp_len do
    let m = zero_bytes (I64.logxor (bword buf (sp_off + i)) rep) in
    if I64.equal m #0L then i <- i + 8 else found <- i + lowest_marked m
  done;
  if found >= 0
  then found
  else (
    let mutable j = i in
    let mutable f = -1 in
    while f < 0 && j < sp_len do
      if Char_u.equal (Buf_read.peek buf (I16.of_int (sp_off + j))) c
      then f <- j
      else j <- j + 1
    done;
    f)

let[@inline] split_on_char (local_ buf : bytes) (sp : t) (c : char#) : #(t * t) =
  let pos = find_char_internal buf sp c in
  if pos < 0 then
    let empty = #{ off = I16.add sp.#off sp.#len; len = I16.of_int 0 } in
    #(sp, empty)
  else
    let before = #{ off = sp.#off; len = I16.of_int pos } in
    let after_off = I16.add sp.#off (I16.of_int (pos + 1)) in
    let after_len = I16.sub sp.#len (I16.of_int (pos + 1)) in
    let after = #{ off = after_off; len = after_len } in
    #(before, after)

let minus_one_i64 : int64# = I64.of_int64 (-1L)

let[@inline] parse_int64 (local_ buf) (sp : t) : #(int64# * bool) =
  let sp_len = len sp in
  if sp_len = 0 then #(minus_one_i64, false)
  else if sp_len > 19 then #(minus_one_i64, true)
  else (
    let mutable acc : int64# = #0L in
    let mutable i = 0 in
    let mutable valid = true in
    let mutable overflow = false in
    let sp_off = off sp in
    while valid && i < sp_len do
      let c = Buf_read.peek buf (I16.of_int (sp_off + i)) in
      match c with
      | #'0' .. #'9' ->
        let digit = I64.of_int (Char_u.code c - 48) in
        let new_acc = I64.add (I64.mul acc #10L) digit in
        if I64.compare new_acc acc < 0 then (
          overflow <- true;
          valid <- false
        ) else (
          acc <- new_acc;
          i <- i + 1
        )
      | _ -> valid <- false
    done;
    if i = 0 then #(minus_one_i64, false)
    else if overflow then #(minus_one_i64, true)
    else #(acc, false))

let to_string (local_ buf : bytes) (sp : t) : string =
  let sp_off = off sp in
  let sp_len = len sp in
  let dst = Bytes.create sp_len in
  for i = 0 to sp_len - 1 do
    Bytes.unsafe_set dst i (Bytes.unsafe_get buf (sp_off + i))
  done;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst
