(* span.ml - Unboxed spans into the parse buffer *)

open Base

module I16 = Stdlib_stable.Int16_u
module I64 = Stdlib_upstream_compatible.Int64_u
module Char_u = Stdlib_stable.Char_u

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

let[@inline] equal (local_ buf : bytes) (sp : t) s =
  let slen = String.length s in
  let sp_len = len sp in
  if sp_len <> slen then false
  else (
    let sp_off = off sp in
    let mutable i = 0 in
    let mutable eq = true in
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
    let sp_off = off sp in
    while eq && i < slen do
      let b1 = Char_u.code (peek buf (sp_off + i)) in
      let b2 = Char_u.code (peek_str s i) in
      let lower_b1 = if b1 >= 65 && b1 <= 90 then b1 + 32 else b1 in
      if lower_b1 <> b2 then eq <- false
      else i <- i + 1
    done;
    eq)

let[@inline] is_empty (sp : t) = I16.compare sp.#len (I16.of_int 0) = 0

(* Internal: find first occurrence of character, returns -1 if not found *)
let[@inline] find_char_internal (local_ buf : bytes) (sp : t) (c : char#) : int =
  let sp_off = off sp in
  let sp_len = len sp in
  let mutable i = 0 in
  let mutable found = -1 in
  while found = -1 && i < sp_len do
    if Char_u.equal (Buf_read.peek buf (I16.of_int (sp_off + i))) c
    then found <- i
    else i <- i + 1
  done;
  found

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
