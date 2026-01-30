open Base

module I16 = Stdlib_stable.Int16_u
module I64 = Stdlib_upstream_compatible.Int64_u
module Char_u = Stdlib_stable.Char_u

let minus_one_i64 : int64# = I64.of_int64 (-1L)

(* Span with int16# fields - sufficient for 32KB max buffer. *)
type t =
  #{ off : int16#
   ; len : int16#
   }

let[@inline] make ~off:(off : int16#) ~len:(len : int16#) : t =
  #{ off; len }

(* Conversions *)
let[@inline] of_int x = I16.of_int x
let[@inline] to_int x = I16.to_int x

(* Accessors - return int16# to minimize conversion *)
let[@inline] off16 (sp : t) = sp.#off
let[@inline] len16 (sp : t) = sp.#len

(* Accessors - return int for compatibility *)
let[@inline] off (sp : t) = to_int sp.#off
let[@inline] len (sp : t) = to_int sp.#len

let[@inline] equal (local_ buf) (sp : t) s =
  let slen = String.length s in
  let sp_len = len sp in
  if sp_len <> slen
  then false
  else Base_bigstring.memcmp_string buf ~pos1:(off sp) s ~pos2:0 ~len:slen = 0
;;

(* Case-insensitive comparison working with int bytes directly.
   Assumes s is lowercase (all call sites use lowercase constants). *)
let[@inline] equal_caseless (local_ buf) (sp : t) s =
  let slen = String.length s in
  let sp_len = len sp in
  if sp_len <> slen
  then false
  else (
    let mutable i = 0 in
    let mutable eq = true in
    let sp_off = off sp in
    while eq && i < slen do
      let b1 = Char.to_int (Base_bigstring.unsafe_get buf (sp_off + i)) in
      let b2 = Char.to_int (String.unsafe_get s i) in
      (* Fast case-insensitive: lowercase b1 if uppercase letter, compare to b2 *)
      let lower_b1 = if b1 >= 65 && b1 <= 90 then b1 + 32 else b1 in
      if lower_b1 <> b2
      then eq <- false
      else i <- i + 1
    done;
    eq)
;;

(* Parse int64 from span - returns -1L for empty/invalid values.
   Note: This does NOT check for overflow. Use parse_int64_limited for security. *)
let[@inline] parse_int64 (local_ buf) (sp : t) : int64# =
  let sp_len = len sp in
  if sp_len = 0
  then minus_one_i64
  else (
    let mutable acc : int64# = #0L in
    let mutable i = 0 in
    let mutable valid = true in
    let sp_off = off sp in
    while valid && i < sp_len do
      let c = Buf_read.peek buf (I16.of_int (sp_off + i)) in
      match c with
      | #'0' .. #'9' ->
        let digit = I64.of_int (Char_u.code c - 48) in
        acc <- I64.add (I64.mul acc #10L) digit;
        i <- i + 1
      | _ -> valid <- false
    done;
    if i = 0 then minus_one_i64 else acc)
;;

(* Parse int64 with overflow protection and maximum value limit.
   Returns unboxed tuple: #(value, overflow_flag)
   - value: parsed value or -1L if empty/invalid
   - overflow_flag: true if value exceeds max_value or has too many digits *)
let[@inline] parse_int64_limited (local_ buf) (sp : t) ~(max_value : int64#) : #(int64# * bool) =
  let sp_len = len sp in
  if sp_len = 0 then #(minus_one_i64, false)
  else if sp_len > 19 then #(minus_one_i64, true)  (* int64 max is 19 digits *)
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
        (* Check for multiplication overflow before multiplying *)
        let new_acc = I64.add (I64.mul acc #10L) digit in
        if I64.compare new_acc acc < 0 then (
          (* Overflow occurred during multiplication *)
          overflow <- true;
          valid <- false
        ) else if I64.compare new_acc max_value > 0 then (
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
    else #(acc, false)
  )
;;

(* {1 Span Utilities} *)

(** Check if span is empty. *)
let[@inline] is_empty (sp : t) = to_int sp.#len = 0

(** Create a sub-span starting at relative offset with given length. *)
let[@inline] sub (sp : t) ~pos ~len : t =
  #{ off = I16.add sp.#off (of_int pos); len = of_int len }

(** Find first occurrence of character in span. Returns -1 if not found. *)
let[@inline] find_char (local_ buf) (sp : t) (c : char) : int =
  let sp_off = off sp in
  let sp_len = len sp in
  let mutable i = 0 in
  let mutable found = -1 in
  while found = -1 && i < sp_len do
    if Char.equal (Base_bigstring.unsafe_get buf (sp_off + i)) c
    then found <- i
    else i <- i + 1
  done;
  found
;;

(** Check if span starts with given character. *)
let[@inline] starts_with (local_ buf) (sp : t) (c : char) : bool =
  len sp > 0 && Char.equal (Base_bigstring.unsafe_get buf (off sp)) c
;;

(** Skip leading character if present, return new span. *)
let[@inline] skip_char (local_ buf) (sp : t) (c : char) : t =
  if starts_with buf sp c
  then #{ off = I16.add sp.#off (of_int 1); len = I16.sub sp.#len (of_int 1) }
  else sp
;;

(** Split span at first occurrence of character.
    Returns unboxed tuple #(before, after) where after excludes the separator.
    If not found, returns #(sp, empty_span). *)
let[@inline] split_on_char (local_ buf) (sp : t) (c : char) : #(t * t) =
  let pos = find_char buf sp c in
  if pos < 0
  then
    let empty = #{ off = I16.add sp.#off sp.#len; len = of_int 0 } in
    #(sp, empty)
  else
    let before = #{ off = sp.#off; len = of_int pos } in
    let after_off = I16.add sp.#off (of_int (pos + 1)) in
    let after_len = I16.sub sp.#len (of_int (pos + 1)) in
    let after = #{ off = after_off; len = after_len } in
    #(before, after)
;;

(** Get character at position in span. No bounds checking. *)
let[@inline] unsafe_get (local_ buf) (sp : t) (pos : int) : char =
  Base_bigstring.unsafe_get buf (off sp + pos)
;;

let to_string (local_ buf) (sp : t) = Base_bigstring.To_string.sub buf ~pos:(off sp) ~len:(len sp)
let to_bytes (local_ buf) (sp : t) = Base_bigstring.To_bytes.sub buf ~pos:(off sp) ~len:(len sp)

let pp_with_buf (local_ buf) fmt (sp : t) =
  Stdlib.Format.fprintf fmt "%s" (to_string buf sp)
;;

let pp fmt (sp : t) =
  Stdlib.Format.fprintf fmt "#{ off = %d; len = %d }" (off sp) (len sp)
;;
