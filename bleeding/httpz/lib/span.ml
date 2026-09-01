open Base
module I16 = Stdlib_stable.Int16_u
module I64 = Stdlib_upstream_compatible.Int64_u
module Char_u = Stdlib_stable.Char_u
module Bits = Ocaml_intrinsics_kernel.Int64.Unboxed

let[@inline always] i16 x = I16.of_int x
let[@inline always] peek buf pos = Buf_read.peek buf (i16 pos)
let[@inline always] peek_str s i = Char_u.of_char (String.unsafe_get s i)
let ( =. ) = Buf_read.( =. )

type t =
  #{ off : int16#
   ; len : int16#
   }

let[@inline] make ~(off : int16#) ~(len : int16#) : t = #{ off; len }
let[@inline] off (sp : t) = I16.to_int sp.#off
let[@inline] len (sp : t) = I16.to_int sp.#len

(* Comparisons process complete machine words before their byte-wise tail. *)

external str_word
  :  (string[@local_opt])
  -> int
  -> int64
  @@ portable
  = "%caml_string_get64u"

external str_half : (string[@local_opt]) -> int -> int @@ portable = "%caml_string_get16u"

let[@inline always] sword (local_ s) i : int64# = I64.of_int64 (str_word s i)

let[@inline always] bword (local_ (buf : bytes)) i : int64# =
  I64.of_int64 (Bytes.unsafe_get_int64 buf i)
;;

(* Mask both sides to prevent signed primitive results affecting equality. *)
let[@inline always] shalf2 (local_ s) i = str_half s i land 0xFFFF

let[@inline always] bhalf2 (local_ (buf : bytes)) i =
  Bytes.unsafe_get_int16 buf i land 0xFFFF
;;

let[@inline always] shalf4 (local_ s) i = shalf2 s i lor (shalf2 s (i + 2) lsl 16)

let[@inline always] bhalf4 (local_ (buf : bytes)) i =
  bhalf2 buf i lor (bhalf2 buf (i + 2) lsl 16)
;;

let[@inline always] to_lower32 (w : int) =
  w lor (((w + 0x3F3F3F3F) land (0xDADADADA - w) land 0x80808080) lsr 2)
;;

let[@inline always] to_lower16 (w : int) =
  w lor (((w + 0x3F3F) land (0xDADA - w) land 0x8080) lsr 2)
;;

let highs : int64# = I64.of_int64 0x8080808080808080L
let ge_bias : int64# = I64.of_int64 0x3F3F3F3F3F3F3F3FL (* 0x80 - 'A' *)
let le_bias : int64# = I64.of_int64 0xDADADADADADADADAL (* 0x80 lor 'Z' *)

(* [ascii w] holds when no byte of [w] has its high bit set. [upper_mask] depends on it:
   with a byte above 0x7F its two range tests would carry into the neighbouring byte and
   give a wrong answer. *)
let[@inline always] ascii (w : int64#) = I64.equal (I64.logand w highs) #0L

(* 0x20 in each byte of [w] that holds an ASCII uppercase letter, 0 elsewhere. Requires
   [ascii w]. *)
let[@inline always] upper_mask (w : int64#) : int64# =
  let ge_a = I64.add w ge_bias in
  (* high bit set iff byte >= 'A' *)
  let le_z = I64.sub le_bias w in
  (* high bit set iff byte <= 'Z' *)
  I64.shift_right_logical (I64.logand (I64.logand ge_a le_z) highs) 2
;;

(* ASCII-lowercase every byte of [w]. Requires [ascii w]. *)
let[@inline always] to_lower_word (w : int64#) = I64.logor w (upper_mask w)

let[@inline] equal (local_ (buf : bytes)) (sp : t) s =
  let slen = String.length s in
  let sp_len = len sp in
  if sp_len <> slen
  then false
  else (
    let sp_off = off sp in
    let mutable i = 0 in
    let mutable eq = true in
    while eq && i + 8 <= slen do
      if not (I64.equal (bword buf (sp_off + i)) (sword s i))
      then eq <- false
      else i <- i + 8
    done;
    if eq && slen - i >= 4
    then if bhalf4 buf (sp_off + i) <> shalf4 s i then eq <- false else i <- i + 4;
    if eq && slen - i >= 2
    then if bhalf2 buf (sp_off + i) <> shalf2 s i then eq <- false else i <- i + 2;
    while eq && i < slen do
      if not (peek buf (sp_off + i) =. peek_str s i) then eq <- false else i <- i + 1
    done;
    eq)
;;

(* Case-insensitive comparison. Assumes s is lowercase. *)
let[@inline] equal_caseless (local_ (buf : bytes)) (sp : t) s =
  let slen = String.length s in
  let sp_len = len sp in
  if sp_len <> slen
  then false
  else (
    let mutable i = 0 in
    let mutable eq = true in
    let mutable word_wise = true in
    let sp_off = off sp in
    (* Only the buffer side is folded, matching the byte loop below: a literal with an
       uppercase letter in it is never matched by either path. *)
    while eq && word_wise && i + 8 <= slen do
      let x = bword buf (sp_off + i) in
      if not (ascii x)
      then word_wise <- false
      else if not (I64.equal (to_lower_word x) (sword s i))
      then eq <- false
      else i <- i + 8
    done;
    (* The ASCII guards prevent range-test carries between adjacent bytes. *)
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
      if lower_b1 <> b2 then eq <- false else i <- i + 1
    done;
    eq)
;;

let[@inline] is_empty (sp : t) = I16.compare sp.#len (I16.of_int 0) = 0

(* XOR makes matching bytes zero. The has-zero trick can also mark the byte after a match,
   but the lowest marked byte is always a genuine match. *)
let ones : int64# = I64.of_int64 0x0101010101010101L

let[@inline always] zero_bytes (w : int64#) : int64# =
  I64.logand (I64.logand (I64.sub w ones) (I64.lognot w)) highs
;;

let[@inline always] lowest_marked (m : int64#) : int =
  I64.to_int (Bits.count_trailing_zeros m) lsr 3
;;

let[@inline] find_char_internal (local_ (buf : bytes)) (sp : t) (c : char#) : int =
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
;;

let[@inline] split_on_char (local_ (buf : bytes)) (sp : t) (c : char#) : #(t * t) =
  let pos = find_char_internal buf sp c in
  if pos < 0
  then (
    let empty = #{ off = I16.add sp.#off sp.#len; len = I16.of_int 0 } in
    #(sp, empty))
  else (
    let before = #{ off = sp.#off; len = I16.of_int pos } in
    let after_off = I16.add sp.#off (I16.of_int (pos + 1)) in
    let after_len = I16.sub sp.#len (I16.of_int (pos + 1)) in
    let after = #{ off = after_off; len = after_len } in
    #(before, after))
;;

let minus_one_i64 : int64# = I64.of_int64 (-1L)
let max_int64_div_10 : int64# = I64.of_int64 922337203685477580L
let max_int64_last_digit = 7

(* RFC 9110 section 8.6 permits a combined field containing repeated, identical decimal
   values. Return the value, whether it overflowed [int64], and whether two valid members
   disagreed. Any other malformed input returns [-1L] with both flags clear. *)
let[@inline] parse_content_length (local_ (buf : bytes)) (sp : t)
  : #(int64# * bool * bool)
  =
  let mutable first : int64# = minus_one_i64 in
  let mutable pos = off sp in
  let stop = pos + len sp in
  let mutable invalid = false in
  let mutable overflow = false in
  let mutable conflicting = false in
  let mutable members = 0 in
  while (not invalid) && (not overflow) && (not conflicting) && pos < stop do
    while pos < stop && Buf_read.is_space (Buf_read.peek buf (I16.of_int pos)) do
      pos <- pos + 1
    done;
    let mutable acc : int64# = #0L in
    let mutable digits = 0 in
    let mutable reading_digits = true in
    while reading_digits && pos < stop do
      match Buf_read.peek buf (I16.of_int pos) with
      | #'0' .. #'9' as c ->
        let digit_int = Char_u.code c - 48 in
        if I64.compare acc max_int64_div_10 > 0
           || (I64.equal acc max_int64_div_10 && digit_int > max_int64_last_digit)
        then (
          overflow <- true;
          reading_digits <- false)
        else (
          acc <- I64.add (I64.mul acc #10L) (I64.of_int digit_int);
          digits <- digits + 1;
          pos <- pos + 1)
      | _ -> reading_digits <- false
    done;
    if digits = 0
    then invalid <- true
    else if not overflow
    then (
      while pos < stop && Buf_read.is_space (Buf_read.peek buf (I16.of_int pos)) do
        pos <- pos + 1
      done;
      members <- members + 1;
      if members = 1
      then first <- acc
      else if not (I64.equal first acc)
      then conflicting <- true;
      if (not conflicting) && pos < stop
      then
        if Buf_read.( =. ) (Buf_read.peek buf (I16.of_int pos)) #','
        then (
          pos <- pos + 1;
          if pos = stop then invalid <- true)
        else invalid <- true)
  done;
  if overflow
  then #(minus_one_i64, true, false)
  else if conflicting
  then #(minus_one_i64, false, true)
  else if invalid || members = 0
  then #(minus_one_i64, false, false)
  else #(first, false, false)
;;

let[@inline] equal_caseless_range (local_ buf) start stop literal =
  let n = stop - start in
  if n <> String.length literal
  then false
  else (
    let mutable i = 0 in
    let mutable same = true in
    while same && i < n do
      let c = Buf_read.to_lower (Buf_read.peek buf (I16.of_int (start + i))) in
      if not (Char_u.equal c (Char_u.of_char (String.unsafe_get literal i)))
      then same <- false
      else i <- i + 1
    done;
    same)
;;

(* Both list walks below need the same three steps for one member, and each step reports a
   single offset so that no pair is built per member. *)
let[@inline] member_end (local_ (buf : bytes)) ~pos ~stop =
  let mutable p = pos in
  while p < stop && Buf_read.( <>. ) (Buf_read.peek buf (I16.of_int p)) #',' do
    p <- p + 1
  done;
  p
;;

(* {!Buf_read.skip_ows} works in [int16#] while these walks index in [int]. *)
let[@inline] skip_ows (local_ (buf : bytes)) ~pos ~stop =
  I16.to_int (Buf_read.skip_ows buf ~pos:(I16.of_int pos) ~len:(I16.of_int stop))
;;

let[@inline] trim_ows_end (local_ (buf : bytes)) ~start ~stop =
  let mutable p = stop in
  while p > start && Buf_read.is_space (Buf_read.peek buf (I16.of_int (p - 1))) do
    p <- p - 1
  done;
  p
;;

(* Parse the HTTP comma-list shape without allocating its members. Empty list elements are
   ignored as RFC 9110 section 5.6.1 permits. *)
let[@inline] token_list_last_is (local_ (buf : bytes)) (sp : t) literal =
  let mutable pos = off sp in
  let stop = pos + len sp in
  let mutable count = 0 in
  let mutable last_equal = false in
  while pos < stop do
    let item_end = member_end buf ~pos ~stop in
    let left = skip_ows buf ~pos ~stop:item_end in
    let right = trim_ows_end buf ~start:left ~stop:item_end in
    if right > left
    then (
      count <- count + 1;
      last_equal <- equal_caseless_range buf left right literal);
    pos <- (if item_end < stop then item_end + 1 else item_end)
  done;
  #(count, last_equal)
;;

let[@inline] token_list_all_are (local_ (buf : bytes)) (sp : t) literal =
  let mutable pos = off sp in
  let stop = pos + len sp in
  let mutable count = 0 in
  let mutable all_equal = true in
  while pos < stop do
    let item_end = member_end buf ~pos ~stop in
    let left = skip_ows buf ~pos ~stop:item_end in
    let right = trim_ows_end buf ~start:left ~stop:item_end in
    if right > left
    then (
      count <- count + 1;
      if not (equal_caseless_range buf left right literal)
      then all_equal <- false);
    pos <- (if item_end < stop then item_end + 1 else item_end)
  done;
  #(count, all_equal)
;;

let[@inline] token_list_contains (local_ (buf : bytes)) (sp : t) literal =
  let mutable pos = off sp in
  let stop = pos + len sp in
  let mutable found = false in
  while (not found) && pos < stop do
    let item_end = member_end buf ~pos ~stop in
    let left = skip_ows buf ~pos ~stop:item_end in
    let right = trim_ows_end buf ~start:left ~stop:item_end in
    found <- equal_caseless_range buf left right literal;
    pos <- (if item_end < stop then item_end + 1 else item_end)
  done;
  found
;;

let[@zero_alloc] token_list_valid (local_ (buf : bytes)) (sp : t) =
  let mutable pos = off sp in
  let stop = pos + len sp in
  let mutable valid = true in
  while valid && pos < stop do
    let item_end = member_end buf ~pos ~stop in
    let left = skip_ows buf ~pos ~stop:item_end in
    let right = trim_ows_end buf ~start:left ~stop:item_end in
    if right > left
    then valid <- Buf_read.skip_token buf ~pos:left ~limit:right = right;
    pos <- (if item_end < stop then item_end + 1 else item_end)
  done;
  valid
;;

(* Parse the Transfer-Encoding list grammar and report its framing-relevant facts:
   non-empty member count, chunked member count, whether chunked is the final member, and
   whether every non-empty member is syntactically valid. Empty list members are ignored
   as RFC 9110 section 5.6.1 requires. *)
let parse_transfer_encoding (local_ (buf : bytes)) (sp : t) =
  let stop = off sp + len sp in
  let mutable pos = off sp in
  let mutable count = 0 in
  let mutable chunked_count = 0 in
  let mutable last_chunked = false in
  let mutable valid = true in
  let skip_ows p = skip_ows buf ~pos:p ~stop in
  let token_end start =
    let mutable p = start in
    while p < stop && Buf_read.is_token_char (Buf_read.peek buf (I16.of_int p)) do
      p <- p + 1
    done;
    p
  in
  let quoted_string p =
    let mutable p = p + 1 in
    let mutable closed = false in
    let mutable ok = true in
    while ok && (not closed) && p < stop do
      let c = Buf_read.peek buf (I16.of_int p) in
      if Buf_read.(c =. #'"')
      then (
        closed <- true;
        p <- p + 1)
      else if Buf_read.(c =. #'\\')
      then
        if p + 1 >= stop
        then ok <- false
        else (
          let escaped = Buf_read.peek buf (I16.of_int (p + 1)) in
          if Buf_read.is_quoted_pair_char escaped
          then p <- p + 2
          else ok <- false)
      else if Buf_read.is_qdtext_char c
      then p <- p + 1
      else ok <- false
    done;
    #(p, closed && ok)
  in
  while valid && pos < stop do
    pos <- skip_ows pos;
    if pos < stop
    then
      if Buf_read.( =. ) (Buf_read.peek buf (I16.of_int pos)) #','
      then pos <- pos + 1
      else (
        let coding_end = token_end pos in
        if coding_end = pos
        then valid <- false
        else (
          let is_chunked = equal_caseless_range buf pos coding_end "chunked" in
          count <- count + 1;
          if is_chunked then chunked_count <- chunked_count + 1;
          last_chunked <- is_chunked;
          pos <- skip_ows coding_end;
          while
            valid
            && (not is_chunked)
            && pos < stop
            && Buf_read.( =. ) (Buf_read.peek buf (I16.of_int pos)) #';'
          do
            pos <- skip_ows (pos + 1);
            let name_end = token_end pos in
            if name_end = pos
            then valid <- false
            else (
              pos <- skip_ows name_end;
              if pos >= stop || Buf_read.( <>. ) (Buf_read.peek buf (I16.of_int pos)) #'='
              then valid <- false
              else (
                pos <- skip_ows (pos + 1);
                if pos >= stop
                then valid <- false
                else if Buf_read.( =. ) (Buf_read.peek buf (I16.of_int pos)) #'"'
                then (
                  let #(next, ok) = quoted_string pos in
                  pos <- next;
                  valid <- ok)
                else (
                  let value_end = token_end pos in
                  if value_end = pos then valid <- false else pos <- value_end));
              pos <- skip_ows pos)
          done;
          if valid && pos < stop
          then
            if Buf_read.( =. ) (Buf_read.peek buf (I16.of_int pos)) #','
            then pos <- pos + 1
            else valid <- false))
  done;
  #(count, chunked_count, last_chunked, valid)
;;

let to_string (local_ (buf : bytes)) (sp : t) : string =
  let sp_off = off sp in
  let sp_len = len sp in
  let dst = Bytes.create sp_len in
  for i = 0 to sp_len - 1 do
    Bytes.unsafe_set dst i (Bytes.unsafe_get buf (sp_off + i))
  done;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst
;;

(* The stub allocates in the caller's region, never on the heap, and cannot
   raise once the length is checked here, so it is declared [noalloc] as Base
   declares its own. *)
external create_local : int -> bytes @ local @@ portable
  = "caml_create_local_bytes"
[@@noalloc]

let[@zero_alloc] to_string_local (buf @ local) (t : t) = exclave_
  let len = I16.to_int t.#len in
  if len > Stdlib.Sys.max_string_length then invalid_arg "Span.to_string_local";
  let b = create_local len in
  Stdlib.Bytes.unsafe_blit buf (I16.to_int t.#off) b 0 len;
  Stdlib.Bytes.unsafe_to_string b
;;
