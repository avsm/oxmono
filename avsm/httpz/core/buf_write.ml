(* buf_write.ml - Low-level buffer writing primitives for HTTP response generation *)

open Base

module I16 = Stdlib_stable.Int16_u
module I64 = Stdlib_upstream_compatible.Int64_u
module Char_u = Stdlib_stable.Char_u

(* int16# conversion helpers - exported for callers *)
let[@inline always] i16 x = I16.of_int x
let[@inline always] to_int x = I16.to_int x
let[@inline always] add16 a b = I16.add a b

let[@inline] char dst ~(off : int16#) c =
  Bytes.unsafe_set dst (to_int off) c;
  add16 off (i16 1)
;;

let[@inline] char_u dst ~(off : int16#) (c : char#) =
  Bytes.unsafe_set dst (to_int off) (Char_u.to_char c);
  add16 off (i16 1)
;;

let[@inline] string dst ~(off : int16#) (local_ s) =
  let len = String.length s in
  let off_int = to_int off in
  Bytes.From_string.unsafe_blit ~src:s ~src_pos:0 ~dst ~dst_pos:off_int ~len;
  add16 off (i16 len)
;;

(* CRLF as int16: little-endian 0x0A0D = '\n' << 8 | '\r' *)
let crlf_int16 = 0x0A0D

let[@inline] crlf dst ~(off : int16#) =
  Bytes.unsafe_set_int16 dst (to_int off) crlf_int16;
  add16 off (i16 2)
;;

(* Digit count by comparison rather than a division loop. Content-Length and
   header integers are almost always under 10^8, so the common cases cost at
   most three compares. *)
let[@inline] count_digits n =
  if n < 10
  then 1
  else if n < 10_000
  then if n < 100 then 2 else if n < 1_000 then 3 else 4
  else if n < 100_000_000
  then
    if n < 100_000
    then 5
    else if n < 1_000_000
    then 6
    else if n < 10_000_000
    then 7
    else 8
  else (
    let mutable temp = n / 100_000_000 in
    let mutable digits = 8 in
    while temp > 0 do
      digits <- digits + 1;
      temp <- temp / 10
    done;
    digits)
;;

(* "00", "01", ... "99", so two digits are emitted per division. *)
let digit_pairs =
  String.init 200 ~f:(fun i ->
    if i % 2 = 0
    then Stdlib.Char.unsafe_chr (48 + (i / 2 / 10))
    else Stdlib.Char.unsafe_chr (48 + (i / 2 % 10)))
;;

(* [n] must be non-negative, as documented in the interface. *)
let[@inline] int dst ~(off : int16#) n =
  let off_int = to_int off in
  if n = 0
  then (
    Bytes.unsafe_set dst off_int '0';
    add16 off (i16 1))
  else (
    let digits = count_digits n in
    let mutable p = off_int + digits in
    let mutable r = n in
    while r >= 100 do
      let q = r / 100 in
      let idx = (r - (q * 100)) * 2 in
      p <- p - 2;
      Bytes.unsafe_set dst p (String.unsafe_get digit_pairs idx);
      Bytes.unsafe_set dst (p + 1) (String.unsafe_get digit_pairs (idx + 1));
      r <- q
    done;
    if r >= 10
    then (
      p <- p - 2;
      Bytes.unsafe_set dst p (String.unsafe_get digit_pairs (r * 2));
      Bytes.unsafe_set dst (p + 1) (String.unsafe_get digit_pairs ((r * 2) + 1)))
    else (
      p <- p - 1;
      Bytes.unsafe_set dst p (Stdlib.Char.unsafe_chr (48 + r)));
    add16 off (i16 digits))
;;

(* Stays in [int64#] throughout: routing this through boxed [Int64] costs two
   24-byte boxes per digit, via Base's [%] and [/]. *)
let[@zero_alloc] int64 dst ~(off : int16#) (n : int64#) =
  let off_int = to_int off in
  if I64.equal n #0L then (
    Bytes.unsafe_set dst off_int '0';
    add16 off (i16 1)
  ) else (
    (* Count digits *)
    let mutable temp = n in
    let mutable digits = 0 in
    while I64.compare temp #0L > 0 do
      digits <- digits + 1;
      temp <- I64.div temp #10L
    done;
    (* Write digits in reverse *)
    let mutable p = off_int + digits - 1 in
    let mutable remaining = n in
    while I64.compare remaining #0L > 0 do
      let q = I64.div remaining #10L in
      let digit = I64.to_int (I64.sub remaining (I64.mul q #10L)) in
      Bytes.unsafe_set dst p (Stdlib.Char.unsafe_chr (48 + digit));
      remaining <- q;
      p <- p - 1
    done;
    add16 off (i16 digits)
  )
;;

let hex_chars = "0123456789abcdef"

let hex dst ~(off : int16#) n =
  let off_int = to_int off in
  if n = 0 then (
    Bytes.unsafe_set dst off_int '0';
    add16 off (i16 1)
  ) else (
    let mutable temp = n in
    let mutable digits = 0 in
    while temp > 0 do
      digits <- digits + 1;
      temp <- temp lsr 4
    done;
    let mutable p = off_int + digits - 1 in
    let mutable remaining = n in
    while remaining > 0 do
      Bytes.unsafe_set dst p (String.unsafe_get hex_chars (remaining land 0xf));
      remaining <- remaining lsr 4;
      p <- p - 1
    done;
    add16 off (i16 digits)
  )
;;

let[@inline] digit2 dst ~(off : int16#) n =
  let off_int = to_int off in
  Bytes.unsafe_set dst off_int (Stdlib.Char.unsafe_chr (48 + n / 10));
  Bytes.unsafe_set dst (off_int + 1) (Stdlib.Char.unsafe_chr (48 + n % 10));
  add16 off (i16 2)
;;

let[@inline] digit4 dst ~(off : int16#) n =
  let off_int = to_int off in
  Bytes.unsafe_set dst off_int (Stdlib.Char.unsafe_chr (48 + n / 1000));
  Bytes.unsafe_set dst (off_int + 1) (Stdlib.Char.unsafe_chr (48 + (n / 100) % 10));
  Bytes.unsafe_set dst (off_int + 2) (Stdlib.Char.unsafe_chr (48 + (n / 10) % 10));
  Bytes.unsafe_set dst (off_int + 3) (Stdlib.Char.unsafe_chr (48 + n % 10));
  add16 off (i16 4)
;;
