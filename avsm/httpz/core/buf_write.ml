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

(* Count digits in a positive integer *)
let[@inline] count_digits n =
  let mutable temp = n in
  let mutable digits = 0 in
  while temp > 0 do
    digits <- digits + 1;
    temp <- temp / 10
  done;
  digits
;;

let int dst ~(off : int16#) n =
  let off_int = to_int off in
  if n = 0 then (
    Bytes.unsafe_set dst off_int '0';
    add16 off (i16 1)
  ) else (
    let digits = count_digits n in
    let mutable p = off_int + digits - 1 in
    let mutable remaining = n in
    while remaining > 0 do
      Bytes.unsafe_set dst p (Char.of_int_exn (48 + Int.rem remaining 10));
      remaining <- remaining / 10;
      p <- p - 1
    done;
    add16 off (i16 digits)
  )
;;

let int64 dst ~(off : int16#) (n : int64#) =
  let off_int = to_int off in
  let n = I64.to_int64 n in
  if Int64.(n = 0L) then (
    Bytes.unsafe_set dst off_int '0';
    add16 off (i16 1)
  ) else (
    (* Count digits *)
    let mutable temp = n in
    let mutable digits = 0 in
    while Int64.(temp > 0L) do
      digits <- digits + 1;
      temp <- Int64.(temp / 10L)
    done;
    (* Write digits in reverse *)
    let mutable p = off_int + digits - 1 in
    let mutable remaining = n in
    while Int64.(remaining > 0L) do
      let digit = Int64.(remaining % 10L) |> Int64.to_int_exn in
      Bytes.unsafe_set dst p (Char.of_int_exn (48 + digit));
      remaining <- Int64.(remaining / 10L);
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
  Bytes.unsafe_set dst off_int (Char.of_int_exn (48 + n / 10));
  Bytes.unsafe_set dst (off_int + 1) (Char.of_int_exn (48 + n % 10));
  add16 off (i16 2)
;;

let[@inline] digit4 dst ~(off : int16#) n =
  let off_int = to_int off in
  Bytes.unsafe_set dst off_int (Char.of_int_exn (48 + n / 1000));
  Bytes.unsafe_set dst (off_int + 1) (Char.of_int_exn (48 + (n / 100) % 10));
  Bytes.unsafe_set dst (off_int + 2) (Char.of_int_exn (48 + (n / 10) % 10));
  Bytes.unsafe_set dst (off_int + 3) (Char.of_int_exn (48 + n % 10));
  add16 off (i16 4)
;;
