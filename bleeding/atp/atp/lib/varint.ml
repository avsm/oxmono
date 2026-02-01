(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type error = [ `Varint_overflow | `Varint_unterminated | `Varint_negative ]

let pp_error ppf = function
  | `Varint_overflow -> Fmt.string ppf "varint overflow"
  | `Varint_unterminated -> Fmt.string ppf "unterminated varint"
  | `Varint_negative -> Fmt.string ppf "negative varint"

type Eio.Exn.err += E of error

let () = Eio_error.register_pp pp_error (function E e -> Some e | _ -> None)
let raise_error e = Eio_error.raise_ (E e)

(* LEB128 constants *)
let msb = 0x80
let mask = 0x7F

let encoded_length n =
  if n < 0 then raise_error `Varint_negative
  else if n < 0x80 then 1
  else if n < 0x4000 then 2
  else if n < 0x200000 then 3
  else if n < 0x10000000 then 4
  else if n < 0x800000000 then 5
  else if n < 0x40000000000 then 6
  else if n < 0x2000000000000 then 7
  else if n < 0x100000000000000 then 8
  else 9

let encode_to_buffer buf n =
  if n < 0 then raise_error `Varint_negative;
  let rec loop n =
    let byte = n land mask in
    let rest = n lsr 7 in
    if rest = 0 then Buffer.add_char buf (Char.chr byte)
    else begin
      Buffer.add_char buf (Char.chr (byte lor msb));
      loop rest
    end
  in
  loop n

let encode n =
  let buf = Buffer.create (encoded_length n) in
  encode_to_buffer buf n;
  Buffer.contents buf

let decode_string s off =
  let len = String.length s in
  let rec loop acc shift i =
    if i >= len then raise_error `Varint_unterminated;
    let byte = Char.code (String.unsafe_get s i) in
    let value = byte land mask in
    (* Check for overflow before shifting *)
    if shift >= 63 && value > 1 then raise_error `Varint_overflow;
    let acc = acc lor (value lsl shift) in
    if byte land msb = 0 then (acc, i - off + 1)
    else loop acc (shift + 7) (i + 1)
  in
  loop 0 0 off

let decode_bytes b off =
  let len = Bytes.length b in
  let rec loop acc shift i =
    if i >= len then raise_error `Varint_unterminated;
    let byte = Bytes.get_uint8 b i in
    let value = byte land mask in
    if shift >= 63 && value > 1 then raise_error `Varint_overflow;
    let acc = acc lor (value lsl shift) in
    if byte land msb = 0 then (acc, i - off + 1)
    else loop acc (shift + 7) (i + 1)
  in
  loop 0 0 off
