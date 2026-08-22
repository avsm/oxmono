(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Binary strings *)

let strf = Printf.sprintf

let[@inline][@zero_alloc] hex_value_unsafe c =
  match c with
  | '0' .. '9' -> Char.code c - 0x30
  | 'A' .. 'F' -> Char.code c - 0x41 + 10
  | 'a' .. 'f' -> Char.code c - 0x61 + 10
  | _ -> -1

let[@inline] hex_value_exn s i =
  let c = String.unsafe_get s i in
  let v = hex_value_unsafe c in
  if v < 0 then failwith (strf "%d: %C is not an ASCII hexadecimal digit" i c);
  v

let to_binary_string' h =
  let len = String.length h in
  if len mod 2 <> 0 then failwith "Missing final hex digit";
  let s_len = len / 2 in
  let s = Bytes.create s_len in
  let mutable i = 0 in
  let mutable k = 0 in
  while i < s_len do
    let hi = hex_value_exn h k in
    let lo = hex_value_exn h (k + 1) in
    Bytes.unsafe_set s i (Char.unsafe_chr ((hi lsl 4) lor lo));
    i <- i + 1;
    k <- k + 2
  done;
  Bytes.unsafe_to_string s

let err_len ~exp ~fnd =
  strf "Expected %d ASCII hexadecimal digits but found %d characters" exp fnd

let to_binary_string ?length hex =
  try match length with
  | None -> Ok (to_binary_string' hex)
  | Some len ->
      let exp = len * 2 in
      let fnd = String.length hex in
      if exp <> fnd then failwith (err_len ~exp ~fnd) else
      Ok (to_binary_string' hex)
  with
  | Failure e -> Error e

let pp_binary_string ppf s =
  let len = String.length s in
  let mutable i = 0 in
  while i < len do
    Format.fprintf ppf "%02x" (Char.code (String.unsafe_get s i));
    i <- i + 1
  done

(* Lookup table for fast hex conversion *)
let hex_chars = "0123456789abcdef"

let of_binary_string s =
  let len = String.length s in
  let result = Bytes.create (len * 2) in
  let mutable i = 0 in
  let mutable j = 0 in
  while i < len do
    let b = Char.code (String.unsafe_get s i) in
    Bytes.unsafe_set result j (String.unsafe_get hex_chars (b lsr 4));
    Bytes.unsafe_set result (j + 1) (String.unsafe_get hex_chars (b land 0x0f));
    i <- i + 1;
    j <- j + 2
  done;
  Bytes.unsafe_to_string result

let check_binary_string_length ~length s =
  let len = String.length s in
  if len = length then Ok s else
  Error (strf "Expected %d bytes but found %d" length len)
