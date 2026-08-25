(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string

let of_bytes s = s
let to_bytes t = t
let length = String.length
let equal = String.equal

let pp ppf t =
  Format.pp_print_string ppf "0x";
  String.iter (fun c -> Format.fprintf ppf "%02x" (Char.code c)) t

(* Failures carry a message that [of_json] prefixes with the data type. *)

exception Bad of string

let bad fmt = Printf.ksprintf (fun s -> raise (Bad s)) fmt

let sort_name = function
  | Jsont.Null _ -> "null"
  | Jsont.Bool _ -> "a boolean"
  | Jsont.Number _ -> "a number"
  | Jsont.String _ -> "a string"
  | Jsont.Array _ -> "an array"
  | Jsont.Object _ -> "an object"

(* Scalars are held as an int64 bit pattern of [size] bytes and laid out
   native endian only at the boundary. *)

let native_of_bits ~size bits =
  let b = Bytes.create size in
  for i = 0 to size - 1 do
    let byte =
      Int64.to_int (Int64.logand (Int64.shift_right_logical bits (8 * i)) 0xFFL)
    in
    Bytes.set_uint8 b (if Sys.big_endian then size - 1 - i else i) byte
  done;
  Bytes.unsafe_to_string b

let bits_of_native ~size s off =
  let mutable v = 0L in
  for i = 0 to size - 1 do
    let idx = off + if Sys.big_endian then size - 1 - i else i in
    v <-
      Int64.logor v
        (Int64.shift_left (Int64.of_int (Char.code s.[idx])) (8 * i))
  done;
  v

let hex_digits = "0123456789abcdef"

let hex_of_bits ~size bits =
  let b = Buffer.create (2 + (2 * size)) in
  Buffer.add_string b "0x";
  for i = (2 * size) - 1 downto 0 do
    let nibble =
      Int64.to_int (Int64.shift_right_logical bits (4 * i)) land 0xF
    in
    Buffer.add_char b hex_digits.[nibble]
  done;
  Buffer.contents b

let hex_digit c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> -1

(* [hex_bits ~size s] is the big endian bit pattern of the [0x] prefixed
   string [s], which must carry exactly [2 * size] digits. *)
let hex_bits ~size s =
  let n = String.length s in
  if n <> 2 + (2 * size) then None
  else if not (s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X')) then None
  else
    let rec loop i acc =
      if i = n then Some acc
      else
        let d = hex_digit s.[i] in
        if d < 0 then None
        else
          loop (i + 1)
            (Int64.logor (Int64.shift_left acc 4) (Int64.of_int d))
    in
    loop 2 0L

let base64_digit c =
  match c with
  | 'A' .. 'Z' -> Char.code c - Char.code 'A'
  | 'a' .. 'z' -> Char.code c - Char.code 'a' + 26
  | '0' .. '9' -> Char.code c - Char.code '0' + 52
  | '+' -> 62
  | '/' -> 63
  | _ -> -1

(* Standard base64. Padding is mandatory, so the length is a multiple of
   four and only the final quad may carry '='. *)
let base64_decode s =
  let n = String.length s in
  if n mod 4 <> 0 then None
  else
    let pad =
      if n = 0 then 0
      else if s.[n - 1] <> '=' then 0
      else if s.[n - 2] = '=' then 2
      else 1
    in
    let exception Bad_base64 in
    let out = Buffer.create (n / 4 * 3) in
    let quad i =
      let last = i + 4 = n in
      let digit k =
        let c = s.[i + k] in
        if last && k >= 4 - pad then if c = '=' then 0 else raise Bad_base64
        else
          let v = base64_digit c in
          if v < 0 then raise Bad_base64 else v
      in
      let a = digit 0 in
      let b = digit 1 in
      let c = digit 2 in
      let d = digit 3 in
      let v = (a lsl 18) lor (b lsl 12) lor (c lsl 6) lor d in
      let emit = if last then 3 - pad else 3 in
      if emit >= 1 then Buffer.add_char out (Char.chr ((v lsr 16) land 0xFF));
      if emit >= 2 then Buffer.add_char out (Char.chr ((v lsr 8) land 0xFF));
      if emit >= 3 then Buffer.add_char out (Char.chr (v land 0xFF))
    in
    match
      for q = 0 to (n / 4) - 1 do
        quad (4 * q)
      done
    with
    | () -> Some (Buffer.contents out)
    | exception Bad_base64 -> None

(* OCaml has no binary16 or bfloat16 type, so narrowing and widening for
   those two are done on the bit patterns. *)

type fkind = F16 | BF16 | F32 | F64

let fsize = function F16 | BF16 -> 2 | F32 -> 4 | F64 -> 8

let exp_mask = function
  | F16 -> 0x7C00L
  | BF16 -> 0x7F80L
  | F32 -> 0x7F800000L
  | F64 -> 0x7FF0000000000000L

let mant_mask = function
  | F16 -> 0x03FFL
  | BF16 -> 0x007FL
  | F32 -> 0x007FFFFFL
  | F64 -> 0x000FFFFFFFFFFFFFL

let sign_mask = function
  | F16 | BF16 -> 0x8000L
  | F32 -> 0x80000000L
  | F64 -> 0x8000000000000000L

(* Sign 0, exponent all ones, mantissa MSB 1 and the rest 0. *)
let canonical_nan = function
  | F16 -> 0x7E00L
  | BF16 -> 0x7FC0L
  | F32 -> 0x7FC00000L
  | F64 -> 0x7FF8000000000000L

let is_nan_bits k bits =
  Int64.equal (Int64.logand bits (exp_mask k)) (exp_mask k)
  && not (Int64.equal (Int64.logand bits (mant_mask k)) 0L)

(* [f16_of_float x] is the IEEE 754 binary16 image of [x], rounded to
   nearest with ties to even. Overflow gives an infinity, underflow a
   zero of the same sign. *)
let f16_of_float x =
  let b = Int64.bits_of_float x in
  let sign = Int64.to_int (Int64.shift_right_logical b 63) land 1 in
  let exp = Int64.to_int (Int64.shift_right_logical b 52) land 0x7FF in
  let mant = Int64.logand b 0x000FFFFFFFFFFFFFL in
  if exp = 0x7FF then
    if Int64.equal mant 0L then (sign lsl 15) lor 0x7C00
    else
      (* Keep it a NaN whatever the payload does under truncation. *)
      (sign lsl 15) lor 0x7C00 lor 0x200
      lor (Int64.to_int (Int64.shift_right_logical mant 42) land 0x1FF)
  else
    let unbiased = exp - 1023 in
    if unbiased > 15 then (sign lsl 15) lor 0x7C00
    else if unbiased >= -14 then begin
      let m = Int64.to_int (Int64.shift_right_logical mant 42) in
      let rem = Int64.logand mant 0x000003FFFFFFFFFFL in
      let half = 0x0000020000000000L in
      let up =
        Int64.compare rem half > 0
        || (Int64.equal rem half && m land 1 = 1)
      in
      let v = ((unbiased + 15) lsl 10) lor m in
      (* A carry out of the mantissa lands in the exponent, and a carry
         out of exponent 30 lands exactly on the infinity pattern. *)
      (sign lsl 15) lor if up then v + 1 else v
    end
    else if unbiased >= -25 then begin
      let full = Int64.logor mant 0x0010000000000000L in
      let shift = 42 + (-unbiased - 14) in
      let m = Int64.to_int (Int64.shift_right_logical full shift) in
      let rem = Int64.logand full (Int64.sub (Int64.shift_left 1L shift) 1L) in
      let half = Int64.shift_left 1L (shift - 1) in
      let up =
        Int64.compare rem half > 0
        || (Int64.equal rem half && m land 1 = 1)
      in
      (sign lsl 15) lor if up then m + 1 else m
    end
    else sign lsl 15

let f16_to_float b =
  let sign = if b land 0x8000 <> 0 then -1.0 else 1.0 in
  let e = (b lsr 10) land 0x1F in
  let m = b land 0x3FF in
  if e = 0 then sign *. float_of_int m *. ldexp 1.0 (-24)
  else if e = 31 then if m = 0 then sign *. infinity else Float.nan
  else sign *. float_of_int (m lor 0x400) *. ldexp 1.0 (e - 25)

(* [bf16_of_float x] is the top 16 bits of the binary32 image of [x],
   rounded to nearest with ties to even. *)
let bf16_of_float x =
  let u = Int32.to_int (Int32.bits_of_float x) land 0xFFFFFFFF in
  let exp = (u lsr 23) land 0xFF in
  let mant = u land 0x7FFFFF in
  if exp = 0xFF then
    if mant = 0 then (u lsr 16) land 0xFFFF
    else ((u lsr 16) land 0xFFFF) lor 0x40
  else
    let lsb = (u lsr 16) land 1 in
    ((u + 0x7FFF + lsb) lsr 16) land 0xFFFF

let narrow k f =
  match k with
  | F64 -> Int64.bits_of_float f
  | F32 -> Int64.logand (Int64.of_int32 (Int32.bits_of_float f)) 0xFFFFFFFFL
  | F16 -> Int64.of_int (f16_of_float f)
  | BF16 -> Int64.of_int (bf16_of_float f)

let widen k bits =
  match k with
  | F64 -> Int64.float_of_bits bits
  | F32 -> Int32.float_of_bits (Int64.to_int32 bits)
  | BF16 -> Int32.float_of_bits (Int32.shift_left (Int64.to_int32 bits) 16)
  | F16 -> f16_to_float (Int64.to_int bits)

let float_bits_of_json k j =
  let size = fsize k in
  match j with
  | Jsont.Number (f, _) -> narrow k f
  | Jsont.String (s, _) -> (
      match s with
      | "Infinity" -> exp_mask k
      | "-Infinity" -> Int64.logor (exp_mask k) (sign_mask k)
      | "NaN" -> canonical_nan k
      | _ -> (
          match hex_bits ~size s with
          | Some bits -> bits
          | None ->
              bad
                "expected a number, \"Infinity\", \"-Infinity\", \"NaN\" or a \
                 %d byte hex string, found %S"
                size s))
  | j -> bad "expected a number or a string, found %s" (sort_name j)

let float_json_of_bits k bits =
  if Int64.equal bits (exp_mask k) then Jsont.Json.string "Infinity"
  else if Int64.equal bits (Int64.logor (exp_mask k) (sign_mask k)) then
    Jsont.Json.string "-Infinity"
  else if Int64.equal bits (canonical_nan k) then Jsont.Json.string "NaN"
  else if is_nan_bits k bits then
    Jsont.Json.string (hex_of_bits ~size:(fsize k) bits)
  else Jsont.Json.number (widen k bits)

(* Integers. [jsont] numbers are floats, so the 64 bit types are capped
   at the largest magnitude a float names exactly. *)

let int_range ~signed ~size =
  if signed then
    if size = 8 then (-.ldexp 1.0 53, ldexp 1.0 53)
    else (-.ldexp 1.0 ((8 * size) - 1), ldexp 1.0 ((8 * size) - 1) -. 1.0)
  else if size = 8 then (0.0, ldexp 1.0 53)
  else (0.0, ldexp 1.0 (8 * size) -. 1.0)

let int_mask size =
  if size = 8 then -1L else Int64.sub (Int64.shift_left 1L (8 * size)) 1L

let int_bits_of_json ~signed ~size j =
  match j with
  | Jsont.Number (f, _) ->
      if not (Float.is_integer f) then
        bad "expected an integer, found %.17g" f
      else
        let lo, hi = int_range ~signed ~size in
        if f < lo || f > hi then
          bad "%.17g is outside the representable range [%.17g;%.17g]" f lo hi
        else Int64.logand (Int64.of_float f) (int_mask size)
  | j -> bad "expected a number, found %s" (sort_name j)

let int_json_of_bits ~signed ~size bits =
  let f =
    if signed then
      let shift = 64 - (8 * size) in
      Int64.to_float (Int64.shift_right (Int64.shift_left bits shift) shift)
    else if size = 8 && Int64.compare bits 0L < 0 then
      18446744073709551616.0 +. Int64.to_float bits
    else Int64.to_float bits
  in
  Jsont.Json.number f

let scalar_int ~signed ~size j =
  native_of_bits ~size (int_bits_of_json ~signed ~size j)

let scalar_float k j = native_of_bits ~size:(fsize k) (float_bits_of_json k j)

let complex k j =
  match j with
  | Jsont.Array ([ re; im ], _) -> scalar_float k re ^ scalar_float k im
  | Jsont.Array (l, _) ->
      bad "expected a 2 element array, found %d elements" (List.length l)
  | j -> bad "expected a 2 element array, found %s" (sort_name j)

let raw n j =
  match j with
  | Jsont.Array (l, _) ->
      let len = List.length l in
      if len <> n then bad "expected %d bytes, found %d" n len
      else
        let b = Buffer.create n in
        List.iter
          (fun e ->
            match e with
            | Jsont.Number (f, _) ->
                if Float.is_integer f && f >= 0.0 && f <= 255.0 then
                  Buffer.add_char b (Char.chr (int_of_float f))
                else bad "%.17g is not a byte in [0;255]" f
            | e -> bad "expected a byte in [0;255], found %s" (sort_name e))
          l;
        Buffer.contents b
  | Jsont.String (s, _) -> (
      match base64_decode s with
      | None -> bad "expected base64, found %S" s
      | Some bytes ->
          if String.length bytes <> n then
            bad "expected %d base64 decoded bytes, found %d" n
              (String.length bytes)
          else bytes)
  | j -> bad "expected an array or a base64 string, found %s" (sort_name j)

let of_json dt j =
  try
    Ok
      (match dt with
      | Dtype.Bool -> (
          match j with
          | Jsont.Bool (b, _) -> if b then "\001" else "\000"
          | j -> bad "expected a boolean, found %s" (sort_name j))
      | Dtype.Int8 -> scalar_int ~signed:true ~size:1 j
      | Dtype.Int16 -> scalar_int ~signed:true ~size:2 j
      | Dtype.Int32 -> scalar_int ~signed:true ~size:4 j
      | Dtype.Int64 -> scalar_int ~signed:true ~size:8 j
      | Dtype.Uint8 -> scalar_int ~signed:false ~size:1 j
      | Dtype.Uint16 -> scalar_int ~signed:false ~size:2 j
      | Dtype.Uint32 -> scalar_int ~signed:false ~size:4 j
      | Dtype.Uint64 -> scalar_int ~signed:false ~size:8 j
      | Dtype.Float16 -> scalar_float F16 j
      | Dtype.Bfloat16 -> scalar_float BF16 j
      | Dtype.Float32 -> scalar_float F32 j
      | Dtype.Float64 -> scalar_float F64 j
      | Dtype.Complex64 -> complex F32 j
      | Dtype.Complex128 -> complex F64 j
      | Dtype.Raw n -> raw n j)
  with Bad m -> Error (Printf.sprintf "%s fill value: %s" (Dtype.name dt) m)

let to_json dt t =
  let size = Dtype.size dt in
  if String.length t <> size then
    Error.raise_
      (Error.Metadata
         (Printf.sprintf "%s fill value: expected %d bytes, found %d"
            (Dtype.name dt) size (String.length t)));
  let int ~signed ~size =
    int_json_of_bits ~signed ~size (bits_of_native ~size t 0)
  in
  let float k = float_json_of_bits k (bits_of_native ~size:(fsize k) t 0) in
  let complex k =
    let size = fsize k in
    Jsont.Json.list
      [
        float_json_of_bits k (bits_of_native ~size t 0);
        float_json_of_bits k (bits_of_native ~size t size);
      ]
  in
  match dt with
  | Dtype.Bool -> Jsont.Json.bool (t.[0] <> '\000')
  | Dtype.Int8 -> int ~signed:true ~size:1
  | Dtype.Int16 -> int ~signed:true ~size:2
  | Dtype.Int32 -> int ~signed:true ~size:4
  | Dtype.Int64 -> int ~signed:true ~size:8
  | Dtype.Uint8 -> int ~signed:false ~size:1
  | Dtype.Uint16 -> int ~signed:false ~size:2
  | Dtype.Uint32 -> int ~signed:false ~size:4
  | Dtype.Uint64 -> int ~signed:false ~size:8
  | Dtype.Float16 -> float F16
  | Dtype.Bfloat16 -> float BF16
  | Dtype.Float32 -> float F32
  | Dtype.Float64 -> float F64
  | Dtype.Complex64 -> complex F32
  | Dtype.Complex128 -> complex F64
  | Dtype.Raw n ->
      Jsont.Json.list
        (List.init n (fun i ->
             Jsont.Json.number (float_of_int (Char.code t.[i]))))
