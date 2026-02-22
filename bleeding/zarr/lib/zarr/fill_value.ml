(** Fill value handling for Zarr v3 *)

module D = Ztypes.Dtype
module FV = Ztypes.Fill_value
module E = Ztypes.Endianness

(** Simple Base64 implementation *)
module Base64 = struct
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  let decode_char c =
    if c >= 'A' && c <= 'Z' then Char.code c - 65
    else if c >= 'a' && c <= 'z' then Char.code c - 71
    else if c >= '0' && c <= '9' then Char.code c + 4
    else if c = '+' then 62
    else if c = '/' then 63
    else -1

  let decode_exn s =
    let s = String.concat "" (String.split_on_char '\n' s) in
    let s = String.concat "" (String.split_on_char '\r' s) in
    let s = String.concat "" (String.split_on_char ' ' s) in
    let len = String.length s in
    let padding = if len >= 2 && s.[len-1] = '=' && s.[len-2] = '=' then 2
                  else if len >= 1 && s.[len-1] = '=' then 1
                  else 0 in
    let out_len = (len * 3 / 4) - padding in
    let buf = Bytes.create out_len in
    let rec loop i j =
      if i >= len - padding then ()
      else begin
        let b0 = decode_char s.[i] in
        let b1 = if i+1 < len then decode_char s.[i+1] else 0 in
        let b2 = if i+2 < len && s.[i+2] <> '=' then decode_char s.[i+2] else 0 in
        let b3 = if i+3 < len && s.[i+3] <> '=' then decode_char s.[i+3] else 0 in
        let n = (b0 lsl 18) lor (b1 lsl 12) lor (b2 lsl 6) lor b3 in
        if j < out_len then Bytes.set buf j (Char.chr ((n lsr 16) land 0xFF));
        if j+1 < out_len then Bytes.set buf (j+1) (Char.chr ((n lsr 8) land 0xFF));
        if j+2 < out_len then Bytes.set buf (j+2) (Char.chr (n land 0xFF));
        loop (i+4) (j+3)
      end
    in
    loop 0 0;
    Bytes.to_string buf

  let encode_string s =
    let len = String.length s in
    let out_len = ((len + 2) / 3) * 4 in
    let buf = Bytes.create out_len in
    let rec loop i j =
      if i >= len then ()
      else begin
        let b0 = Char.code s.[i] in
        let b1 = if i+1 < len then Char.code s.[i+1] else 0 in
        let b2 = if i+2 < len then Char.code s.[i+2] else 0 in
        let n = (b0 lsl 16) lor (b1 lsl 8) lor b2 in
        Bytes.set buf j alphabet.[(n lsr 18) land 0x3F];
        Bytes.set buf (j+1) alphabet.[(n lsr 12) land 0x3F];
        Bytes.set buf (j+2) (if i+1 < len then alphabet.[(n lsr 6) land 0x3F] else '=');
        Bytes.set buf (j+3) (if i+2 < len then alphabet.[n land 0x3F] else '=');
        loop (i+3) (j+4)
      end
    in
    loop 0 0;
    Bytes.to_string buf
end

(** Parse a fill value from JSON for a given data type *)
let [@warning "-33"] of_json dtype json =
  let open Yojson.Safe.Util in
  match dtype, json with
  (* Boolean *)
  | D.Bool, `Bool b -> Ok (FV.Bool b)
  | D.Bool, _ -> Error (`Invalid_metadata "fill_value for bool must be a boolean")

  (* Integer types *)
  | (D.Int8 | D.Int16 | D.Int32 | D.Int64), `Int i -> Ok (FV.Int (Int64.of_int i))
  | (D.Int8 | D.Int16 | D.Int32 | D.Int64), `Intlit s ->
    (try Ok (FV.Int (Int64.of_string s))
     with Failure _ -> Error (`Invalid_metadata ("invalid integer fill_value: " ^ s)))

  (* Unsigned integer types *)
  | (D.Uint8 | D.Uint16 | D.Uint32 | D.Uint64), `Int i when i >= 0 ->
    Ok (FV.Uint (Int64.of_int i))
  | (D.Uint8 | D.Uint16 | D.Uint32 | D.Uint64), `Intlit s ->
    (try Ok (FV.Uint (Int64.of_string s))
     with Failure _ -> Error (`Invalid_metadata ("invalid unsigned integer fill_value: " ^ s)))
  | (D.Uint8 | D.Uint16 | D.Uint32 | D.Uint64), `Int i ->
    Error (`Invalid_metadata (Printf.sprintf "negative fill_value %d for unsigned type" i))

  (* Float types - handle special values *)
  | (D.Float16 | D.Float32 | D.Float64), `Float f -> Ok (FV.Float f)
  | (D.Float16 | D.Float32 | D.Float64), `Int i -> Ok (FV.Float (Float.of_int i))
  | (D.Float16 | D.Float32 | D.Float64), `String "NaN" -> Ok FV.NaN
  | (D.Float16 | D.Float32 | D.Float64), `String "Infinity" -> Ok FV.Infinity
  | (D.Float16 | D.Float32 | D.Float64), `String "-Infinity" -> Ok FV.NegInfinity
  | (D.Float16 | D.Float32 | D.Float64), `String s when String.length s > 2 &&
      String.sub s 0 2 = "0x" -> Ok (FV.Hex s)

  (* Complex types - expect array of two numbers *)
  | (D.Complex64 | D.Complex128), `List [`Float re; `Float im] ->
    Ok (FV.Complex (re, im))
  | (D.Complex64 | D.Complex128), `List [`Int re; `Int im] ->
    Ok (FV.Complex (Float.of_int re, Float.of_int im))
  | (D.Complex64 | D.Complex128), `List [`Float re; `Int im] ->
    Ok (FV.Complex (re, Float.of_int im))
  | (D.Complex64 | D.Complex128), `List [`Int re; `Float im] ->
    Ok (FV.Complex (Float.of_int re, im))
  | (D.Complex64 | D.Complex128), `List [`String "NaN"; `Float im] ->
    Ok (FV.Complex (Float.nan, im))
  | (D.Complex64 | D.Complex128), `List [`Float re; `String "NaN"] ->
    Ok (FV.Complex (re, Float.nan))
  | (D.Complex64 | D.Complex128), `List [`String "NaN"; `String "NaN"] ->
    Ok (FV.Complex (Float.nan, Float.nan))
  | (D.Complex64 | D.Complex128), _ ->
    Error (`Invalid_metadata "fill_value for complex must be [real, imag] array")

  (* Raw bytes - expect base64 or array of bytes *)
  | D.Raw bits, `String s ->
    let expected_len = bits / 8 in
    let decoded = Base64.decode_exn s in
    if String.length decoded = expected_len then
      Ok (FV.Raw (Bytes.of_string decoded))
    else
      Error (`Invalid_metadata (Printf.sprintf
        "fill_value for r%d must be %d bytes, got %d"
        bits expected_len (String.length decoded)))
  | D.Raw bits, `List bytes_list ->
    let expected_len = bits / 8 in
    (try
       let bytes = Bytes.create expected_len in
       List.iteri (fun i json ->
         match json with
         | `Int v when v >= 0 && v <= 255 ->
           Bytes.set bytes i (Char.chr v)
         | _ -> failwith "invalid byte"
       ) bytes_list;
       if List.length bytes_list = expected_len then Ok (FV.Raw bytes)
       else failwith "wrong length"
     with Failure msg ->
       Error (`Invalid_metadata ("invalid fill_value for raw type: " ^ msg)))

  | _, _ ->
    Error (`Invalid_metadata (Printf.sprintf
      "invalid fill_value for %s" (Data_type.to_string dtype)))

(** Convert a fill value to JSON for a given data type *)
let to_json _dtype fv =
  match fv with
  | FV.Bool b -> `Bool b
  | FV.Int i -> `Intlit (Int64.to_string i)
  | FV.Uint u -> `Intlit (Int64.to_string u)
  | FV.Float f when Float.is_nan f -> `String "NaN"
  | FV.Float f when f = Float.infinity -> `String "Infinity"
  | FV.Float f when f = Float.neg_infinity -> `String "-Infinity"
  | FV.Float f -> `Float f
  | FV.Complex (re, im) ->
    let float_to_json f =
      if Float.is_nan f then `String "NaN"
      else if f = Float.infinity then `String "Infinity"
      else if f = Float.neg_infinity then `String "-Infinity"
      else `Float f
    in
    `List [float_to_json re; float_to_json im]
  | FV.Raw bytes -> `String (Base64.encode_string (Bytes.to_string bytes))
  | FV.NaN -> `String "NaN"
  | FV.Infinity -> `String "Infinity"
  | FV.NegInfinity -> `String "-Infinity"
  | FV.Hex s -> `String s

(** Convert float to float16 bits (IEEE 754 half-precision) *)
let float16_of_float f =
  let bits32 = Int32.bits_of_float f in
  let sign = Int32.(logand (shift_right_logical bits32 31) 1l) in
  let exp = Int32.(logand (shift_right_logical bits32 23) 0xFFl) in
  let mant = Int32.(logand bits32 0x7FFFFFl) in

  let sign16 = Int32.(shift_left sign 15) in

  if Int32.equal exp 0l then
    (* Zero or denormalized *)
    Int32.to_int sign16
  else if Int32.equal exp 0xFFl then
    (* Infinity or NaN *)
    let exp16 = 0x1F in
    let mant16 = if Int32.equal mant 0l then 0 else 0x200 in
    Int32.to_int sign16 lor (exp16 lsl 10) lor mant16
  else
    (* Normalized number *)
    let exp_val = Int32.to_int exp - 127 in
    if exp_val < -24 then
      (* Too small - underflow to zero *)
      Int32.to_int sign16
    else if exp_val < -14 then
      (* Denormalized float16 *)
      let mant16 = Int32.(to_int (shift_right_logical (logor mant 0x800000l) (-(exp_val + 14)))) lsr 13 in
      Int32.to_int sign16 lor mant16
    else if exp_val > 15 then
      (* Too large - overflow to infinity *)
      Int32.to_int sign16 lor 0x7C00
    else
      (* Normal float16 *)
      let exp16 = exp_val + 15 in
      let mant16 = Int32.to_int (Int32.shift_right_logical mant 13) in
      Int32.to_int sign16 lor (exp16 lsl 10) lor mant16

(** Convert a fill value variant to a float *)
let float_of_fv = function
  | FV.Float f -> f
  | FV.NaN -> Float.nan
  | FV.Infinity -> Float.infinity
  | FV.NegInfinity -> Float.neg_infinity
  | FV.Hex s -> Int32.float_of_bits (Int32.of_string s)
  | _ -> 0.0

(** Write a float as 4 bytes (IEEE 754 single) with given endianness *)
let write_f32 endian f =
  let buf = Bytes.create 4 in
  let bits = Int32.bits_of_float f in
  (match endian with
   | E.Little -> Bytes.set_int32_le buf 0 bits
   | E.Big -> Bytes.set_int32_be buf 0 bits);
  buf

(** Write a float as 8 bytes (IEEE 754 double) with given endianness *)
let write_f64 endian f =
  let buf = Bytes.create 8 in
  let bits = Int64.bits_of_float f in
  (match endian with
   | E.Little -> Bytes.set_int64_le buf 0 bits
   | E.Big -> Bytes.set_int64_be buf 0 bits);
  buf

(** Convert a fill value to bytes with given endianness *)
let to_bytes dtype endian fv =
  let open Bytes in
  match dtype, fv with
  | D.Bool, FV.Bool b ->
    let buf = create 1 in
    set buf 0 (if b then '\x01' else '\x00');
    buf
  | D.Int8, FV.Int i ->
    let buf = create 1 in
    set buf 0 (Char.chr (Int64.to_int i land 0xFF));
    buf
  | D.Uint8, FV.Uint u ->
    let buf = create 1 in
    set buf 0 (Char.chr (Int64.to_int u land 0xFF));
    buf
  | D.Int16, FV.Int i ->
    let buf = create 2 in
    let v = Int64.to_int i in
    (match endian with
     | E.Little -> set_int16_le buf 0 v
     | E.Big -> set_int16_be buf 0 v);
    buf
  | D.Uint16, FV.Uint u ->
    let buf = create 2 in
    let v = Int64.to_int u in
    (match endian with
     | E.Little -> set_uint16_le buf 0 v
     | E.Big -> set_uint16_be buf 0 v);
    buf
  | D.Int32, FV.Int i ->
    let buf = create 4 in
    let v = Int64.to_int32 i in
    (match endian with
     | E.Little -> set_int32_le buf 0 v
     | E.Big -> set_int32_be buf 0 v);
    buf
  | D.Uint32, FV.Uint u ->
    let buf = create 4 in
    let v = Int64.to_int32 u in
    (match endian with
     | E.Little -> set_int32_le buf 0 v
     | E.Big -> set_int32_be buf 0 v);
    buf
  | D.Int64, FV.Int i ->
    let buf = create 8 in
    (match endian with
     | E.Little -> set_int64_le buf 0 i
     | E.Big -> set_int64_be buf 0 i);
    buf
  | D.Uint64, FV.Uint u ->
    let buf = create 8 in
    (match endian with
     | E.Little -> set_int64_le buf 0 u
     | E.Big -> set_int64_be buf 0 u);
    buf
  | D.Float32, (FV.Float _ | FV.NaN | FV.Infinity | FV.NegInfinity | FV.Hex _) ->
    let f = float_of_fv fv in
    write_f32 endian f
  | D.Float64, (FV.Float _ | FV.NaN | FV.Infinity | FV.NegInfinity | FV.Hex _) ->
    let f = float_of_fv fv in
    write_f64 endian f
  | D.Float16, _ ->
    (* Float16 needs special handling - convert through float32 *)
    let f = float_of_fv fv in
    let buf = create 2 in
    let bits = float16_of_float f in
    (match endian with
     | E.Little -> set_uint16_le buf 0 bits
     | E.Big -> set_uint16_be buf 0 bits);
    buf
  | D.Complex64, FV.Complex (re, im) ->
    let buf = create 8 in
    let re_bits = Int32.bits_of_float re in
    let im_bits = Int32.bits_of_float im in
    (match endian with
     | E.Little ->
       set_int32_le buf 0 re_bits;
       set_int32_le buf 4 im_bits
     | E.Big ->
       set_int32_be buf 0 re_bits;
       set_int32_be buf 4 im_bits);
    buf
  | D.Complex128, FV.Complex (re, im) ->
    let buf = create 16 in
    let re_bits = Int64.bits_of_float re in
    let im_bits = Int64.bits_of_float im in
    (match endian with
     | E.Little ->
       set_int64_le buf 0 re_bits;
       set_int64_le buf 8 im_bits
     | E.Big ->
       set_int64_be buf 0 re_bits;
       set_int64_be buf 8 im_bits);
    buf
  | D.Raw _, FV.Raw bytes -> bytes
  | _, _ ->
    (* Default: create zero-filled buffer of appropriate size *)
    create (Data_type.size dtype)

(** Get the default fill value for a data type *)
let default dtype =
  match dtype with
  | D.Bool -> FV.Bool false
  | D.Int8 | D.Int16 | D.Int32 | D.Int64 -> FV.Int 0L
  | D.Uint8 | D.Uint16 | D.Uint32 | D.Uint64 -> FV.Uint 0L
  | D.Float16 | D.Float32 | D.Float64 -> FV.Float 0.0
  | D.Complex64 | D.Complex128 -> FV.Complex (0.0, 0.0)
  | D.Raw bits -> FV.Raw (Bytes.make (bits / 8) '\x00')
