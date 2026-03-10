(** Fill value handling for Zarr v3.

    Fill values specify the default value for uninitialised array regions.
    See Zarr v3.1 spec, "Fill Value" section. *)

(** Fill value representation. *)
type t =
  | Bool of bool
  | Int of int64
  | Uint of int64
  | Float of float
  | Raw of bytes
  | NaN
  | Infinity
  | NegInfinity
  | Hex of string  (** Custom bit pattern, e.g. ["0x7fc00000"] *)

(** [default dtype] returns the zero / false fill value for [dtype]. *)
let default = function
  | Zarr_dtype.Bool -> Bool false
  | Int8 | Int16 | Int32 | Int64 -> Int 0L
  | Uint8 | Uint16 | Uint32 | Uint64 -> Uint 0L
  | Float32 | Float64 -> Float 0.0
  | Raw bits -> Raw (Bytes.make (bits / 8) '\x00')

(** Convert a fill-value variant to a float, for float-typed arrays. *)
let to_float = function
  | Float f -> f
  | NaN -> Float.nan
  | Infinity -> Float.infinity
  | NegInfinity -> Float.neg_infinity
  | Hex s -> Int32.float_of_bits (Int32.of_string s)
  | _ -> 0.0

(** {2 Bytes serialization}

    Write one fill-value element into a byte buffer at native endianness,
    then replicate it across the entire chunk buffer. *)

(** [fill_bytes dtype fv buf] fills [buf] by repeating the fill value
    element across the entire buffer length. The buffer length must be a
    multiple of [Zarr_dtype.byte_size dtype]. *)
let fill_bytes dtype fv buf =
  let elem_size = Zarr_dtype.byte_size dtype in
  let len = Bytes.length buf in
  (* Write one element at offset 0 *)
  (match dtype, fv with
   | Bool, Bool b ->
     Bytes.set buf 0 (if b then '\x01' else '\x00')
   | Int8, Int i ->
     Bytes.set buf 0 (Char.chr (Int64.to_int i land 0xFF))
   | Uint8, Uint u ->
     Bytes.set buf 0 (Char.chr (Int64.to_int u land 0xFF))
   | Int16, Int i ->
     Bytes.set_int16_le buf 0 (Int64.to_int i)
   | Uint16, Uint u ->
     Bytes.set_uint16_le buf 0 (Int64.to_int u)
   | Int32, Int i ->
     Bytes.set_int32_le buf 0 (Int64.to_int32 i)
   | Uint32, Uint u ->
     Bytes.set_int32_le buf 0 (Int64.to_int32 u)
   | Int64, Int i ->
     Bytes.set_int64_le buf 0 i
   | Uint64, Uint u ->
     Bytes.set_int64_le buf 0 u
   | Float32, (Float _ | NaN | Infinity | NegInfinity | Hex _) ->
     Bytes.set_int32_le buf 0 (Int32.bits_of_float (to_float fv))
   | Float64, (Float _ | NaN | Infinity | NegInfinity | Hex _) ->
     Bytes.set_int64_le buf 0 (Int64.bits_of_float (to_float fv))
   | Raw _, Raw b ->
     Bytes.blit b 0 buf 0 (min (Bytes.length b) elem_size)
   | _, _ ->
     (* Default: zero-fill first element *)
     Bytes.fill buf 0 elem_size '\x00');
  (* Replicate first element across entire buffer *)
  if elem_size > 0 && len > elem_size then begin
    let mutable pos = elem_size in
    while pos + pos <= len do
      Bytes.blit buf 0 buf pos pos;
      pos <- pos + pos
    done;
    if pos < len then
      Bytes.blit buf 0 buf pos (len - pos)
  end

(** {2 JSON serialization} *)

(** Simple Base64 codec for raw fill values. *)
module Base64 = struct
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  let[@inline] decode_char c =
    if c >= 'A' && c <= 'Z' then Char.code c - 65
    else if c >= 'a' && c <= 'z' then Char.code c - 71
    else if c >= '0' && c <= '9' then Char.code c + 4
    else if c = '+' then 62
    else if c = '/' then 63
    else -1

  let decode_exn s =
    (* Strip whitespace inline without intermediate allocations *)
    let slen = String.length s in
    let clean = Bytes.create slen in
    let mutable clen = 0 in
    for i = 0 to slen - 1 do
      let c = String.unsafe_get s i in
      if c <> '\n' && c <> '\r' && c <> ' ' then begin
        Bytes.unsafe_set clean clen c;
        clen <- clen + 1
      end
    done;
    let padding =
      if clen >= 2 && Bytes.get clean (clen - 1) = '=' && Bytes.get clean (clen - 2) = '='
      then 2
      else if clen >= 1 && Bytes.get clean (clen - 1) = '=' then 1
      else 0
    in
    let out_len = (clen * 3 / 4) - padding in
    let buf = Bytes.create out_len in
    let mutable i = 0 in
    let mutable j = 0 in
    while i < clen - padding do
      let b0 = decode_char (Bytes.get clean i) in
      let b1 = if i + 1 < clen then decode_char (Bytes.get clean (i + 1)) else 0 in
      let b2 = if i + 2 < clen && Bytes.get clean (i + 2) <> '=' then decode_char (Bytes.get clean (i + 2)) else 0 in
      let b3 = if i + 3 < clen && Bytes.get clean (i + 3) <> '=' then decode_char (Bytes.get clean (i + 3)) else 0 in
      let n = (b0 lsl 18) lor (b1 lsl 12) lor (b2 lsl 6) lor b3 in
      if j < out_len then Bytes.set buf j (Char.chr ((n lsr 16) land 0xFF));
      if j + 1 < out_len then Bytes.set buf (j + 1) (Char.chr ((n lsr 8) land 0xFF));
      if j + 2 < out_len then Bytes.set buf (j + 2) (Char.chr (n land 0xFF));
      i <- i + 4;
      j <- j + 3
    done;
    Bytes.to_string buf

  let encode_string s =
    let len = String.length s in
    let out_len = ((len + 2) / 3) * 4 in
    let buf = Bytes.create out_len in
    let mutable i = 0 in
    let mutable j = 0 in
    while i < len do
      let b0 = Char.code s.[i] in
      let b1 = if i + 1 < len then Char.code s.[i + 1] else 0 in
      let b2 = if i + 2 < len then Char.code s.[i + 2] else 0 in
      let n = (b0 lsl 16) lor (b1 lsl 8) lor b2 in
      Bytes.set buf j alphabet.[(n lsr 18) land 0x3F];
      Bytes.set buf (j + 1) alphabet.[(n lsr 12) land 0x3F];
      Bytes.set buf (j + 2) (if i + 1 < len then alphabet.[(n lsr 6) land 0x3F] else '=');
      Bytes.set buf (j + 3) (if i + 2 < len then alphabet.[n land 0x3F] else '=');
      i <- i + 3;
      j <- j + 4
    done;
    Bytes.to_string buf
end

(** [of_json dtype json] parses a fill value from JSON for the given data type. *)
let of_json dtype (json : Jsont.json) =
  match (dtype : Zarr_dtype.t), json with
  | Bool, Jsont.Bool (b, _) -> Ok (Bool b)
  | Bool, _ -> Error "fill_value for bool must be a boolean"
  | (Int8 | Int16 | Int32 | Int64), Jsont.Number (f, _) ->
    Ok (Int (Int64.of_float f))
  | (Int8 | Int16 | Int32 | Int64), Jsont.String (s, _) ->
    (try Ok (Int (Int64.of_string s))
     with Failure _ -> Error ("invalid integer fill_value: " ^ s))
  | (Uint8 | Uint16 | Uint32 | Uint64), Jsont.Number (f, _) when f >= 0.0 ->
    Ok (Uint (Int64.of_float f))
  | (Uint8 | Uint16 | Uint32 | Uint64), Jsont.String (s, _) ->
    (try Ok (Uint (Int64.of_string s))
     with Failure _ -> Error ("invalid unsigned fill_value: " ^ s))
  | (Uint8 | Uint16 | Uint32 | Uint64), Jsont.Number (f, _) ->
    Error (Printf.sprintf "negative fill_value %d for unsigned type"
             (Float.to_int f))
  | (Float32 | Float64), Jsont.Number (f, _) -> Ok (Float f)
  | (Float32 | Float64), Jsont.String ("NaN", _) -> Ok NaN
  | (Float32 | Float64), Jsont.String ("Infinity", _) -> Ok Infinity
  | (Float32 | Float64), Jsont.String ("-Infinity", _) -> Ok NegInfinity
  | (Float32 | Float64), Jsont.String (s, _)
    when String.length s > 2 && String.sub s 0 2 = "0x" ->
    Ok (Hex s)
  | Raw bits, Jsont.String (s, _) ->
    let expected_len = bits / 8 in
    let decoded = Base64.decode_exn s in
    if String.length decoded = expected_len then
      Ok (Raw (Bytes.of_string decoded))
    else
      Error (Printf.sprintf "fill_value for r%d must be %d bytes, got %d"
               bits expected_len (String.length decoded))
  | Raw bits, Jsont.Array (bytes_list, _) ->
    let expected_len = bits / 8 in
    (try
       let buf = Bytes.create expected_len in
       List.iteri (fun i json ->
         match json with
         | Jsont.Number (f, _) ->
           let v = Float.to_int f in
           if v >= 0 && v <= 255 then
             Bytes.set buf i (Char.chr v)
           else failwith "invalid byte"
         | _ -> failwith "invalid byte"
       ) bytes_list;
       if List.length bytes_list = expected_len then Ok (Raw buf)
       else failwith "wrong length"
     with Failure msg ->
       Error ("invalid fill_value for raw type: " ^ msg))
  | _, _ ->
    Error (Printf.sprintf "invalid fill_value for %s"
             (Zarr_dtype.to_string dtype))

(** [to_json fv] converts a fill value to its JSON representation. *)
let to_json = function
  | Bool b -> Jsont.Json.bool b
  | Int i -> Jsont.Json.int64 i
  | Uint u -> Jsont.Json.int64 u
  | Float f when Float.is_nan f -> Jsont.Json.string "NaN"
  | Float f when f = Float.infinity -> Jsont.Json.string "Infinity"
  | Float f when f = Float.neg_infinity -> Jsont.Json.string "-Infinity"
  | Float f -> Jsont.Json.number f
  | Raw bytes -> Jsont.Json.string (Base64.encode_string (Bytes.to_string bytes))
  | NaN -> Jsont.Json.string "NaN"
  | Infinity -> Jsont.Json.string "Infinity"
  | NegInfinity -> Jsont.Json.string "-Infinity"
  | Hex s -> Jsont.Json.string s
