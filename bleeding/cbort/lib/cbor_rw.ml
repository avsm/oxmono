(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

(* CBOR Major Types *)
let major_uint = 0
let major_nint = 1
let major_bytes = 2
let major_text = 3
let major_array = 4
let major_map = 5
let major_tag = 6
let major_simple = 7

(* CBOR Simple Values *)
let simple_false = 20
let simple_true = 21
let simple_null = 22
let simple_undefined = 23

(* Additional Information *)
let ai_1byte = 24
let ai_2byte = 25
let ai_4byte = 26
let ai_8byte = 27
let ai_indefinite = 31
let break_code = 0xff

(* Bignum tags *)
let tag_positive_bignum = 2
let tag_negative_bignum = 3

(* Limits for integer encoding *)
let max_uint64 = Z.of_string "18446744073709551615" (* 2^64 - 1 *)
let max_nint64 = Z.of_string "-18446744073709551616" (* -2^64 *)

(* ========== Encoder ========== *)

type encoder = { writer : Bytes.Writer.t; buf : bytes; mutable buf_pos : int }

let buf_size = 4096

let make_encoder writer =
  { writer; buf = Stdlib.Bytes.create buf_size; buf_pos = 0 }

let flush_encoder enc =
  if enc.buf_pos > 0 then begin
    let slice = Bytes.Slice.make enc.buf ~first:0 ~length:enc.buf_pos in
    Bytes.Writer.write enc.writer slice;
    enc.buf_pos <- 0
  end

let ensure_space enc n = if enc.buf_pos + n > buf_size then flush_encoder enc

let write_byte enc b =
  ensure_space enc 1;
  Stdlib.Bytes.set_uint8 enc.buf enc.buf_pos b;
  enc.buf_pos <- enc.buf_pos + 1

let write_bytes enc s =
  let len = String.length s in
  if len <= buf_size - enc.buf_pos then begin
    Stdlib.Bytes.blit_string s 0 enc.buf enc.buf_pos len;
    enc.buf_pos <- enc.buf_pos + len
  end
  else begin
    flush_encoder enc;
    if len <= buf_size then begin
      Stdlib.Bytes.blit_string s 0 enc.buf 0 len;
      enc.buf_pos <- len
    end
    else begin
      let slice = Bytes.Slice.of_string s in
      Bytes.Writer.write enc.writer slice
    end
  end

let write_u16_be enc v =
  ensure_space enc 2;
  Stdlib.Bytes.set_uint16_be enc.buf enc.buf_pos v;
  enc.buf_pos <- enc.buf_pos + 2

let write_u32_be enc v =
  ensure_space enc 4;
  Stdlib.Bytes.set_int32_be enc.buf enc.buf_pos v;
  enc.buf_pos <- enc.buf_pos + 4

let write_u64_be enc v =
  ensure_space enc 8;
  Stdlib.Bytes.set_int64_be enc.buf enc.buf_pos v;
  enc.buf_pos <- enc.buf_pos + 8

let write_type_arg enc major arg =
  let base = major lsl 5 in
  if arg < 24 then write_byte enc (base lor arg)
  else if arg < 0x100 then begin
    write_byte enc (base lor ai_1byte);
    write_byte enc arg
  end
  else if arg < 0x10000 then begin
    write_byte enc (base lor ai_2byte);
    write_u16_be enc arg
  end
  else if arg < 0x100000000 then begin
    write_byte enc (base lor ai_4byte);
    write_u32_be enc (Int32.of_int arg)
  end
  else begin
    write_byte enc (base lor ai_8byte);
    write_u64_be enc (Int64.of_int arg)
  end

let write_type_arg64 enc major arg =
  let base = major lsl 5 in
  if arg < 24L then write_byte enc (base lor Int64.to_int arg)
  else if arg < 0x100L then begin
    write_byte enc (base lor ai_1byte);
    write_byte enc (Int64.to_int arg)
  end
  else if arg < 0x10000L then begin
    write_byte enc (base lor ai_2byte);
    write_u16_be enc (Int64.to_int arg)
  end
  else if arg < 0x100000000L then begin
    write_byte enc (base lor ai_4byte);
    write_u32_be enc (Int64.to_int32 arg)
  end
  else begin
    write_byte enc (base lor ai_8byte);
    write_u64_be enc arg
  end

let write_null enc = write_byte enc ((major_simple lsl 5) lor simple_null)

let write_undefined enc =
  write_byte enc ((major_simple lsl 5) lor simple_undefined)

let write_bool enc b =
  let v = if b then simple_true else simple_false in
  write_byte enc ((major_simple lsl 5) lor v)

let write_simple enc n =
  if n < 24 then write_byte enc ((major_simple lsl 5) lor n)
  else begin
    write_byte enc ((major_simple lsl 5) lor ai_1byte);
    write_byte enc n
  end

(* Half-precision float encoding *)
let encode_half f =
  let bits = Int32.to_int (Int32.bits_of_float f) in
  let sign = (bits lsr 31) land 1 in
  let exp = (bits lsr 23) land 0xff in
  let mant = bits land 0x7fffff in
  if exp = 0 then
    (* Zero or subnormal - may lose precision *)
    (sign lsl 15) lor (mant lsr 13)
  else if exp = 0xff then
    (* Inf or NaN *)
    (sign lsl 15) lor 0x7c00 lor if mant <> 0 then 0x200 else 0
  else
    let new_exp = exp - 127 + 15 in
    if new_exp <= 0 then
      (* Subnormal in half *)
      (sign lsl 15) lor ((mant lor 0x800000) lsr (14 - new_exp))
    else if new_exp >= 31 then
      (* Overflow to infinity *)
      (sign lsl 15) lor 0x7c00
    else (sign lsl 15) lor (new_exp lsl 10) lor (mant lsr 13)

let decode_half bits =
  let sign = (bits lsr 15) land 1 in
  let exp = (bits lsr 10) land 0x1f in
  let mant = bits land 0x3ff in
  let f =
    if exp = 0 then ldexp (float_of_int mant) (-24)
    else if exp = 31 then if mant = 0 then infinity else nan
    else ldexp (float_of_int (mant lor 0x400)) (exp - 25)
  in
  if sign = 1 then -.f else f

(* Check if float can be exactly represented in half precision *)
let can_encode_half f =
  let half = encode_half f in
  let back = decode_half half in
  (* Compare bit patterns to handle -0.0 and NaN correctly *)
  Int64.bits_of_float f = Int64.bits_of_float back

(* Check if float can be exactly represented in single precision *)
let can_encode_single f =
  let single = Int32.float_of_bits (Int32.bits_of_float f) in
  Int64.bits_of_float f = Int64.bits_of_float single

let write_float16 enc f =
  write_byte enc ((major_simple lsl 5) lor ai_2byte);
  write_u16_be enc (encode_half f)

let write_float32 enc f =
  write_byte enc ((major_simple lsl 5) lor ai_4byte);
  write_u32_be enc (Int32.bits_of_float f)

let write_float64 enc f =
  write_byte enc ((major_simple lsl 5) lor ai_8byte);
  write_u64_be enc (Int64.bits_of_float f)

(* Write float using minimal encoding *)
let write_float enc f =
  if can_encode_half f then write_float16 enc f
  else if can_encode_single f then write_float32 enc f
  else write_float64 enc f

let write_int enc n =
  if n >= 0 then write_type_arg enc major_uint n
  else write_type_arg enc major_nint (-n - 1)

let write_int64 enc n =
  if n >= 0L then write_type_arg64 enc major_uint n
  else write_type_arg64 enc major_nint (Int64.neg n |> Int64.pred)

(* Convert Z.t to big-endian bytes (minimal representation) *)
let bigint_to_bytes n =
  if Z.equal n Z.zero then "\x00"
  else
    let s = Z.to_bits n in
    (* Z.to_bits gives little-endian, we need big-endian *)
    let len = String.length s in
    let buf = Stdlib.Bytes.create len in
    for i = 0 to len - 1 do
      Stdlib.Bytes.set buf i s.[len - 1 - i]
    done;
    (* Remove leading zeros *)
    let start = ref 0 in
    while !start < len - 1 && Stdlib.Bytes.get buf !start = '\x00' do
      incr start
    done;
    Stdlib.Bytes.sub_string buf !start (len - !start)

(* Convert big-endian bytes to Z.t *)
let bytes_to_bigint s =
  let len = String.length s in
  if len = 0 then Z.zero
  else begin
    (* Convert big-endian to little-endian for Z.of_bits *)
    let buf = Stdlib.Bytes.create len in
    for i = 0 to len - 1 do
      Stdlib.Bytes.set buf i s.[len - 1 - i]
    done;
    Z.of_bits (Stdlib.Bytes.unsafe_to_string buf)
  end

(* Write a Z.t integer, using bignum tags for large values *)
let write_bigint enc n =
  if Z.sign n >= 0 then begin
    if Z.leq n max_uint64 then
      (* Fits in uint64 *)
      if Z.fits_int64 n then write_type_arg64 enc major_uint (Z.to_int64 n)
      else begin
        (* Between 2^63 and 2^64-1: need unsigned encoding *)
        write_byte enc ((major_uint lsl 5) lor ai_8byte);
        (* Z.to_int64 would overflow, use Z.to_bits and convert *)
        let bytes = bigint_to_bytes n in
        let padded = Stdlib.Bytes.make 8 '\x00' in
        let offset = 8 - String.length bytes in
        Stdlib.Bytes.blit_string bytes 0 padded offset (String.length bytes);
        for i = 0 to 7 do
          write_byte enc (Char.code (Stdlib.Bytes.get padded i))
        done
      end
    else begin
      (* Need bignum tag 2 *)
      write_type_arg enc major_tag tag_positive_bignum;
      let bytes = bigint_to_bytes n in
      write_type_arg enc major_bytes (String.length bytes);
      write_bytes enc bytes
    end
  end
  else begin
    (* Negative number *)
    let abs_minus_1 = Z.pred (Z.neg n) in
    (* -1 - n = |n| - 1 *)
    if Z.geq n max_nint64 then
      (* Fits in nint64 *)
      if Z.fits_int64 abs_minus_1 then
        write_type_arg64 enc major_nint (Z.to_int64 abs_minus_1)
      else begin
        (* Need full 8 bytes *)
        write_byte enc ((major_nint lsl 5) lor ai_8byte);
        let bytes = bigint_to_bytes abs_minus_1 in
        let padded = Stdlib.Bytes.make 8 '\x00' in
        let offset = 8 - String.length bytes in
        Stdlib.Bytes.blit_string bytes 0 padded offset (String.length bytes);
        for i = 0 to 7 do
          write_byte enc (Char.code (Stdlib.Bytes.get padded i))
        done
      end
    else begin
      (* Need bignum tag 3 *)
      write_type_arg enc major_tag tag_negative_bignum;
      let bytes = bigint_to_bytes abs_minus_1 in
      write_type_arg enc major_bytes (String.length bytes);
      write_bytes enc bytes
    end
  end

let write_text enc s =
  write_type_arg enc major_text (String.length s);
  write_bytes enc s

let write_text_start enc = write_byte enc ((major_text lsl 5) lor ai_indefinite)
let write_text_chunk enc s = write_text enc s

let write_bytes_data enc s =
  write_type_arg enc major_bytes (String.length s);
  write_bytes enc s

let write_bytes_header enc len = write_type_arg enc major_bytes len

let write_bytes_start enc =
  write_byte enc ((major_bytes lsl 5) lor ai_indefinite)

let write_bytes_chunk enc s = write_bytes_data enc s
let write_array_start enc n = write_type_arg enc major_array n

let write_array_indef enc =
  write_byte enc ((major_array lsl 5) lor ai_indefinite)

let write_map_start enc n = write_type_arg enc major_map n
let write_map_indef enc = write_byte enc ((major_map lsl 5) lor ai_indefinite)
let write_tag enc n = write_type_arg enc major_tag n
let write_break enc = write_byte enc break_code

(* ========== Decoder ========== *)

type decoder = {
  reader : Bytes.Reader.t;
  mutable slice : Bytes.Slice.t;
  mutable slice_pos : int;
  mutable position : int;
}

let make_decoder reader =
  (* Initialize with eod slice - will be replaced on first read *)
  { reader; slice = Bytes.Slice.eod; slice_pos = 0; position = 0 }

let decoder_at_end dec =
  if dec.slice_pos < Bytes.Slice.length dec.slice then false
  else begin
    dec.slice <- Bytes.Reader.read dec.reader;
    dec.slice_pos <- 0;
    Bytes.Slice.is_eod dec.slice
  end

let decoder_position dec = dec.position

let refill dec =
  dec.slice <- Bytes.Reader.read dec.reader;
  dec.slice_pos <- 0;
  if Bytes.Slice.is_eod dec.slice then raise End_of_file

let slice_get_byte slice pos =
  Stdlib.Bytes.get_uint8 (Bytes.Slice.bytes slice)
    (Bytes.Slice.first slice + pos)

let peek_byte dec =
  if dec.slice_pos >= Bytes.Slice.length dec.slice then begin
    dec.slice <- Bytes.Reader.read dec.reader;
    dec.slice_pos <- 0;
    if Bytes.Slice.is_eod dec.slice then None
    else Some (slice_get_byte dec.slice dec.slice_pos)
  end
  else Some (slice_get_byte dec.slice dec.slice_pos)

let read_byte dec =
  if dec.slice_pos >= Bytes.Slice.length dec.slice then refill dec;
  let b = slice_get_byte dec.slice dec.slice_pos in
  dec.slice_pos <- dec.slice_pos + 1;
  dec.position <- dec.position + 1;
  b

let read_bytes dec n =
  if n = 0 then ""
  else
    let buf = Stdlib.Bytes.create n in
    let rec loop off remaining =
      if remaining = 0 then ()
      else begin
        if dec.slice_pos >= Bytes.Slice.length dec.slice then refill dec;
        let avail = Bytes.Slice.length dec.slice - dec.slice_pos in
        let take = min avail remaining in
        let src = Bytes.Slice.bytes dec.slice in
        let src_off = Bytes.Slice.first dec.slice + dec.slice_pos in
        Stdlib.Bytes.blit src src_off buf off take;
        dec.slice_pos <- dec.slice_pos + take;
        dec.position <- dec.position + take;
        loop (off + take) (remaining - take)
      end
    in
    loop 0 n;
    Stdlib.Bytes.unsafe_to_string buf

let read_u16_be dec =
  let b0 = read_byte dec in
  let b1 = read_byte dec in
  (b0 lsl 8) lor b1

let read_u32_be dec =
  let b0 = read_byte dec in
  let b1 = read_byte dec in
  let b2 = read_byte dec in
  let b3 = read_byte dec in
  Int32.(
    logor
      (shift_left (of_int b0) 24)
      (logor
         (shift_left (of_int b1) 16)
         (logor (shift_left (of_int b2) 8) (of_int b3))))

let read_u64_be dec =
  let b0 = read_byte dec in
  let b1 = read_byte dec in
  let b2 = read_byte dec in
  let b3 = read_byte dec in
  let b4 = read_byte dec in
  let b5 = read_byte dec in
  let b6 = read_byte dec in
  let b7 = read_byte dec in
  Int64.(
    logor
      (shift_left (of_int b0) 56)
      (logor
         (shift_left (of_int b1) 48)
         (logor
            (shift_left (of_int b2) 40)
            (logor
               (shift_left (of_int b3) 32)
               (logor
                  (shift_left (of_int b4) 24)
                  (logor
                     (shift_left (of_int b5) 16)
                     (logor (shift_left (of_int b6) 8) (of_int b7))))))))

(* Read unsigned 64-bit as Z.t to handle full range *)
let read_uint64_as_z dec =
  let bytes = read_bytes dec 8 in
  bytes_to_bigint bytes

type header = { major : int; info : int }

let read_header dec =
  let b = read_byte dec in
  { major = b lsr 5; info = b land 0x1f }

(* Read argument as Z.t to handle full unsigned range *)
let read_argument_z dec hdr =
  let info = hdr.info in
  if info < 24 then Z.of_int info
  else if info = ai_1byte then Z.of_int (read_byte dec)
  else if info = ai_2byte then Z.of_int (read_u16_be dec)
  else if info = ai_4byte then
    Z.of_int64 (Int64.logand (Int64.of_int32 (read_u32_be dec)) 0xffffffffL)
  else if info = ai_8byte then read_uint64_as_z dec
  else if info = ai_indefinite then Z.minus_one
  else failwith (Printf.sprintf "Invalid additional info: %d" info)

let read_argument dec hdr =
  let info = hdr.info in
  if info < 24 then Int64.of_int info
  else if info = ai_1byte then Int64.of_int (read_byte dec)
  else if info = ai_2byte then Int64.of_int (read_u16_be dec)
  else if info = ai_4byte then
    Int64.logand (Int64.of_int32 (read_u32_be dec)) 0xffffffffL
  else if info = ai_8byte then read_u64_be dec
  else if info = ai_indefinite then -1L
  else failwith (Printf.sprintf "Invalid additional info: %d" info)

let read_int dec =
  let hdr = read_header dec in
  let arg = read_argument dec hdr in
  if hdr.major = major_uint then arg
  else if hdr.major = major_nint then Int64.(neg (succ arg))
  else failwith "Expected integer"

let read_text dec =
  let hdr = read_header dec in
  if hdr.major <> major_text then failwith "Expected text string";
  let arg = read_argument dec hdr in
  if arg >= 0L then read_bytes dec (Int64.to_int arg)
  else begin
    let buf = Buffer.create 64 in
    while
      match peek_byte dec with
      | Some 0xff ->
          ignore (read_byte dec);
          false
      | _ -> true
    do
      let hdr = read_header dec in
      if hdr.major <> major_text then failwith "Expected text chunk";
      let len = read_argument dec hdr in
      if len < 0L then failwith "Nested indefinite text";
      Buffer.add_string buf (read_bytes dec (Int64.to_int len))
    done;
    Buffer.contents buf
  end

let read_bytes_data dec =
  let hdr = read_header dec in
  if hdr.major <> major_bytes then failwith "Expected byte string";
  let arg = read_argument dec hdr in
  if arg >= 0L then read_bytes dec (Int64.to_int arg)
  else begin
    let buf = Buffer.create 64 in
    while
      match peek_byte dec with
      | Some 0xff ->
          ignore (read_byte dec);
          false
      | _ -> true
    do
      let hdr = read_header dec in
      if hdr.major <> major_bytes then failwith "Expected bytes chunk";
      let len = read_argument dec hdr in
      if len < 0L then failwith "Nested indefinite bytes";
      Buffer.add_string buf (read_bytes dec (Int64.to_int len))
    done;
    Buffer.contents buf
  end

let read_float dec =
  let hdr = read_header dec in
  if hdr.major <> major_simple then failwith "Expected float";
  if hdr.info = ai_2byte then decode_half (read_u16_be dec)
  else if hdr.info = ai_4byte then Int32.float_of_bits (read_u32_be dec)
  else if hdr.info = ai_8byte then Int64.float_of_bits (read_u64_be dec)
  else failwith "Expected float"

let read_bool dec =
  let hdr = read_header dec in
  if hdr.major <> major_simple then failwith "Expected boolean";
  if hdr.info = simple_false then false
  else if hdr.info = simple_true then true
  else failwith "Expected boolean"

let read_null dec =
  let hdr = read_header dec in
  if hdr.major <> major_simple || hdr.info <> simple_null then
    failwith "Expected null"

let read_undefined dec =
  let hdr = read_header dec in
  if hdr.major <> major_simple || hdr.info <> simple_undefined then
    failwith "Expected undefined"

let read_simple dec =
  let hdr = read_header dec in
  if hdr.major <> major_simple then failwith "Expected simple value";
  if hdr.info < 24 then hdr.info
  else if hdr.info = ai_1byte then read_byte dec
  else failwith "Expected simple value"

let read_array_start dec =
  let hdr = read_header dec in
  if hdr.major <> major_array then failwith "Expected array";
  let arg = read_argument dec hdr in
  if arg < 0L then None else Some (Int64.to_int arg)

let read_map_start dec =
  let hdr = read_header dec in
  if hdr.major <> major_map then failwith "Expected map";
  let arg = read_argument dec hdr in
  if arg < 0L then None else Some (Int64.to_int arg)

let read_tag dec =
  let hdr = read_header dec in
  if hdr.major <> major_tag then failwith "Expected tag";
  Int64.to_int (read_argument dec hdr)

let is_break dec = match peek_byte dec with Some 0xff -> true | _ -> false

let skip_break dec =
  let b = read_byte dec in
  if b <> break_code then failwith "Expected break"

let rec skip dec =
  let hdr = read_header dec in
  match hdr.major with
  | 0 | 1 -> ignore (read_argument dec hdr)
  | 2 | 3 ->
      let arg = read_argument dec hdr in
      if arg >= 0L then ignore (read_bytes dec (Int64.to_int arg))
      else
        while not (is_break dec) do
          skip dec
        done;
      skip_break dec
  | 4 ->
      let arg = read_argument dec hdr in
      if arg >= 0L then
        for _ = 1 to Int64.to_int arg do
          skip dec
        done
      else begin
        while not (is_break dec) do
          skip dec
        done;
        skip_break dec
      end
  | 5 ->
      let arg = read_argument dec hdr in
      if arg >= 0L then
        for _ = 1 to Int64.to_int arg do
          skip dec;
          skip dec
        done
      else begin
        while not (is_break dec) do
          skip dec;
          skip dec
        done;
        skip_break dec
      end
  | 6 ->
      ignore (read_argument dec hdr);
      skip dec
  | 7 ->
      let info = hdr.info in
      if info < 24 then ()
      else if info = ai_1byte then ignore (read_byte dec)
      else if info = ai_2byte then ignore (read_u16_be dec)
      else if info = ai_4byte then ignore (read_u32_be dec)
      else if info = ai_8byte then ignore (read_u64_be dec)
  | _ -> failwith "Invalid major type"

(* ========== CBOR Value I/O ========== *)

let rec write_cbor enc (v : Cbor.t) =
  match v with
  | Cbor.Int n -> write_bigint enc n
  | Cbor.Bytes s -> write_bytes_data enc s
  | Cbor.Text s -> write_text enc s
  | Cbor.Array items ->
      write_array_start enc (List.length items);
      List.iter (write_cbor enc) items
  | Cbor.Map pairs ->
      write_map_start enc (List.length pairs);
      List.iter
        (fun (k, v) ->
          write_cbor enc k;
          write_cbor enc v)
        pairs
  | Cbor.Tag (n, v) ->
      write_tag enc n;
      write_cbor enc v
  | Cbor.Bool b -> write_bool enc b
  | Cbor.Null -> write_null enc
  | Cbor.Undefined -> write_undefined enc
  | Cbor.Simple n -> write_simple enc n
  | Cbor.Float f -> write_float enc f

let rec read_cbor dec : Cbor.t =
  let hdr = read_header dec in
  match hdr.major with
  | 0 ->
      let arg = read_argument_z dec hdr in
      Cbor.Int arg
  | 1 ->
      let arg = read_argument_z dec hdr in
      Cbor.Int (Z.neg (Z.succ arg))
  | 2 ->
      let arg = read_argument dec hdr in
      if arg >= 0L then Cbor.Bytes (read_bytes dec (Int64.to_int arg))
      else begin
        let buf = Buffer.create 64 in
        while not (is_break dec) do
          let hdr = read_header dec in
          if hdr.major <> major_bytes then failwith "Expected bytes chunk";
          let len = read_argument dec hdr in
          if len < 0L then failwith "Nested indefinite bytes";
          Buffer.add_string buf (read_bytes dec (Int64.to_int len))
        done;
        skip_break dec;
        Cbor.Bytes (Buffer.contents buf)
      end
  | 3 ->
      let arg = read_argument dec hdr in
      if arg >= 0L then Cbor.Text (read_bytes dec (Int64.to_int arg))
      else begin
        let buf = Buffer.create 64 in
        while not (is_break dec) do
          let hdr = read_header dec in
          if hdr.major <> major_text then failwith "Expected text chunk";
          let len = read_argument dec hdr in
          if len < 0L then failwith "Nested indefinite text";
          Buffer.add_string buf (read_bytes dec (Int64.to_int len))
        done;
        skip_break dec;
        Cbor.Text (Buffer.contents buf)
      end
  | 4 ->
      let arg = read_argument dec hdr in
      if arg >= 0L then begin
        let n = Int64.to_int arg in
        let items = List.init n (fun _ -> read_cbor dec) in
        Cbor.Array items
      end
      else begin
        let items = ref [] in
        while not (is_break dec) do
          items := read_cbor dec :: !items
        done;
        skip_break dec;
        Cbor.Array (List.rev !items)
      end
  | 5 ->
      let arg = read_argument dec hdr in
      if arg >= 0L then begin
        let n = Int64.to_int arg in
        let pairs =
          List.init n (fun _ ->
              let k = read_cbor dec in
              let v = read_cbor dec in
              (k, v))
        in
        Cbor.Map pairs
      end
      else begin
        let pairs = ref [] in
        while not (is_break dec) do
          let k = read_cbor dec in
          let v = read_cbor dec in
          pairs := (k, v) :: !pairs
        done;
        skip_break dec;
        Cbor.Map (List.rev !pairs)
      end
  | 6 ->
      let tag = Int64.to_int (read_argument dec hdr) in
      let content = read_cbor dec in
      (* Handle bignum tags specially *)
      if tag = tag_positive_bignum then begin
        match content with
        | Cbor.Bytes s -> Cbor.Int (bytes_to_bigint s)
        | _ -> Cbor.Tag (tag, content)
      end
      else if tag = tag_negative_bignum then begin
        match content with
        | Cbor.Bytes s -> Cbor.Int (Z.neg (Z.succ (bytes_to_bigint s)))
        | _ -> Cbor.Tag (tag, content)
      end
      else Cbor.Tag (tag, content)
  | 7 ->
      let info = hdr.info in
      if info = simple_false then Cbor.Bool false
      else if info = simple_true then Cbor.Bool true
      else if info = simple_null then Cbor.Null
      else if info = simple_undefined then Cbor.Undefined
      else if info < 24 then Cbor.Simple info
      else if info = ai_1byte then Cbor.Simple (read_byte dec)
      else if info = ai_2byte then Cbor.Float (decode_half (read_u16_be dec))
      else if info = ai_4byte then
        Cbor.Float (Int32.float_of_bits (read_u32_be dec))
      else if info = ai_8byte then
        Cbor.Float (Int64.float_of_bits (read_u64_be dec))
      else failwith "Invalid simple/float encoding"
  | _ -> failwith "Invalid major type"
