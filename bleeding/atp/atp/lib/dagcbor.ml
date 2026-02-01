(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

(* CBOR constants *)
let major_uint = 0
let major_nint = 1
let major_bytes = 2
let major_text = 3
let major_array = 4
let major_map = 5
let _major_tag = 6
let major_simple = 7
let simple_false = 20
let simple_true = 21
let simple_null = 22
let ai_1byte = 24
let ai_2byte = 25
let ai_4byte = 26
let ai_8byte = 27
let ai_indefinite = 31
let tag_cid = 42

(* IPLD value type *)
type value =
  [ `Null
  | `Bool of bool
  | `Int of int64
  | `Float of float
  | `String of string
  | `Bytes of string
  | `Link of Cid.t
  | `List of value list
  | `Map of (string * value) list ]

(* CID encoding format for Link values *)
type cid_format = [ `Standard | `Atproto ]

(* Errors *)
type error =
  [ `Dagcbor_invalid_tag of int
  | `Dagcbor_invalid_map_key
  | `Dagcbor_invalid_float of string
  | `Dagcbor_unsorted_keys
  | `Dagcbor_non_canonical_int
  | `Dagcbor_non_canonical_float
  | `Dagcbor_indefinite_length
  | `Dagcbor_invalid_cid of string
  | `Dagcbor_trailing_data
  | `Dagcbor_unexpected_eof
  | `Dagcbor_decode_error of string ]

let pp_error ppf = function
  | `Dagcbor_invalid_tag n ->
      Fmt.pf ppf "invalid CBOR tag %d (only tag 42 allowed)" n
  | `Dagcbor_invalid_map_key -> Fmt.string ppf "map keys must be strings"
  | `Dagcbor_invalid_float s -> Fmt.pf ppf "invalid float: %s" s
  | `Dagcbor_unsorted_keys -> Fmt.string ppf "map keys not in canonical order"
  | `Dagcbor_non_canonical_int ->
      Fmt.string ppf "integer not in shortest encoding"
  | `Dagcbor_non_canonical_float -> Fmt.string ppf "float must be 64-bit"
  | `Dagcbor_indefinite_length ->
      Fmt.string ppf "indefinite-length encoding not allowed"
  | `Dagcbor_invalid_cid s -> Fmt.pf ppf "invalid CID: %s" s
  | `Dagcbor_trailing_data -> Fmt.string ppf "trailing data after value"
  | `Dagcbor_unexpected_eof -> Fmt.string ppf "unexpected end of data"
  | `Dagcbor_decode_error s -> Fmt.pf ppf "decode error: %s" s

type Eio.Exn.err += E of error

let () = Eio_error.register_pp pp_error (function E e -> Some e | _ -> None)
let raise_error e = Eio_error.raise_ (E e)

(* Encoder *)
type encoder = {
  writer : Bytes.Writer.t;
  buf : bytes;
  mutable buf_pos : int;
  cid_format : cid_format;
}

let encoder_buf_size = 4096

let make_encoder ?(cid_format = `Standard) writer =
  {
    writer;
    buf = Stdlib.Bytes.create encoder_buf_size;
    buf_pos = 0;
    cid_format;
  }

let flush_encoder e =
  if e.buf_pos > 0 then begin
    let slice = Bytes.Slice.make e.buf ~first:0 ~length:e.buf_pos in
    Bytes.Writer.write e.writer slice;
    e.buf_pos <- 0
  end

let ensure_space e n =
  if e.buf_pos + n > Stdlib.Bytes.length e.buf then flush_encoder e

let write_byte e b =
  ensure_space e 1;
  Stdlib.Bytes.set_uint8 e.buf e.buf_pos b;
  e.buf_pos <- e.buf_pos + 1

let write_bytes e bs =
  let len = String.length bs in
  ensure_space e len;
  Stdlib.Bytes.blit_string bs 0 e.buf e.buf_pos len;
  e.buf_pos <- e.buf_pos + len

let write_u16_be e v =
  ensure_space e 2;
  Stdlib.Bytes.set_uint16_be e.buf e.buf_pos v;
  e.buf_pos <- e.buf_pos + 2

let write_u32_be e v =
  ensure_space e 4;
  Stdlib.Bytes.set_int32_be e.buf e.buf_pos v;
  e.buf_pos <- e.buf_pos + 4

let write_u64_be e v =
  ensure_space e 8;
  Stdlib.Bytes.set_int64_be e.buf e.buf_pos v;
  e.buf_pos <- e.buf_pos + 8

(* Write CBOR type header with shortest encoding *)
let write_type_arg e major arg =
  let h = major lsl 5 in
  if arg <= 23 then write_byte e (h lor arg)
  else if arg <= 0xff then begin
    write_byte e (h lor ai_1byte);
    write_byte e arg
  end
  else if arg <= 0xffff then begin
    write_byte e (h lor ai_2byte);
    write_u16_be e arg
  end
  else if Int64.(compare (of_int arg) 0xffffffffL) <= 0 then begin
    write_byte e (h lor ai_4byte);
    write_u32_be e (Int32.of_int arg)
  end
  else begin
    write_byte e (h lor ai_8byte);
    write_u64_be e (Int64.of_int arg)
  end

let write_type_arg64 e major arg =
  let h = major lsl 5 in
  if Int64.compare arg 24L < 0 then write_byte e (h lor Int64.to_int arg)
  else if Int64.compare arg 0x100L < 0 then begin
    write_byte e (h lor ai_1byte);
    write_byte e (Int64.to_int arg)
  end
  else if Int64.compare arg 0x10000L < 0 then begin
    write_byte e (h lor ai_2byte);
    write_u16_be e (Int64.to_int arg)
  end
  else if Int64.compare arg 0x100000000L < 0 then begin
    write_byte e (h lor ai_4byte);
    write_u32_be e (Int64.to_int32 arg)
  end
  else begin
    write_byte e (h lor ai_8byte);
    write_u64_be e arg
  end

(* Canonical map key comparison: sort by length first, then lexicographically *)
let canonical_key_compare k1 k2 =
  let len1 = String.length k1 and len2 = String.length k2 in
  if len1 <> len2 then compare len1 len2 else String.compare k1 k2

let canonical_entry_compare (k1, _) (k2, _) = canonical_key_compare k1 k2

(* Validate no NaN/Infinity *)
let rec validate_value : value -> unit = function
  | `Float f when Float.is_nan f -> raise_error (`Dagcbor_invalid_float "NaN")
  | `Float f when Float.is_infinite f ->
      raise_error
        (`Dagcbor_invalid_float (if f > 0. then "Infinity" else "-Infinity"))
  | `List items -> List.iter validate_value items
  | `Map entries -> List.iter (fun (_, v) -> validate_value v) entries
  | _ -> ()

(* Encode IPLD value *)
let rec encode_value e (v : value) =
  match v with
  | `Null -> write_byte e ((major_simple lsl 5) lor simple_null)
  | `Bool b ->
      let v = if b then simple_true else simple_false in
      write_byte e ((major_simple lsl 5) lor v)
  | `Int i ->
      if Int64.compare i 0L >= 0 then write_type_arg64 e major_uint i
      else write_type_arg64 e major_nint (Int64.sub (Int64.neg i) 1L)
  | `Float f ->
      write_byte e ((major_simple lsl 5) lor ai_8byte);
      write_u64_be e (Int64.bits_of_float f)
  | `String s ->
      write_type_arg e major_text (String.length s);
      write_bytes e s
  | `Bytes b ->
      write_type_arg e major_bytes (String.length b);
      write_bytes e b
  | `Link cid ->
      (* CID: tag 42 + byte string containing CID *)
      write_byte e 0xd8;
      write_byte e tag_cid;
      let cid_bytes =
        match e.cid_format with
        | `Standard ->
            (* Standard DAG-CBOR: 0x00 multibase prefix + full CID *)
            "\x00" ^ Cid.to_raw_bytes cid
        | `Atproto ->
            (* AT Protocol: simplified format without multibase or length byte.
               Per draft-holmgren-at-repository.md Section 6.5 (line 368). *)
            Cid.to_atproto_bytes cid
      in
      write_type_arg e major_bytes (String.length cid_bytes);
      write_bytes e cid_bytes
  | `List items ->
      write_type_arg e major_array (List.length items);
      List.iter (encode_value e) items
  | `Map entries ->
      let sorted = List.sort canonical_entry_compare entries in
      write_type_arg e major_map (List.length sorted);
      List.iter
        (fun (k, v) ->
          write_type_arg e major_text (String.length k);
          write_bytes e k;
          encode_value e v)
        sorted

let encode ?cid_format v ~eod writer =
  validate_value v;
  let e = make_encoder ?cid_format writer in
  encode_value e v;
  flush_encoder e;
  if eod then Bytes.Writer.write e.writer Bytes.Slice.eod

let encode_string ?cid_format v =
  let buf = Buffer.create 256 in
  let w = Bytes.Writer.of_buffer buf in
  encode ?cid_format v ~eod:false w;
  Buffer.contents buf

(* Decoder *)
type decoder = {
  reader : Bytes.Reader.t;
  mutable slice : Bytes.Slice.t;
  mutable pos : int;
  mutable byte_count : int;
  strict : bool;
  cid_format : cid_format;
}

let make_decoder ?(strict = true) ?(cid_format = `Standard) reader =
  {
    reader;
    slice = Bytes.Slice.eod;
    pos = 0;
    byte_count = 0;
    strict;
    cid_format;
  }

let decoder_refill d =
  d.slice <- Bytes.Reader.read d.reader;
  d.pos <- Bytes.Slice.first d.slice

let available d =
  Bytes.Slice.length d.slice - (d.pos - Bytes.Slice.first d.slice)

let read_byte d =
  if available d = 0 then decoder_refill d;
  if available d = 0 then raise_error `Dagcbor_unexpected_eof;
  let b = Stdlib.Bytes.get_uint8 (Bytes.Slice.bytes d.slice) d.pos in
  d.pos <- d.pos + 1;
  d.byte_count <- d.byte_count + 1;
  b

let read_u16_be d =
  if available d < 2 then begin
    let b1 = read_byte d in
    let b2 = read_byte d in
    (b1 lsl 8) lor b2
  end
  else begin
    let v = Stdlib.Bytes.get_uint16_be (Bytes.Slice.bytes d.slice) d.pos in
    d.pos <- d.pos + 2;
    d.byte_count <- d.byte_count + 2;
    v
  end

let read_u32_be d =
  if available d < 4 then begin
    let b1 = read_byte d in
    let b2 = read_byte d in
    let b3 = read_byte d in
    let b4 = read_byte d in
    (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4
  end
  else begin
    let v = Stdlib.Bytes.get_int32_be (Bytes.Slice.bytes d.slice) d.pos in
    d.pos <- d.pos + 4;
    d.byte_count <- d.byte_count + 4;
    Int32.to_int v
  end

let read_u64_be d =
  if available d < 8 then begin
    let hi = Int64.of_int (read_u32_be d) in
    let lo = Int64.of_int32 (Int32.of_int (read_u32_be d)) in
    Int64.(logor (shift_left hi 32) (logand lo 0xffffffffL))
  end
  else begin
    let v = Stdlib.Bytes.get_int64_be (Bytes.Slice.bytes d.slice) d.pos in
    d.pos <- d.pos + 8;
    d.byte_count <- d.byte_count + 8;
    v
  end

let read_bytes_to_string d len =
  let buf = Stdlib.Bytes.create len in
  let rec fill offset remaining =
    if remaining <= 0 then ()
    else begin
      if available d = 0 then decoder_refill d;
      let avail = available d in
      if avail = 0 then raise_error `Dagcbor_unexpected_eof;
      let take = min avail remaining in
      Stdlib.Bytes.blit (Bytes.Slice.bytes d.slice) d.pos buf offset take;
      d.pos <- d.pos + take;
      d.byte_count <- d.byte_count + take;
      fill (offset + take) (remaining - take)
    end
  in
  fill 0 len;
  Stdlib.Bytes.unsafe_to_string buf

(* Read CBOR argument, checking for canonical encoding if strict *)
let read_arg d ai =
  let check_canonical min v =
    if d.strict && v <= min then raise_error `Dagcbor_non_canonical_int
  in
  match ai with
  | _ when ai <= 23 -> ai
  | _ when ai = ai_1byte ->
      let v = read_byte d in
      if d.strict && v < 24 then raise_error `Dagcbor_non_canonical_int;
      v
  | _ when ai = ai_2byte ->
      let v = read_u16_be d in
      check_canonical 0xff v;
      v
  | _ when ai = ai_4byte ->
      let v = read_u32_be d in
      check_canonical 0xffff v;
      v
  | _ when ai = ai_8byte ->
      let v = Int64.to_int (read_u64_be d) in
      check_canonical 0xffffffff v;
      v
  | _ when ai = ai_indefinite -> raise_error `Dagcbor_indefinite_length
  | _ ->
      raise_error
        (`Dagcbor_decode_error (Printf.sprintf "invalid additional info: %d" ai))

let read_arg64 d ai =
  let check_canonical64 min v =
    if d.strict && Int64.compare v min <= 0 then
      raise_error `Dagcbor_non_canonical_int
  in
  match ai with
  | _ when ai <= 23 -> Int64.of_int ai
  | _ when ai = ai_1byte ->
      let v = read_byte d in
      if d.strict && v < 24 then raise_error `Dagcbor_non_canonical_int;
      Int64.of_int v
  | _ when ai = ai_2byte ->
      let v = read_u16_be d in
      check_canonical64 0xffL (Int64.of_int v);
      Int64.of_int v
  | _ when ai = ai_4byte ->
      let v = read_u32_be d in
      check_canonical64 0x10000L (Int64.of_int v);
      Int64.of_int v
  | _ when ai = ai_8byte ->
      let v = read_u64_be d in
      check_canonical64 0x100000000L v;
      v
  | _ when ai = ai_indefinite -> raise_error `Dagcbor_indefinite_length
  | _ ->
      raise_error
        (`Dagcbor_decode_error (Printf.sprintf "invalid additional info: %d" ai))

(* Check if map keys are in canonical order *)
let check_key_order d prev_key new_key =
  if d.strict then begin
    let cmp = canonical_key_compare prev_key new_key in
    if cmp >= 0 then raise_error `Dagcbor_unsorted_keys
  end

(* Decode IPLD value *)
let rec decode_value d : value =
  let b = read_byte d in
  let major = b lsr 5 in
  let ai = b land 0x1f in

  if ai = ai_indefinite && major <> major_simple then
    raise_error `Dagcbor_indefinite_length;

  match major with
  | 0 -> `Int (read_arg64 d ai)
  | 1 ->
      let v = read_arg64 d ai in
      `Int (Int64.sub (Int64.neg v) 1L)
  | 2 ->
      let len = read_arg d ai in
      `Bytes (read_bytes_to_string d len)
  | 3 ->
      let len = read_arg d ai in
      `String (read_bytes_to_string d len)
  | 4 ->
      let len = read_arg d ai in
      let items = List.init len (fun _ -> decode_value d) in
      `List items
  | 5 ->
      let len = read_arg d ai in
      let rec read_entries i prev_key acc =
        if i >= len then `Map (List.rev acc)
        else begin
          let kb = read_byte d in
          let kmajor = kb lsr 5 in
          let kai = kb land 0x1f in
          if kmajor <> major_text then raise_error `Dagcbor_invalid_map_key;
          let klen = read_arg d kai in
          let key = read_bytes_to_string d klen in
          (match prev_key with
          | Some pk -> check_key_order d pk key
          | None -> ());
          let value = decode_value d in
          read_entries (i + 1) (Some key) ((key, value) :: acc)
        end
      in
      read_entries 0 None []
  | 6 -> (
      let tag = read_arg d ai in
      if tag <> tag_cid then raise_error (`Dagcbor_invalid_tag tag);
      let vb = read_byte d in
      let vmajor = vb lsr 5 in
      let vai = vb land 0x1f in
      if vmajor <> major_bytes then
        raise_error (`Dagcbor_invalid_cid "must be byte string");
      let len = read_arg d vai in
      match d.cid_format with
      | `Standard ->
          (* Standard DAG-CBOR: expect 0x00 multibase prefix + full CID *)
          if len < 1 then raise_error (`Dagcbor_invalid_cid "too short");
          let prefix = read_byte d in
          if prefix <> 0x00 then
            raise_error
              (`Dagcbor_invalid_cid
                 (Printf.sprintf "expected 0x00 prefix, got 0x%02x" prefix));
          let cid_bytes = read_bytes_to_string d (len - 1) in
          `Link (Cid.of_raw_bytes cid_bytes)
      | `Atproto ->
          (* AT Protocol: simplified format without multibase or length byte.
              Per draft-holmgren-at-repository.md Section 6.5 (line 368). *)
          if len <> 35 then
            raise_error
              (`Dagcbor_invalid_cid
                 (Printf.sprintf "AT Protocol CID must be 35 bytes, got %d" len));
          let cid_bytes = read_bytes_to_string d len in
          `Link (Cid.of_atproto_bytes cid_bytes))
  | 7 -> (
      match ai with
      | 20 -> `Bool false
      | 21 -> `Bool true
      | 22 -> `Null
      | 23 -> raise_error (`Dagcbor_invalid_float "undefined")
      | 25 ->
          if d.strict then raise_error `Dagcbor_non_canonical_float;
          let bits = read_u16_be d in
          let sign = if bits land 0x8000 <> 0 then -1.0 else 1.0 in
          let exp = (bits lsr 10) land 0x1f in
          let mant = bits land 0x3ff in
          let f =
            if exp = 0 then sign *. Float.ldexp (float_of_int mant) (-24)
            else if exp = 31 then
              if mant = 0 then sign *. Float.infinity else Float.nan
            else sign *. Float.ldexp (float_of_int (mant + 1024)) (exp - 25)
          in
          if Float.is_nan f then raise_error (`Dagcbor_invalid_float "NaN");
          if Float.is_infinite f then
            raise_error (`Dagcbor_invalid_float "Infinity");
          `Float f
      | 26 ->
          if d.strict then raise_error `Dagcbor_non_canonical_float;
          let bits = Int32.of_int (read_u32_be d) in
          let f = Int32.float_of_bits bits in
          if Float.is_nan f then raise_error (`Dagcbor_invalid_float "NaN");
          if Float.is_infinite f then
            raise_error (`Dagcbor_invalid_float "Infinity");
          `Float f
      | 27 ->
          let bits = read_u64_be d in
          let f = Int64.float_of_bits bits in
          if Float.is_nan f then raise_error (`Dagcbor_invalid_float "NaN");
          if Float.is_infinite f then
            raise_error (`Dagcbor_invalid_float "Infinity");
          `Float f
      | _ ->
          raise_error
            (`Dagcbor_decode_error
               (Printf.sprintf "simple value %d not allowed" ai)))
  | _ ->
      raise_error
        (`Dagcbor_decode_error (Printf.sprintf "unknown major type: %d" major))

let decode ?(strict = true) ?cid_format reader =
  try
    let d = make_decoder ~strict ?cid_format reader in
    decoder_refill d;
    let v = decode_value d in
    if d.strict then begin
      if available d > 0 then raise_error `Dagcbor_trailing_data;
      decoder_refill d;
      if available d > 0 then raise_error `Dagcbor_trailing_data
    end;
    v
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "decoding DAG-CBOR"

let decode_string ?(strict = true) ?cid_format s =
  try
    let r = Bytes.Reader.of_string s in
    decode ~strict ?cid_format r
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt
      "decoding DAG-CBOR from string (%d bytes)" (String.length s)

(* Equality and comparison *)
let rec equal (a : value) (b : value) =
  match (a, b) with
  | `Null, `Null -> true
  | `Bool a, `Bool b -> a = b
  | `Int a, `Int b -> Int64.equal a b
  | `Float a, `Float b -> Float.equal a b
  | `String a, `String b -> String.equal a b
  | `Bytes a, `Bytes b -> String.equal a b
  | `Link a, `Link b -> Cid.equal a b
  | `List a, `List b -> List.equal equal a b
  | `Map a, `Map b ->
      List.length a = List.length b
      && List.for_all2 (fun (k1, v1) (k2, v2) -> k1 = k2 && equal v1 v2) a b
  | _ -> false

let rec compare (a : value) (b : value) =
  match (a, b) with
  | `Null, `Null -> 0
  | `Null, _ -> -1
  | _, `Null -> 1
  | `Bool a, `Bool b -> Stdlib.compare a b
  | `Bool _, _ -> -1
  | _, `Bool _ -> 1
  | `Int a, `Int b -> Int64.compare a b
  | `Int _, _ -> -1
  | _, `Int _ -> 1
  | `Float a, `Float b -> Float.compare a b
  | `Float _, _ -> -1
  | _, `Float _ -> 1
  | `String a, `String b -> String.compare a b
  | `String _, _ -> -1
  | _, `String _ -> 1
  | `Bytes a, `Bytes b -> String.compare a b
  | `Bytes _, _ -> -1
  | _, `Bytes _ -> 1
  | `Link a, `Link b -> Cid.compare a b
  | `Link _, _ -> -1
  | _, `Link _ -> 1
  | `List a, `List b -> List.compare compare a b
  | `List _, _ -> -1
  | _, `List _ -> 1
  | `Map a, `Map b ->
      let cmp_entry (k1, v1) (k2, v2) =
        let c = String.compare k1 k2 in
        if c <> 0 then c else compare v1 v2
      in
      List.compare cmp_entry a b

(* Pretty printer *)
let rec pp ppf (v : value) =
  match v with
  | `Null -> Fmt.string ppf "null"
  | `Bool b -> Fmt.bool ppf b
  | `Int i -> Fmt.pf ppf "%Ld" i
  | `Float f -> Fmt.float ppf f
  | `String s -> Fmt.pf ppf "%S" s
  | `Bytes b ->
      Fmt.pf ppf "h'";
      String.iter (fun c -> Fmt.pf ppf "%02x" (Char.code c)) b;
      Fmt.pf ppf "'"
  | `Link cid -> Cid.pp ppf cid
  | `List items -> Fmt.pf ppf "[%a]" Fmt.(list ~sep:(any ", ") pp) items
  | `Map entries ->
      Fmt.pf ppf "{%a}"
        Fmt.(list ~sep:(any ", ") (pair ~sep:(any ": ") (quote string) pp))
        entries

(* JSON interop *)

(* Result helpers for cleaner error handling *)
let ( let* ) = Result.bind
let ( let+ ) x f = Result.map f x

let result_map_list f items =
  let+ items =
    List.fold_left
      (fun acc x ->
        let* acc = acc in
        let+ v = f x in
        v :: acc)
      (Ok []) items
  in
  List.rev items

let rec of_json (j : Jsont.json) : (value, string) result =
  match j with
  | Jsont.Null _ -> Ok `Null
  | Jsont.Bool (b, _) -> Ok (`Bool b)
  | Jsont.Number (f, _) ->
      if Float.is_nan f then Error "NaN not allowed in IPLD"
      else if Float.is_infinite f then Error "Infinity not allowed in IPLD"
      else
        let i = Int64.of_float f in
        if Float.equal (Int64.to_float i) f then Ok (`Int i) else Ok (`Float f)
  | Jsont.String (s, _) -> Ok (`String s)
  | Jsont.Array (items, _) ->
      let+ items = result_map_list of_json items in
      `List items
  | Jsont.Object (mems, _) ->
      let+ entries =
        result_map_list
          (fun ((name, _), v) ->
            let+ v = of_json v in
            (name, v))
          mems
      in
      `Map (List.sort canonical_entry_compare entries)

let rec to_json (v : value) : (Jsont.json, string) result =
  match v with
  | `Null -> Ok (Jsont.Json.null ())
  | `Bool b -> Ok (Jsont.Json.bool b)
  | `Int i -> Ok (Jsont.Json.number (Int64.to_float i))
  | `Float f -> Ok (Jsont.Json.number f)
  | `String s -> Ok (Jsont.Json.string s)
  | `Bytes _ -> Error "Bytes cannot be represented in JSON"
  | `Link _ -> Error "Link (CID) cannot be represented in JSON"
  | `List items ->
      let+ items = result_map_list to_json items in
      Jsont.Json.list items
  | `Map entries ->
      let+ mems =
        result_map_list
          (fun (k, v) ->
            let+ v = to_json v in
            Jsont.Json.mem (Jsont.Json.name k) v)
          entries
      in
      Jsont.Json.object' mems
