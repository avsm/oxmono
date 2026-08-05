(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

type block = Cid.t * string
type header = { version : int; roots : Cid.t list }
type cid_format = Dagcbor.cid_format

type error =
  [ `Car_invalid_header of string
  | `Car_invalid_block of string
  | `Car_unsupported_version of int
  | `Car_unexpected_eof ]

let pp_error ppf = function
  | `Car_invalid_header s -> Fmt.pf ppf "invalid CAR header: %s" s
  | `Car_invalid_block s -> Fmt.pf ppf "invalid CAR block: %s" s
  | `Car_unsupported_version v -> Fmt.pf ppf "unsupported CAR version: %d" v
  | `Car_unexpected_eof -> Fmt.string ppf "unexpected end of CAR data"

type Eio.Exn.err += E of error

let () = Eio_error.register_pp pp_error (function E e -> Some e | _ -> None)
let raise_error e = Eio_error.raise_ (E e)

(* Writing *)

let write_header ?cid_format header writer =
  (* Encode header as DAG-CBOR *)
  let roots_value : Dagcbor.value =
    `List (List.map (fun cid -> `Link cid) header.roots)
  in
  let header_value : Dagcbor.value =
    `Map
      [
        ("roots", roots_value); ("version", `Int (Int64.of_int header.version));
      ]
  in
  let header_bytes = Dagcbor.encode_string ?cid_format header_value in
  (* Write varint length + header *)
  let len_bytes = Varint.encode (String.length header_bytes) in
  let slice =
    Bytes.Slice.make
      (Stdlib.Bytes.of_string len_bytes)
      ~first:0 ~length:(String.length len_bytes)
  in
  Bytes.Writer.write writer slice;
  let slice =
    Bytes.Slice.make
      (Stdlib.Bytes.of_string header_bytes)
      ~first:0
      ~length:(String.length header_bytes)
  in
  Bytes.Writer.write writer slice

let write_block ?(cid_format = `Standard) (cid, data) writer =
  let cid_bytes =
    match cid_format with
    | `Standard -> Cid.to_raw_bytes cid
    | `Atproto -> Cid.to_atproto_bytes cid
  in
  let total_len = String.length cid_bytes + String.length data in
  let len_bytes = Varint.encode total_len in
  (* Write: varint length, CID bytes, data bytes *)
  let slice =
    Bytes.Slice.make
      (Stdlib.Bytes.of_string len_bytes)
      ~first:0 ~length:(String.length len_bytes)
  in
  Bytes.Writer.write writer slice;
  let slice =
    Bytes.Slice.make
      (Stdlib.Bytes.of_string cid_bytes)
      ~first:0 ~length:(String.length cid_bytes)
  in
  Bytes.Writer.write writer slice;
  let slice =
    Bytes.Slice.make
      (Stdlib.Bytes.of_string data)
      ~first:0 ~length:(String.length data)
  in
  Bytes.Writer.write writer slice

let write ?cid_format header blocks writer =
  write_header ?cid_format header writer;
  let cid_format = Option.value cid_format ~default:`Standard in
  Seq.iter (fun block -> write_block ~cid_format block writer) blocks

let to_string ?cid_format header blocks =
  let buf = Buffer.create 4096 in
  let writer = Bytes.Writer.of_buffer buf in
  write ?cid_format header blocks writer;
  Buffer.contents buf

(* Reading *)

type reader_state = {
  reader : Bytes.Reader.t;
  mutable slice : Bytes.Slice.t;
  mutable pos : int;
  cid_format : cid_format;
}

let make_reader_state ?(cid_format = `Standard) reader =
  { reader; slice = Bytes.Slice.eod; pos = 0; cid_format }

let refill state =
  state.slice <- Bytes.Reader.read state.reader;
  state.pos <- Bytes.Slice.first state.slice

let available state =
  Bytes.Slice.length state.slice - (state.pos - Bytes.Slice.first state.slice)

let read_byte state =
  if available state = 0 then refill state;
  if available state = 0 then None
  else begin
    let b = Stdlib.Bytes.get_uint8 (Bytes.Slice.bytes state.slice) state.pos in
    state.pos <- state.pos + 1;
    Some b
  end

let read_exact state len =
  let buf = Stdlib.Bytes.create len in
  let rec fill offset remaining =
    if remaining <= 0 then ()
    else begin
      if available state = 0 then refill state;
      if available state = 0 then raise_error `Car_unexpected_eof;
      let avail = available state in
      let take = min avail remaining in
      Stdlib.Bytes.blit
        (Bytes.Slice.bytes state.slice)
        state.pos buf offset take;
      state.pos <- state.pos + take;
      fill (offset + take) (remaining - take)
    end
  in
  fill 0 len;
  Stdlib.Bytes.unsafe_to_string buf

let read_varint state =
  let rec loop acc shift =
    if shift > 63 then raise_error (`Car_invalid_block "varint overflow")
    else
      match read_byte state with
      | None when shift = 0 -> None
      | None -> raise_error `Car_unexpected_eof
      | Some b ->
          let value = b land 0x7f in
          let acc = acc lor (value lsl shift) in
          if b land 0x80 = 0 then Some acc else loop acc (shift + 7)
  in
  loop 0 0

let read_varint_exn state =
  match read_varint state with
  | Some v -> v
  | None -> raise_error `Car_unexpected_eof

let parse_header_value header_value =
  match header_value with
  | `Map entries ->
      let version =
        match List.assoc_opt "version" entries with
        | Some (`Int v) -> Int64.to_int v
        | _ -> raise_error (`Car_invalid_header "missing or invalid version")
      in
      if version <> 1 then raise_error (`Car_unsupported_version version);
      let roots =
        match List.assoc_opt "roots" entries with
        | Some (`List links) ->
            List.filter_map (function `Link cid -> Some cid | _ -> None) links
        | _ -> raise_error (`Car_invalid_header "missing or invalid roots")
      in
      { version; roots }
  | _ -> raise_error (`Car_invalid_header "header must be a map")

let read_header ?cid_format reader =
  try
    let state = make_reader_state ?cid_format reader in
    refill state;
    let header_len = read_varint_exn state in
    let header_bytes = read_exact state header_len in
    let header_value = Dagcbor.decode_string ?cid_format header_bytes in
    parse_header_value header_value
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "reading CAR header"

let read_block_internal state =
  match read_varint state with
  | None -> None
  | Some 0 -> None
  | Some block_len ->
      let block_bytes = read_exact state block_len in
      (* Parse CID from beginning of block based on format *)
      let cid, cid_len =
        match state.cid_format with
        | `Standard -> Cid.of_raw_bytes_prefix block_bytes
        | `Atproto ->
            (* AT Protocol CIDs are 35 bytes: version + codec + hash-codec + 32-byte hash.
               Per draft-holmgren-at-repository.md Section 7.2 (lines 391-392). *)
            let cid_bytes = String.sub block_bytes 0 35 in
            let cid = Cid.of_atproto_bytes cid_bytes in
            (cid, 35)
      in
      let data = String.sub block_bytes cid_len (block_len - cid_len) in
      Some (cid, data)

let read_block ?cid_format reader =
  try
    let state = make_reader_state ?cid_format reader in
    refill state;
    read_block_internal state
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "reading CAR block"

let read ?cid_format reader =
  try
    let state = make_reader_state ?cid_format reader in
    refill state;
    let header_len = read_varint_exn state in
    let header_bytes = read_exact state header_len in
    let header_value = Dagcbor.decode_string ?cid_format header_bytes in
    let header = parse_header_value header_value in
    let blocks =
      Seq.unfold
        (fun () ->
          Option.map (fun block -> (block, ())) (read_block_internal state))
        ()
    in
    (header, blocks)
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "reading CAR file"

let of_string ?cid_format s =
  try
    let reader = Bytes.Reader.of_string s in
    let header, blocks = read ?cid_format reader in
    (header, List.of_seq blocks)
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "parsing CAR from string (%d bytes)"
      (String.length s)

(* Utilities *)

let import ?cid_format (store : Blockstore.writable) reader =
  let header, blocks = read ?cid_format reader in
  Seq.iter (fun (cid, data) -> store#put cid data) blocks;
  header

let export ?cid_format ~root (store : Blockstore.readable) cids =
  let header = { version = 1; roots = [ root ] } in
  let blocks =
    Seq.filter_map
      (fun cid -> Option.map (fun data -> (cid, data)) (store#get cid))
      cids
  in
  to_string ?cid_format header blocks
