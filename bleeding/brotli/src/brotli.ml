(*---------------------------------------------------------------------------
   Copyright (c) 2024 The brotli programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Brotli: Pure OCaml implementation of RFC 7932 compressed data format.

   This module provides the public API for Brotli compression and
   decompression. It delegates to internal modules:
   - Brotli_encode: compression implementation
   - Brotli_decode: decompression implementation
   - Lz77: LZ77 matching algorithms *)

(* Quality levels *)

type quality = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 | Q10 | Q11

let quality_to_int = function
  | Q0 -> 0 | Q1 -> 1 | Q2 -> 2 | Q3 -> 3 | Q4 -> 4 | Q5 -> 5
  | Q6 -> 6 | Q7 -> 7 | Q8 -> 8 | Q9 -> 9 | Q10 -> 10 | Q11 -> 11

let quality_of_int n =
  if n <= 0 then Q0
  else match n with
    | 1 -> Q1 | 2 -> Q2 | 3 -> Q3 | 4 -> Q4 | 5 -> Q5
    | 6 -> Q6 | 7 -> Q7 | 8 -> Q8 | 9 -> Q9 | 10 -> Q10
    | _ -> Q11

(* Error types - re-exported from decoder *)

type error = Brotli_decode.error =
  | Invalid_stream_header
  | Invalid_meta_block_header
  | Invalid_huffman_code
  | Invalid_distance
  | Invalid_backward_reference
  | Invalid_context_map
  | Truncated_input
  | Output_overrun

exception Brotli_error = Brotli_decode.Brotli_error

let error_to_string = Brotli_decode.error_to_string

(* Low-allocation API *)

let compress_into ?(quality = Q1) ~src ~src_pos ~src_len ~dst ~dst_pos () =
  Brotli_encode.compress_into
    ~quality:(quality_to_int quality)
    ~src ~src_pos ~src_len ~dst ~dst_pos ()

let decompress_into ~src ~src_pos ~src_len ~dst ~dst_pos =
  Brotli_decode.decompress_into ~src ~src_pos ~src_len ~dst ~dst_pos

(* Utilities *)

let max_compressed_length = Brotli_encode.max_compressed_length

(* Simple string API *)

let compress ?(quality = Q1) s =
  let src = Bytes.unsafe_of_string s in
  let src_len = String.length s in
  let max_len = max_compressed_length src_len in
  let dst = Bytes.create max_len in
  let len = compress_into ~quality ~src ~src_pos:0 ~src_len ~dst ~dst_pos:0 () in
  Bytes.sub_string dst 0 len

let decompress s =
  let src = Bytes.unsafe_of_string s in
  let src_len = String.length s in
  (* Start with 4x input size, grow as needed *)
  let initial_size = max 256 (src_len * 4) in
  let rec try_decompress size =
    let dst = Bytes.create size in
    try
      let len = decompress_into ~src ~src_pos:0 ~src_len ~dst ~dst_pos:0 in
      Bytes.sub_string dst 0 len
    with Brotli_error Output_overrun ->
      (* Double buffer size, up to 256 MB limit *)
      if size > 256 * 1024 * 1024 then
        raise (Brotli_error Output_overrun)
      else
        try_decompress (size * 2)
  in
  try_decompress initial_size

(* Streaming compression API *)

type streaming_encoder = Brotli_encode.streaming_encoder

let create_streaming_encoder ?(quality = Q1) ~dst ~dst_pos () =
  Brotli_encode.create_streaming_encoder
    ~quality:(quality_to_int quality) ~dst ~dst_pos ()

let streaming_write = Brotli_encode.streaming_write
let streaming_finish = Brotli_encode.streaming_finish
let streaming_bytes_written = Brotli_encode.streaming_bytes_written

(* Constants *)

let max_window_bits = 22

(* Debug module for testing and inspection *)

module Debug = struct
  type command = Brotli_encode.command =
    | InsertCopy of {
        lit_start: int;
        lit_len: int;
        copy_len: int;
        distance: int;
        dist_code: int;
      }
    | Literals of { start: int; len: int }

  let generate_commands src src_pos src_len =
    Brotli_encode.generate_commands src src_pos src_len
end
