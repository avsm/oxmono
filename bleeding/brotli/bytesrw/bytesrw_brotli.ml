(*---------------------------------------------------------------------------
   Copyright (c) 2024 The brotli programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Bytesrw integration for Brotli compression (RFC 7932)

   This implementation provides streaming compression and decompression
   using the Brotli format. Both compression and decompression buffer
   the entire input to achieve optimal compression ratios. *)

open Bytesrw

(* Error handling *)

type Bytes.Stream.error += Error of string

let format_error =
  let case msg = Error msg in
  let message = function Error msg -> msg | _ -> assert false in
  Bytes.Stream.make_format_error ~format:"brotli" ~case ~message

let error = Bytes.Stream.error format_error
let reader_error = Bytes.Reader.error format_error
let writer_error = Bytes.Writer.error format_error

(* Library parameters *)

let default_slice_length = 65536  (* 64KB *)

type quality = Brotli.quality
let default_quality = Brotli.Q1
let no_compression = Brotli.Q0
let best_speed = Brotli.Q1
let best_compression = Brotli.Q11

(* Decompress reads - buffers entire input, decompresses, then emits slices *)

let decompress_reads () ?pos ?(slice_length = default_slice_length) r =
  (* Buffer all input first *)
  let input_buffer = Buffer.create slice_length in
  let rec read_all () =
    let slice = Bytes.Reader.read r in
    if Bytes.Slice.is_eod slice then ()
    else begin
      Bytes.Slice.add_to_buffer input_buffer slice;
      read_all ()
    end
  in
  read_all ();

  (* Decompress using low-allocation API *)
  let input_len = Buffer.length input_buffer in
  let input = Bytes.unsafe_of_string (Buffer.contents input_buffer) in
  (* Start with 4x input size estimate, grow if needed *)
  let initial_size = max 256 (input_len * 4) in
  let output = ref (Bytes.create initial_size) in
  let rec try_decompress size =
    output := Bytes.create size;
    try
      Brotli.decompress_into ~src:input ~src_pos:0 ~src_len:input_len
        ~dst:!output ~dst_pos:0
    with
    | Brotli.Brotli_error Brotli.Output_overrun ->
      if size > 256 * 1024 * 1024 then
        reader_error r "Output too large"
      else
        try_decompress (size * 2)
  in
  let decompressed_len = try_decompress initial_size in

  (* Create a reader from the decompressed data *)
  let output_pos = ref 0 in

  let read () =
    if !output_pos >= decompressed_len then Bytes.Slice.eod
    else begin
      let len = min slice_length (decompressed_len - !output_pos) in
      let slice = Bytes.Slice.make !output ~first:!output_pos ~length:len in
      output_pos := !output_pos + len;
      slice
    end
  in
  Bytes.Reader.make ?pos ~slice_length read

(* Decompress writes - buffers input, decompresses on eod *)

let decompress_writes () ?pos ?(slice_length = default_slice_length) ~eod w =
  let input_buffer = Buffer.create slice_length in

  let write = function
    | slice when Bytes.Slice.is_eod slice ->
        (* Decompress using low-allocation API *)
        let input_len = Buffer.length input_buffer in
        let input = Bytes.unsafe_of_string (Buffer.contents input_buffer) in
        let initial_size = max 256 (input_len * 4) in
        let output = ref (Bytes.create initial_size) in
        let rec try_decompress size =
          output := Bytes.create size;
          try
            Brotli.decompress_into ~src:input ~src_pos:0 ~src_len:input_len
              ~dst:!output ~dst_pos:0
          with
          | Brotli.Brotli_error Brotli.Output_overrun ->
            if size > 256 * 1024 * 1024 then
              writer_error w "Output too large"
            else
              try_decompress (size * 2)
        in
        let decompressed_len = try_decompress initial_size in
        Bytes.Writer.write_string w (Bytes.sub_string !output 0 decompressed_len);
        if eod then Bytes.Writer.write_eod w
    | slice ->
        Bytes.Slice.add_to_buffer input_buffer slice
  in
  Bytes.Writer.make ?pos ~slice_length write

(* Compress reads - buffers entire input, compresses, then emits slices *)

let compress_reads ?(quality = default_quality) ()
    ?pos ?(slice_length = default_slice_length) r
  =
  (* Buffer all input first - this allows better compression *)
  let input_buffer = Buffer.create slice_length in
  let rec read_all () =
    let slice = Bytes.Reader.read r in
    if Bytes.Slice.is_eod slice then ()
    else begin
      Bytes.Slice.add_to_buffer input_buffer slice;
      read_all ()
    end
  in
  read_all ();

  (* Compress using low-allocation API *)
  let input_len = Buffer.length input_buffer in
  let input = Bytes.unsafe_of_string (Buffer.contents input_buffer) in
  let max_len = Brotli.max_compressed_length input_len in
  let compressed = Bytes.create max_len in
  let compressed_len =
    try Brotli.compress_into ~quality ~src:input ~src_pos:0 ~src_len:input_len
          ~dst:compressed ~dst_pos:0 ()
    with exn -> error (Printexc.to_string exn)
  in

  (* Create a reader from the compressed data *)
  let output_pos = ref 0 in

  let read () =
    if !output_pos >= compressed_len then Bytes.Slice.eod
    else begin
      let len = min slice_length (compressed_len - !output_pos) in
      let slice = Bytes.Slice.make compressed ~first:!output_pos ~length:len in
      output_pos := !output_pos + len;
      slice
    end
  in
  Bytes.Reader.make ?pos ~slice_length read

(* Compress writes - buffers input, compresses on eod *)

let compress_writes ?(quality = default_quality) ()
    ?pos ?(slice_length = default_slice_length) ~eod w
  =
  let input_buffer = Buffer.create slice_length in

  let write = function
    | slice when Bytes.Slice.is_eod slice ->
        (* Compress using low-allocation API *)
        let input_len = Buffer.length input_buffer in
        let input = Bytes.unsafe_of_string (Buffer.contents input_buffer) in
        let max_len = Brotli.max_compressed_length input_len in
        let compressed = Bytes.create max_len in
        let compressed_len =
          try Brotli.compress_into ~quality ~src:input ~src_pos:0 ~src_len:input_len
                ~dst:compressed ~dst_pos:0 ()
          with exn -> writer_error w (Printexc.to_string exn)
        in
        Bytes.Writer.write_string w (Bytes.sub_string compressed 0 compressed_len);
        if eod then Bytes.Writer.write_eod w
    | slice ->
        Bytes.Slice.add_to_buffer input_buffer slice
  in
  Bytes.Writer.make ?pos ~slice_length write
