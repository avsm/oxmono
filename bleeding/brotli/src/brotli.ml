(* Pure OCaml implementation of Brotli compression (RFC 7932) *)

(* Re-export error types from decoder *)
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

let compress_into ?(quality=1) ~src ~src_pos ~src_len ~dst ~dst_pos () =
  Brotli_encode.compress_into ~quality ~src ~src_pos ~src_len ~dst ~dst_pos ()

let decompress_into ~src ~src_pos ~src_len ~dst ~dst_pos =
  Brotli_decode.decompress_into ~src ~src_pos ~src_len ~dst ~dst_pos

(* Utilities *)

let max_compressed_length = Brotli_encode.max_compressed_length

(* Simple string API *)

let compress ?(quality = 1) s =
  let src = Bytes.unsafe_of_string s in
  let src_len = String.length s in
  let max_len = max_compressed_length src_len in
  let dst = Bytes.create max_len in
  let len = Brotli_encode.compress_into ~quality ~src ~src_pos:0 ~src_len ~dst ~dst_pos:0 () in
  Bytes.sub_string dst 0 len

let decompress s =
  try
    let src = Bytes.unsafe_of_string s in
    let src_len = String.length s in
    (* Estimate decompressed size - start with 4x input size *)
    let initial_size = max 256 (src_len * 4) in
    let dst = ref (Bytes.create initial_size) in
    let rec try_decompress size =
      try
        dst := Bytes.create size;
        let len = decompress_into ~src ~src_pos:0 ~src_len ~dst:!dst ~dst_pos:0 in
        Ok (Bytes.sub_string !dst 0 len)
      with
      | Brotli_error Output_overrun ->
        (* Double buffer size and retry *)
        if size > 256 * 1024 * 1024 then
          Error "Output too large"
        else
          try_decompress (size * 2)
    in
    try_decompress initial_size
  with
  | Brotli_error e -> Error (error_to_string e)
  | Bit_reader.End_of_input -> Error "Truncated input"

let decompress_exn s =
  match decompress s with
  | Ok result -> result
  | Error msg -> failwith msg

(* Streaming compression API *)
type streaming_encoder = Brotli_encode.streaming_encoder

let create_streaming_encoder = Brotli_encode.create_streaming_encoder
let streaming_write = Brotli_encode.streaming_write
let streaming_finish = Brotli_encode.streaming_finish
let streaming_bytes_written = Brotli_encode.streaming_bytes_written

(* Constants *)
let min_quality = 0
let max_quality = 11
let default_quality = 1
let max_window_bits = 22

(* Debug module for testing *)
module Debug = struct
  type command = Brotli_encode.command =
    | InsertCopy of { lit_start: int; lit_len: int; copy_len: int; distance: int; dist_code: int }
    | Literals of { start: int; len: int }

  let generate_commands src src_pos src_len =
    Brotli_encode.generate_commands src src_pos src_len
end
