(** Blosc - high performance meta-compressor with shuffling

    Direct C bindings with OxCaml optimizations for unboxed/untagged values. *)

exception Error of string

(** Shuffle modes *)
type shuffle = NoShuffle | Shuffle | BitShuffle

let shuffle_to_int = function
  | NoShuffle -> 0
  | Shuffle -> 1
  | BitShuffle -> 2

(** Compressor names *)
type compressor = LZ4 | LZ4HC | BloscLZ | Zstd | Snappy | Zlib

let compressor_to_string = function
  | LZ4 -> "lz4"
  | LZ4HC -> "lz4hc"
  | BloscLZ -> "blosclz"
  | Zstd -> "zstd"
  | Snappy -> "snappy"
  | Zlib -> "zlib"

(* Check for blosc errors and raise *)
let[@inline] check r =
  if Blosc_stubs.is_error r then
    raise (Error (Printf.sprintf "blosc: operation failed (error code %d)" r))

(* BLOSC_MAX_OVERHEAD constant *)
let max_overhead = Blosc_stubs.max_overhead ()

(* Compression bound: src_size + BLOSC_MAX_OVERHEAD *)
let compress_bound src_size = src_size + max_overhead

(* Get decompressed (original) size from compressed data header *)
let get_decompressed_size s =
  let nbytes = Blosc_stubs.cbuffer_sizes s in
  if nbytes = 0 then
    raise (Error "blosc: invalid compressed data (nbytes=0)");
  nbytes

(* Get compressor library name from compressed data *)
let get_complib s =
  Blosc_stubs.cbuffer_complib s

(* Compress string, returning compressed string *)
let compress ~cname ~clevel ~shuffle ~typesize ~blocksize s =
  let src_len = String.length s in
  if src_len = 0 then ""
  else begin
    let dst_size = compress_bound src_len in
    let dst = Bytes.create dst_size in
    let r = Blosc_stubs.compress dst dst_size s src_len
              clevel (shuffle_to_int shuffle) typesize
              (compressor_to_string cname) blocksize in
    check r;
    Bytes.sub_string dst 0 r
  end

(* Compress string into provided buffer, returning bytes written *)
let compress_to ~cname ~clevel ~shuffle ~typesize ~blocksize s (dst : bytes) : int =
  let src_len = String.length s in
  if src_len = 0 then 0
  else begin
    let dst_size = Bytes.length dst in
    let r = Blosc_stubs.compress dst dst_size s src_len
              clevel (shuffle_to_int shuffle) typesize
              (compressor_to_string cname) blocksize in
    check r;
    r
  end

(* Decompress string, extracting size from header *)
let decompress s =
  let src_len = String.length s in
  if src_len = 0 then ""
  else begin
    let nbytes = get_decompressed_size s in
    let dst = Bytes.create nbytes in
    let r = Blosc_stubs.decompress dst nbytes s src_len in
    check r;
    Bytes.sub_string dst 0 r
  end

(* Decompress string into provided buffer, returning bytes written *)
let decompress_to s (dst : bytes) : int =
  let src_len = String.length s in
  if src_len = 0 then 0
  else begin
    let dst_size = Bytes.length dst in
    let r = Blosc_stubs.decompress dst dst_size s src_len in
    check r;
    r
  end
