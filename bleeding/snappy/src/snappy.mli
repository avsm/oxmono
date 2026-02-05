(** Pure OCaml implementation of Google's Snappy compression.

    Snappy is a fast compression algorithm that prioritizes speed over
    compression ratio. This implementation is designed for minimal
    memory allocation during compression and decompression.

    {1 Quick Start}

    {[
      (* Compress a string *)
      let compressed = Snappy.compress "Hello, World!"

      (* Decompress *)
      match Snappy.decompress compressed with
      | Ok original -> print_endline original
      | Error msg -> failwith msg
    ]}

    {1 Format}

    Snappy is an LZ77-type compressor with fixed, byte-oriented encoding.
    The format consists of:
    - A varint-encoded uncompressed length
    - A sequence of literals (raw bytes) and copies (back-references)

    {1 Performance}

    This implementation uses unsafe byte access in verified hot paths
    and avoids intermediate allocations where possible. *)

(** {1 Error handling} *)

(** Decompression error types *)
type error =
  | Truncated_input       (** Input ended unexpectedly *)
  | Invalid_literal_length (** Literal length encoding is invalid *)
  | Invalid_copy_offset   (** Copy offset is zero or exceeds current position *)
  | Output_overrun        (** Output would exceed declared uncompressed length *)
  | Invalid_varint        (** Varint encoding is malformed *)

val error_to_string : error -> string
(** Convert an error to a human-readable string. *)

exception Snappy_error of error
(** Exception raised by [*_exn] functions. *)

(** {1 String-based API} *)

val compress : string -> string
(** [compress s] compresses string [s] using Snappy.
    Returns the compressed data as a string. *)

val decompress : string -> (string, string) result
(** [decompress s] decompresses Snappy-compressed string [s].
    Returns [Ok original] on success or [Error msg] on failure. *)

val decompress_exn : string -> string
(** [decompress_exn s] is like {!decompress} but raises
    {!Snappy_error} on failure. *)

(** {1 Bytes-based API}

    These functions operate on [bytes] values with explicit positions
    and lengths, allowing processing of subranges without copying. *)

val compress_to_bytes : bytes -> pos:int -> len:int -> bytes
(** [compress_to_bytes src ~pos ~len] compresses [len] bytes from [src]
    starting at [pos]. Returns newly allocated compressed bytes. *)

val decompress_to_bytes : bytes -> pos:int -> len:int -> bytes
(** [decompress_to_bytes src ~pos ~len] decompresses [len] bytes from [src]
    starting at [pos]. Returns newly allocated decompressed bytes.
    @raise Snappy_error on invalid input. *)

(** {1 Low-allocation API}

    These functions write output to pre-allocated buffers, avoiding
    allocation in the output path. *)

val compress_into :
  src:bytes -> src_pos:int -> src_len:int ->
  dst:bytes -> dst_pos:int -> int
(** [compress_into ~src ~src_pos ~src_len ~dst ~dst_pos] compresses
    [src_len] bytes from [src] starting at [src_pos] into [dst] starting
    at [dst_pos].

    Returns the number of bytes written to [dst].

    The destination must have at least {!max_compressed_length}[ src_len]
    bytes available starting at [dst_pos]. *)

val decompress_into :
  src:bytes -> src_pos:int -> src_len:int ->
  dst:bytes -> dst_pos:int -> int
(** [decompress_into ~src ~src_pos ~src_len ~dst ~dst_pos] decompresses
    [src_len] bytes from [src] starting at [src_pos] into [dst] starting
    at [dst_pos].

    Returns the number of bytes written to [dst] (the uncompressed length).

    The destination must have at least {!get_uncompressed_length} bytes
    available. Use that function to query the required size.

    @raise Snappy_error on invalid input or if destination is too small. *)

(** {1 Utilities} *)

val max_compressed_length : int -> int
(** [max_compressed_length n] returns the maximum size of compressed output
    for input of [n] bytes. Use this to allocate destination buffers for
    {!compress_into}. *)

val get_uncompressed_length : bytes -> pos:int -> len:int -> int option
(** [get_uncompressed_length src ~pos ~len] reads the uncompressed length
    from the header of Snappy-compressed data without decompressing.
    Returns [None] if the header cannot be parsed. *)

val is_valid_compressed : string -> bool
(** [is_valid_compressed s] returns [true] if [s] has a valid Snappy
    header. Does not validate the entire stream. *)

(** {1 Reusable Compression Context}

    For applications that compress many small messages, using a reusable
    compression context avoids repeated allocation of the internal hash table.

    {[
      (* Create context once *)
      let ctx = Snappy.create_compress_ctx ()

      (* Reuse for multiple compressions *)
      let dst = Bytes.create (Snappy.max_compressed_length 1024) in
      let len1 = Snappy.compress_with_ctx ctx ~src:msg1 ~src_pos:0 ~src_len:1024
                   ~dst ~dst_pos:0 in
      let len2 = Snappy.compress_with_ctx ctx ~src:msg2 ~src_pos:0 ~src_len:1024
                   ~dst ~dst_pos:0 in
    ]} *)

(** Reusable compression context. *)
type compress_ctx

val create_compress_ctx : unit -> compress_ctx
(** [create_compress_ctx ()] creates a new compression context. The context
    can be reused for multiple compressions to reduce allocation overhead. *)

val compress_with_ctx :
  compress_ctx ->
  src:bytes -> src_pos:int -> src_len:int ->
  dst:bytes -> dst_pos:int -> int
(** [compress_with_ctx ctx ~src ~src_pos ~src_len ~dst ~dst_pos] compresses
    using a reusable context. Works like {!compress_into} but reuses the
    hash table between calls. Returns the number of bytes written. *)

(** {1 Varint utilities}

    These are exposed for testing and advanced use. *)

val varint_length : int -> int
(** [varint_length n] returns the number of bytes needed to encode [n]
    as a varint (1-5 bytes). *)

val encode_varint : bytes -> pos:int -> int -> int
(** [encode_varint dst ~pos value] encodes [value] as a varint into [dst]
    starting at [pos]. Returns the number of bytes written (1-5). *)

val decode_varint : bytes -> pos:int -> len:int -> int * int
(** [decode_varint src ~pos ~len] decodes a varint from [src] starting
    at [pos] with [len] bytes available.
    Returns [(value, bytes_consumed)].
    @raise Snappy_error if the varint is truncated or invalid. *)

(** {1 CRC32-C Checksum}

    CRC32-C is used by the Snappy framing format for data integrity. *)

val crc32c : bytes -> pos:int -> len:int -> int32
(** [crc32c buf ~pos ~len] computes the CRC32-C checksum of [len] bytes
    from [buf] starting at [pos]. *)

val mask_checksum : int32 -> int32
(** [mask_checksum crc] applies the Snappy framing format mask to a CRC32-C
    checksum. The masking prevents issues when checksumming data that
    contains its own checksum. *)

val unmask_checksum : int32 -> int32
(** [unmask_checksum masked] reverses the masking applied by {!mask_checksum}. *)

(** {1 Streaming API}

    The streaming API allows processing large data without holding the
    entire input or output in memory. Data is processed in 64KB blocks
    using the Snappy framing format.

    {2 Streaming Compression}

    {[
      (* Create a stream that writes to a channel *)
      let cs = Snappy.create_compress_stream ~output:(fun buf pos len ->
        output oc buf pos len
      ) in

      (* Feed data in chunks *)
      Snappy.compress_stream_feed cs data ~pos:0 ~len:(Bytes.length data);

      (* Finish the stream *)
      Snappy.compress_stream_finish cs
    ]}

    {2 Streaming Decompression}

    {[
      let ds = Snappy.create_decompress_stream ~output:(fun buf pos len ->
        Buffer.add_subbytes result buf pos len
      ) in
      Snappy.decompress_stream_feed ds compressed ~pos:0 ~len;
      if not (Snappy.decompress_stream_is_complete ds) then
        failwith "incomplete stream"
    ]} *)

(** Streaming compression state. *)
type compress_stream

(** Streaming decompression state. *)
type decompress_stream

val stream_block_size : int
(** Block size for streaming (65536 bytes). Each block is compressed
    independently and wrapped in a framing format chunk. *)

val create_compress_stream : output:(bytes -> int -> int -> unit) -> compress_stream
(** [create_compress_stream ~output] creates a new compression stream.
    The [output] callback is called with compressed chunks as they are
    produced. The callback receives the buffer, position, and length. *)

val compress_stream_feed : compress_stream -> bytes -> pos:int -> len:int -> unit
(** [compress_stream_feed cs data ~pos ~len] feeds [len] bytes of data
    from [data] starting at [pos] to the compression stream. The stream's
    output callback may be called zero or more times. *)

val compress_stream_finish : compress_stream -> unit
(** [compress_stream_finish cs] flushes any remaining data and finishes
    the compression stream. The output callback will be called for any
    remaining data. *)

val create_decompress_stream : output:(bytes -> int -> int -> unit) -> decompress_stream
(** [create_decompress_stream ~output] creates a new decompression stream.
    The [output] callback is called with decompressed data as it becomes
    available. *)

val decompress_stream_feed : decompress_stream -> bytes -> pos:int -> len:int -> unit
(** [decompress_stream_feed ds data ~pos ~len] feeds [len] bytes of
    compressed framed data to the decompression stream.
    @raise Framing_error on invalid framed data. *)

val decompress_stream_is_complete : decompress_stream -> bool
(** [decompress_stream_is_complete ds] returns [true] if the stream is
    in a valid final state (at a chunk boundary). *)

(** {1 Snappy Framing Format}

    The framing format wraps Snappy-compressed blocks with headers and
    CRC32-C checksums for streaming and integrity checking.

    {2 Format Structure}

    A framed stream consists of:
    - Stream identifier chunk (0xff): "sNaPpY" magic bytes
    - Data chunks (0x00 for compressed, 0x01 for uncompressed)
    - Optional padding chunks (0xfe)

    Each data chunk contains:
    - 1-byte chunk type
    - 3-byte little-endian chunk length
    - 4-byte masked CRC32-C of uncompressed data
    - Compressed or uncompressed data (max 65536 bytes uncompressed)

    {2 File Extension}

    The recommended file extension for framed Snappy data is [.sz]. *)

(** Framing format error types *)
type framing_error =
  | Missing_stream_identifier  (** No stream identifier at start *)
  | Invalid_stream_identifier  (** Stream identifier is malformed *)
  | Invalid_chunk_type of int  (** Unknown or reserved chunk type *)
  | Checksum_mismatch          (** CRC32-C verification failed *)
  | Chunk_too_large            (** Chunk exceeds maximum size *)
  | Decompression_failed of error  (** Inner Snappy decompression error *)

exception Framing_error of framing_error
(** Exception raised when framing format parsing fails. *)

val framing_error_to_string : framing_error -> string
(** Convert a framing error to a human-readable string. *)

val stream_identifier : string
(** The 10-byte stream identifier that starts every framed stream:
    [\xff\x06\x00\x00sNaPpY]. *)

val compress_framed : string -> string
(** [compress_framed s] compresses string [s] using the Snappy framing
    format. The result includes the stream identifier and CRC32-C
    checksums for each block. *)

val decompress_framed : string -> (string, string) result
(** [decompress_framed s] decompresses framed Snappy data.
    Returns [Ok original] on success or [Error msg] on failure.
    Verifies CRC32-C checksums for each block. *)

val is_framed_format : string -> bool
(** [is_framed_format s] returns [true] if [s] starts with the Snappy
    framing format stream identifier. *)
