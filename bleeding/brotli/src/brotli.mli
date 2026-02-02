(*---------------------------------------------------------------------------
   Copyright (c) 2024 The brotli programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Pure OCaml Brotli compression and decompression.

    This module implements the {{:https://www.rfc-editor.org/rfc/rfc7932}Brotli}
    compressed data format as specified in RFC 7932.

    Brotli is a general-purpose lossless compression algorithm that uses
    LZ77 matching, Huffman coding, and 2nd order context modeling, with a
    pre-defined 122 KB static dictionary for improved text compression.

    {2 Compression quality levels}

    Quality levels control the trade-off between compression ratio and speed:
    {ul
    {- Quality [0]: Stored (uncompressed) blocks only.}
    {- Quality [1]: Huffman-only compression, no LZ77 matching.}
    {- Quality [2]-[3]: LZ77 with simple hash table matching.}
    {- Quality [4]: Hash chains (16 depth) with dictionary matching.}
    {- Quality [5]-[6]: Context mode selection for better literal coding.}
    {- Quality [7]-[9]: Multiple literal Huffman trees (2-4 trees).}
    {- Quality [10]-[11]: Optimal parsing with deep hash chains (512 depth).}}

    {2 RFC 7932 specification mapping}

    This implementation covers the following RFC 7932 sections:
    {ul
    {- {{:https://www.rfc-editor.org/rfc/rfc7932#section-3}Section 3}: Header
       (WBITS window size encoding)}
    {- {{:https://www.rfc-editor.org/rfc/rfc7932#section-4}Section 4}: Meta-block
       structure (MLEN, ISUNCOMPRESSED)}
    {- {{:https://www.rfc-editor.org/rfc/rfc7932#section-5}Section 5}: Prefix
       codes (simple and complex Huffman codes)}
    {- {{:https://www.rfc-editor.org/rfc/rfc7932#section-6}Section 6}: Context
       modeling (LSB6, MSB6, UTF8, SIGNED modes)}
    {- {{:https://www.rfc-editor.org/rfc/rfc7932#section-7}Section 7}: Block
       types and block counts}
    {- {{:https://www.rfc-editor.org/rfc/rfc7932#section-8}Section 8}: Distance
       codes (ring buffer with 16 short codes)}
    {- {{:https://www.rfc-editor.org/rfc/rfc7932#section-9}Section 9}: LZ77
       commands (insert-and-copy, literals)}
    {- {{:https://www.rfc-editor.org/rfc/rfc7932#appendix-A}Appendix A}: Static
       dictionary (122 KB, 121 transforms)}}
*)

(** {1:errors Error handling} *)

type error =
  | Invalid_stream_header
      (** WBITS value in stream header is invalid
          ({{:https://www.rfc-editor.org/rfc/rfc7932#section-9.1}RFC 7932 Section 9.1}) *)
  | Invalid_meta_block_header
      (** Meta-block header is malformed
          ({{:https://www.rfc-editor.org/rfc/rfc7932#section-9.2}RFC 7932 Section 9.2}) *)
  | Invalid_huffman_code
      (** Prefix code definition is invalid
          ({{:https://www.rfc-editor.org/rfc/rfc7932#section-3.2}RFC 7932 Section 3.2}) *)
  | Invalid_distance
      (** Distance code or value is out of range
          ({{:https://www.rfc-editor.org/rfc/rfc7932#section-4}RFC 7932 Section 4}) *)
  | Invalid_backward_reference
      (** Backward reference points before start of output *)
  | Invalid_context_map
      (** Context map encoding is invalid
          ({{:https://www.rfc-editor.org/rfc/rfc7932#section-7.3}RFC 7932 Section 7.3}) *)
  | Truncated_input
      (** Input stream ended unexpectedly *)
  | Output_overrun
      (** Decompressed size exceeds output buffer *)
(** The type for decompression errors. Error constructors reference the
    relevant RFC 7932 sections. *)

exception Brotli_error of error
(** Exception raised on decompression errors. *)

val error_to_string : error -> string
(** [error_to_string e] returns a human-readable description of error [e]. *)

(** {1:simple Simple API} *)

val compress : ?quality:int -> string -> string
(** [compress ?quality s] compresses string [s] using Brotli.

    @param quality Compression quality [0]-[11] (default: [1]).
    Higher values give better compression at the cost of speed.
    See {{!quality_levels}quality levels} for details. *)

val decompress : string -> (string, string) result
(** [decompress s] decompresses a Brotli-compressed string.

    Returns [Ok decompressed] on success or [Error message] on failure.
    The input must be a complete, valid Brotli stream. *)

val decompress_exn : string -> string
(** [decompress_exn s] decompresses a Brotli-compressed string.

    @raise Brotli_error on decompression failure. *)

(** {1:low_alloc Low-allocation API}

    These functions avoid intermediate string allocations by operating
    directly on byte buffers. Use {!max_compressed_length} to size output
    buffers for compression. *)

val compress_into :
  ?quality:int ->
  src:bytes -> src_pos:int -> src_len:int ->
  dst:bytes -> dst_pos:int -> unit -> int
(** [compress_into ?quality ~src ~src_pos ~src_len ~dst ~dst_pos ()]
    compresses [src_len] bytes from [src] starting at [src_pos] into [dst]
    starting at [dst_pos].

    @return the number of bytes written to [dst].

    The caller must ensure [dst] has at least [max_compressed_length src_len]
    bytes available starting at [dst_pos]. *)

val decompress_into :
  src:bytes -> src_pos:int -> src_len:int ->
  dst:bytes -> dst_pos:int -> int
(** [decompress_into ~src ~src_pos ~src_len ~dst ~dst_pos] decompresses
    [src_len] bytes from [src] starting at [src_pos] into [dst] starting at
    [dst_pos].

    @return the number of bytes written to [dst].
    @raise Brotli_error if the input is invalid or the output buffer is
    too small. *)

(** {1:utils Utilities} *)

val max_compressed_length : int -> int
(** [max_compressed_length n] returns the maximum possible compressed size
    for an input of [n] bytes. Use this to allocate output buffers for
    {!compress_into}. *)

(** {1:streaming Streaming compression API}

    The streaming API allows compressing data in chunks. Each chunk is
    encoded as a complete meta-block
    ({{:https://www.rfc-editor.org/rfc/rfc7932#section-9.2}RFC 7932 Section 9.2}}),
    which allows the decoder to process chunks independently.

    {b Note}: For best compression, prefer the simple API with the complete
    input when possible. The streaming API trades compression ratio for
    the ability to process data incrementally. *)

type streaming_encoder
(** Opaque type for streaming compression state. *)

val create_streaming_encoder :
  ?quality:int -> dst:bytes -> dst_pos:int -> unit -> streaming_encoder
(** [create_streaming_encoder ?quality ~dst ~dst_pos ()] creates a new
    streaming encoder that writes to [dst] starting at [dst_pos].

    @param quality Compression quality [0]-[11] (default: [1]). *)

val streaming_write :
  streaming_encoder ->
  src:bytes -> src_pos:int -> src_len:int -> is_last:bool -> int
(** [streaming_write encoder ~src ~src_pos ~src_len ~is_last] compresses
    [src_len] bytes from [src] starting at [src_pos] and writes them to
    the encoder's output buffer.

    @param is_last Set to [true] for the final chunk to emit the stream
    trailer (ISLAST=1 meta-block).
    @return the number of bytes written to the output buffer. *)

val streaming_finish : streaming_encoder -> int
(** [streaming_finish encoder] finishes the stream if not already finished.

    @return bytes written (0 if already finished). *)

val streaming_bytes_written : streaming_encoder -> int
(** [streaming_bytes_written encoder] returns total bytes written so far. *)

(** {1:constants Constants}

    These constants correspond to values defined in
    {{:https://www.rfc-editor.org/rfc/rfc7932}RFC 7932}. *)

val min_quality : int
(** [min_quality] is [0], the minimum compression quality (stored blocks). *)

val max_quality : int
(** [max_quality] is [11], the maximum compression quality. *)

val default_quality : int
(** [default_quality] is [1], the default compression quality. *)

val max_window_bits : int
(** [max_window_bits] is [22], the maximum window size (4 MB).
    See {{:https://www.rfc-editor.org/rfc/rfc7932#section-9.1}RFC 7932 Section 9.1}. *)

(** {1:internals Internals}

    These functions are exposed for testing and debugging. They are not
    part of the stable API. *)

module Debug : sig
  (** Debug utilities for inspecting LZ77 commands. *)

  type command =
    | InsertCopy of {
        lit_start: int;   (** Start offset in source for literals *)
        lit_len: int;     (** Number of literal bytes to insert *)
        copy_len: int;    (** Number of bytes to copy from back-reference *)
        distance: int;    (** Back-reference distance in bytes *)
        dist_code: int;   (** Short distance code: -1=none, 0-15=short code *)
      }
    | Literals of { start: int; len: int }
  (** LZ77 command representation.
      See {{:https://www.rfc-editor.org/rfc/rfc7932#section-5}RFC 7932 Section 5}. *)

  val generate_commands : bytes -> int -> int -> command list
  (** [generate_commands src pos len] generates LZ77 commands for the input. *)
end
