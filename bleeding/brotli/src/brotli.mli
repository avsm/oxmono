(*---------------------------------------------------------------------------
   Copyright (c) 2024 The brotli programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Pure OCaml Brotli compression and decompression.

    This module implements the Brotli compressed data format as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc7932}RFC 7932}.

    Brotli is a general-purpose lossless compression algorithm that achieves
    compression ratios comparable to the best currently available methods.
    It uses a combination of:
    {ul
    {- {{:https://datatracker.ietf.org/doc/html/rfc7932#section-2}LZ77}
       sliding window compression}
    {- {{:https://datatracker.ietf.org/doc/html/rfc7932#section-3}Canonical
       Huffman prefix codes} with 2nd-order context modeling}
    {- A {{:https://datatracker.ietf.org/doc/html/rfc7932#section-8}122 KB
       static dictionary} with 121 word transforms}}

    {1:overview Overview}

    For most use cases, the {!section:simple} provides everything needed:

    {[
      (* Compress a string *)
      let compressed = Brotli.compress ~quality:Q4 "Hello, world!"

      (* Decompress back to original *)
      let original = Brotli.decompress compressed
    ]}

    For performance-critical applications that want to avoid allocations, use
    the {!section:low_alloc}.

    For streaming or incremental compression, use the {!section:streaming}.

    {1:quality Compression Quality}

    Quality levels control the trade-off between compression ratio and speed.
    Higher values produce smaller output but take more time.

    {2 Quality level summary}

    {ul
    {- {b {!Q0}}: Stored blocks only (no compression). Fastest but no size
       reduction.}
    {- {b {!Q1}}: Huffman-only compression without LZ77. Very fast with modest
       compression.}
    {- {b {!Q2}-{!Q3}}: LZ77 with simple hash table matching. Good balance of
       speed and compression.}
    {- {b {!Q4}}: Hash chains (depth 16) with static dictionary matching.
       Recommended for most use cases.}
    {- {b {!Q5}-{!Q6}}: Adds context mode selection for better literal coding.}
    {- {b {!Q7}-{!Q9}}: Multiple literal Huffman trees (2-4 trees) for
       heterogeneous data.}
    {- {b {!Q10}-{!Q11}}: Optimal parsing with deep hash chains (512 depth).
       Maximum compression.}}

    {2 RFC 7932 correspondence}

    This implementation covers the complete
    {{:https://datatracker.ietf.org/doc/html/rfc7932}RFC 7932} specification:

    {ul
    {- {{:https://datatracker.ietf.org/doc/html/rfc7932#section-9.1}Section 9.1}:
       Stream header (WBITS window size encoding)}
    {- {{:https://datatracker.ietf.org/doc/html/rfc7932#section-9.2}Section 9.2}:
       Meta-block structure (MLEN, ISUNCOMPRESSED, ISLAST)}
    {- {{:https://datatracker.ietf.org/doc/html/rfc7932#section-3}Section 3}:
       Prefix codes (simple and complex Huffman codes)}
    {- {{:https://datatracker.ietf.org/doc/html/rfc7932#section-7}Section 7}:
       Context modeling (LSB6, MSB6, UTF8, SIGNED modes)}
    {- {{:https://datatracker.ietf.org/doc/html/rfc7932#section-6}Section 6}:
       Block types and block counts}
    {- {{:https://datatracker.ietf.org/doc/html/rfc7932#section-4}Section 4}:
       Distance codes (ring buffer with 16 short codes)}
    {- {{:https://datatracker.ietf.org/doc/html/rfc7932#section-5}Section 5}:
       Insert-and-copy length encoding}
    {- {{:https://datatracker.ietf.org/doc/html/rfc7932#appendix-A}Appendix A}:
       Static dictionary (122 KB, 121 transforms)}}
*)

(** {1:types Types} *)

type quality =
  | Q0   (** Stored blocks only (no compression). *)
  | Q1   (** Huffman-only, no LZ77 matching. *)
  | Q2   (** LZ77 with simple hash matching. *)
  | Q3   (** LZ77 with dictionary matching. *)
  | Q4   (** Hash chains (depth 16) with dictionary. *)
  | Q5   (** Context mode selection. *)
  | Q6   (** Context mode selection, improved. *)
  | Q7   (** Multiple literal trees (2 trees). *)
  | Q8   (** Multiple literal trees (2-4 trees). *)
  | Q9   (** Multiple literal trees (4 trees). *)
  | Q10  (** Optimal parsing with deep hash chains. *)
  | Q11  (** Maximum compression. *)
(** Compression quality levels. See {{!quality}Compression Quality} for
    detailed descriptions and trade-offs. *)

type error =
  | Invalid_stream_header
      (** WBITS value in stream header is invalid.
          See {{:https://datatracker.ietf.org/doc/html/rfc7932#section-9.1}
          RFC 7932 Section 9.1}. *)
  | Invalid_meta_block_header
      (** Meta-block header is malformed.
          See {{:https://datatracker.ietf.org/doc/html/rfc7932#section-9.2}
          RFC 7932 Section 9.2}. *)
  | Invalid_huffman_code
      (** Prefix code definition is invalid.
          See {{:https://datatracker.ietf.org/doc/html/rfc7932#section-3.5}
          RFC 7932 Section 3.5}. *)
  | Invalid_distance
      (** Distance code or value is out of range.
          See {{:https://datatracker.ietf.org/doc/html/rfc7932#section-4}
          RFC 7932 Section 4}. *)
  | Invalid_backward_reference
      (** Backward reference points before start of output or into
          unavailable dictionary. *)
  | Invalid_context_map
      (** Context map encoding is invalid.
          See {{:https://datatracker.ietf.org/doc/html/rfc7932#section-7.3}
          RFC 7932 Section 7.3}. *)
  | Truncated_input
      (** Input stream ended unexpectedly before ISLAST meta-block. *)
  | Output_overrun
      (** Decompressed size exceeds output buffer capacity. *)
(** Decompression error types. Each variant references the relevant
    {{:https://datatracker.ietf.org/doc/html/rfc7932}RFC 7932} section. *)

exception Brotli_error of error
(** Raised on decompression errors. Use {!error_to_string} for diagnostics. *)

(** {1:simple Simple API}

    The simple API is the easiest way to use Brotli compression. It handles
    buffer allocation automatically but may not be optimal for very large
    data or performance-critical applications. *)

val compress : ?quality:quality -> string -> string
(** [compress ?quality s] compresses string [s] using Brotli.

    @param quality Compression quality level (default: {!Q1}).
    @return The compressed data as a string.

    Example:
    {[
      let compressed = Brotli.compress ~quality:Q4 input_data
    ]} *)

val decompress : string -> string
(** [decompress s] decompresses a Brotli-compressed string.

    The function automatically allocates and resizes the output buffer as
    needed, starting with 4x the input size and growing up to 256 MB.

    @return The decompressed data as a string.
    @raise Brotli_error if the input is not valid Brotli data.

    Example:
    {[
      let original = Brotli.decompress compressed_data
    ]} *)

(** {1:low_alloc Low-Allocation API}

    These functions operate directly on byte buffers to avoid intermediate
    allocations. Use {!max_compressed_length} to size output buffers for
    compression. *)

val compress_into :
  ?quality:quality ->
  src:bytes -> src_pos:int -> src_len:int ->
  dst:bytes -> dst_pos:int -> unit -> int
(** [compress_into ?quality ~src ~src_pos ~src_len ~dst ~dst_pos ()]
    compresses [src_len] bytes from [src] starting at [src_pos] into [dst]
    starting at [dst_pos].

    @param quality Compression quality level (default: {!Q1}).
    @return The number of bytes written to [dst].

    {b Important}: The caller must ensure [dst] has at least
    [max_compressed_length src_len] bytes available starting at [dst_pos].

    Example:
    {[
      let dst = Bytes.create (Brotli.max_compressed_length src_len) in
      let len = Brotli.compress_into ~quality:Q4
        ~src ~src_pos:0 ~src_len ~dst ~dst_pos:0 ()
    ]} *)

val decompress_into :
  src:bytes -> src_pos:int -> src_len:int ->
  dst:bytes -> dst_pos:int -> int
(** [decompress_into ~src ~src_pos ~src_len ~dst ~dst_pos] decompresses
    [src_len] bytes from [src] starting at [src_pos] into [dst] starting
    at [dst_pos].

    @return The number of bytes written to [dst].
    @raise Brotli_error with {!Output_overrun} if [dst] is too small.
    @raise Brotli_error if the input is not valid Brotli data.

    {b Note}: Unlike compression, the decompressed size is not known in
    advance. You may need to retry with a larger buffer if {!Output_overrun}
    is raised. *)

(** {1:streaming Streaming API}

    The streaming API allows compressing data in chunks. Each chunk is
    encoded as a complete
    {{:https://datatracker.ietf.org/doc/html/rfc7932#section-9.2}meta-block},
    allowing independent processing.

    {b Note}: For best compression, prefer the {!section:simple} when the
    complete input is available. The streaming API trades some compression
    ratio for incremental processing. *)

type streaming_encoder
(** Opaque state for streaming compression. *)

val create_streaming_encoder :
  ?quality:quality -> dst:bytes -> dst_pos:int -> unit -> streaming_encoder
(** [create_streaming_encoder ?quality ~dst ~dst_pos ()] creates a new
    streaming encoder that writes to [dst] starting at [dst_pos].

    @param quality Compression quality (default: {!Q1}). *)

val streaming_write :
  streaming_encoder ->
  src:bytes -> src_pos:int -> src_len:int -> is_last:bool -> int
(** [streaming_write encoder ~src ~src_pos ~src_len ~is_last] compresses
    [src_len] bytes from [src] and writes them to the encoder's output.

    @param is_last Set to [true] for the final chunk. This emits the
    ISLAST=1 meta-block trailer per
    {{:https://datatracker.ietf.org/doc/html/rfc7932#section-9.2}
    RFC 7932 Section 9.2}.
    @return The number of bytes written to the output buffer. *)

val streaming_finish : streaming_encoder -> int
(** [streaming_finish encoder] finishes the stream if not already finished.

    @return Bytes written (0 if already finished). *)

val streaming_bytes_written : streaming_encoder -> int
(** [streaming_bytes_written encoder] returns total bytes written so far. *)

(** {1:errors Error Handling} *)

val error_to_string : error -> string
(** [error_to_string e] returns a human-readable description of error [e].

    Example output: ["Invalid stream header"], ["Truncated input"]. *)

(** {1:utils Utilities} *)

val quality_to_int : quality -> int
(** [quality_to_int q] returns the numeric quality level (0-11). *)

val quality_of_int : int -> quality
(** [quality_of_int n] converts a numeric quality level to {!type:quality}.
    Values outside 0-11 are clamped to the valid range. *)

val max_compressed_length : int -> int
(** [max_compressed_length n] returns the maximum possible compressed size
    for an input of [n] bytes.

    The formula is approximately [n + n/8 + 64], accounting for:
    {ul
    {- Meta-block headers}
    {- Huffman code definitions}
    {- Incompressible data expansion}}

    Use this to allocate output buffers for {!compress_into}. *)

(** {1:constants Constants}

    Format constants from {{:https://datatracker.ietf.org/doc/html/rfc7932}
    RFC 7932}. *)

val max_window_bits : int
(** Maximum window size bits (22, giving a 4 MB window).
    See {{:https://datatracker.ietf.org/doc/html/rfc7932#section-9.1}
    RFC 7932 Section 9.1}. *)

(** {1:internals Debug Utilities}

    These functions are exposed for testing and debugging. They are {b not}
    part of the stable API and may change without notice. *)

module Debug : sig
  (** LZ77 command inspection for debugging and testing.

      These types and functions expose the internal LZ77 command representation,
      which may be useful for understanding compression behavior or testing.

      See {{:https://datatracker.ietf.org/doc/html/rfc7932#section-5}
      RFC 7932 Section 5} for the command encoding specification. *)

  type command =
    | InsertCopy of {
        lit_start: int;   (** Start offset in source for literal bytes *)
        lit_len: int;     (** Number of literal bytes to insert *)
        copy_len: int;    (** Number of bytes to copy from back-reference *)
        distance: int;    (** Back-reference distance in bytes *)
        dist_code: int;   (** Short distance code: -1=none, 0-15=short code *)
      }
    | Literals of { start: int; len: int }
        (** Insert literal bytes only (no copy) *)
  (** LZ77 command representation. Commands consist of literal insertions
      and backward-reference copies.

      Per {{:https://datatracker.ietf.org/doc/html/rfc7932#section-5}
      RFC 7932 Section 5}, each command specifies:
      {ul
      {- A sequence of literal bytes to insert}
      {- A (length, distance) pair for copying from the sliding window}} *)

  val generate_commands : bytes -> int -> int -> command list
  (** [generate_commands src pos len] generates LZ77 commands for the input.

      This function runs the LZ77 matching algorithm at the current quality
      level and returns the resulting command sequence. Useful for
      understanding how data will be compressed. *)
end
