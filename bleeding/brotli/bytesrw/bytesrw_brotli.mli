(*---------------------------------------------------------------------------
   Copyright (c) 2024 The brotli programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Brotli streams for Bytesrw (pure OCaml).

    This module provides {{:https://datatracker.ietf.org/doc/html/rfc7932}
    Brotli} compression and decompression filters for the
    {{!Bytesrw}Bytesrw} streaming I/O library, using a pure OCaml
    implementation.

    {1:usage Usage}

    {2 Decompression example}

    {[
      let decompress_file input_path =
        let r = Bytes.Reader.of_in_channel (open_in input_path) in
        let r = Bytesrw_brotli.decompress_reads () r in
        Bytes.Reader.to_string r
    ]}

    {2 Compression example}

    {[
      let compress_string ~quality s =
        let r = Bytes.Reader.of_string s in
        let r = Bytesrw_brotli.compress_reads ~quality () r in
        Bytes.Reader.to_string r
    ]}

    {1:notes Implementation Notes}

    {b Buffering.} Unlike streaming compression formats, Brotli achieves
    better compression by seeing more context. This implementation buffers
    the entire input before compressing/decompressing to achieve optimal
    compression ratios. For truly streaming compression with lower memory
    usage but reduced compression, consider chunking your data.

    {b Slice lengths.} The slice length of readers created by filters of
    this module defaults to {!default_slice_length}. Writers respect
    their configured slice length preferences.

    {b Positions.} The positions of readers and writers created by filters
    of this module start at [0]. *)

open Bytesrw

(** {1:errors Errors} *)

type Bytes.Stream.error += Error of string (** *)
(** The type for Brotli stream errors.

    Except for the {{!lib}library parameters}, all functions of this
    module and resulting readers and writers may raise
    {!Bytesrw.Bytes.Stream.Error} with this error.

    The error message includes details from the underlying {!Brotli}
    decompression, such as:
    {ul
    {- Invalid stream header (WBITS encoding)}
    {- Invalid meta-block header}
    {- Invalid Huffman code}
    {- Invalid distance or backward reference}
    {- Truncated input}} *)

(** {1:decompress Decompress} *)

val decompress_reads : unit -> Bytes.Reader.filter
(** [decompress_reads () r] filters the reads of [r] by decompressing
    a {{:https://datatracker.ietf.org/doc/html/rfc7932}Brotli} stream.

    The reader errors if the stream is malformed, truncated, or contains
    invalid Brotli data per
    {{:https://datatracker.ietf.org/doc/html/rfc7932#section-10}
    RFC 7932 Section 10}. *)

val decompress_writes : unit -> Bytes.Writer.filter
(** [decompress_writes () w ~eod] filters writes on [w] by decompressing
    a Brotli stream until {!Bytesrw.Bytes.Slice.eod} is written.

    If [eod] is [false], the final {!Bytesrw.Bytes.Slice.eod} is not
    written to [w], allowing [w] to be reused for additional non-filtered
    writes. *)

(** {1:compress Compress} *)

type quality = Brotli.quality
(** Compression quality levels.

    See {!Brotli.quality} for detailed descriptions of each level
    ({!Brotli.Q0} through {!Brotli.Q11}). Defaults to {!default_quality}. *)

val compress_reads : ?quality:quality -> unit -> Bytes.Reader.filter
(** [compress_reads ?quality () r] filters the reads of [r] by compressing
    them to a {{:https://datatracker.ietf.org/doc/html/rfc7932}Brotli}
    stream at quality [quality] (defaults to {!default_quality}).

    The resulting stream is a valid Brotli stream with proper stream
    header and ISLAST meta-block trailer per
    {{:https://datatracker.ietf.org/doc/html/rfc7932#section-9}
    RFC 7932 Section 9}. *)

val compress_writes : ?quality:quality -> unit -> Bytes.Writer.filter
(** [compress_writes ?quality () w ~eod] filters writes on [w] by
    compressing them to a Brotli stream at quality [quality] (defaults
    to {!default_quality}) until {!Bytesrw.Bytes.Slice.eod} is written.

    If [eod] is [false], the final {!Bytesrw.Bytes.Slice.eod} is not
    written to [w], allowing [w] to be reused for additional non-filtered
    writes. *)

(** {1:lib Library Parameters} *)

val default_slice_length : int
(** [default_slice_length] is [65536] (64 KB).

    This is the default slice length for readers created by filters in
    this module. *)

(** {2:quality_levels Quality Level Aliases}

    Convenient aliases for common quality levels. *)

val default_quality : quality
(** [default_quality] is {!Brotli.Q1}, providing fast compression with
    Huffman-only encoding. Suitable for latency-sensitive applications. *)

val no_compression : quality
(** [no_compression] is {!Brotli.Q0}, stored blocks only. The data is
    wrapped in Brotli format but not compressed. Fastest possible encoding. *)

val best_speed : quality
(** [best_speed] is {!Brotli.Q1}, Huffman-only compression without LZ77
    matching. Very fast with modest compression. *)

val best_compression : quality
(** [best_compression] is {!Brotli.Q11}, maximum compression using optimal
    parsing with deep hash chains (512 depth). Slowest but smallest output. *)
