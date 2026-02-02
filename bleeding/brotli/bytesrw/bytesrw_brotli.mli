(*---------------------------------------------------------------------------
   Copyright (c) 2024 The brotli programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Brotli streams (pure OCaml)

    This module provides support for reading and writing
    {{:https://www.rfc-editor.org/rfc/rfc7932}Brotli} compressed streams
    using a pure OCaml implementation.

    {b Slice lengths.} The slice length of readers created by filters of
    this module defaults to {!default_slice_length}. The hinted slice length
    of writers created by filters of this module defaults to
    {!default_slice_length} and they write on their writers with slices
    that respect their desires.

    {b Positions.} The positions of readers and writers created by filters
    of this module default to [0].

    {b Buffering.} Unlike streaming compression formats, Brotli achieves
    better compression by seeing more context. This implementation buffers
    the entire input before compressing/decompressing to achieve optimal
    compression ratios. *)

open Bytesrw

(** {1:errors Errors} *)

type Bytes.Stream.error += Error of string (** *)
(** The type for Brotli stream errors.

    Except for the {{!lib}library parameters}, all functions of this
    module and resulting readers and writers may raise
    {!Bytesrw.Bytes.Stream.Error} with this error. *)

(** {1:decompress Decompress} *)

val decompress_reads : unit -> Bytes.Reader.filter
(** [decompress_reads () r] filters the reads of [r] by decompressing
    a Brotli stream. The reader errors if the stream is malformed or
    truncated. *)

val decompress_writes : unit -> Bytes.Writer.filter
(** [decompress_writes () w ~eod] filters writes on [w] by decompressing
    a Brotli stream until {!Bytesrw.Bytes.Slice.eod} is written. If [eod]
    is [false], the last {!Bytesrw.Bytes.Slice.eod} is not written on [w]
    and at this point [w] can be used again to perform other non-filtered
    writes. *)

(** {1:compress Compress} *)

type quality = int
(** The type for compression quality levels.

    An integer between [0] and [11]. See {!Brotli} for quality level
    descriptions. Defaults to {!default_quality}. *)

val compress_reads : ?quality:quality -> unit -> Bytes.Reader.filter
(** [compress_reads ?quality () r] filters the reads of [r] by compressing
    them to a Brotli stream at quality [quality] (defaults to
    {!default_quality}). *)

val compress_writes : ?quality:quality -> unit -> Bytes.Writer.filter
(** [compress_writes ?quality () w ~eod] filters writes on [w] by compressing
    them to a Brotli stream at quality [quality] (defaults to
    {!default_quality}) until {!Bytesrw.Bytes.Slice.eod} is written. If [eod]
    is [false], the latter is not written on [w] and at that point [w] can
    be used again to perform non-filtered writes. *)

(** {1:lib Library parameters} *)

val default_slice_length : int
(** [default_slice_length] is [64KB]. *)

(** {2:quality_levels Quality levels} *)

val default_quality : quality
(** [default_quality] is [1], fast compression. *)

val no_compression : quality
(** [no_compression] is [0], stored blocks only. *)

val best_speed : quality
(** [best_speed] is [1], Huffman-only compression. *)

val best_compression : quality
(** [best_compression] is [11], optimal parsing with deep hash chains. *)
