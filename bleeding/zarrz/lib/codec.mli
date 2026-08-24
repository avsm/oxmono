(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Codec pipeline.

    A chunk's [codecs] metadata lists zero or more array to array
    codecs, exactly one array to bytes codec and zero or more bytes to
    bytes codecs. {!chain_of_exts} binds such a list to a data type and
    fill value, and {!decode_chunk}, {!encode_chunk} and
    {!partial_decode} run it. Bound codecs are records of closures, so
    user extensions are ordinary values supplied through a
    {!type-resolver}. *)

type size = Fixed of int | Bounded of int | Unbounded
(** Encoded byte size of a stage. [Bounded n] is at most [n] bytes. *)

type repr = { dtype : Dtype.t; shape : int array }
(** The decoded representation entering a stage. *)

type a2a = {
  name : string;
  encoded_repr : repr -> repr;
  encode : Slab.t -> Slab.t;
  decode : Slab.t -> repr -> Slab.t;
      (** [decode s r] is [s] decoded, where [r] is the decoded
          representation to produce. *)
}
(** An array to array codec bound to a data type and fill value. *)

type a2b = {
  name : string;
  encoded_size : repr -> size;
  encode : Slab.t -> Base_bigstring.t;
  decode : Base_bigstring.t -> repr -> Slab.t;
  partial_decode : (Byte_source.t -> repr -> Subset.t -> Slab.t) option;
      (** [partial_decode src r sub] decodes only [sub] of the chunk
          whose decoded representation is [r], reading ranges of the
          encoded bytes through [src]. *)
}
(** An array to bytes codec bound to a data type and fill value. *)

type b2b = {
  name : string;
  encoded_size : size -> size;
  encode : Base_bigstring.t -> Base_bigstring.t;
  decode : Base_bigstring.t -> decoded_size:size -> Base_bigstring.t;
}
(** A bytes to bytes codec. *)

type bound = A2a of a2a | A2b of a2b | B2b of b2b

type resolver = Ext.t -> dtype:Dtype.t -> fill_value:Fill_value.t ->
  (bound, string) result option
(** [resolver ext ~dtype ~fill_value] is [Some] when the resolver
    recognises [ext.name], carrying the bound codec or a construction
    error. [None] falls through to the built-in codecs. *)

type chain
(** A bound codec chain together with the metadata it came from. *)

val chain_of_exts :
  ?resolver:resolver -> dtype:Dtype.t -> fill_value:Fill_value.t ->
  Ext.t list -> (chain, string) result
(** [chain_of_exts ~dtype ~fill_value exts] buckets [exts] into array to
    array, array to bytes and bytes to bytes codecs, preserving list
    order within each bucket, and binds each. An unknown name whose
    [must_understand] is false is skipped. Errors on an unknown name
    otherwise, on no array to bytes codec and on more than one.
    [resolver] does not reach chains nested inside [sharding_indexed],
    whose inner and index codecs resolve against the built-ins only. *)

val chain_exts : chain -> Ext.t list
(** [chain_exts c] is the metadata [c] was built from, with skipped
    entries removed. *)

val encoded_size : chain -> repr -> size
(** [encoded_size c r] is the stored byte size of a chunk whose decoded
    representation is [r]. *)

val decode_chunk : chain -> repr -> Base_bigstring.t -> Slab.t
(** [decode_chunk c r bytes] decodes one whole chunk. [r] is the chunk's
    decoded representation, with the full chunk shape. The result may
    alias [bytes], so the caller must treat [bytes] as owned by the
    slab afterwards. Raises {!Error.E} on malformed input. *)

val encode_chunk : chain -> Slab.t -> Base_bigstring.t
(** [encode_chunk c slab] encodes one whole chunk. The result may alias
    the slab's buffer, so store it or copy it before mutating [slab]. *)

val supports_partial : chain -> bool
(** [supports_partial c] is true when [c] has no array to array and no
    bytes to bytes codecs and its array to bytes codec can decode
    ranges. *)

val partial_decode :
  chain -> repr -> Byte_source.t -> Subset.t -> Slab.t option
(** [partial_decode c r src sub] decodes only [sub] of a chunk through
    ranged reads, or [None] when {!supports_partial} is false and the
    caller must fetch the whole chunk and use {!decode_chunk}. *)
