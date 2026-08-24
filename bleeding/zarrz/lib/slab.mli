(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Decoded chunk blocks.

    A slab is a C-order block of elements: a bigstring holding the native
    endian element bytes, the {!Dtype.t} those bytes encode, and the shape
    of the block. Chunks, shards and assembled array subsets are all
    slabs.

    Elements are addressed by a linear C-order index, so the element at
    coordinate [(i, j)] of a slab of shape [[: r; c :]] is at index
    [i * c + j]. {!index2} and {!index3} compute that index.

    Each accessor module reads and writes one data type through an
    unboxed scalar. [get] and [set] check that the slab's dtype is the
    one the module serves and that the index is in range, both once per
    call. [unsafe_get] and [unsafe_set] check nothing: an out of range
    index reads or writes memory outside the buffer. Every operation in
    every accessor module is [@zero_alloc], verified under the
    [release-check] profile. *)

type t

val create : Dtype.t -> int iarray -> t
(** [create dtype shape] is a slab over a fresh buffer of
    [num_elements * Dtype.size dtype] bytes. The buffer contents are
    {b uninitialised}, so a caller must either {!fill} it or write every
    element before reading. Raises [Invalid_argument] if any dimension is
    negative or the buffer size overflows. *)

val of_bigstring : Dtype.t -> int iarray -> Base_bigstring.t -> t
(** [of_bigstring dtype shape buf] is a slab sharing [buf], with no copy.
    Raises [Invalid_argument] unless [Base_bigstring.length buf] is
    exactly [num_elements * Dtype.size dtype]. Later writes through the
    slab are visible through [buf] and the reverse. *)

val dtype : t -> Dtype.t
(** [dtype t] is the data type of the elements of [t]. *)
[@@zero_alloc]

val shape : t -> int iarray
(** [shape t] is the extent of [t] in each dimension, outermost first. *)
[@@zero_alloc]

val rank : t -> int
(** [rank t] is the number of dimensions of [t]. A rank 0 slab holds one
    element. *)
[@@zero_alloc]

val num_elements : t -> int
(** [num_elements t] is the product of {!shape}, the number of elements
    [t] holds. *)
[@@zero_alloc]

val bigstring : t -> Base_bigstring.t
(** [bigstring t] is the buffer of [t]. It is the live buffer, not a
    copy. *)
[@@zero_alloc]

val fill : t -> string -> unit
(** [fill t elem] writes the bytes [elem] over every element of [t].
    [elem] is the native endian encoding of one element, as
    {!Fill_value} holds it. Raises [Invalid_argument] unless
    [String.length elem] equals [Dtype.size (dtype t)]. *)

val index2 : t -> int -> int -> int
(** [index2 t i j] is the linear C-order index of coordinate [(i, j)].
    Raises [Invalid_argument] if [rank t] is not 2 or a coordinate is out
    of range. *)
[@@zero_alloc]

val index3 : t -> int -> int -> int -> int
(** [index3 t i j k] is the linear C-order index of coordinate
    [(i, j, k)]. Raises [Invalid_argument] if [rank t] is not 3 or a
    coordinate is out of range. *)
[@@zero_alloc]

(** {1 Accessor signatures}

    One signature per unboxed element type. [get2] and [set2] address a
    rank 2 slab, [get3] and [set3] a rank 3 slab, both through {!index2}
    and {!index3}. They check the dtype and the coordinates, never the
    linear index, which those helpers already bound. *)

(** Accessors presenting an element as [float#]. *)
module type S_f64 = sig
  val get : t -> int -> float#
  [@@zero_alloc]

  val unsafe_get : t -> int -> float#
  [@@zero_alloc]

  val set : t -> int -> float# -> unit
  [@@zero_alloc]

  val unsafe_set : t -> int -> float# -> unit
  [@@zero_alloc]

  val get2 : t -> int -> int -> float#
  [@@zero_alloc]

  val set2 : t -> int -> int -> float# -> unit
  [@@zero_alloc]

  val get3 : t -> int -> int -> int -> float#
  [@@zero_alloc]

  val set3 : t -> int -> int -> int -> float# -> unit
  [@@zero_alloc]
end

(** Accessors presenting an element as [float32#]. *)
module type S_f32 = sig
  val get : t -> int -> float32#
  [@@zero_alloc]

  val unsafe_get : t -> int -> float32#
  [@@zero_alloc]

  val set : t -> int -> float32# -> unit
  [@@zero_alloc]

  val unsafe_set : t -> int -> float32# -> unit
  [@@zero_alloc]

  val get2 : t -> int -> int -> float32#
  [@@zero_alloc]

  val set2 : t -> int -> int -> float32# -> unit
  [@@zero_alloc]

  val get3 : t -> int -> int -> int -> float32#
  [@@zero_alloc]

  val set3 : t -> int -> int -> int -> float32# -> unit
  [@@zero_alloc]
end

(** Accessors presenting an element as [int64#]. *)
module type S_i64 = sig
  val get : t -> int -> int64#
  [@@zero_alloc]

  val unsafe_get : t -> int -> int64#
  [@@zero_alloc]

  val set : t -> int -> int64# -> unit
  [@@zero_alloc]

  val unsafe_set : t -> int -> int64# -> unit
  [@@zero_alloc]

  val get2 : t -> int -> int -> int64#
  [@@zero_alloc]

  val set2 : t -> int -> int -> int64# -> unit
  [@@zero_alloc]

  val get3 : t -> int -> int -> int -> int64#
  [@@zero_alloc]

  val set3 : t -> int -> int -> int -> int64# -> unit
  [@@zero_alloc]
end

(** Accessors presenting an element as [int32#]. *)
module type S_i32 = sig
  val get : t -> int -> int32#
  [@@zero_alloc]

  val unsafe_get : t -> int -> int32#
  [@@zero_alloc]

  val set : t -> int -> int32# -> unit
  [@@zero_alloc]

  val unsafe_set : t -> int -> int32# -> unit
  [@@zero_alloc]

  val get2 : t -> int -> int -> int32#
  [@@zero_alloc]

  val set2 : t -> int -> int -> int32# -> unit
  [@@zero_alloc]

  val get3 : t -> int -> int -> int -> int32#
  [@@zero_alloc]

  val set3 : t -> int -> int -> int -> int32# -> unit
  [@@zero_alloc]
end

(** Accessors presenting an element as [int16#]. *)
module type S_i16 = sig
  val get : t -> int -> int16#
  [@@zero_alloc]

  val unsafe_get : t -> int -> int16#
  [@@zero_alloc]

  val set : t -> int -> int16# -> unit
  [@@zero_alloc]

  val unsafe_set : t -> int -> int16# -> unit
  [@@zero_alloc]

  val get2 : t -> int -> int -> int16#
  [@@zero_alloc]

  val set2 : t -> int -> int -> int16# -> unit
  [@@zero_alloc]

  val get3 : t -> int -> int -> int -> int16#
  [@@zero_alloc]

  val set3 : t -> int -> int -> int -> int16# -> unit
  [@@zero_alloc]
end

(** Accessors presenting an element as [int8#]. *)
module type S_i8 = sig
  val get : t -> int -> int8#
  [@@zero_alloc]

  val unsafe_get : t -> int -> int8#
  [@@zero_alloc]

  val set : t -> int -> int8# -> unit
  [@@zero_alloc]

  val unsafe_set : t -> int -> int8# -> unit
  [@@zero_alloc]

  val get2 : t -> int -> int -> int8#
  [@@zero_alloc]

  val set2 : t -> int -> int -> int8# -> unit
  [@@zero_alloc]

  val get3 : t -> int -> int -> int -> int8#
  [@@zero_alloc]

  val set3 : t -> int -> int -> int -> int8# -> unit
  [@@zero_alloc]
end

(** Accessors presenting an element as [bool#]. *)
module type S_bool = sig
  val get : t -> int -> bool#
  [@@zero_alloc]

  val unsafe_get : t -> int -> bool#
  [@@zero_alloc]

  val set : t -> int -> bool# -> unit
  [@@zero_alloc]

  val unsafe_set : t -> int -> bool# -> unit
  [@@zero_alloc]

  val get2 : t -> int -> int -> bool#
  [@@zero_alloc]

  val set2 : t -> int -> int -> bool# -> unit
  [@@zero_alloc]

  val get3 : t -> int -> int -> int -> bool#
  [@@zero_alloc]

  val set3 : t -> int -> int -> int -> bool# -> unit
  [@@zero_alloc]
end

(** Accessors presenting the two halves of a complex element as
    [float32#]. *)
module type S_c64 = sig
  val get_re : t -> int -> float32#
  [@@zero_alloc]

  val get_im : t -> int -> float32#
  [@@zero_alloc]

  val unsafe_get_re : t -> int -> float32#
  [@@zero_alloc]

  val unsafe_get_im : t -> int -> float32#
  [@@zero_alloc]

  val set_re : t -> int -> float32# -> unit
  [@@zero_alloc]

  val set_im : t -> int -> float32# -> unit
  [@@zero_alloc]

  val unsafe_set_re : t -> int -> float32# -> unit
  [@@zero_alloc]

  val unsafe_set_im : t -> int -> float32# -> unit
  [@@zero_alloc]
end

(** Accessors presenting the two halves of a complex element as
    [float#]. *)
module type S_c128 = sig
  val get_re : t -> int -> float#
  [@@zero_alloc]

  val get_im : t -> int -> float#
  [@@zero_alloc]

  val unsafe_get_re : t -> int -> float#
  [@@zero_alloc]

  val unsafe_get_im : t -> int -> float#
  [@@zero_alloc]

  val set_re : t -> int -> float# -> unit
  [@@zero_alloc]

  val set_im : t -> int -> float# -> unit
  [@@zero_alloc]

  val unsafe_set_re : t -> int -> float# -> unit
  [@@zero_alloc]

  val unsafe_set_im : t -> int -> float# -> unit
  [@@zero_alloc]
end

(** {1 Accessors} *)

module F64 : S_f64
(** [Dtype.Float64] elements. *)

module F32 : S_f32
(** [Dtype.Float32] elements. *)

module F16 : S_f64
(** [Dtype.Float16] elements. [get] widens the stored IEEE 754 binary16
    exactly, subnormals and NaN payloads included. [set] narrows with
    round to nearest, ties to even. A magnitude that rounds above the
    largest binary16, 65504, becomes an infinity, one that rounds below
    half the smallest subnormal becomes a zero of the same sign, and a
    NaN stays a NaN. *)

module BF16 : S_f64
(** [Dtype.Bfloat16] elements, the top 16 bits of an IEEE 754 binary32.
    [get] and [set] behave as {!F16} over the wider exponent range of
    binary32. *)

module I64 : S_i64
(** [Dtype.Int64] elements. *)

module U64 : S_i64
(** [Dtype.Uint64] elements. The 64 bits are returned as [int64#]
    unchanged, so a value above [Int64.max_int] reads as negative. This
    is the reinterpretation Rust's [u64 as i64] performs, and the Zarr
    shard index relies on it. *)

module I32 : S_i32
(** [Dtype.Int32] elements. *)

module U32 : S_i32
(** [Dtype.Uint32] elements, reinterpreted as [int32#] as {!U64}
    reinterprets [Dtype.Uint64]. *)

module I16 : S_i16
(** [Dtype.Int16] elements. *)

module U16 : S_i16
(** [Dtype.Uint16] elements, reinterpreted as [int16#] as {!U64}
    reinterprets [Dtype.Uint64]. *)

module I8 : S_i8
(** [Dtype.Int8] elements. *)

module U8 : S_i8
(** [Dtype.Uint8] elements, reinterpreted as [int8#] as {!U64}
    reinterprets [Dtype.Uint64]. *)

module Bl : S_bool
(** [Dtype.Bool] elements. A stored byte reads as [#false] when it is
    zero and [#true] otherwise. [set] writes 1 or 0. *)

module C64 : S_c64
(** [Dtype.Complex64] elements, a binary32 real part followed by a
    binary32 imaginary part. *)

module C128 : S_c128
(** [Dtype.Complex128] elements, a binary64 real part followed by a
    binary64 imaginary part. *)

(** {1 Interop} *)

val to_genarray :
  t -> ('a, 'b) Bigarray.kind -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t
(** [to_genarray t kind] is a bigarray of [kind] and shape [shape t]
    sharing the buffer of [t] with no copy. Writes through either view
    are visible through the other, and the buffer stays alive as long as
    either view does.

    [kind] must be the one kind this table gives for [dtype t]:

    - [Dtype.Bool], [Dtype.Bfloat16] and [Dtype.Raw]: no kind, always an
      error.
    - [Dtype.Int8]: [Bigarray.Int8_signed].
    - [Dtype.Uint8]: [Bigarray.Int8_unsigned].
    - [Dtype.Int16]: [Bigarray.Int16_signed].
    - [Dtype.Uint16]: [Bigarray.Int16_unsigned].
    - [Dtype.Int32]: [Bigarray.Int32].
    - [Dtype.Uint32]: [Bigarray.Int32], the {!U32} reinterpretation.
    - [Dtype.Int64]: [Bigarray.Int64].
    - [Dtype.Uint64]: [Bigarray.Int64], the {!U64} reinterpretation.
    - [Dtype.Float16]: [Bigarray.Float16].
    - [Dtype.Float32]: [Bigarray.Float32].
    - [Dtype.Float64]: [Bigarray.Float64].
    - [Dtype.Complex64]: [Bigarray.Complex32], which is the pair of
      binary32 halves Zarr calls complex64.
    - [Dtype.Complex128]: [Bigarray.Complex64].

    Raises [Invalid_argument] on any other pairing, and on a rank above
    16, which is the bigarray limit. *)
