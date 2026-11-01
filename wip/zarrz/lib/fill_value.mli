(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Array fill values.

    A fill value is the native endian byte image of one element of the
    array's data type. Matches the
    {{:https://zarr-specs.readthedocs.io/en/latest/v3/core/}Zarr V3}
    [fill_value] rules, including the string forms for floats and the
    canonical NaN. *)

type t
(** The type for fill values. Its byte length is the element size of the
    data type it was made for. *)

val of_bytes : string -> t
(** [of_bytes s] is the fill value whose element image is [s]. No data
    type is implied, so nothing is checked here. *)

val to_bytes : t -> string
(** [to_bytes t] is the native endian byte image of one element. *)

val length : t -> int
(** [length t] is the byte length of {!to_bytes}[ t]. *)

val equal : t -> t -> bool
(** [equal a b] is byte equality of [a] and [b]. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints [t] as a hex byte string. *)

val of_json : Dtype.t -> Jsont.json -> (t, string) result
(** [of_json dt j] decodes the [fill_value] member [j] of an array whose
    data type is [dt]. The accepted forms are:

    {ul
    {- [Bool]: a JSON boolean.}
    {- integers: a JSON number with no fractional part, within the
       range of the type. Because [jsont] represents every JSON number
       as a [float], [int64] and [uint64] additionally reject any
       magnitude above 2{^53}, where a [float] no longer names each
       integer, and an integer written with an exponent, such as
       [1e2], is accepted where the specification asks for none.}
    {- floats: a JSON number, or one of the strings ["Infinity"],
       ["-Infinity"], ["NaN"], or a hex string of the big endian byte
       image. The hex string must carry a [0x] prefix and exactly two
       digits per byte of the float type. The [x] of the prefix and the
       digits may each be upper or lower case. ["NaN"] decodes to the
       canonical Zarr NaN, sign [0], exponent all ones, mantissa MSB
       [1] and the rest [0], which is [0x7e00] for [Float16], [0x7fc0]
       for [Bfloat16], [0x7fc00000] for [Float32] and
       [0x7ff8000000000000] for [Float64].}
    {- [Complex64] and [Complex128]: a two element JSON array of the
       real then the imaginary component, each by the float rules of
       the component type.}
    {- [Raw n]: a JSON array of [n] numbers in \[0;255\], or a standard
       base64 string, padded to a multiple of four characters, decoding
       to exactly [n] bytes. The array is the only form the
       specification defines. The base64 string is accepted on decoding
       alone, because writers emit it, and {!to_json} never produces
       one.}}

    The result is [Error msg] with a message naming the data type when
    [j] is not one of these. *)

val to_json : Dtype.t -> t -> Jsont.json
(** [to_json dt t] is the [fill_value] member for [t] under data type
    [dt]. [Bool] becomes a JSON boolean, [true] for any non zero byte.
    An infinity becomes ["Infinity"] or ["-Infinity"], the canonical
    NaN becomes ["NaN"], any other NaN becomes the lowercase [0x]
    prefixed hex string of its big endian bytes, and any other number
    becomes a JSON number. [Raw n] becomes an array of byte numbers,
    never base64.

    An [int64] or [uint64] whose magnitude exceeds 2{^53} still encodes
    as a JSON number, which {!of_json} then rejects.

    @raise Error.E if [length t] is not [Dtype.size dt]. *)
