(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Zarr V3 data types.

    The fixed-size core data types of the Zarr V3 specification. Every
    type has a known element size in bytes. In-memory element bytes are
    native endian once a chunk is decoded. *)

type t =
  | Bool
  | Int8
  | Int16
  | Int32
  | Int64
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Float16
  | Bfloat16
  | Float32
  | Float64
  | Complex64
  | Complex128
  | Raw of int  (** Size in bytes. [Raw n] is the identifier [r{8n}]. *)

val size : t -> int
(** [size t] is the number of bytes one element of [t] occupies. *)

val name : t -> string
(** [name t] is the Zarr V3 identifier of [t], for example ["float64"]
    or ["r24"]. *)

val of_name : string -> t option
(** [of_name s] is the data type named [s], or [None] when [s] is not a
    recognised identifier. [r*] identifiers whose bit count is zero or
    not a multiple of 8 are rejected. *)

val equal : t -> t -> bool
(** [equal a b] is structural equality of [a] and [b]. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints {!name}[ t] on [ppf]. *)
