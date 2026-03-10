(** {1 Zarr v3 Data Types}

    Defines the supported element types for Zarr v3 arrays.

    Follows the {{:https://zarr-specs.readthedocs.io/en/latest/v3/core/index.html}
    Zarr v3.1 specification, section "Data Types"}.

    {2 Supported types}

    All core data types from the spec with fixed element size are
    supported:
    - Boolean: 1 byte, [0x00] = false, [0x01] = true
    - Signed integers: int8 (1B), int16 (2B), int32 (4B), int64 (8B),
      two's complement
    - Unsigned integers: uint8 (1B), uint16 (2B), uint32 (4B), uint64 (8B)
    - IEEE 754 floats: float32 (4B), float64 (8B)
    - Raw bytes: r<N> where N is a positive multiple of 8 (N/8 bytes),
      opaque pass-through

    {2 Unsupported types}

    The following core types are not yet implemented:
    - [float16]: IEEE 754 binary16 half-precision float
    - [complex64]: pair of float32 (real + imaginary)
    - [complex128]: pair of float64 (real + imaginary) *)

(** {2 Types} *)

(** Data type of array elements.

    Each variant corresponds to a Zarr v3 core data type identifier. *)
type t =
  | Bool      (** ["bool"] -- Boolean, stored as a single byte (0 or 1) *)
  | Int8      (** ["int8"] -- Signed 8-bit integer *)
  | Int16     (** ["int16"] -- Signed 16-bit integer *)
  | Int32     (** ["int32"] -- Signed 32-bit integer *)
  | Int64     (** ["int64"] -- Signed 64-bit integer *)
  | Uint8     (** ["uint8"] -- Unsigned 8-bit integer *)
  | Uint16    (** ["uint16"] -- Unsigned 16-bit integer *)
  | Uint32    (** ["uint32"] -- Unsigned 32-bit integer *)
  | Uint64    (** ["uint64"] -- Unsigned 64-bit integer *)
  | Float32   (** ["float32"] -- IEEE 754 single-precision float *)
  | Float64   (** ["float64"] -- IEEE 754 double-precision float *)
  | Raw of int  (** ["r<N>"] -- Raw opaque bytes, width in bits (positive multiple of 8) *)

(** Byte order for multi-byte data types.

    The Zarr v3 bytes codec requires an endianness specification for
    multi-byte types.  Single-byte types and raw types do not require
    one. *)
type endian =
  | Little  (** Least-significant byte first *)
  | Big     (** Most-significant byte first *)

(** {2 Properties} *)

val byte_size : t -> int
(** [byte_size dtype] returns the number of bytes per element. *)

val requires_endianness : t -> bool
(** [requires_endianness dtype] is [true] for multi-byte numeric types
    that need an endianness specification in the bytes codec.  Returns
    [false] for single-byte types ([Bool], [Int8], [Uint8]) and all
    [Raw] types (raw bytes are opaque and never byte-swapped per the
    Zarr v3.1 spec). *)

(** {2 Serialization} *)

val of_string : string -> t option
(** [of_string s] parses a Zarr v3 data type identifier.
    Returns [None] for unrecognised or unsupported types (including
    [float16], [complex64], [complex128]). *)

val to_string : t -> string
(** [to_string dtype] returns the canonical Zarr v3 identifier string. *)

(** {2 Predicates} *)

val is_integer : t -> bool
(** [is_integer dtype] is [true] for all integer types (signed and unsigned). *)

val is_signed : t -> bool
(** [is_signed dtype] is [true] for signed integer types. *)

val is_unsigned : t -> bool
(** [is_unsigned dtype] is [true] for unsigned integer types. *)

val is_float : t -> bool
(** [is_float dtype] is [true] for floating-point types. *)
