(** {1 Chunk Data}

    Bytes-backed N-dimensional typed array for Zarr v3 chunk storage.

    Elements are stored in C-order (row-major) in a flat [bytes] buffer at
    native (little-endian) byte order.  The bytes codec handles endianness
    conversion during serialization.

    {2 Design rationale}

    Using flat [bytes] instead of [Bigarray] provides:
    - Fast [Bytes.blit] for contiguous innermost-dimension copies
    - Simple [Bytes.copy] when the output endianness is little-endian
    - Compatibility with OxCaml's [let mutable] for zero-allocation loops
    - Direct use of [Bytes.get_int32_le] etc. with no FFI overhead

    {b Zarr v3.1 spec reference:} The bytes codec encodes elements in
    lexicographic (C/row-major) order, which matches our internal layout.
    For column-major (Fortran) order, a transpose codec must precede the
    bytes codec. *)

(** {2 Types} *)

(** A typed, shaped view over a flat byte buffer. *)
type t

(** Element value, tagged by dtype family. *)
type value =
  [ `Bool of bool
  | `Int of int
  | `Int32 of int32
  | `Int64 of int64
  | `Float of float
  | `Char of char ]

(** {2 Creation} *)

val create : Zarr_dtype.t -> int array -> Zarr_fill.t -> t
(** [create dtype shape fv] allocates a chunk filled with fill value [fv].
    Uses a doubling-copy algorithm for efficient replication. *)

val create_zero : Zarr_dtype.t -> int array -> t
(** [create_zero dtype shape] allocates a zero-filled chunk. *)

(** {2 Properties} *)

val shape : t -> int array
val dtype : t -> Zarr_dtype.t
val ndim : t -> int
val numel : t -> int
val byte_length : t -> int

(** {2 Bytes Conversion}

    Internal storage uses native (little-endian) byte order.
    Raw types ({!Zarr_dtype.Raw}) are never byte-swapped regardless of
    the requested endianness, per the Zarr v3.1 spec. *)

val to_bytes : Zarr_dtype.endian -> t -> bytes
(** [to_bytes endian t] serialises the chunk to bytes in the given byte
    order.  When [endian] is [Little] (or for raw/single-byte types),
    this is a simple [Bytes.copy]. *)

val of_bytes : Zarr_dtype.t -> Zarr_dtype.endian -> int array -> bytes -> t
(** [of_bytes dtype endian shape buf] creates chunk data from serialised
    bytes.
    @raise Invalid_argument if [buf] is shorter than
    [numel * byte_size dtype]. *)

(** {2 Typed Element Access}

    Direct, inlined accessors for specific types.  These bypass the
    generic polymorphic-variant dispatch and are suitable for hot inner
    loops. *)

val get_int8 : t -> int array -> int
val get_uint8 : t -> int array -> int
val get_int16 : t -> int array -> int
val get_uint16 : t -> int array -> int
val get_int32 : t -> int array -> int32
val get_int64 : t -> int array -> int64
val get_float32 : t -> int array -> float
val get_float64 : t -> int array -> float

val set_int8 : t -> int array -> int -> unit
val set_uint8 : t -> int array -> int -> unit
val set_int16 : t -> int array -> int -> unit
val set_uint16 : t -> int array -> int -> unit
val set_int32 : t -> int array -> int32 -> unit
val set_int64 : t -> int array -> int64 -> unit
val set_float32 : t -> int array -> float -> unit
val set_float64 : t -> int array -> float -> unit

(** {2 Generic Element Access}

    Dtype-agnostic access returning tagged values.  Less efficient than
    the typed accessors above due to polymorphic variant allocation. *)

val get : t -> int array -> value
(** [get t idx] reads the element at [idx], returning a tagged value. *)

val set : t -> int array -> value -> unit
(** [set t idx v] writes a tagged value at [idx]. Silently ignores
    type-mismatched writes. *)

(** {2 Bulk Operations} *)

val blit : src:t -> src_off:int array -> dst:t -> dst_off:int array
  -> shape:int array -> unit
(** [blit ~src ~src_off ~dst ~dst_off ~shape] copies a rectangular region.
    Uses [Bytes.blit] for the contiguous innermost dimension, iterating
    over higher dimensions.  This is the primary fast path for
    {!Zarr_array.get_slice} / {!Zarr_array.set_slice}. *)

val transpose : t -> int array -> t
(** [transpose t perm] returns a new chunk with permuted dimensions.
    [perm] maps new dimension [i] to old dimension [perm.(i)].  Used by
    the transpose codec. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same dtype, shape,
    and byte contents. *)

(** {2 Index Utilities} *)

val index_to_offset : int array -> int array -> int
(** [index_to_offset shape idx] converts a multi-dimensional index to a
    linear element offset (C-order, not byte offset). *)

val offset_to_index : int array -> int -> int array
(** [offset_to_index shape offset] converts a linear element offset to a
    multi-dimensional index (C-order). *)
