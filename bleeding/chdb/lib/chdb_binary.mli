(** High-performance RowBinary parsing using {!Base_bigstring}.

    Provides zero-copy readers for ClickHouse
    {{:https://clickhouse.com/docs/en/interfaces/formats#rowbinary}RowBinary}
    format values, backed by [base_bigstring]'s optimized accessors
    with locality annotations.

    {2 Usage}

    {[
      Query.with_buffer conn ~format:RowBinary
        "SELECT toUInt64(number) FROM numbers(1000)"
        ~f:(fun buf ->
          let sum = Binary.sum_uint64 buf in
          Printf.printf "sum = %Ld\n" sum)
    ]} *)

type buf = Base_bigstring.t
(** Buffer type, compatible with {!Chdb_result.bigstring}. *)

(** {2 Bounds-checked accessors}

    Each function reads a little-endian value at the given byte
    offset, raising [Invalid_argument] if [pos] is out of bounds. *)

val get_uint8 : buf -> pos:int -> int
val get_int16_le : buf -> pos:int -> int
val get_uint16_le : buf -> pos:int -> int
val get_int32_le : buf -> pos:int -> int
val get_uint32_le : buf -> pos:int -> int
val get_int32_t_le : buf -> pos:int -> Int32.t
val get_int64_t_le : buf -> pos:int -> Int64.t
val get_int64_le_trunc : buf -> pos:int -> int
val get_float64_le : buf -> pos:int -> float

(** {2 Unsafe accessors}

    Same as above but without bounds checks.  The caller must
    ensure [pos] and [pos + width - 1] are within the buffer. *)

val unsafe_get_uint8 : buf -> pos:int -> int
val unsafe_get_int16_le : buf -> pos:int -> int
val unsafe_get_uint16_le : buf -> pos:int -> int
val unsafe_get_int32_le : buf -> pos:int -> int
val unsafe_get_uint32_le : buf -> pos:int -> int
val unsafe_get_int32_t_le : buf -> pos:int -> Int32.t
val unsafe_get_int64_t_le : buf -> pos:int -> Int64.t
val unsafe_get_int64_le_trunc : buf -> pos:int -> int
val unsafe_get_float64_le : buf -> pos:int -> float

(** {2 RowBinary variable-length encoding} *)

val get_varint : buf -> pos:int -> int * int
(** [get_varint buf ~pos] reads a LEB128-encoded unsigned integer
    starting at [pos].  Returns [(value, bytes_consumed)].
    Used internally for String length prefixes in RowBinary. *)

val get_string : buf -> pos:int -> string * int
(** [get_string buf ~pos] reads a RowBinary String value
    (varint length prefix followed by raw bytes).
    Returns [(string_value, total_bytes_consumed)]. *)

(** {2 Column iterators}

    Iterate over single-type RowBinary columns where values are
    packed contiguously at a fixed width.  The callback receives
    [(index, value)] for each element. *)

val iter_uint64 : buf -> f:(int -> Int64.t -> unit) -> unit
(** [iter_uint64 buf ~f] iterates over UInt64 values (8 bytes LE). *)

val fold_uint64 : buf -> init:'a -> f:('a -> int -> Int64.t -> 'a) -> 'a
(** [fold_uint64 buf ~init ~f] folds over UInt64 values. *)

val sum_uint64 : buf -> Int64.t
(** [sum_uint64 buf] returns the sum of all UInt64 values. *)

val iter_uint32 : buf -> f:(int -> int -> unit) -> unit
(** [iter_uint32 buf ~f] iterates over UInt32 values (4 bytes LE). *)

val fold_uint32 : buf -> init:'a -> f:('a -> int -> int -> 'a) -> 'a
(** [fold_uint32 buf ~init ~f] folds over UInt32 values. *)

val iter_float64 : buf -> f:(int -> float -> unit) -> unit
(** [iter_float64 buf ~f] iterates over Float64 values (8 bytes LE). *)

val length : buf -> int
(** [length buf] is the buffer length in bytes. *)
