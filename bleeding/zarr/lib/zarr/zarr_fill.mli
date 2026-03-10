(** {1 Fill Values}

    Fill values specify the default value for uninitialised array regions.

    See {{:https://zarr-specs.readthedocs.io/en/latest/v3/core/index.html}
    Zarr v3.1 spec, "Fill Value" section}.

    Every Zarr v3 array must have a fill value in its metadata.  When a
    chunk is missing from the store, it is implicitly filled with this
    value.  The sharding codec also uses it for uninitialised inner
    chunks.

    {2 JSON representation}

    | Type       | Allowed JSON values                                    |
    |:-----------|:-------------------------------------------------------|
    | bool       | [true], [false]                                        |
    | int*       | JSON number (no fraction/exponent), in range            |
    | uint*      | JSON number (non-negative), in range                   |
    | float*     | JSON number, or string: ["NaN"], ["Infinity"],         |
    |            | ["-Infinity"], or hex pattern (["0xYYYYYYYY"])          |
    | r<N>       | Base64 string or array of integers 0-255               | *)

(** Fill value representation. *)
type t =
  | Bool of bool
  | Int of int64       (** For signed integer types *)
  | Uint of int64      (** For unsigned integer types *)
  | Float of float     (** For float32 and float64 *)
  | Raw of bytes       (** For raw byte types *)
  | NaN                (** IEEE 754 NaN *)
  | Infinity           (** Positive infinity *)
  | NegInfinity        (** Negative infinity *)
  | Hex of string      (** Custom float bit pattern, e.g. ["0x7fc00000"] *)

(** {2 Defaults} *)

val default : Zarr_dtype.t -> t
(** [default dtype] returns the zero / false fill value for [dtype]. *)

val to_float : t -> float
(** [to_float fv] interprets [fv] as a float. For non-float fill values,
    returns [0.0]. *)

(** {2 Byte-Level Operations} *)

val fill_bytes : Zarr_dtype.t -> t -> bytes -> unit
(** [fill_bytes dtype fv buf] fills [buf] by writing the fill value element
    at offset 0 and then replicating it across the entire buffer using
    doubling copies.

    The buffer length must be a multiple of [Zarr_dtype.byte_size dtype].
    Uses native (little-endian) byte order. *)

(** {2 JSON Serialization} *)

val of_json : Zarr_dtype.t -> Jsont.json -> (t, string) result
(** [of_json dtype json] parses a fill value from its JSON representation
    for the given data type.

    Supports all JSON formats required by the Zarr v3.1 spec:
    - Booleans for [bool]
    - Numbers and string-encoded integers for integer types
    - Numbers, ["NaN"], ["Infinity"], ["-Infinity"], and hex patterns
      for float types
    - Base64 strings and integer arrays for raw types

    @return [Error msg] if the JSON value is not valid for [dtype]. *)

val to_json : t -> Jsont.json
(** [to_json fv] converts a fill value to its JSON representation. *)
