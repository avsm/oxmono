(** {1 Zarr v3 Data Types}

    Defines the supported data types for array elements, following the
    Zarr v3.1 specification. Float16 and complex types are not supported. *)

(** Data type of array elements.

    Each variant corresponds to a Zarr v3 core data type with fixed element
    size. See {{:https://zarr-specs.readthedocs.io/en/latest/v3/core/index.html}
    Zarr v3 spec, section "Data Types"}. *)
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
  | Float32
  | Float64
  | Raw of int  (** Raw bytes with specified bit width (must be multiple of 8) *)

(** Byte order for multi-byte data types.

    Single-byte types (bool, int8, uint8) do not require endianness. *)
type endian =
  | Little
  | Big

(** Byte size of a single element for the given data type. *)
let byte_size = function
  | Bool -> 1
  | Int8 -> 1
  | Int16 -> 2
  | Int32 -> 4
  | Int64 -> 8
  | Uint8 -> 1
  | Uint16 -> 2
  | Uint32 -> 4
  | Uint64 -> 8
  | Float32 -> 4
  | Float64 -> 8
  | Raw bits -> bits / 8

(** Whether this data type requires an endianness specification.

    Returns [false] for single-byte types ([Bool], [Int8], [Uint8]) and
    all raw types (raw bytes are opaque and must never be byte-swapped,
    per Zarr v3.1 spec). *)
let requires_endianness = function
  | Bool | Int8 | Uint8 -> false
  | Raw _ -> false
  | _ -> true

(** Parse a data type from its Zarr v3 string identifier.

    Recognised identifiers: ["bool"], ["int8"], ["int16"], ["int32"],
    ["int64"], ["uint8"], ["uint16"], ["uint32"], ["uint64"],
    ["float32"], ["float64"], and raw types like ["r8"], ["r16"]. *)
let of_string s =
  match String.lowercase_ascii s with
  | "bool" -> Some Bool
  | "int8" -> Some Int8
  | "int16" -> Some Int16
  | "int32" -> Some Int32
  | "int64" -> Some Int64
  | "uint8" -> Some Uint8
  | "uint16" -> Some Uint16
  | "uint32" -> Some Uint32
  | "uint64" -> Some Uint64
  | "float32" -> Some Float32
  | "float64" -> Some Float64
  | s when String.length s > 1 && s.[0] = 'r' ->
    (try
       let bits = int_of_string (String.sub s 1 (String.length s - 1)) in
       if bits mod 8 = 0 && bits > 0 then Some (Raw bits)
       else None
     with Failure _ -> None)
  | _ -> None

(** Convert a data type to its Zarr v3 string identifier. *)
let to_string = function
  | Bool -> "bool"
  | Int8 -> "int8"
  | Int16 -> "int16"
  | Int32 -> "int32"
  | Int64 -> "int64"
  | Uint8 -> "uint8"
  | Uint16 -> "uint16"
  | Uint32 -> "uint32"
  | Uint64 -> "uint64"
  | Float32 -> "float32"
  | Float64 -> "float64"
  | Raw bits -> "r" ^ string_of_int bits

let is_integer = function
  | Int8 | Int16 | Int32 | Int64
  | Uint8 | Uint16 | Uint32 | Uint64 -> true
  | _ -> false

let is_signed = function
  | Int8 | Int16 | Int32 | Int64 -> true
  | _ -> false

let is_unsigned = function
  | Uint8 | Uint16 | Uint32 | Uint64 -> true
  | _ -> false

let is_float = function
  | Float32 | Float64 -> true
  | _ -> false
