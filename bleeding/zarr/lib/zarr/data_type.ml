(** Data type operations for Zarr v3 *)

module D = Ztypes.Dtype

(** Get the size in bytes for a data type *)
let size = function
  | D.Bool -> 1
  | D.Int8 -> 1
  | D.Int16 -> 2
  | D.Int32 -> 4
  | D.Int64 -> 8
  | D.Uint8 -> 1
  | D.Uint16 -> 2
  | D.Uint32 -> 4
  | D.Uint64 -> 8
  | D.Float16 -> 2
  | D.Float32 -> 4
  | D.Float64 -> 8
  | D.Complex64 -> 8
  | D.Complex128 -> 16
  | D.Raw bits -> bits / 8

(** Parse a data type from its string representation *)
let of_string s =
  match String.lowercase_ascii s with
  | "bool" -> Ok D.Bool
  | "int8" -> Ok D.Int8
  | "int16" -> Ok D.Int16
  | "int32" -> Ok D.Int32
  | "int64" -> Ok D.Int64
  | "uint8" -> Ok D.Uint8
  | "uint16" -> Ok D.Uint16
  | "uint32" -> Ok D.Uint32
  | "uint64" -> Ok D.Uint64
  | "float16" -> Ok D.Float16
  | "float32" -> Ok D.Float32
  | "float64" -> Ok D.Float64
  | "complex64" -> Ok D.Complex64
  | "complex128" -> Ok D.Complex128
  | s when String.length s > 1 && s.[0] = 'r' ->
    (try
       let bits = int_of_string (String.sub s 1 (String.length s - 1)) in
       if bits mod 8 = 0 && bits > 0 then Ok (D.Raw bits)
       else Error (`Unsupported_dtype s)
     with Failure _ -> Error (`Unsupported_dtype s))
  | s -> Error (`Unsupported_dtype s)

(** Convert a data type to its string representation *)
let to_string = function
  | D.Bool -> "bool"
  | D.Int8 -> "int8"
  | D.Int16 -> "int16"
  | D.Int32 -> "int32"
  | D.Int64 -> "int64"
  | D.Uint8 -> "uint8"
  | D.Uint16 -> "uint16"
  | D.Uint32 -> "uint32"
  | D.Uint64 -> "uint64"
  | D.Float16 -> "float16"
  | D.Float32 -> "float32"
  | D.Float64 -> "float64"
  | D.Complex64 -> "complex64"
  | D.Complex128 -> "complex128"
  | D.Raw bits -> "r" ^ string_of_int bits

(** Check if a data type is an integer type *)
let is_integer = function
  | D.Int8 | D.Int16 | D.Int32 | D.Int64
  | D.Uint8 | D.Uint16 | D.Uint32 | D.Uint64 -> true
  | _ -> false

(** Check if a data type is a signed integer type *)
let is_signed_integer = function
  | D.Int8 | D.Int16 | D.Int32 | D.Int64 -> true
  | _ -> false

(** Check if a data type is an unsigned integer type *)
let is_unsigned_integer = function
  | D.Uint8 | D.Uint16 | D.Uint32 | D.Uint64 -> true
  | _ -> false

(** Check if a data type is a floating-point type *)
let is_float = function
  | D.Float16 | D.Float32 | D.Float64 -> true
  | _ -> false

(** Check if a data type is a complex type *)
let is_complex = function
  | D.Complex64 | D.Complex128 -> true
  | _ -> false

(** Check if a data type requires endianness specification (multi-byte types) *)
let requires_endianness = function
  | D.Bool | D.Int8 | D.Uint8 -> false
  | D.Raw bits -> bits > 8
  | _ -> true

(** Get the Bigarray kind for a data type *)
let to_bigarray_kind : type a b. D.t -> (a, b) Bigarray.kind option = function
  | D.Int8 -> Some (Obj.magic Bigarray.Int8_signed)
  | D.Int16 -> Some (Obj.magic Bigarray.Int16_signed)
  | D.Int32 -> Some (Obj.magic Bigarray.Int32)
  | D.Int64 -> Some (Obj.magic Bigarray.Int64)
  | D.Uint8 -> Some (Obj.magic Bigarray.Int8_unsigned)
  | D.Uint16 -> Some (Obj.magic Bigarray.Int16_unsigned)
  | D.Float32 -> Some (Obj.magic Bigarray.Float32)
  | D.Float64 -> Some (Obj.magic Bigarray.Float64)
  | D.Complex64 -> Some (Obj.magic Bigarray.Complex32)
  | D.Complex128 -> Some (Obj.magic Bigarray.Complex64)
  | _ -> None

(** Get the bigarray kind as a polymorphic variant for pattern matching *)
type bigarray_kind =
  | BA_int8_signed
  | BA_int8_unsigned
  | BA_int16_signed
  | BA_int16_unsigned
  | BA_int32
  | BA_int64
  | BA_float32
  | BA_float64
  | BA_complex32
  | BA_complex64
  | BA_char
  | BA_nativeint
  | BA_unsupported

let bigarray_kind_of_dtype = function
  | D.Bool -> BA_int8_unsigned
  | D.Int8 -> BA_int8_signed
  | D.Int16 -> BA_int16_signed
  | D.Int32 -> BA_int32
  | D.Int64 -> BA_int64
  | D.Uint8 -> BA_int8_unsigned
  | D.Uint16 -> BA_int16_unsigned
  | D.Uint32 -> BA_int64  (* Use int64 for uint32 to avoid overflow *)
  | D.Uint64 -> BA_int64  (* Best approximation, may overflow for large values *)
  | D.Float16 -> BA_float32  (* Store float16 in float32 *)
  | D.Float32 -> BA_float32
  | D.Float64 -> BA_float64
  | D.Complex64 -> BA_complex32
  | D.Complex128 -> BA_complex64
  | D.Raw _ -> BA_char
