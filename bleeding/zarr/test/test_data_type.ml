(** Tests for data type operations *)

open Alcotest
open Zarr

(* Module aliases for nested types *)
module D = Zarr.Ztypes.Dtype

let dtype_testable = testable
  (fun fmt dt -> Format.pp_print_string fmt (Data_type.to_string dt))
  (=)

let error_testable : Zarr.error testable = testable
  (fun fmt e -> match e with
    | `Unsupported_dtype s -> Format.fprintf fmt "`Unsupported_dtype %s" s
    | `Invalid_metadata s -> Format.fprintf fmt "`Invalid_metadata %s" s
    | `Codec_error s -> Format.fprintf fmt "`Codec_error %s" s
    | `Store_error s -> Format.fprintf fmt "`Store_error %s" s
    | `Not_found s -> Format.fprintf fmt "`Not_found %s" s
    | `Invalid_slice s -> Format.fprintf fmt "`Invalid_slice %s" s
    | `Checksum_mismatch -> Format.fprintf fmt "`Checksum_mismatch"
    | `Invalid_chunk_coords s -> Format.fprintf fmt "`Invalid_chunk_coords %s" s
    | `Shape_mismatch s -> Format.fprintf fmt "`Shape_mismatch %s" s)
  (=)

let test_dtype_size () =
  check int "bool size" 1 (Data_type.size D.Bool);
  check int "int8 size" 1 (Data_type.size D.Int8);
  check int "int16 size" 2 (Data_type.size D.Int16);
  check int "int32 size" 4 (Data_type.size D.Int32);
  check int "int64 size" 8 (Data_type.size D.Int64);
  check int "uint8 size" 1 (Data_type.size D.Uint8);
  check int "uint16 size" 2 (Data_type.size D.Uint16);
  check int "uint32 size" 4 (Data_type.size D.Uint32);
  check int "uint64 size" 8 (Data_type.size D.Uint64);
  check int "float16 size" 2 (Data_type.size D.Float16);
  check int "float32 size" 4 (Data_type.size D.Float32);
  check int "float64 size" 8 (Data_type.size D.Float64);
  check int "complex64 size" 8 (Data_type.size D.Complex64);
  check int "complex128 size" 16 (Data_type.size D.Complex128);
  check int "raw8 size" 1 (Data_type.size (D.Raw 8));
  check int "raw16 size" 2 (Data_type.size (D.Raw 16))

let test_dtype_of_string () =
  check (result dtype_testable error_testable) "parse bool"
    (Ok D.Bool) (Data_type.of_string "bool");
  check (result dtype_testable error_testable) "parse int8"
    (Ok D.Int8) (Data_type.of_string "int8");
  check (result dtype_testable error_testable) "parse int16"
    (Ok D.Int16) (Data_type.of_string "int16");
  check (result dtype_testable error_testable) "parse int32"
    (Ok D.Int32) (Data_type.of_string "int32");
  check (result dtype_testable error_testable) "parse int64"
    (Ok D.Int64) (Data_type.of_string "int64");
  check (result dtype_testable error_testable) "parse uint8"
    (Ok D.Uint8) (Data_type.of_string "uint8");
  check (result dtype_testable error_testable) "parse uint16"
    (Ok D.Uint16) (Data_type.of_string "uint16");
  check (result dtype_testable error_testable) "parse uint32"
    (Ok D.Uint32) (Data_type.of_string "uint32");
  check (result dtype_testable error_testable) "parse uint64"
    (Ok D.Uint64) (Data_type.of_string "uint64");
  check (result dtype_testable error_testable) "parse float16"
    (Ok D.Float16) (Data_type.of_string "float16");
  check (result dtype_testable error_testable) "parse float32"
    (Ok D.Float32) (Data_type.of_string "float32");
  check (result dtype_testable error_testable) "parse float64"
    (Ok D.Float64) (Data_type.of_string "float64");
  check (result dtype_testable error_testable) "parse complex64"
    (Ok D.Complex64) (Data_type.of_string "complex64");
  check (result dtype_testable error_testable) "parse complex128"
    (Ok D.Complex128) (Data_type.of_string "complex128");
  check (result dtype_testable error_testable) "parse r8"
    (Ok (D.Raw 8)) (Data_type.of_string "r8");
  check (result dtype_testable error_testable) "parse r16"
    (Ok (D.Raw 16)) (Data_type.of_string "r16")

let test_dtype_of_string_errors () =
  check (result dtype_testable error_testable) "parse invalid"
    (Error (`Unsupported_dtype "foo")) (Data_type.of_string "foo");
  check (result dtype_testable error_testable) "parse r5"
    (Error (`Unsupported_dtype "r5")) (Data_type.of_string "r5")

let test_dtype_to_string () =
  check string "bool to_string" "bool" (Data_type.to_string D.Bool);
  check string "int32 to_string" "int32" (Data_type.to_string D.Int32);
  check string "float64 to_string" "float64" (Data_type.to_string D.Float64);
  check string "complex128 to_string" "complex128" (Data_type.to_string D.Complex128);
  check string "r8 to_string" "r8" (Data_type.to_string (D.Raw 8))

let test_dtype_predicates () =
  check bool "int32 is_integer" true (Data_type.is_integer D.Int32);
  check bool "float64 is_integer" false (Data_type.is_integer D.Float64);
  check bool "int32 is_signed" true (Data_type.is_signed_integer D.Int32);
  check bool "uint32 is_signed" false (Data_type.is_signed_integer D.Uint32);
  check bool "uint32 is_unsigned" true (Data_type.is_unsigned_integer D.Uint32);
  check bool "int32 is_unsigned" false (Data_type.is_unsigned_integer D.Int32);
  check bool "float32 is_float" true (Data_type.is_float D.Float32);
  check bool "int32 is_float" false (Data_type.is_float D.Int32);
  check bool "complex64 is_complex" true (Data_type.is_complex D.Complex64);
  check bool "float64 is_complex" false (Data_type.is_complex D.Float64)

let test_requires_endianness () =
  check bool "bool no endian" false (Data_type.requires_endianness D.Bool);
  check bool "int8 no endian" false (Data_type.requires_endianness D.Int8);
  check bool "uint8 no endian" false (Data_type.requires_endianness D.Uint8);
  check bool "int16 needs endian" true (Data_type.requires_endianness D.Int16);
  check bool "int32 needs endian" true (Data_type.requires_endianness D.Int32);
  check bool "float64 needs endian" true (Data_type.requires_endianness D.Float64)

let tests = [
  "size", `Quick, test_dtype_size;
  "of_string", `Quick, test_dtype_of_string;
  "of_string errors", `Quick, test_dtype_of_string_errors;
  "to_string", `Quick, test_dtype_to_string;
  "predicates", `Quick, test_dtype_predicates;
  "requires_endianness", `Quick, test_requires_endianness;
]
