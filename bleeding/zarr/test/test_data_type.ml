(** Tests for data type operations *)

open Alcotest
open Zarr

let dtype_testable = testable
  (fun fmt dt -> Format.pp_print_string fmt (Dtype.to_string dt))
  (=)

let test_dtype_size () =
  check int "bool size" 1 (Dtype.byte_size Bool);
  check int "int8 size" 1 (Dtype.byte_size Int8);
  check int "int16 size" 2 (Dtype.byte_size Int16);
  check int "int32 size" 4 (Dtype.byte_size Int32);
  check int "int64 size" 8 (Dtype.byte_size Int64);
  check int "uint8 size" 1 (Dtype.byte_size Uint8);
  check int "uint16 size" 2 (Dtype.byte_size Uint16);
  check int "uint32 size" 4 (Dtype.byte_size Uint32);
  check int "uint64 size" 8 (Dtype.byte_size Uint64);
  check int "float32 size" 4 (Dtype.byte_size Float32);
  check int "float64 size" 8 (Dtype.byte_size Float64);
  check int "raw8 size" 1 (Dtype.byte_size (Raw 8));
  check int "raw16 size" 2 (Dtype.byte_size (Raw 16))

let test_dtype_of_string () =
  check (option dtype_testable) "parse bool"
    (Some Dtype.Bool) (Dtype.of_string "bool");
  check (option dtype_testable) "parse int8"
    (Some Dtype.Int8) (Dtype.of_string "int8");
  check (option dtype_testable) "parse int16"
    (Some Dtype.Int16) (Dtype.of_string "int16");
  check (option dtype_testable) "parse int32"
    (Some Dtype.Int32) (Dtype.of_string "int32");
  check (option dtype_testable) "parse int64"
    (Some Dtype.Int64) (Dtype.of_string "int64");
  check (option dtype_testable) "parse uint8"
    (Some Dtype.Uint8) (Dtype.of_string "uint8");
  check (option dtype_testable) "parse uint16"
    (Some Dtype.Uint16) (Dtype.of_string "uint16");
  check (option dtype_testable) "parse uint32"
    (Some Dtype.Uint32) (Dtype.of_string "uint32");
  check (option dtype_testable) "parse uint64"
    (Some Dtype.Uint64) (Dtype.of_string "uint64");
  check (option dtype_testable) "parse float32"
    (Some Dtype.Float32) (Dtype.of_string "float32");
  check (option dtype_testable) "parse float64"
    (Some Dtype.Float64) (Dtype.of_string "float64");
  check (option dtype_testable) "parse r8"
    (Some (Dtype.Raw 8)) (Dtype.of_string "r8");
  check (option dtype_testable) "parse r16"
    (Some (Dtype.Raw 16)) (Dtype.of_string "r16")

let test_dtype_of_string_errors () =
  check (option dtype_testable) "parse invalid"
    None (Dtype.of_string "foo");
  check (option dtype_testable) "parse r5"
    None (Dtype.of_string "r5")

let test_dtype_to_string () =
  check string "bool to_string" "bool" (Dtype.to_string Bool);
  check string "int32 to_string" "int32" (Dtype.to_string Int32);
  check string "float64 to_string" "float64" (Dtype.to_string Float64);
  check string "r8 to_string" "r8" (Dtype.to_string (Raw 8))

let test_dtype_predicates () =
  check bool "int32 is_integer" true (Dtype.is_integer Int32);
  check bool "float64 is_integer" false (Dtype.is_integer Float64);
  check bool "int32 is_signed" true (Dtype.is_signed Int32);
  check bool "uint32 is_signed" false (Dtype.is_signed Uint32);
  check bool "uint32 is_unsigned" true (Dtype.is_unsigned Uint32);
  check bool "int32 is_unsigned" false (Dtype.is_unsigned Int32);
  check bool "float32 is_float" true (Dtype.is_float Float32);
  check bool "int32 is_float" false (Dtype.is_float Int32)

let test_requires_endianness () =
  check bool "bool no endian" false (Dtype.requires_endianness Bool);
  check bool "int8 no endian" false (Dtype.requires_endianness Int8);
  check bool "uint8 no endian" false (Dtype.requires_endianness Uint8);
  check bool "int16 needs endian" true (Dtype.requires_endianness Int16);
  check bool "int32 needs endian" true (Dtype.requires_endianness Int32);
  check bool "float64 needs endian" true (Dtype.requires_endianness Float64)

let tests = [
  "size", `Quick, test_dtype_size;
  "of_string", `Quick, test_dtype_of_string;
  "of_string errors", `Quick, test_dtype_of_string_errors;
  "to_string", `Quick, test_dtype_to_string;
  "predicates", `Quick, test_dtype_predicates;
  "requires_endianness", `Quick, test_requires_endianness;
]
