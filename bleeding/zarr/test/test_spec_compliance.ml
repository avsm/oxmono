(** Spec compliance tests for Zarr v3

    Each test references requirements from the Zarr v3.1 specification.
*)

open Alcotest

(* Alias for Fill_value module from fill_value.ml - BEFORE open Zarr *)
module FV_mod = Zarr.Fill_value

open Zarr

(* Module aliases for nested types *)
module D = Zarr.Ztypes.Dtype
module E = Zarr.Ztypes.Endianness
module FV = Zarr.Ztypes.Fill_value
module S = Zarr.Ztypes.Separator

(** {2 Metadata Compliance} *)

module Metadata_compliance = struct
  (* "zarr_format... must be 3 here" *)
  let test_zarr_format_must_be_3 () =
    let invalid = {| {"zarr_format": 2, "node_type": "array", "shape": [10],
                      "data_type": "int32",
                      "chunk_grid": {"name": "regular", "configuration": {"chunk_shape": [10]}},
                      "chunk_key_encoding": {"name": "default", "configuration": {"separator": "/"}},
                      "codecs": [{"name": "bytes", "configuration": {"endian": "little"}}],
                      "fill_value": 0} |} in
    check bool "rejects v2"
      true (Result.is_error (Metadata.array_of_json invalid))

  (* "node_type... must be array here" *)
  let test_node_type_must_be_array () =
    let invalid = {| {"zarr_format": 3, "node_type": "group", "shape": [10],
                      "data_type": "int32",
                      "chunk_grid": {"name": "regular", "configuration": {"chunk_shape": [10]}},
                      "chunk_key_encoding": {"name": "default", "configuration": {"separator": "/"}},
                      "codecs": [{"name": "bytes", "configuration": {"endian": "little"}}],
                      "fill_value": 0} |} in
    check bool "rejects group for array"
      true (Result.is_error (Metadata.array_of_json invalid))

  (* "shape... An array of integers" *)
  let test_shape_must_be_integers () =
    let invalid = {| {"zarr_format": 3, "node_type": "array", "shape": [10.5],
                      "data_type": "int32",
                      "chunk_grid": {"name": "regular", "configuration": {"chunk_shape": [10]}},
                      "chunk_key_encoding": {"name": "default", "configuration": {"separator": "/"}},
                      "codecs": [{"name": "bytes", "configuration": {"endian": "little"}}],
                      "fill_value": 0} |} in
    check bool "rejects float shape"
      true (Result.is_error (Metadata.array_of_json invalid))

  (* "codecs MUST contain an array -> bytes codec" *)
  let test_codecs_must_have_array_to_bytes () =
    let no_a2b = {| {"zarr_format": 3, "node_type": "array", "shape": [10],
                    "data_type": "int32",
                    "chunk_grid": {"name": "regular", "configuration": {"chunk_shape": [10]}},
                    "chunk_key_encoding": {"name": "default", "configuration": {"separator": "/"}},
                    "codecs": [{"name": "gzip", "configuration": {"level": 5}}],
                    "fill_value": 0} |} in
    check bool "rejects missing array->bytes"
      true (Result.is_error (Metadata.array_of_json no_a2b))

  let tests = [
    "zarr_format must be 3", `Quick, test_zarr_format_must_be_3;
    "node_type must be array", `Quick, test_node_type_must_be_array;
    "shape must be integers", `Quick, test_shape_must_be_integers;
    "codecs must have array->bytes", `Quick, test_codecs_must_have_array_to_bytes;
  ]
end

(** {2 Fill Value Compliance} *)

module Fill_value_compliance = struct
  (* "NaN" string for float NaN *)
  let test_nan_encoding () =
    let fv = FV_mod.of_json D.Float64 (`String "NaN") in
    match fv with
    | Ok FV.NaN -> ()
    | _ -> fail "should parse NaN"

  (* "Infinity" and "-Infinity" for infinities *)
  let test_infinity_encoding () =
    (match FV_mod.of_json D.Float64 (`String "Infinity") with
    | Ok FV.Infinity -> ()
    | _ -> fail "should parse Infinity");
    (match FV_mod.of_json D.Float64 (`String "-Infinity") with
    | Ok FV.NegInfinity -> ()
    | _ -> fail "should parse -Infinity")

  (* Hex format for custom NaN patterns *)
  let test_hex_nan_encoding () =
    match FV_mod.of_json D.Float32 (`String "0x7fc00001") with
    | Ok (FV.Hex s) -> check string "hex" "0x7fc00001" s
    | _ -> fail "should parse hex NaN"

  (* Complex fill values as [real, imag] *)
  let test_complex_fill_value () =
    match FV_mod.of_json D.Complex64 (`List [`Float 1.0; `Float 2.0]) with
    | Ok (FV.Complex (re, im)) ->
      check (float 0.001) "real" 1.0 re;
      check (float 0.001) "imag" 2.0 im
    | _ -> fail "should parse complex"

  let tests = [
    "NaN encoding", `Quick, test_nan_encoding;
    "Infinity encoding", `Quick, test_infinity_encoding;
    "hex NaN encoding", `Quick, test_hex_nan_encoding;
    "complex fill value", `Quick, test_complex_fill_value;
  ]
end

(** {2 Chunk Grid Compliance} *)

module Chunk_grid_compliance = struct
  (* "regular" chunk grid *)
  let test_regular_chunk_grid () =
    let json = `Assoc [
      ("name", `String "regular");
      ("configuration", `Assoc [
        ("chunk_shape", `List [`Int 10; `Int 20])
      ])
    ] in
    match Chunk_grid.of_json json with
    | Ok (Zarr.Regular { chunk_shape }) ->
      check (Alcotest.array Alcotest.int) "chunk_shape" [|10; 20|] chunk_shape
    | Error _ -> fail "should parse regular grid"

  let tests = [
    "regular chunk grid", `Quick, test_regular_chunk_grid;
  ]
end

(** {2 Chunk Key Compliance} *)

module Chunk_key_compliance = struct
  (* "default" encoding with separator *)
  let test_default_encoding () =
    let enc = Zarr.Default { separator = S.Slash } in
    check string "encodes with c prefix" "c/1/2/3"
      (Chunk_key.encode enc [|1; 2; 3|])

  (* "v2" encoding without prefix *)
  let test_v2_encoding () =
    let enc = Zarr.V2 { separator = S.Dot } in
    check string "encodes without c prefix" "1.2.3"
      (Chunk_key.encode enc [|1; 2; 3|])

  let tests = [
    "default encoding", `Quick, test_default_encoding;
    "v2 encoding", `Quick, test_v2_encoding;
  ]
end

(** {2 Sharding Compliance} *)

module Sharding_compliance = struct
  (* "Empty inner chunks are denoted by setting both offset and nbytes to 2^64 - 1" *)
  let test_empty_chunk_marker () =
    check int64 "empty marker is 2^64-1 (as signed -1)"
      Int64.minus_one Codecs.Sharding.empty_marker

  let tests = [
    "empty chunk marker", `Quick, test_empty_chunk_marker;
  ]
end

(** {2 Codec Compliance} *)

module Codec_compliance = struct
  (* Bytes codec endianness *)
  let test_bytes_endianness () =
    let arr = Ndarray.create D.Int32 [|2|] in
    Ndarray.set arr [|0|] (`Int32 0x01020304l);

    let codec_le = Codecs.Bytes_codec.create E.Little in
    let bytes_le = codec_le.encode arr in
    check int "LE first byte" 4 (Char.code (Bytes.get bytes_le 0));

    let codec_be = Codecs.Bytes_codec.create E.Big in
    let bytes_be = codec_be.encode arr in
    check int "BE first byte" 1 (Char.code (Bytes.get bytes_be 0))

  (* CRC32C uses Castagnoli polynomial *)
  let test_crc32c_algorithm () =
    let input = Bytes.of_string "123456789" in
    let encoded = Codecs.Crc32c.encode input in
    let checksum = Bytes.get_int32_le encoded 9 in
    (* RFC 3720 / iSCSI test vector *)
    check int32 "CRC32C test vector" 0xe3069283l checksum

  let tests = [
    "bytes endianness", `Quick, test_bytes_endianness;
    "crc32c algorithm", `Quick, test_crc32c_algorithm;
  ]
end

(** {2 Data Type Compliance} *)

module Data_type_compliance = struct
  (* All required data types *)
  let test_required_dtypes () =
    let required = [
      "bool"; "int8"; "int16"; "int32"; "int64";
      "uint8"; "uint16"; "uint32"; "uint64";
      "float16"; "float32"; "float64";
      "complex64"; "complex128"
    ] in
    List.iter (fun name ->
      match Data_type.of_string name with
      | Ok _ -> ()
      | Error _ -> fail ("should support " ^ name)
    ) required

  (* Raw data types r* *)
  let test_raw_dtypes () =
    check bool "r8" true (Result.is_ok (Data_type.of_string "r8"));
    check bool "r16" true (Result.is_ok (Data_type.of_string "r16"));
    check bool "r32" true (Result.is_ok (Data_type.of_string "r32"))

  let tests = [
    "required dtypes", `Quick, test_required_dtypes;
    "raw dtypes", `Quick, test_raw_dtypes;
  ]
end

let tests =
  Metadata_compliance.tests @
  Fill_value_compliance.tests @
  Chunk_grid_compliance.tests @
  Chunk_key_compliance.tests @
  Sharding_compliance.tests @
  Codec_compliance.tests @
  Data_type_compliance.tests
