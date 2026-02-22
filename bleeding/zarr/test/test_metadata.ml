(** Tests for metadata parsing and serialization *)

open Alcotest
open Zarr

(* Module aliases for nested types *)
module D = Zarr.Ztypes.Dtype
module E = Zarr.Ztypes.Endianness
module FV = Zarr.Ztypes.Fill_value

(* Helper for substring check - must be defined before use *)
module String_ext = struct
  let is_substring s ~sub =
    let len = String.length sub in
    let slen = String.length s in
    if len > slen then false
    else begin
      let rec check i =
        if i > slen - len then false
        else if String.sub s i len = sub then true
        else check (i + 1)
      in
      check 0
    end
end

let spec_example_array = {|
{
    "zarr_format": 3,
    "node_type": "array",
    "shape": [10000, 1000],
    "dimension_names": ["rows", "columns"],
    "data_type": "float64",
    "chunk_grid": {
        "name": "regular",
        "configuration": { "chunk_shape": [1000, 100] }
    },
    "chunk_key_encoding": {
        "name": "default",
        "configuration": { "separator": "/" }
    },
    "codecs": [{
        "name": "bytes",
        "configuration": { "endian": "little" }
    }],
    "fill_value": "NaN"
}
|}

let test_parse_spec_example () =
  match Metadata.array_of_json spec_example_array with
  | Error e ->
    let msg = match e with
      | `Invalid_metadata s -> s
      | _ -> "unknown error"
    in
    fail ("failed to parse: " ^ msg)
  | Ok meta ->
    check (array int) "shape" [|10000; 1000|] meta.shape;
    check int "zarr_format" 3 meta.zarr_format;
    (match meta.data_type with
     | D.Float64 -> ()
     | _ -> fail "expected Float64");
    (match meta.fill_value with
     | FV.NaN -> ()
     | _ -> fail "expected NaN fill value");
    (match meta.chunk_grid with
     | Zarr.Regular { chunk_shape } ->
       check (array int) "chunk_shape" [|1000; 100|] chunk_shape);
    (match meta.dimension_names with
     | Some names ->
       check (array (option string)) "dimension_names"
         [|Some "rows"; Some "columns"|] names
     | None -> fail "expected dimension_names")

let test_reject_zarr_format_2 () =
  let json = {| {"zarr_format": 2, "node_type": "array", "shape": [10], "data_type": "int32",
                 "chunk_grid": {"name": "regular", "configuration": {"chunk_shape": [10]}},
                 "chunk_key_encoding": {"name": "default", "configuration": {"separator": "/"}},
                 "codecs": [{"name": "bytes", "configuration": {"endian": "little"}}],
                 "fill_value": 0} |} in
  match Metadata.array_of_json json with
  | Error (`Invalid_metadata msg) ->
    check bool "mentions zarr_format" true (String.length msg > 0)
  | Ok _ -> fail "should reject zarr_format 2"
  | Error _ -> fail "wrong error type"

let test_reject_node_type_group () =
  let json = {| {"zarr_format": 3, "node_type": "group", "shape": [10], "data_type": "int32",
                 "chunk_grid": {"name": "regular", "configuration": {"chunk_shape": [10]}},
                 "chunk_key_encoding": {"name": "default", "configuration": {"separator": "/"}},
                 "codecs": [{"name": "bytes", "configuration": {"endian": "little"}}],
                 "fill_value": 0} |} in
  match Metadata.array_of_json json with
  | Error (`Invalid_metadata _) -> ()
  | Ok _ -> fail "should reject node_type group for array"
  | Error _ -> fail "wrong error type"

let test_reject_missing_array_to_bytes () =
  let json = {| {"zarr_format": 3, "node_type": "array", "shape": [10], "data_type": "int32",
                 "chunk_grid": {"name": "regular", "configuration": {"chunk_shape": [10]}},
                 "chunk_key_encoding": {"name": "default", "configuration": {"separator": "/"}},
                 "codecs": [{"name": "gzip", "configuration": {"level": 5}}],
                 "fill_value": 0} |} in
  match Metadata.array_of_json json with
  | Error (`Invalid_metadata msg) ->
    check bool "mentions array->bytes" true
      (String_ext.is_substring msg ~sub:"array")
  | Ok _ -> fail "should reject missing array->bytes codec"
  | Error _ -> fail "wrong error type"

let test_parse_group_metadata () =
  let json = {| {"zarr_format": 3, "node_type": "group", "attributes": {"foo": "bar"}} |} in
  match Metadata.group_of_json json with
  | Error e ->
    let msg = match e with `Invalid_metadata s -> s | _ -> "?" in
    fail ("failed to parse group: " ^ msg)
  | Ok meta ->
    check int "zarr_format" 3 meta.zarr_format;
    (match meta.attributes with
     | Some (`Assoc [("foo", `String "bar")]) -> ()
     | _ -> fail "expected attributes")

let test_array_roundtrip () =
  let meta = Metadata.create_array_metadata
    ~shape:[|100; 100|]
    ~chunks:[|10; 10|]
    ~dtype:D.Float32
    ~fill_value:(FV.Float 0.0)
    ~codecs:[Zarr.Bytes { endian = Some E.Little }]
    () in
  let json = Metadata.array_to_json meta in
  match Metadata.array_of_json json with
  | Error _ -> fail "roundtrip failed"
  | Ok meta2 ->
    check (array int) "shape" meta.shape meta2.shape;
    check int "zarr_format" meta.zarr_format meta2.zarr_format

let test_group_roundtrip () =
  let meta = Metadata.create_group_metadata
    ~attributes:(Some (`Assoc [("key", `String "value")]))
    () in
  let json = Metadata.group_to_json meta in
  match Metadata.group_of_json json with
  | Error _ -> fail "group roundtrip failed"
  | Ok meta2 ->
    check int "zarr_format" meta.zarr_format meta2.zarr_format

let tests = [
  "parse spec example", `Quick, test_parse_spec_example;
  "reject zarr_format 2", `Quick, test_reject_zarr_format_2;
  "reject node_type group", `Quick, test_reject_node_type_group;
  "reject missing array->bytes", `Quick, test_reject_missing_array_to_bytes;
  "parse group metadata", `Quick, test_parse_group_metadata;
  "array roundtrip", `Quick, test_array_roundtrip;
  "group roundtrip", `Quick, test_group_roundtrip;
]
