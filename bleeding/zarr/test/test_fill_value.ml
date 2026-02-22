(** Tests for fill value handling *)

open Alcotest

(* Alias for Fill_value module from fill_value.ml *)
module FV_mod = Zarr.Fill_value

(* Module aliases for nested types *)
module D = Zarr.Ztypes.Dtype
module E = Zarr.Ztypes.Endianness
module FV = Zarr.Ztypes.Fill_value

let fill_value_testable = testable
  (fun fmt fv -> match fv with
    | FV.Bool b -> Format.fprintf fmt "Bool %b" b
    | FV.Int i -> Format.fprintf fmt "Int %Ld" i
    | FV.Uint u -> Format.fprintf fmt "Uint %Ld" u
    | FV.Float f -> Format.fprintf fmt "Float %f" f
    | FV.Complex (re, im) -> Format.fprintf fmt "Complex (%f, %f)" re im
    | FV.Raw b -> Format.fprintf fmt "Raw %d bytes" (Bytes.length b)
    | FV.NaN -> Format.fprintf fmt "NaN"
    | FV.Infinity -> Format.fprintf fmt "Infinity"
    | FV.NegInfinity -> Format.fprintf fmt "NegInfinity"
    | FV.Hex s -> Format.fprintf fmt "Hex %s" s)
  (fun a b -> match a, b with
    | FV.Bool a, FV.Bool b -> a = b
    | FV.Int a, FV.Int b -> Int64.equal a b
    | FV.Uint a, FV.Uint b -> Int64.equal a b
    | FV.Float a, FV.Float b -> Float.equal a b || (Float.is_nan a && Float.is_nan b)
    | FV.Complex (ar, ai), FV.Complex (br, bi) ->
      (Float.equal ar br || (Float.is_nan ar && Float.is_nan br)) &&
      (Float.equal ai bi || (Float.is_nan ai && Float.is_nan bi))
    | FV.Raw a, FV.Raw b -> Bytes.equal a b
    | FV.NaN, FV.NaN -> true
    | FV.Infinity, FV.Infinity -> true
    | FV.NegInfinity, FV.NegInfinity -> true
    | FV.Hex a, FV.Hex b -> String.equal a b
    | _ -> false)

let error_testable : Zarr.error testable = testable
  (fun fmt _ -> Format.pp_print_string fmt "<error>")
  (fun _ _ -> true)  (* Just check it's an error *)

let test_fill_value_bool () =
  check (result fill_value_testable error_testable) "true"
    (Ok (FV.Bool true)) (FV_mod.of_json D.Bool (`Bool true));
  check (result fill_value_testable error_testable) "false"
    (Ok (FV.Bool false)) (FV_mod.of_json D.Bool (`Bool false))

let test_fill_value_int () =
  check (result fill_value_testable error_testable) "int32 0"
    (Ok (FV.Int 0L)) (FV_mod.of_json D.Int32 (`Int 0));
  check (result fill_value_testable error_testable) "int32 42"
    (Ok (FV.Int 42L)) (FV_mod.of_json D.Int32 (`Int 42));
  check (result fill_value_testable error_testable) "int64 large"
    (Ok (FV.Int 9223372036854775807L)) (FV_mod.of_json D.Int64 (`Intlit "9223372036854775807"))

let test_fill_value_uint () =
  check (result fill_value_testable error_testable) "uint32 0"
    (Ok (FV.Uint 0L)) (FV_mod.of_json D.Uint32 (`Int 0));
  check (result fill_value_testable error_testable) "uint8 255"
    (Ok (FV.Uint 255L)) (FV_mod.of_json D.Uint8 (`Int 255))

let test_fill_value_float () =
  check (result fill_value_testable error_testable) "float64 0.0"
    (Ok (FV.Float 0.0)) (FV_mod.of_json D.Float64 (`Float 0.0));
  check (result fill_value_testable error_testable) "float64 3.14"
    (Ok (FV.Float 3.14)) (FV_mod.of_json D.Float64 (`Float 3.14));
  check (result fill_value_testable error_testable) "float64 from int"
    (Ok (FV.Float 42.0)) (FV_mod.of_json D.Float64 (`Int 42))

let test_fill_value_special_floats () =
  check (result fill_value_testable error_testable) "NaN"
    (Ok FV.NaN) (FV_mod.of_json D.Float64 (`String "NaN"));
  check (result fill_value_testable error_testable) "Infinity"
    (Ok FV.Infinity) (FV_mod.of_json D.Float64 (`String "Infinity"));
  check (result fill_value_testable error_testable) "-Infinity"
    (Ok FV.NegInfinity) (FV_mod.of_json D.Float64 (`String "-Infinity"));
  check (result fill_value_testable error_testable) "hex NaN"
    (Ok (FV.Hex "0x7fc00000")) (FV_mod.of_json D.Float32 (`String "0x7fc00000"))

let test_fill_value_complex () =
  check (result fill_value_testable error_testable) "complex64 floats"
    (Ok (FV.Complex (1.0, 2.0))) (FV_mod.of_json D.Complex64 (`List [`Float 1.0; `Float 2.0]));
  check (result fill_value_testable error_testable) "complex64 ints"
    (Ok (FV.Complex (1.0, 2.0))) (FV_mod.of_json D.Complex64 (`List [`Int 1; `Int 2]));
  check (result fill_value_testable error_testable) "complex64 mixed"
    (Ok (FV.Complex (1.5, 2.0))) (FV_mod.of_json D.Complex64 (`List [`Float 1.5; `Int 2]))

let test_fill_value_to_json () =
  check Alcotest.(option bool) "bool true"
    (Some true) (Yojson.Safe.Util.to_bool_option (FV_mod.to_json D.Bool (FV.Bool true)));
  check Alcotest.string "NaN"
    "\"NaN\"" (Yojson.Safe.to_string (FV_mod.to_json D.Float64 FV.NaN));
  check Alcotest.string "Infinity"
    "\"Infinity\"" (Yojson.Safe.to_string (FV_mod.to_json D.Float64 FV.Infinity))

let test_fill_value_to_bytes () =
  let buf = FV_mod.to_bytes D.Int32 E.Little (FV.Int 1L) in
  check bytes "int32 1 LE" (Bytes.of_string "\x01\x00\x00\x00") buf;

  let buf = FV_mod.to_bytes D.Int32 E.Big (FV.Int 1L) in
  check bytes "int32 1 BE" (Bytes.of_string "\x00\x00\x00\x01") buf;

  let buf = FV_mod.to_bytes D.Float64 E.Little (FV.Float 0.0) in
  check int "float64 0.0 length" 8 (Bytes.length buf)

let test_fill_value_default () =
  check fill_value_testable "bool default"
    (FV.Bool false) (FV_mod.default D.Bool);
  check fill_value_testable "int32 default"
    (FV.Int 0L) (FV_mod.default D.Int32);
  check fill_value_testable "uint8 default"
    (FV.Uint 0L) (FV_mod.default D.Uint8);
  check fill_value_testable "float64 default"
    (FV.Float 0.0) (FV_mod.default D.Float64);
  check fill_value_testable "complex64 default"
    (FV.Complex (0.0, 0.0)) (FV_mod.default D.Complex64)

let tests = [
  "bool", `Quick, test_fill_value_bool;
  "int", `Quick, test_fill_value_int;
  "uint", `Quick, test_fill_value_uint;
  "float", `Quick, test_fill_value_float;
  "special floats", `Quick, test_fill_value_special_floats;
  "complex", `Quick, test_fill_value_complex;
  "to_json", `Quick, test_fill_value_to_json;
  "to_bytes", `Quick, test_fill_value_to_bytes;
  "default", `Quick, test_fill_value_default;
]
