(** Tests for fill value handling *)

open Alcotest
open Zarr

let fill_value_testable = testable
  (fun fmt fv -> match fv with
    | Fill.Bool b -> Format.fprintf fmt "Bool %b" b
    | Fill.Int i -> Format.fprintf fmt "Int %Ld" i
    | Fill.Uint u -> Format.fprintf fmt "Uint %Ld" u
    | Fill.Float f -> Format.fprintf fmt "Float %f" f
    | Fill.Raw b -> Format.fprintf fmt "Raw %d bytes" (Bytes.length b)
    | Fill.NaN -> Format.fprintf fmt "NaN"
    | Fill.Infinity -> Format.fprintf fmt "Infinity"
    | Fill.NegInfinity -> Format.fprintf fmt "NegInfinity"
    | Fill.Hex s -> Format.fprintf fmt "Hex %s" s)
  (fun a b -> match a, b with
    | Fill.Bool a, Fill.Bool b -> a = b
    | Fill.Int a, Fill.Int b -> Int64.equal a b
    | Fill.Uint a, Fill.Uint b -> Int64.equal a b
    | Fill.Float a, Fill.Float b -> Float.equal a b || (Float.is_nan a && Float.is_nan b)
    | Fill.Raw a, Fill.Raw b -> Bytes.equal a b
    | Fill.NaN, Fill.NaN -> true
    | Fill.Infinity, Fill.Infinity -> true
    | Fill.NegInfinity, Fill.NegInfinity -> true
    | Fill.Hex a, Fill.Hex b -> String.equal a b
    | _ -> false)

let json_to_bool = function Jsont.Bool (b, _) -> Some b | _ -> None
let json_to_str = function Jsont.String (s, _) -> "\"" ^ s ^ "\"" | _ -> ""

let test_fill_value_bool () =
  check fill_value_testable "true"
    (Fill.Bool true) (Fill.of_json Dtype.Bool (Jsont.Json.bool true));
  check fill_value_testable "false"
    (Fill.Bool false) (Fill.of_json Dtype.Bool (Jsont.Json.bool false))

let test_fill_value_int () =
  check fill_value_testable "int32 0"
    (Fill.Int 0L) (Fill.of_json Dtype.Int32 (Jsont.Json.int 0));
  check fill_value_testable "int32 42"
    (Fill.Int 42L) (Fill.of_json Dtype.Int32 (Jsont.Json.int 42));
  check fill_value_testable "int64 large"
    (Fill.Int 9223372036854775807L) (Fill.of_json Dtype.Int64 (Jsont.Json.string "9223372036854775807"))

let test_fill_value_uint () =
  check fill_value_testable "uint32 0"
    (Fill.Uint 0L) (Fill.of_json Dtype.Uint32 (Jsont.Json.int 0));
  check fill_value_testable "uint8 255"
    (Fill.Uint 255L) (Fill.of_json Dtype.Uint8 (Jsont.Json.int 255))

let test_fill_value_float () =
  check fill_value_testable "float64 0.0"
    (Fill.Float 0.0) (Fill.of_json Dtype.Float64 (Jsont.Json.number 0.0));
  check fill_value_testable "float64 3.14"
    (Fill.Float 3.14) (Fill.of_json Dtype.Float64 (Jsont.Json.number 3.14));
  check fill_value_testable "float64 from int"
    (Fill.Float 42.0) (Fill.of_json Dtype.Float64 (Jsont.Json.int 42))

let test_fill_value_special_floats () =
  check fill_value_testable "NaN"
    Fill.NaN (Fill.of_json Dtype.Float64 (Jsont.Json.string "NaN"));
  check fill_value_testable "Infinity"
    Fill.Infinity (Fill.of_json Dtype.Float64 (Jsont.Json.string "Infinity"));
  check fill_value_testable "-Infinity"
    Fill.NegInfinity (Fill.of_json Dtype.Float64 (Jsont.Json.string "-Infinity"));
  check fill_value_testable "hex NaN"
    (Fill.Hex "0x7fc00000") (Fill.of_json Dtype.Float32 (Jsont.Json.string "0x7fc00000"))

let test_fill_value_to_json () =
  check Alcotest.(option bool) "bool true"
    (Some true) (json_to_bool (Fill.to_json (Fill.Bool true)));
  check Alcotest.string "NaN"
    "\"NaN\"" (json_to_str (Fill.to_json Fill.NaN));
  check Alcotest.string "Infinity"
    "\"Infinity\"" (json_to_str (Fill.to_json Fill.Infinity))

let test_fill_value_default () =
  check fill_value_testable "bool default"
    (Fill.Bool false) (Fill.default Dtype.Bool);
  check fill_value_testable "int32 default"
    (Fill.Int 0L) (Fill.default Dtype.Int32);
  check fill_value_testable "uint8 default"
    (Fill.Uint 0L) (Fill.default Dtype.Uint8);
  check fill_value_testable "float64 default"
    (Fill.Float 0.0) (Fill.default Dtype.Float64)

let tests = [
  "bool", `Quick, test_fill_value_bool;
  "int", `Quick, test_fill_value_int;
  "uint", `Quick, test_fill_value_uint;
  "float", `Quick, test_fill_value_float;
  "special floats", `Quick, test_fill_value_special_floats;
  "to_json", `Quick, test_fill_value_to_json;
  "default", `Quick, test_fill_value_default;
]
