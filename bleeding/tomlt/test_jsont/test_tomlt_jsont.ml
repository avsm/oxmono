(* Tests for tomlt-jsont module *)

open Alcotest

module Toml = Tomlt.Toml

(* Test jsont decode/encode *)
let test_jsont_decode_encode name json expected_toml () =
  (* Test jsont decode *)
  match Tomlt_jsont.decode_jsont json with
  | Error e -> Alcotest.fail ("decode failed: " ^ e)
  | Ok toml ->
      check bool (name ^ " jsont decode") true (Toml.equal toml expected_toml);
      (* Test jsont encode then decode roundtrip *)
      match Tomlt_jsont.encode_jsont toml with
      | Error e -> Alcotest.fail ("encode failed: " ^ e)
      | Ok json' ->
          match Tomlt_jsont.decode_jsont json' with
          | Error e -> Alcotest.fail ("roundtrip decode failed: " ^ e)
          | Ok toml' ->
              check bool (name ^ " jsont roundtrip") true (Toml.equal toml toml')

(* Test native encode/decode with table documents *)
let test_native_roundtrip name toml () =
  let json = Tomlt_jsont.encode toml in
  let toml' = Tomlt_jsont.decode json in
  check bool (name ^ " roundtrip") true (Toml.equal toml toml')

(* Test cases for jsont codec (handles scalar tagged values correctly) *)
let jsont_tests = [
  "string", `Quick, test_jsont_decode_encode "string"
    {|{"type":"string","value":"hello"}|}
    (Toml.String "hello");

  "integer", `Quick, test_jsont_decode_encode "integer"
    {|{"type":"integer","value":"42"}|}
    (Toml.Int 42L);

  "float", `Quick, test_jsont_decode_encode "float"
    {|{"type":"float","value":"3.14"}|}
    (Toml.Float 3.14);

  "bool true", `Quick, test_jsont_decode_encode "bool true"
    {|{"type":"bool","value":"true"}|}
    (Toml.Bool true);

  "bool false", `Quick, test_jsont_decode_encode "bool false"
    {|{"type":"bool","value":"false"}|}
    (Toml.Bool false);

  "datetime", `Quick, test_jsont_decode_encode "datetime"
    {|{"type":"datetime","value":"1979-05-27T07:32:00Z"}|}
    (Toml.Datetime "1979-05-27T07:32:00Z");

  "datetime-local", `Quick, test_jsont_decode_encode "datetime-local"
    {|{"type":"datetime-local","value":"1979-05-27T07:32:00"}|}
    (Toml.Datetime_local "1979-05-27T07:32:00");

  "date-local", `Quick, test_jsont_decode_encode "date-local"
    {|{"type":"date-local","value":"1979-05-27"}|}
    (Toml.Date_local "1979-05-27");

  "time-local", `Quick, test_jsont_decode_encode "time-local"
    {|{"type":"time-local","value":"07:32:00"}|}
    (Toml.Time_local "07:32:00");

  "array of integers", `Quick, test_jsont_decode_encode "array of integers"
    {|[{"type":"integer","value":"1"},{"type":"integer","value":"2"},{"type":"integer","value":"3"}]|}
    (Toml.Array [Toml.Int 1L; Toml.Int 2L; Toml.Int 3L]);

  "array of strings", `Quick, test_jsont_decode_encode "array of strings"
    {|[{"type":"string","value":"a"},{"type":"string","value":"b"}]|}
    (Toml.Array [Toml.String "a"; Toml.String "b"]);

  "empty table", `Quick, test_jsont_decode_encode "empty table"
    {|{}|}
    (Toml.Table []);

  "simple table", `Quick, test_jsont_decode_encode "simple table"
    {|{"name":{"type":"string","value":"test"}}|}
    (Toml.Table [("name", Toml.String "test")]);

  "table with multiple types", `Quick, test_jsont_decode_encode "table with multiple types"
    {|{"name":{"type":"string","value":"test"},"count":{"type":"integer","value":"5"},"enabled":{"type":"bool","value":"true"}}|}
    (* Note: jsont uses String_map which sorts keys alphabetically *)
    (Toml.Table [
      ("count", Toml.Int 5L);
      ("enabled", Toml.Bool true);
      ("name", Toml.String "test")
    ]);

  "nested table", `Quick, test_jsont_decode_encode "nested table"
    {|{"outer":{"inner":{"type":"string","value":"value"}}}|}
    (Toml.Table [("outer", Toml.Table [("inner", Toml.String "value")])]);

  "table with array", `Quick, test_jsont_decode_encode "table with array"
    {|{"items":[{"type":"integer","value":"1"},{"type":"integer","value":"2"}]}|}
    (Toml.Table [("items", Toml.Array [Toml.Int 1L; Toml.Int 2L])]);
]

(* Test cases for native encode/decode (roundtrip with table documents) *)
let native_tests = [
  "empty table", `Quick, test_native_roundtrip "empty table"
    (Toml.Table []);

  "simple table", `Quick, test_native_roundtrip "simple table"
    (Toml.Table [("key", Toml.String "value")]);

  "table with all types", `Quick, test_native_roundtrip "table with all types"
    (Toml.Table [
      ("string", Toml.String "hello");
      ("integer", Toml.Int 42L);
      ("float", Toml.Float 3.14);
      ("bool", Toml.Bool true);
      ("datetime", Toml.Datetime "1979-05-27T07:32:00Z");
      ("datetime_local", Toml.Datetime_local "1979-05-27T07:32:00");
      ("date_local", Toml.Date_local "1979-05-27");
      ("time_local", Toml.Time_local "07:32:00");
    ]);

  "nested table", `Quick, test_native_roundtrip "nested table"
    (Toml.Table [
      ("outer", Toml.Table [
        ("inner", Toml.String "value")
      ])
    ]);

  "table with array", `Quick, test_native_roundtrip "table with array"
    (Toml.Table [
      ("items", Toml.Array [Toml.Int 1L; Toml.Int 2L; Toml.Int 3L])
    ]);

  "complex document", `Quick, test_native_roundtrip "complex document"
    (Toml.Table [
      ("title", Toml.String "TOML Example");
      ("database", Toml.Table [
        ("server", Toml.String "192.168.1.1");
        ("ports", Toml.Array [Toml.Int 8000L; Toml.Int 8001L; Toml.Int 8002L]);
        ("enabled", Toml.Bool true);
      ]);
    ]);
]

(* Test native compatibility with existing tests *)
let compatibility_tests = [
  "valid toml roundtrip", `Quick, (fun () ->
    let toml_str = {|
      [server]
      host = "localhost"
      port = 8080
    |} in
    match Tomlt_bytesrw.of_string toml_str with
    | Error _ -> Alcotest.fail "TOML parse failed"
    | Ok toml ->
        let json = Tomlt_jsont.encode toml in
        let toml' = Tomlt_jsont.decode json in
        check bool "roundtrip" true (Toml.equal toml toml')
  );
]

let () =
  run "tomlt_jsont" [
    "jsont", jsont_tests;
    "native", native_tests;
    "compatibility", compatibility_tests;
  ]
