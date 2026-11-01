(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Tests for the Yamlrw library *)

open Yamlrw

(** Test helpers *)

let check_value msg expected actual =
  Alcotest.(check bool) msg true (Value.equal expected actual)

let _check_string msg expected actual =
  Alcotest.(check string) msg expected actual

(** Scanner tests *)

let test_scanner_simple () =
  let scanner = Scanner.of_string "hello: world" in
  let tokens = Scanner.to_list scanner in
  let token_types = List.map (fun (t : Token.spanned) -> t.token) tokens in
  Alcotest.(check int) "token count" 8 (List.length token_types);
  (* Stream_start, Block_mapping_start, Key, Scalar, Value, Scalar, Block_end, Stream_end *)
  match token_types with
  | [
   Token.Stream_start _;
   Token.Block_mapping_start;
   Token.Key;
   Token.Scalar { value = "hello"; _ };
   Token.Value;
   Token.Scalar { value = "world"; _ };
   Token.Block_end;
   Token.Stream_end;
  ] ->
      ()
  | _ -> Alcotest.fail "unexpected token sequence"

let test_scanner_sequence () =
  let scanner = Scanner.of_string "- one\n- two\n- three" in
  let tokens = Scanner.to_list scanner in
  Alcotest.(check bool) "has tokens" true (List.length tokens > 0)

let test_scanner_flow () =
  let scanner = Scanner.of_string "[1, 2, 3]" in
  let tokens = Scanner.to_list scanner in
  let has_flow_start =
    List.exists
      (fun (t : Token.spanned) ->
        match t.token with Token.Flow_sequence_start -> true | _ -> false)
      tokens
  in
  Alcotest.(check bool) "has flow sequence start" true has_flow_start

let scanner_tests =
  [
    ("simple mapping", `Quick, test_scanner_simple);
    ("sequence", `Quick, test_scanner_sequence);
    ("flow sequence", `Quick, test_scanner_flow);
  ]

(** Parser tests *)

let test_parser_events () =
  let parser = Parser.of_string "key: value" in
  let events = Parser.to_list parser in
  Alcotest.(check bool) "has events" true (List.length events > 0);
  let has_stream_start =
    List.exists
      (fun (e : Event.spanned) ->
        match e.event with Event.Stream_start _ -> true | _ -> false)
      events
  in
  Alcotest.(check bool) "has stream start" true has_stream_start

let test_parser_sequence_events () =
  let parser = Parser.of_string "- a\n- b" in
  let events = Parser.to_list parser in
  let has_seq_start =
    List.exists
      (fun (e : Event.spanned) ->
        match e.event with Event.Sequence_start _ -> true | _ -> false)
      events
  in
  Alcotest.(check bool) "has sequence start" true has_seq_start

let parser_tests =
  [
    ("parse events", `Quick, test_parser_events);
    ("sequence events", `Quick, test_parser_sequence_events);
  ]

(** Value parsing tests *)

let test_parse_null () =
  check_value "null" `Null (of_string "null");
  check_value "~" `Null (of_string "~");
  check_value "empty" `Null (of_string "")

let test_parse_bool () =
  check_value "true" (`Bool true) (of_string "true");
  check_value "false" (`Bool false) (of_string "false");
  check_value "yes" (`Bool true) (of_string "yes");
  check_value "no" (`Bool false) (of_string "no")

let test_parse_number () =
  check_value "integer" (`Float 42.0) (of_string "42");
  check_value "negative" (`Float (-17.0)) (of_string "-17");
  check_value "float" (`Float 3.14) (of_string "3.14")

let test_parse_string () =
  check_value "plain" (`String "hello")
    ( of_string "hello world" |> function
      | `String s -> `String (String.sub s 0 5)
      | v -> v );
  check_value "quoted" (`String "hello") (of_string {|"hello"|})

let test_parse_sequence () =
  let result = of_string "- one\n- two\n- three" in
  match result with
  | `A [ _; _; _ ] -> ()
  | _ -> Alcotest.fail "expected sequence with 3 elements"

let test_parse_mapping () =
  let result = of_string "name: Alice\nage: 30" in
  match result with
  | `O pairs when List.length pairs = 2 -> ()
  | _ -> Alcotest.fail "expected mapping with 2 pairs"

let test_parse_nested () =
  let yaml = {|
person:
  name: Bob
  hobbies:
    - reading
    - coding
|} in
  let result = of_string yaml in
  match result with
  | `O [ ("person", `O _) ] -> ()
  | _ -> Alcotest.fail "expected nested structure"

let test_parse_flow_sequence () =
  let result = of_string "[1, 2, 3]" in
  match result with
  | `A [ `Float 1.0; `Float 2.0; `Float 3.0 ] -> ()
  | _ -> Alcotest.fail "expected flow sequence [1, 2, 3]"

let test_parse_flow_mapping () =
  let result = of_string "{a: 1, b: 2}" in
  match result with
  | `O [ ("a", `Float 1.0); ("b", `Float 2.0) ] -> ()
  | _ -> Alcotest.fail "expected flow mapping {a: 1, b: 2}"

let test_parse_flow_mapping_trailing_comma () =
  let result = of_string "{ a: 1, }" in
  match result with
  | `O [ ("a", `Float 1.0) ] -> ()
  | `O pairs ->
      Alcotest.failf
        "expected 1 pair but got %d pairs (trailing comma should not create \
         empty entry)"
        (List.length pairs)
  | _ -> Alcotest.fail "expected flow mapping with 1 pair"

let value_tests =
  [
    ("parse null", `Quick, test_parse_null);
    ("parse bool", `Quick, test_parse_bool);
    ("parse number", `Quick, test_parse_number);
    ("parse string", `Quick, test_parse_string);
    ("parse sequence", `Quick, test_parse_sequence);
    ("parse mapping", `Quick, test_parse_mapping);
    ("parse nested", `Quick, test_parse_nested);
    ("parse flow sequence", `Quick, test_parse_flow_sequence);
    ("parse flow mapping", `Quick, test_parse_flow_mapping);
    ( "flow mapping trailing comma",
      `Quick,
      test_parse_flow_mapping_trailing_comma );
  ]

(** Emitter tests *)

let test_emit_null () =
  let result = to_string `Null in
  Alcotest.(check bool) "contains null" true (String.length result > 0)

let starts_with prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let test_emit_mapping () =
  let value = `O [ ("name", `String "Alice"); ("age", `Float 30.0) ] in
  let result = to_string value in
  let trimmed = String.trim result in
  Alcotest.(check bool)
    "contains name" true
    (starts_with "name" trimmed || starts_with "\"name\"" trimmed)

let test_roundtrip_simple () =
  let yaml = "name: Alice" in
  let value = of_string yaml in
  let _ = to_string value in
  (* Just check it doesn't crash *)
  ()

let test_roundtrip_sequence () =
  let yaml = "- one\n- two\n- three" in
  let value = of_string yaml in
  match value with
  | `A items when List.length items = 3 ->
      let _ = to_string value in
      ()
  | _ -> Alcotest.fail "roundtrip failed"

let emitter_tests =
  [
    ("emit null", `Quick, test_emit_null);
    ("emit mapping", `Quick, test_emit_mapping);
    ("roundtrip simple", `Quick, test_roundtrip_simple);
    ("roundtrip sequence", `Quick, test_roundtrip_sequence);
  ]

(** YAML-specific tests *)

let test_yaml_anchor () =
  let yaml = "&anchor hello" in
  let result = yaml_of_string yaml in
  match result with
  | `Scalar s when Scalar.anchor s = Some "anchor" -> ()
  | _ -> Alcotest.fail "expected scalar with anchor"

let test_yaml_alias () =
  let yaml =
    {|
defaults: &defaults
  timeout: 30
production:
  <<: *defaults
  port: 8080
|}
  in
  (* Just check it parses without error *)
  let _ = yaml_of_string yaml in
  ()

let yaml_tests =
  [
    ("yaml anchor", `Quick, test_yaml_anchor);
    ("yaml alias", `Quick, test_yaml_alias);
  ]

(** Multiline scalar tests *)

let test_literal_block () =
  let yaml = {|description: |
  This is a
  multi-line
  description
|} in
  let result = of_string yaml in
  match result with
  | `O [ ("description", `String _) ] -> ()
  | _ -> Alcotest.fail "expected mapping with literal block"

let test_folded_block () =
  let yaml = {|description: >
  This is a
  folded
  description
|} in
  let result = of_string yaml in
  match result with
  | `O [ ("description", `String _) ] -> ()
  | _ -> Alcotest.fail "expected mapping with folded block"

(* Test that block scalars don't create double newlines when emitted as values.
   This was a bug where write_scalar would add a trailing newline for block
   scalars, and then the caller would also add a newline, creating a blank line
   between the value and the next key. *)
let test_block_scalar_no_double_newline () =
  (* Create a value that will use folded style due to length > 80 chars,
     or explicitly use events to force block scalar style *)
  let emitter = Emitter.create () in
  Emitter.emit emitter (Event.Stream_start { encoding = `Utf8 });
  Emitter.emit emitter (Event.Document_start { version = None; implicit = true });
  Emitter.emit emitter (Event.Mapping_start { anchor = None; tag = None; implicit = true; style = `Block });
  (* Emit a key *)
  Emitter.emit emitter (Event.Scalar { anchor = None; tag = None; value = "url"; plain_implicit = true; quoted_implicit = true; style = `Plain });
  (* Emit a folded scalar value *)
  Emitter.emit emitter (Event.Scalar { anchor = None; tag = None; value = "https://example.org/very/long/path"; plain_implicit = true; quoted_implicit = true; style = `Folded });
  (* Emit another key-value pair *)
  Emitter.emit emitter (Event.Scalar { anchor = None; tag = None; value = "next"; plain_implicit = true; quoted_implicit = true; style = `Plain });
  Emitter.emit emitter (Event.Scalar { anchor = None; tag = None; value = "value"; plain_implicit = true; quoted_implicit = true; style = `Plain });
  Emitter.emit emitter Event.Mapping_end;
  Emitter.emit emitter (Event.Document_end { implicit = true });
  Emitter.emit emitter Event.Stream_end;
  let result = Emitter.contents emitter in
  (* Check there's no double newline (blank line) in the output *)
  let has_double_newline =
    let rec check i =
      if i >= String.length result - 1 then false
      else if result.[i] = '\n' && result.[i+1] = '\n' then true
      else check (i + 1)
    in
    check 0
  in
  Alcotest.(check bool) "no double newlines in block scalar output" false has_double_newline;
  (* Also verify the output can be parsed back *)
  let parsed = of_string result in
  match parsed with
  | `O [ ("url", `String _); ("next", `String "value") ] -> ()
  | _ -> Alcotest.fail ("expected mapping with url and next keys, got: " ^ result)

let test_literal_block_no_double_newline () =
  let emitter = Emitter.create () in
  Emitter.emit emitter (Event.Stream_start { encoding = `Utf8 });
  Emitter.emit emitter (Event.Document_start { version = None; implicit = true });
  Emitter.emit emitter (Event.Mapping_start { anchor = None; tag = None; implicit = true; style = `Block });
  Emitter.emit emitter (Event.Scalar { anchor = None; tag = None; value = "desc"; plain_implicit = true; quoted_implicit = true; style = `Plain });
  Emitter.emit emitter (Event.Scalar { anchor = None; tag = None; value = "line1\nline2"; plain_implicit = true; quoted_implicit = true; style = `Literal });
  Emitter.emit emitter (Event.Scalar { anchor = None; tag = None; value = "next"; plain_implicit = true; quoted_implicit = true; style = `Plain });
  Emitter.emit emitter (Event.Scalar { anchor = None; tag = None; value = "value"; plain_implicit = true; quoted_implicit = true; style = `Plain });
  Emitter.emit emitter Event.Mapping_end;
  Emitter.emit emitter (Event.Document_end { implicit = true });
  Emitter.emit emitter Event.Stream_end;
  let result = Emitter.contents emitter in
  (* The output should be parseable and not have a blank line between the literal and next key *)
  let parsed = of_string result in
  match parsed with
  | `O [ ("desc", `String _); ("next", `String "value") ] -> ()
  | _ -> Alcotest.fail ("expected mapping with desc and next keys, got: " ^ result)

let multiline_tests =
  [
    ("literal block", `Quick, test_literal_block);
    ("folded block", `Quick, test_folded_block);
    ("folded block no double newline", `Quick, test_block_scalar_no_double_newline);
    ("literal block no double newline", `Quick, test_literal_block_no_double_newline);
  ]

(** Error handling tests *)

let test_error_position () =
  try
    let _ = of_string "key: [unclosed" in
    Alcotest.fail "expected error"
  with Yamlrw_error e -> Alcotest.(check bool) "has span" true (e.span <> None)

let test_invalid_unicode_escape () =
  (* Unicode scalar values must be 0x0000-0x10FFFF, excluding surrogates *)
  (* Test \U with value > 0x10FFFF (maximum valid Unicode codepoint) *)
  (try
     let _ = of_string "\"\\U88888888\"" in
     Alcotest.fail "expected Invalid_unicode_escape error for out-of-range"
   with Yamlrw_error e -> (
     match e.Error.kind with
     | Error.Invalid_unicode_escape _ -> ()
     | _ ->
         Alcotest.fail
           ("expected Invalid_unicode_escape error, got: "
           ^ Error.kind_to_string e.Error.kind)));
  (* Test \u with surrogate codepoint (should error) *)
  (try
     let _ = of_string "\"\\uD800\"" in
     Alcotest.fail "expected Invalid_unicode_escape error for surrogate"
   with Yamlrw_error e -> (
     match e.Error.kind with
     | Error.Invalid_unicode_escape _ -> ()
     | _ ->
         Alcotest.fail
           ("expected Invalid_unicode_escape error, got: "
           ^ Error.kind_to_string e.Error.kind)));
  (* Test \u with valid value (should work) *)
  let v = of_string "\"\\u0041\"" in
  Alcotest.(check string) "valid \\u escape" "A" (Value.to_string v);
  (* Test \U with valid value at max boundary (should work) *)
  let v2 = of_string "\"\\U0010FFFF\"" in
  Alcotest.(check bool)
    "valid \\U at max boundary" true
    (String.length (Value.to_string v2) > 0)

let error_tests =
  [
    ("error position", `Quick, test_error_position);
    ("invalid unicode escape", `Quick, test_invalid_unicode_escape);
  ]

(** Alias expansion limit tests (billion laughs protection) *)

let test_node_limit () =
  (* Small bomb that would expand to 9^4 = 6561 nodes *)
  let yaml =
    {|
a: &a [1,2,3,4,5,6,7,8,9]
b: &b [*a,*a,*a,*a,*a,*a,*a,*a,*a]
c: &c [*b,*b,*b,*b,*b,*b,*b,*b,*b]
d: &d [*c,*c,*c,*c,*c,*c,*c,*c,*c]
|}
  in
  (* Should fail with a small node limit *)
  try
    let _ = of_string ~max_nodes:100 yaml in
    Alcotest.fail "expected node limit error"
  with Yamlrw_error e -> (
    match e.Error.kind with
    | Error.Alias_expansion_node_limit _ -> ()
    | _ -> Alcotest.fail "expected Alias_expansion_node_limit error")

let test_depth_limit () =
  (* Create deeply nested alias chain:
     *e -> [*d,*d] -> [*c,*c] -> [*b,*b] -> [*a,*a] -> [x,y,z]
     Each alias resolution increases depth by 1 *)
  let yaml =
    {|
a: &a [x, y, z]
b: &b [*a, *a]
c: &c [*b, *b]
d: &d [*c, *c]
e: &e [*d, *d]
result: *e
|}
  in
  (* Should fail with a small depth limit (depth 3 means max 3 alias hops) *)
  try
    let _ = of_string ~max_depth:3 yaml in
    Alcotest.fail "expected depth limit error"
  with Yamlrw_error e -> (
    match e.Error.kind with
    | Error.Alias_expansion_depth_limit _ -> ()
    | _ ->
        Alcotest.fail
          ("expected Alias_expansion_depth_limit error, got: "
          ^ Error.kind_to_string e.Error.kind))

let test_normal_aliases_work () =
  (* Normal alias usage should work fine *)
  let yaml =
    {|
defaults: &defaults
  timeout: 30
  retries: 3
production:
  <<: *defaults
  port: 8080
|}
  in
  let result = of_string yaml in
  match result with `O _ -> () | _ -> Alcotest.fail "expected mapping"

let test_resolve_aliases_false () =
  (* With resolve_aliases=false, aliases should remain unresolved *)
  let yaml = {|
a: &anchor value
b: *anchor
|} in
  let result = yaml_of_string ~resolve_aliases:false yaml in
  (* Check that alias is preserved *)
  match result with
  | `O map -> (
      let pairs = Mapping.members map in
      match List.assoc_opt (`Scalar (Scalar.make "b")) pairs with
      | Some (`Alias "anchor") -> ()
      | _ -> Alcotest.fail "expected alias to be preserved")
  | _ -> Alcotest.fail "expected mapping"

let alias_limit_tests =
  [
    ("node limit", `Quick, test_node_limit);
    ("depth limit", `Quick, test_depth_limit);
    ("normal aliases work", `Quick, test_normal_aliases_work);
    ("resolve_aliases false", `Quick, test_resolve_aliases_false);
  ]

(** Bug fix regression tests These tests verify that issues fixed in ocaml-yaml
    don't occur in ocaml-yamlrw *)

(* Test for roundtrip of special string values (ocaml-yaml fix 225387d)
   Strings like "true", "1.0", "null" etc. must be quoted on output so that
   they round-trip correctly as strings, not as booleans/numbers/null *)
let test_roundtrip_string_true () =
  let original = `String "true" in
  let emitted = to_string original in
  let parsed = of_string emitted in
  check_value "String 'true' roundtrips" original parsed

let test_roundtrip_string_false () =
  let original = `String "false" in
  let emitted = to_string original in
  let parsed = of_string emitted in
  check_value "String 'false' roundtrips" original parsed

let test_roundtrip_string_null () =
  let original = `String "null" in
  let emitted = to_string original in
  let parsed = of_string emitted in
  check_value "String 'null' roundtrips" original parsed

let test_roundtrip_string_number () =
  let original = `String "1.0" in
  let emitted = to_string original in
  let parsed = of_string emitted in
  check_value "String '1.0' roundtrips" original parsed

let test_roundtrip_string_integer () =
  let original = `String "42" in
  let emitted = to_string original in
  let parsed = of_string emitted in
  check_value "String '42' roundtrips" original parsed

let test_roundtrip_string_yes () =
  let original = `String "yes" in
  let emitted = to_string original in
  let parsed = of_string emitted in
  check_value "String 'yes' roundtrips" original parsed

let test_roundtrip_string_no () =
  let original = `String "no" in
  let emitted = to_string original in
  let parsed = of_string emitted in
  check_value "String 'no' roundtrips" original parsed

(* Test for integer display without decimal point (ocaml-yaml fix 999b1aa)
   Float values that are integers should be emitted as "42" not "42." or "42.0" *)
let test_emit_integer_float () =
  let value = `Float 42.0 in
  let result = to_string value in
  (* Check the result doesn't contain "42." or "42.0" *)
  Alcotest.(check bool)
    "no trailing dot" true
    (not
       (String.length result >= 3
       && result.[0] = '4'
       && result.[1] = '2'
       && result.[2] = '.'))

let test_emit_negative_integer_float () =
  let value = `Float (-17.0) in
  let result = to_string value in
  let parsed = of_string result in
  check_value "negative integer float roundtrips" value parsed

(* Test for special YAML floats: .nan, .inf, -.inf *)
let test_parse_special_floats () =
  let inf_result = of_string ".inf" in
  (match inf_result with
  | `Float f when Float.is_infinite f && f > 0.0 -> ()
  | _ -> Alcotest.fail "expected positive infinity");
  let neg_inf_result = of_string "-.inf" in
  (match neg_inf_result with
  | `Float f when Float.is_infinite f && f < 0.0 -> ()
  | _ -> Alcotest.fail "expected negative infinity");
  let nan_result = of_string ".nan" in
  match nan_result with
  | `Float f when Float.is_nan f -> ()
  | _ -> Alcotest.fail "expected NaN"

(* Test that bare "inf", "nan", "infinity" are NOT parsed as floats
   (ocaml-yaml issue - OCaml's Float.of_string accepts these but YAML doesn't) *)
let test_bare_inf_nan_are_strings () =
  let inf_result = of_string "inf" in
  (match inf_result with
  | `String "inf" -> ()
  | `Float _ -> Alcotest.fail "'inf' should be string, not float"
  | _ -> Alcotest.fail "expected string 'inf'");
  let nan_result = of_string "nan" in
  (match nan_result with
  | `String "nan" -> ()
  | `Float _ -> Alcotest.fail "'nan' should be string, not float"
  | _ -> Alcotest.fail "expected string 'nan'");
  let infinity_result = of_string "infinity" in
  match infinity_result with
  | `String "infinity" -> ()
  | `Float _ -> Alcotest.fail "'infinity' should be string, not float"
  | _ -> Alcotest.fail "expected string 'infinity'"

(* Test for quoted scalar preservation *)
let test_quoted_scalar_preserved () =
  (* When a scalar is quoted, it should be preserved as a string even if
     it looks like a number/boolean *)
  check_value "double-quoted true is string" (`String "true")
    (of_string {|"true"|});
  check_value "single-quoted 42 is string" (`String "42") (of_string "'42'");
  check_value "double-quoted null is string" (`String "null")
    (of_string {|"null"|})

(* Test complex roundtrip with mixed types *)
let test_complex_roundtrip () =
  let original =
    `O
      [
        ("string_true", `String "true");
        ("bool_true", `Bool true);
        ("string_42", `String "42");
        ("int_42", `Float 42.0);
        ("string_null", `String "null");
        ("actual_null", `Null);
      ]
  in
  let emitted = to_string original in
  let parsed = of_string emitted in
  check_value "complex roundtrip preserves types" original parsed

let bugfix_regression_tests =
  [
    ("roundtrip string 'true'", `Quick, test_roundtrip_string_true);
    ("roundtrip string 'false'", `Quick, test_roundtrip_string_false);
    ("roundtrip string 'null'", `Quick, test_roundtrip_string_null);
    ("roundtrip string '1.0'", `Quick, test_roundtrip_string_number);
    ("roundtrip string '42'", `Quick, test_roundtrip_string_integer);
    ("roundtrip string 'yes'", `Quick, test_roundtrip_string_yes);
    ("roundtrip string 'no'", `Quick, test_roundtrip_string_no);
    ("emit integer float without decimal", `Quick, test_emit_integer_float);
    ("emit negative integer float", `Quick, test_emit_negative_integer_float);
    ( "parse special floats (.inf, -.inf, .nan)",
      `Quick,
      test_parse_special_floats );
    ("bare inf/nan/infinity are strings", `Quick, test_bare_inf_nan_are_strings);
    ("quoted scalars preserved as strings", `Quick, test_quoted_scalar_preserved);
    ("complex roundtrip preserves types", `Quick, test_complex_roundtrip);
  ]

(** Run all tests *)

let () =
  Alcotest.run "yamlrw"
    [
      ("scanner", scanner_tests);
      ("parser", parser_tests);
      ("value", value_tests);
      ("emitter", emitter_tests);
      ("yaml", yaml_tests);
      ("multiline", multiline_tests);
      ("errors", error_tests);
      ("alias_limits", alias_limit_tests);
      ("bugfix_regression", bugfix_regression_tests);
    ]
