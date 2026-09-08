let check_string = Alcotest.(check string)
let canonical = Matrix_proto.Signed_json.canonical_json
let canonical_result = Matrix_proto.Signed_json.canonical_json_result
let number f = Jsont.Json.number f
let member name value = Jsont.Json.mem (Jsont.Json.name name) value

let test_matrix_numbers () =
  check_string "safe integer maximum" "9007199254740991"
    (canonical (number 9_007_199_254_740_991.));
  check_string "safe integer minimum" "-9007199254740991"
    (canonical (number (-9_007_199_254_740_991.)));
  check_string "negative zero" "0" (canonical (number (-0.)));
  List.iter
    (fun value ->
      match canonical_result (number value) with
      | Error (`Msg _) -> ()
      | Ok encoded -> Alcotest.failf "invalid number encoded as %s" encoded)
    [
      0.5;
      -0.5;
      9_007_199_254_740_992.;
      -9_007_199_254_740_992.;
      Float.nan;
      Float.infinity;
      Float.neg_infinity;
    ]

let test_matrix_values () =
  let duplicate =
    Jsont.Json.object' [ member "a" (number 1.); member "a" (number 2.) ]
  in
  let duplicate_dropped =
    Jsont.Json.object'
      [
        member "signatures" (Jsont.Json.object' []);
        member "signatures" (Jsont.Json.object' []);
      ]
  in
  let invalid_utf8 = Jsont.Json.string "\xC3" in
  List.iter
    (fun (name, value) ->
      match canonical_result value with
      | Error (`Msg _) -> ()
      | Ok encoded -> Alcotest.failf "%s encoded as %s" name encoded)
    [ ("duplicate member", duplicate); ("invalid UTF-8", invalid_utf8) ];
  (match Matrix_proto.Signed_json.json_for_signing duplicate_dropped with
  | _ -> Alcotest.fail "json_for_signing accepted duplicate dropped members"
  | exception Invalid_argument _ -> ());
  List.iter
    (fun value ->
      match canonical value with
      | _ -> Alcotest.fail "canonical_json accepted an invalid value"
      | exception Invalid_argument _ -> ())
    [ duplicate; invalid_utf8 ]

let test_unicode_order () =
  let value =
    Jsont.Json.object' [ member "é" (number 2.); member "z" (number 1.) ]
  in
  check_string "UTF-8 byte-wise key order" {|{"z":1,"é":2}|} (canonical value)

let () =
  Alcotest.run "signed JSON"
    [
      ( "canonical JSON",
        [
          Alcotest.test_case "Matrix numbers" `Quick test_matrix_numbers;
          Alcotest.test_case "Matrix values" `Quick test_matrix_values;
          Alcotest.test_case "Unicode key order" `Quick test_unicode_order;
        ] );
    ]
