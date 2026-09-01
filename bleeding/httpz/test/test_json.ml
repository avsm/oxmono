module Media = Httpz.Media
module Json = Httpz.Json

let check_depth_limit () =
  Alcotest.(check int) "default max depth" 128 Json.default_max_depth;
  let codec max_depth = Json.v ~max_depth Jsont.json in
  let decode ?(max_depth = 2) source = Media.decode (codec max_depth) source in
  (match decode {|{"literal":"[[[","nested":[0]}|} with
  | Ok _ -> ()
  | Error error -> Alcotest.fail (Media.error_to_string error));
  (match decode "[[0]]" with
  | Ok _ -> ()
  | Error error -> Alcotest.fail (Media.error_to_string error));
  let split = Bytesrw.Bytes.Reader.of_string ~slice_length:1 "[[0]]" in
  (match Media.decode_reader (codec 2) split with
  | Ok _ -> ()
  | Error error -> Alcotest.fail (Media.error_to_string error));
  (match decode "[[[0]]]" with
  | Error (Media.Malformed { message; _ }) ->
      Alcotest.(check string)
        "depth diagnostic" "JSON nesting deeper than 2" message
  | Ok _ -> Alcotest.fail "expected depth rejection"
  | Error error -> Alcotest.fail (Media.error_to_string error));
  let split_deep = Bytesrw.Bytes.Reader.of_string ~slice_length:1 "[[[0]]]" in
  (match Media.decode_reader (codec 2) split_deep with
  | Error (Media.Malformed { message; _ }) ->
      Alcotest.(check string)
        "split depth diagnostic" "JSON nesting deeper than 2" message
  | Ok _ -> Alcotest.fail "expected split depth rejection"
  | Error error -> Alcotest.fail (Media.error_to_string error));
  (match decode ~max_depth:0 "0" with
  | Ok _ -> ()
  | Error error -> Alcotest.fail (Media.error_to_string error));
  (match decode ~max_depth:0 "[]" with
  | Error (Media.Malformed _) -> ()
  | Ok _ -> Alcotest.fail "expected container rejection"
  | Error error -> Alcotest.fail (Media.error_to_string error));
  Alcotest.check_raises "negative depth"
    (Invalid_argument "Httpz.Json.v: max_depth must be non-negative")
    (fun () -> ignore (codec (-1)));
  let lines = Json.lines ~max_depth:1 Jsont.json in
  (match Media.decode_items lines "[0]\n" with
  | Ok [ _ ] -> ()
  | _ -> Alcotest.fail "a JSON line at the depth limit must decode");
  match Media.decode_items lines "[0]\n[[0]]\n" with
  | Error (Media.Malformed _) -> ()
  | _ -> Alcotest.fail "each JSON line must have its own depth budget"

let () =
  Alcotest.run "json"
    [ ("bounded decoding", [ Alcotest.test_case "depth limit" `Quick check_depth_limit ]) ]
