module Media = Httpz_media
module Json = Httpz_media_jsont

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
    (Invalid_argument "Httpz_media_jsont.v: max_depth must be non-negative")
    (fun () -> ignore (codec (-1)));
  let lines = Json.lines ~max_depth:1 Jsont.json in
  (match Media.decode_items lines "[0]\n" with
  | Ok [ _ ] -> ()
  | _ -> Alcotest.fail "a JSON line at the depth limit must decode");
  match Media.decode_items lines "[0]\n[[0]]\n" with
  | Error (Media.Malformed _) -> ()
  | _ -> Alcotest.fail "each JSON line must have its own depth budget"

let check_entry_point_names () =
  Alcotest.check_raises "decode' names itself"
    (Invalid_argument "Httpz_jsont.decode': max_depth must be non-negative")
    (fun () ->
      ignore
        (Json.decode' ~max_depth:(-1) Jsont.json
           (Bytesrw.Bytes.Reader.of_string "0")));
  Alcotest.check_raises "decode_string' names itself"
    (Invalid_argument "Httpz_jsont.decode_string': max_depth must be non-negative")
    (fun () -> ignore (Json.decode_string' ~max_depth:(-1) Jsont.json "0"));
  Alcotest.check_raises "lines names itself"
    (Invalid_argument "Httpz_media_jsont.lines: max_depth must be non-negative")
    (fun () -> ignore (Json.lines ~max_depth:(-1) Jsont.json))

let check_ndjson_spellings () =
  let lines = Json.lines Jsont.json in
  List.iter
    (fun ct ->
      Alcotest.(check bool) ct true (Media.seq_accepts lines (Some ct)))
    [ "application/jsonl";
      "application/ndjson";
      "application/x-ndjson";
      "application/jsonlines";
      "application/x-jsonlines" ];
  Alcotest.(check bool) "other type" false
    (Media.seq_accepts lines (Some "text/plain"))

(* The depth guard rests on a lexical pre-pass, which stops at the first byte
   it cannot read. Input holding such a byte is a decode failure, so nesting
   past it is never accepted unbounded. *)
let check_lexical_guard () =
  let codec = Json.v ~max_depth:2 Jsont.json in
  let deep = String.concat "" (List.init 64 (fun _ -> "[")) in
  List.iter
    (fun source ->
      match Media.decode codec source with
      | Error (Media.Malformed _) -> ()
      | Ok _ -> Alcotest.failf "%S was accepted" source
      | Error error -> Alcotest.fail (Media.error_to_string error))
    [ "\xff" ^ deep; "[1] " ^ deep; deep ]

let () =
  Alcotest.run "json"
    [ ( "bounded decoding",
        [ Alcotest.test_case "depth limit" `Quick check_depth_limit;
          Alcotest.test_case "lexical guard" `Quick check_lexical_guard ] );
      ( "media types",
        [ Alcotest.test_case "entry point names" `Quick check_entry_point_names;
          Alcotest.test_case "ndjson spellings" `Quick check_ndjson_spellings ] )
    ]
