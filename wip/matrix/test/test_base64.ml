(** Tests for {!Matrix_proto.Base64}. *)

module B64 = Matrix_proto.Base64

let check_string = Alcotest.(check string)

let msg =
  Alcotest.testable
    (fun ppf (`Msg m) -> Format.pp_print_string ppf m)
    (fun (`Msg a) (`Msg b) -> String.equal a b)

let check_result = Alcotest.(check (result string msg))

let test_forms () =
  check_string "unpadded output" "aGVsbG8" (B64.encode "hello");
  check_result "unpadded input" (Ok "hello") (B64.decode "aGVsbG8");
  check_result "padded input" (Ok "hello") (B64.decode "aGVsbG8=");
  check_result "two pad characters" (Ok "hell") (B64.decode "aGVsbA==");
  check_result "empty" (Ok "") (B64.decode "")

let test_canonical_lengths () =
  let cases =
    [ ("f", "Zg==", "Zg"); ("fo", "Zm8=", "Zm8"); ("foo", "Zm9v", "Zm9v") ]
  in
  List.iter
    (fun (raw, padded, unpadded) ->
      check_result ("padded " ^ raw) (Ok raw) (B64.decode padded);
      check_result ("unpadded " ^ raw) (Ok raw) (B64.decode unpadded))
    cases

let test_rejects () =
  check_result "garbage"
    (Error (`Msg "invalid base64"))
    (B64.decode "not base64!");
  List.iter
    (fun value ->
      check_result ("reject " ^ value)
        (Error (`Msg "invalid base64"))
        (B64.decode value))
    [ "Zg="; "Zg==="; "Zh"; " Zg"; "Zg\n"; "-w"; "__8"; "====" ];
  Alcotest.(check (option string)) "option form" None (B64.decode_opt "*")

let test_roundtrip () =
  let bytes = String.init 256 Char.chr in
  check_result "all byte values" (Ok bytes) (B64.decode (B64.encode bytes))

let () =
  Alcotest.run "base64"
    [
      ( "base64",
        [
          Alcotest.test_case "padded and unpadded" `Quick test_forms;
          Alcotest.test_case "canonical lengths" `Quick test_canonical_lengths;
          Alcotest.test_case "rejects" `Quick test_rejects;
          Alcotest.test_case "round trip" `Quick test_roundtrip;
        ] );
    ]
