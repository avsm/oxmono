(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tests for DAG-CBOR AT Protocol CID format support.

    Verifies that the cid_format parameter correctly handles both standard
    DAG-CBOR format and AT Protocol simplified format.

    Reference: draft-holmgren-at-repository.md Section 6.5 (line 368). *)

(* Test result tracking *)
type test_result = {
  mutable passed : int;
  mutable failed : int;
  mutable errors : string list;
}

let results = { passed = 0; failed = 0; errors = [] }

let pass name =
  results.passed <- results.passed + 1;
  Printf.printf "PASS: %s\n%!" name

let fail name msg =
  results.failed <- results.failed + 1;
  results.errors <- (name ^ ": " ^ msg) :: results.errors;
  Printf.printf "FAIL: %s: %s\n%!" name msg

(* Hex encoding for debugging *)
let hex_of_string s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter
    (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
    s;
  Buffer.contents buf

(* Create a test CID from known data *)
let make_test_cid () = Atp.Cid.create `Dag_cbor "test data for CID"

(* Test: Standard format CID encoding produces expected size *)
let test_standard_cid_size () =
  let name = "standard CID size" in
  let cid = make_test_cid () in
  let value : Atp.Dagcbor.value = `Link cid in
  let encoded = Atp.Dagcbor.encode_string ~cid_format:`Standard value in
  (* Standard format: tag 42 (2 bytes: 0xd8 0x2a) + byte string header (2 bytes for len 37)
     + 0x00 multibase + 36 bytes CID = 2 + 2 + 1 + 36 = 41 bytes *)
  let expected_len = 41 in
  if String.length encoded = expected_len then pass name
  else
    fail name
      (Printf.sprintf "expected %d bytes, got %d (hex: %s)" expected_len
         (String.length encoded) (hex_of_string encoded))

(* Test: AT Protocol format CID encoding produces expected size *)
let test_atproto_cid_size () =
  let name = "atproto CID size" in
  let cid = make_test_cid () in
  let value : Atp.Dagcbor.value = `Link cid in
  let encoded = Atp.Dagcbor.encode_string ~cid_format:`Atproto value in
  (* AT Protocol format: tag 42 (2 bytes: 0xd8 0x2a) + byte string header (2 bytes for len 35)
     + 35 bytes CID (no 0x00, no length byte) = 2 + 2 + 35 = 39 bytes *)
  let expected_len = 39 in
  if String.length encoded = expected_len then pass name
  else
    fail name
      (Printf.sprintf "expected %d bytes, got %d (hex: %s)" expected_len
         (String.length encoded) (hex_of_string encoded))

(* Test: Standard format roundtrip *)
let test_standard_roundtrip () =
  let name = "standard format roundtrip" in
  let cid = make_test_cid () in
  let value : Atp.Dagcbor.value = `Link cid in
  let encoded = Atp.Dagcbor.encode_string ~cid_format:`Standard value in
  try
    let decoded = Atp.Dagcbor.decode_string ~cid_format:`Standard encoded in
    match decoded with
    | `Link decoded_cid when Atp.Cid.equal cid decoded_cid -> pass name
    | `Link decoded_cid ->
        fail name
          (Printf.sprintf "CID mismatch: expected %s, got %s"
             (Atp.Cid.to_string cid)
             (Atp.Cid.to_string decoded_cid))
    | _ -> fail name "decoded value is not a Link"
  with exn -> fail name (Printexc.to_string exn)

(* Test: AT Protocol format roundtrip *)
let test_atproto_roundtrip () =
  let name = "atproto format roundtrip" in
  let cid = make_test_cid () in
  let value : Atp.Dagcbor.value = `Link cid in
  let encoded = Atp.Dagcbor.encode_string ~cid_format:`Atproto value in
  try
    let decoded = Atp.Dagcbor.decode_string ~cid_format:`Atproto encoded in
    match decoded with
    | `Link decoded_cid when Atp.Cid.equal cid decoded_cid -> pass name
    | `Link decoded_cid ->
        fail name
          (Printf.sprintf "CID mismatch: expected %s, got %s"
             (Atp.Cid.to_string cid)
             (Atp.Cid.to_string decoded_cid))
    | _ -> fail name "decoded value is not a Link"
  with exn -> fail name (Printexc.to_string exn)

(* Test: Standard encoded data fails with atproto decoder *)
let test_format_mismatch_standard_to_atproto () =
  let name = "format mismatch: standard -> atproto" in
  let cid = make_test_cid () in
  let value : Atp.Dagcbor.value = `Link cid in
  let encoded = Atp.Dagcbor.encode_string ~cid_format:`Standard value in
  try
    let _ = Atp.Dagcbor.decode_string ~cid_format:`Atproto encoded in
    fail name "expected decode to fail but it succeeded"
  with _ -> pass name

(* Test: AT Protocol encoded data fails with standard decoder *)
let test_format_mismatch_atproto_to_standard () =
  let name = "format mismatch: atproto -> standard" in
  let cid = make_test_cid () in
  let value : Atp.Dagcbor.value = `Link cid in
  let encoded = Atp.Dagcbor.encode_string ~cid_format:`Atproto value in
  try
    let _ = Atp.Dagcbor.decode_string ~cid_format:`Standard encoded in
    fail name "expected decode to fail but it succeeded"
  with _ -> pass name

(* Test: AT Protocol CID bytes format *)
let test_atproto_cid_bytes_format () =
  let name = "atproto CID bytes format" in
  let cid = make_test_cid () in
  let atproto_bytes = Atp.Cid.to_atproto_bytes cid in
  (* AT Protocol format is 35 bytes: 0x01 + codec + 0x12 + 32-byte hash
     For dag-cbor codec (0x71): 0x01 0x71 0x12 + hash *)
  if String.length atproto_bytes <> 35 then
    fail name
      (Printf.sprintf "expected 35 bytes, got %d" (String.length atproto_bytes))
  else if Char.code atproto_bytes.[0] <> 0x01 then
    fail name
      (Printf.sprintf "expected version 0x01, got 0x%02x"
         (Char.code atproto_bytes.[0]))
  else if Char.code atproto_bytes.[1] <> 0x71 then
    fail name
      (Printf.sprintf "expected codec 0x71 (dag-cbor), got 0x%02x"
         (Char.code atproto_bytes.[1]))
  else if Char.code atproto_bytes.[2] <> 0x12 then
    fail name
      (Printf.sprintf "expected hash codec 0x12 (sha256), got 0x%02x"
         (Char.code atproto_bytes.[2]))
  else pass name

(* Test: Standard vs AT Protocol byte difference *)
let test_standard_vs_atproto_bytes () =
  let name = "standard vs atproto byte difference" in
  let cid = make_test_cid () in
  let standard_bytes = Atp.Cid.to_raw_bytes cid in
  let atproto_bytes = Atp.Cid.to_atproto_bytes cid in
  (* Standard is 36 bytes (with length byte 0x20), atproto is 35 bytes (no length byte) *)
  if String.length standard_bytes <> 36 then
    fail name
      (Printf.sprintf "standard: expected 36 bytes, got %d"
         (String.length standard_bytes))
  else if String.length atproto_bytes <> 35 then
    fail name
      (Printf.sprintf "atproto: expected 35 bytes, got %d"
         (String.length atproto_bytes))
  else begin
    (* First 3 bytes should match: version, codec, hash-codec *)
    let std_prefix = String.sub standard_bytes 0 3 in
    let atp_prefix = String.sub atproto_bytes 0 3 in
    if std_prefix <> atp_prefix then fail name "prefix bytes differ"
    else begin
      (* Standard has length byte at position 3, atproto has hash starting at position 3 *)
      let std_length_byte = Char.code standard_bytes.[3] in
      if std_length_byte <> 0x20 then
        fail name
          (Printf.sprintf "standard length byte: expected 0x20, got 0x%02x"
             std_length_byte)
      else begin
        (* Hash in standard starts at position 4, in atproto at position 3 *)
        let std_hash = String.sub standard_bytes 4 32 in
        let atp_hash = String.sub atproto_bytes 3 32 in
        if std_hash <> atp_hash then fail name "hash bytes differ"
        else pass name
      end
    end
  end

(* Test: Complex IPLD value with links *)
let test_complex_value_roundtrip () =
  let name = "complex value with links roundtrip" in
  let cid1 = Atp.Cid.create `Dag_cbor "data 1" in
  let cid2 = Atp.Cid.create `Dag_cbor "data 2" in
  let value : Atp.Dagcbor.value =
    `Map
      [
        ("$type", `String "sh.tangled.test");
        ("links", `List [ `Link cid1; `Link cid2 ]);
        ("nested", `Map [ ("ref", `Link cid1) ]);
      ]
  in
  (* Test both formats *)
  List.iter
    (fun (format_name, cid_format) ->
      let test_name = name ^ " (" ^ format_name ^ ")" in
      try
        let encoded = Atp.Dagcbor.encode_string ~cid_format value in
        let decoded = Atp.Dagcbor.decode_string ~cid_format encoded in
        if Atp.Dagcbor.equal value decoded then pass test_name
        else fail test_name "values not equal after roundtrip"
      with exn -> fail test_name (Printexc.to_string exn))
    [ ("standard", `Standard); ("atproto", `Atproto) ]

(* Main test runner *)
let () =
  Printf.printf "=== DAG-CBOR AT Protocol CID Format Tests ===\n%!";
  Printf.printf "Reference: draft-holmgren-at-repository.md Section 6.5\n\n%!";

  test_standard_cid_size ();
  test_atproto_cid_size ();
  test_standard_roundtrip ();
  test_atproto_roundtrip ();
  test_format_mismatch_standard_to_atproto ();
  test_format_mismatch_atproto_to_standard ();
  test_atproto_cid_bytes_format ();
  test_standard_vs_atproto_bytes ();
  test_complex_value_roundtrip ();

  Printf.printf "\n=== Test Summary ===\n%!";
  Printf.printf "Passed: %d\n%!" results.passed;
  Printf.printf "Failed: %d\n%!" results.failed;

  if results.failed > 0 then begin
    Printf.printf "\nFailures:\n%!";
    List.iter (Printf.printf "  - %s\n%!") (List.rev results.errors);
    exit 1
  end
  else exit 0
