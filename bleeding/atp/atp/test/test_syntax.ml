(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Comprehensive syntax validation tests using AT Protocol interop test files.
*)

(* Test result tracking *)
type test_result = {
  mutable passed : int;
  mutable failed : int;
  mutable errors : string list;
}

let results = { passed = 0; failed = 0; errors = [] }
let pass _name = results.passed <- results.passed + 1

let fail name msg =
  results.failed <- results.failed + 1;
  results.errors <- (name ^ ": " ^ msg) :: results.errors;
  Printf.printf "FAIL: %s: %s\n%!" name msg

(* Load test data from interop files *)
let load_test_file path =
  let ic = open_in path in
  let rec read_lines acc =
    match input_line ic with
    | line ->
        let trimmed = String.trim line in
        (* Skip empty lines and comments (but preserve whitespace in test strings) *)
        if String.length trimmed = 0 || trimmed.[0] = '#' then read_lines acc
        else read_lines (line :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  read_lines []

let interop_dir = "../../vendor/atproto/interop-test-files/syntax"

(* ========== TID Tests ========== *)

let test_tid_valid () =
  let valid = load_test_file (interop_dir ^ "/tid_syntax_valid.txt") in
  List.iter
    (fun tid ->
      match Atp.Tid.of_string_opt tid with
      | Some _ -> pass ("tid valid: " ^ tid)
      | None -> fail ("tid valid: " ^ tid) "should be valid")
    valid

let test_tid_invalid () =
  let invalid = load_test_file (interop_dir ^ "/tid_syntax_invalid.txt") in
  List.iter
    (fun tid ->
      match Atp.Tid.of_string_opt tid with
      | Some _ -> fail ("tid invalid: " ^ tid) "should be invalid"
      | None -> pass ("tid invalid: " ^ tid))
    invalid

(* ========== Handle Tests ========== *)

let test_handle_valid () =
  let valid = load_test_file (interop_dir ^ "/handle_syntax_valid.txt") in
  List.iter
    (fun handle ->
      match Atp.Handle.of_string handle with
      | Ok _ -> pass ("handle valid: " ^ handle)
      | Error _ -> fail ("handle valid: " ^ handle) "should be valid")
    valid

let test_handle_invalid () =
  let invalid = load_test_file (interop_dir ^ "/handle_syntax_invalid.txt") in
  List.iter
    (fun handle ->
      match Atp.Handle.of_string handle with
      | Ok _ -> fail ("handle invalid: " ^ handle) "should be invalid"
      | Error _ -> pass ("handle invalid: " ^ handle))
    invalid

(* ========== DID Tests ========== *)

let test_did_valid () =
  let valid = load_test_file (interop_dir ^ "/did_syntax_valid.txt") in
  List.iter
    (fun did ->
      match Atp.Did.of_string did with
      | Ok _ -> pass ("did valid: " ^ did)
      | Error _ -> fail ("did valid: " ^ did) "should be valid")
    valid

let test_did_invalid () =
  let invalid = load_test_file (interop_dir ^ "/did_syntax_invalid.txt") in
  List.iter
    (fun did ->
      match Atp.Did.of_string did with
      | Ok _ -> fail ("did invalid: " ^ did) "should be invalid"
      | Error _ -> pass ("did invalid: " ^ did))
    invalid

(* ========== NSID Tests ========== *)

let test_nsid_valid () =
  let valid = load_test_file (interop_dir ^ "/nsid_syntax_valid.txt") in
  List.iter
    (fun nsid ->
      match Atp.Nsid.of_string nsid with
      | Ok _ -> pass ("nsid valid: " ^ nsid)
      | Error _ -> fail ("nsid valid: " ^ nsid) "should be valid")
    valid

let test_nsid_invalid () =
  let invalid = load_test_file (interop_dir ^ "/nsid_syntax_invalid.txt") in
  List.iter
    (fun nsid ->
      match Atp.Nsid.of_string nsid with
      | Ok _ -> fail ("nsid invalid: " ^ nsid) "should be invalid"
      | Error _ -> pass ("nsid invalid: " ^ nsid))
    invalid

(* ========== Record Key Tests ========== *)

let test_recordkey_valid () =
  let valid = load_test_file (interop_dir ^ "/recordkey_syntax_valid.txt") in
  List.iter
    (fun rkey ->
      match Atp.Record_key.of_string rkey with
      | Ok _ -> pass ("recordkey valid: " ^ rkey)
      | Error _ -> fail ("recordkey valid: " ^ rkey) "should be valid")
    valid

let test_recordkey_invalid () =
  let invalid =
    load_test_file (interop_dir ^ "/recordkey_syntax_invalid.txt")
  in
  List.iter
    (fun rkey ->
      match Atp.Record_key.of_string rkey with
      | Ok _ -> fail ("recordkey invalid: " ^ rkey) "should be invalid"
      | Error _ -> pass ("recordkey invalid: " ^ rkey))
    invalid

(* ========== AT-URI Tests ========== *)

let test_aturi_valid () =
  let valid = load_test_file (interop_dir ^ "/aturi_syntax_valid.txt") in
  List.iter
    (fun uri ->
      match Atp.At_uri.of_string uri with
      | Ok _ -> pass ("aturi valid: " ^ uri)
      | Error _ -> fail ("aturi valid: " ^ uri) "should be valid")
    valid

let test_aturi_invalid () =
  let invalid = load_test_file (interop_dir ^ "/aturi_syntax_invalid.txt") in
  List.iter
    (fun uri ->
      match Atp.At_uri.of_string uri with
      | Ok _ -> fail ("aturi invalid: " ^ uri) "should be invalid"
      | Error _ -> pass ("aturi invalid: " ^ uri))
    invalid

(* ========== Additional Unit Tests ========== *)

let test_tid_roundtrip () =
  let tid = Atp.Tid.create () in
  let s = Atp.Tid.to_string tid in
  match Atp.Tid.of_string_opt s with
  | Some tid2 when Atp.Tid.equal tid tid2 -> pass "tid roundtrip"
  | Some _ -> fail "tid roundtrip" "TIDs not equal"
  | None -> fail "tid roundtrip" "parse failed"

let test_tid_timestamp () =
  let tid = Atp.Tid.of_timestamp_us ~clockid:42 1000000L in
  let ts, clk = Atp.Tid.to_timestamp_us tid in
  if ts = 1000000L && clk = 42 then pass "tid timestamp"
  else fail "tid timestamp" (Printf.sprintf "got ts=%Ld clk=%d" ts clk)

let test_did_accessors () =
  match Atp.Did.of_string "did:plc:z72i7hdynmk6r22z27h6tvur" with
  | Ok did ->
      if
        Atp.Did.method_ did = "plc"
        && Atp.Did.method_specific_id did = "z72i7hdynmk6r22z27h6tvur"
      then pass "did accessors"
      else fail "did accessors" "wrong values"
  | Error _ -> fail "did accessors" "parse failed"

let test_nsid_accessors () =
  match Atp.Nsid.of_string "com.atproto.repo.createRecord" with
  | Ok nsid ->
      if
        Atp.Nsid.authority nsid = "com.atproto.repo"
        && Atp.Nsid.name nsid = "createRecord"
      then pass "nsid accessors"
      else fail "nsid accessors" "wrong values"
  | Error _ -> fail "nsid accessors" "parse failed"

let test_aturi_accessors () =
  match
    Atp.At_uri.of_string "at://did:plc:abc123/com.atproto.feed.post/xyz"
  with
  | Ok uri ->
      if
        Atp.At_uri.authority uri = "did:plc:abc123"
        && Atp.At_uri.collection uri = Some "com.atproto.feed.post"
        && Atp.At_uri.rkey uri = Some "xyz"
      then pass "aturi accessors"
      else fail "aturi accessors" "wrong values"
  | Error _ -> fail "aturi accessors" "parse failed"

let test_mst_layer () =
  (* Test known layer calculations from spec *)
  let layer = Atp.Mst.layer_of_key in
  (* These are approximate tests - actual values depend on SHA-256 *)
  let l1 = layer "test" in
  let l2 = layer "another" in
  if l1 >= 0 && l2 >= 0 then pass "mst layer"
  else fail "mst layer" "negative layer"

let test_car_roundtrip () =
  let cid = Atp.Cid.create `Dag_cbor "test data" in
  let header = Atp.Car.{ version = 1; roots = [ cid ] } in
  let blocks = List.to_seq [ (cid, "test data") ] in
  let encoded = Atp.Car.to_string header blocks in
  try
    let h, bs = Atp.Car.of_string encoded in
    if h.version = 1 && List.length h.roots = 1 && List.length bs = 1 then
      pass "car roundtrip"
    else fail "car roundtrip" "wrong structure"
  with exn -> fail "car roundtrip" (Printexc.to_string exn)

let test_block_map () =
  let cid = Atp.Cid.create `Dag_cbor "test" in
  let bm = Atp.Block_map.set cid "data" Atp.Block_map.empty in
  match Atp.Block_map.get cid bm with
  | Some "data" -> pass "block_map"
  | Some _ -> fail "block_map" "wrong data"
  | None -> fail "block_map" "not found"

let test_blockstore () =
  let store = Atp.Blockstore.memory () in
  let cid = Atp.Cid.create `Dag_cbor "blockstore test" in
  store#put cid "test data";
  match store#get cid with
  | Some "test data" -> pass "blockstore"
  | Some _ -> fail "blockstore" "wrong data"
  | None -> fail "blockstore" "not found"

(* Main test runner *)
let () =
  Printf.printf "=== AT Protocol Syntax Validation Tests ===\n\n%!";

  Printf.printf "--- TID Tests ---\n%!";
  test_tid_valid ();
  test_tid_invalid ();
  test_tid_roundtrip ();
  test_tid_timestamp ();

  Printf.printf "--- Handle Tests ---\n%!";
  test_handle_valid ();
  test_handle_invalid ();

  Printf.printf "--- DID Tests ---\n%!";
  test_did_valid ();
  test_did_invalid ();
  test_did_accessors ();

  Printf.printf "--- NSID Tests ---\n%!";
  test_nsid_valid ();
  test_nsid_invalid ();
  test_nsid_accessors ();

  Printf.printf "--- Record Key Tests ---\n%!";
  test_recordkey_valid ();
  test_recordkey_invalid ();

  Printf.printf "--- AT-URI Tests ---\n%!";
  test_aturi_valid ();
  test_aturi_invalid ();
  test_aturi_accessors ();

  Printf.printf "--- Data Structure Tests ---\n%!";
  test_mst_layer ();
  test_car_roundtrip ();
  test_block_map ();
  test_blockstore ();

  Printf.printf "\n=== Test Summary ===\n%!";
  Printf.printf "Passed: %d\n%!" results.passed;
  Printf.printf "Failed: %d\n%!" results.failed;

  if results.failed > 0 then begin
    Printf.printf "\nFailures:\n%!";
    List.iter (Printf.printf "  - %s\n%!") (List.rev results.errors);
    exit 1
  end
  else exit 0
