(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Unit tests for H2_hpack module. *)



(* ============================================================
   Test Utilities
   ============================================================ *)

let check_ok msg = function
  | Ok v -> v
  | Error e ->
      Alcotest.fail (Format.asprintf "%s: %a" msg H2_hpack.pp_error e)

(* ============================================================
   Huffman Tests
   ============================================================ *)

let test_huffman_encode_decode () =
  let test_str s =
    let encoded_len = H2_hpack.Huffman.encoded_length s in
    let buf = Cstruct.create (encoded_len + 10) in
    let len = H2_hpack.Huffman.encode buf 0 s in
    Alcotest.(check int) "encoded length matches" encoded_len len;
    let encoded = Cstruct.to_string ~off:0 ~len buf in
    let decoded = check_ok "huffman decode" (H2_hpack.Huffman.decode encoded) in
    Alcotest.(check string) "roundtrip" s decoded
  in
  test_str "www.example.com";
  test_str "no-cache";
  test_str "custom-key";
  test_str "custom-value";
  test_str ":method";
  test_str "GET";
  test_str "/"

let test_huffman_common_strings () =
  (* Test common HTTP header values *)
  let test_str s =
    let encoded_len = H2_hpack.Huffman.encoded_length s in
    let raw_len = String.length s in
    (* Huffman should usually be shorter for common HTTP text *)
    Alcotest.(check bool) (Printf.sprintf "huffman efficient for '%s'" s)
      true (encoded_len <= raw_len + 2)
  in
  test_str "text/html";
  test_str "application/json";
  test_str "gzip, deflate";
  test_str "Mozilla/5.0"

(* ============================================================
   Static Table Tests
   ============================================================ *)

let test_static_table_size () =
  Alcotest.(check int) "static table has 61 entries"
    61 H2_hpack_tables.static_table_size

let test_static_table_entries () =
  (* Verify some well-known entries per RFC 7541 Appendix A *)
  Alcotest.(check (pair string string)) "entry 1"
    (":authority", "") H2_hpack_tables.static_table.(0);
  Alcotest.(check (pair string string)) "entry 2"
    (":method", "GET") H2_hpack_tables.static_table.(1);
  Alcotest.(check (pair string string)) "entry 3"
    (":method", "POST") H2_hpack_tables.static_table.(2);
  Alcotest.(check (pair string string)) "entry 4"
    (":path", "/") H2_hpack_tables.static_table.(3);
  Alcotest.(check (pair string string)) "entry 7"
    (":scheme", "https") H2_hpack_tables.static_table.(6);
  Alcotest.(check (pair string string)) "entry 8"
    (":status", "200") H2_hpack_tables.static_table.(7)

let test_static_table_lookup () =
  Alcotest.(check int) ":authority index"
    0 (H2_hpack_tables.lookup_token_index ":authority");
  Alcotest.(check int) ":method index"
    1 (H2_hpack_tables.lookup_token_index ":method");
  Alcotest.(check int) ":path index"
    3 (H2_hpack_tables.lookup_token_index ":path");
  Alcotest.(check int) ":scheme index"
    5 (H2_hpack_tables.lookup_token_index ":scheme");
  Alcotest.(check int) ":status index"
    7 (H2_hpack_tables.lookup_token_index ":status");
  Alcotest.(check int) "content-type index"
    30 (H2_hpack_tables.lookup_token_index "content-type");
  Alcotest.(check int) "unknown returns -1"
    (-1) (H2_hpack_tables.lookup_token_index "x-custom-header")

(* ============================================================
   Encoder/Decoder Round-trip Tests
   ============================================================ *)

let test_encode_decode_simple () =
  let encoder = H2_hpack.Encoder.create 4096 in
  let decoder = H2_hpack.Decoder.create 4096 in
  let headers = [
    { H2_hpack.name = ":method"; value = "GET"; sensitive = false };
    { H2_hpack.name = ":path"; value = "/"; sensitive = false };
    { H2_hpack.name = ":scheme"; value = "https"; sensitive = false };
    { H2_hpack.name = ":authority"; value = "example.com"; sensitive = false };
  ] in
  let buf = Cstruct.create 1024 in
  let len = H2_hpack.Encoder.encode_headers encoder buf headers in
  Alcotest.(check bool) "encoded some bytes" true (len > 0);
  let header_block = Cstruct.sub buf 0 len in
  let decoded = check_ok "decode" (H2_hpack.Decoder.decode decoder header_block) in
  Alcotest.(check int) "same number of headers" (List.length headers) (List.length decoded);
  List.iter2 (fun expected actual ->
    Alcotest.(check string) "name matches" expected.H2_hpack.name actual.H2_hpack.name;
    Alcotest.(check string) "value matches" expected.H2_hpack.value actual.H2_hpack.value
  ) headers decoded

let test_encode_decode_with_custom_headers () =
  let encoder = H2_hpack.Encoder.create 4096 in
  let decoder = H2_hpack.Decoder.create 4096 in
  let headers = [
    { H2_hpack.name = ":method"; value = "POST"; sensitive = false };
    { H2_hpack.name = ":path"; value = "/api/data"; sensitive = false };
    { H2_hpack.name = "content-type"; value = "application/json"; sensitive = false };
    { H2_hpack.name = "x-custom-header"; value = "custom-value"; sensitive = false };
  ] in
  let buf = Cstruct.create 1024 in
  let len = H2_hpack.Encoder.encode_headers encoder buf headers in
  let header_block = Cstruct.sub buf 0 len in
  let decoded = check_ok "decode" (H2_hpack.Decoder.decode decoder header_block) in
  Alcotest.(check int) "header count" 4 (List.length decoded);
  let custom = List.find (fun h -> h.H2_hpack.name = "x-custom-header") decoded in
  Alcotest.(check string) "custom value" "custom-value" custom.H2_hpack.value

let test_encode_decode_sensitive () =
  let encoder = H2_hpack.Encoder.create 4096 in
  let decoder = H2_hpack.Decoder.create 4096 in
  let headers = [
    { H2_hpack.name = "authorization"; value = "Bearer secret123"; sensitive = true };
  ] in
  let buf = Cstruct.create 1024 in
  let len = H2_hpack.Encoder.encode_headers encoder buf headers in
  let header_block = Cstruct.sub buf 0 len in
  let decoded = check_ok "decode" (H2_hpack.Decoder.decode decoder header_block) in
  Alcotest.(check int) "header count" 1 (List.length decoded);
  let auth = List.hd decoded in
  Alcotest.(check string) "auth name" "authorization" auth.H2_hpack.name;
  Alcotest.(check string) "auth value" "Bearer secret123" auth.H2_hpack.value;
  Alcotest.(check bool) "marked sensitive" true auth.H2_hpack.sensitive

(* ============================================================
   Dynamic Table Tests
   ============================================================ *)

let test_dynamic_table_indexing () =
  (* Encode same header twice - second should use indexed representation *)
  let encoder = H2_hpack.Encoder.create 4096 in
  let decoder = H2_hpack.Decoder.create 4096 in
  let headers1 = [
    { H2_hpack.name = "x-repeated"; value = "same-value"; sensitive = false };
  ] in
  let headers2 = [
    { H2_hpack.name = "x-repeated"; value = "same-value"; sensitive = false };
  ] in
  let buf1 = Cstruct.create 1024 in
  let len1 = H2_hpack.Encoder.encode_headers encoder buf1 headers1 in
  let buf2 = Cstruct.create 1024 in
  let len2 = H2_hpack.Encoder.encode_headers encoder buf2 headers2 in
  (* Second encoding should be shorter due to indexing *)
  Alcotest.(check bool) "second encoding shorter" true (len2 < len1);
  (* Both should decode correctly *)
  let _ = check_ok "decode1" (H2_hpack.Decoder.decode decoder (Cstruct.sub buf1 0 len1)) in
  let decoded2 = check_ok "decode2" (H2_hpack.Decoder.decode decoder (Cstruct.sub buf2 0 len2)) in
  let h = List.hd decoded2 in
  Alcotest.(check string) "name" "x-repeated" h.H2_hpack.name;
  Alcotest.(check string) "value" "same-value" h.H2_hpack.value

let test_encoder_decoder_separate_contexts () =
  (* Each connection needs its own encoder/decoder pair *)
  let encoder1 = H2_hpack.Encoder.create 4096 in
  let decoder1 = H2_hpack.Decoder.create 4096 in
  let encoder2 = H2_hpack.Encoder.create 4096 in
  let decoder2 = H2_hpack.Decoder.create 4096 in
  let headers = [
    { H2_hpack.name = "x-test"; value = "value"; sensitive = false };
  ] in
  (* Encode with encoder1 *)
  let buf1 = Cstruct.create 1024 in
  let len1 = H2_hpack.Encoder.encode_headers encoder1 buf1 headers in
  (* Decode with decoder1 should work *)
  let _ = check_ok "decode with decoder1"
    (H2_hpack.Decoder.decode decoder1 (Cstruct.sub buf1 0 len1)) in
  (* Encode with encoder2 *)
  let buf2 = Cstruct.create 1024 in
  let len2 = H2_hpack.Encoder.encode_headers encoder2 buf2 headers in
  (* Decode with decoder2 should work *)
  let _ = check_ok "decode with decoder2"
    (H2_hpack.Decoder.decode decoder2 (Cstruct.sub buf2 0 len2)) in
  (* Lengths should be the same since dynamic tables haven't diverged *)
  Alcotest.(check int) "same encoding length" len1 len2

(* ============================================================
   RFC 7541 Appendix C Examples
   ============================================================ *)

(* C.2.1 - Literal Header Field with Indexing *)
let test_rfc_c2_1 () =
  let decoder = H2_hpack.Decoder.create 4096 in
  (* Hex: 400a 6375 7374 6f6d 2d6b 6579 0d63 7573 746f 6d2d 6865 6164 6572 *)
  let hex_bytes = [|
    0x40; 0x0a; 0x63; 0x75; 0x73; 0x74; 0x6f; 0x6d;
    0x2d; 0x6b; 0x65; 0x79; 0x0d; 0x63; 0x75; 0x73;
    0x74; 0x6f; 0x6d; 0x2d; 0x68; 0x65; 0x61; 0x64;
    0x65; 0x72
  |] in
  let buf = Cstruct.create (Array.length hex_bytes) in
  Array.iteri (fun i b -> Cstruct.set_uint8 buf i b) hex_bytes;
  let decoded = check_ok "RFC C.2.1" (H2_hpack.Decoder.decode decoder buf) in
  Alcotest.(check int) "one header" 1 (List.length decoded);
  let h = List.hd decoded in
  Alcotest.(check string) "name" "custom-key" h.H2_hpack.name;
  Alcotest.(check string) "value" "custom-header" h.H2_hpack.value

(* C.3.1 - First Request (with Huffman) *)
let test_rfc_c3_1 () =
  let decoder = H2_hpack.Decoder.create 4096 in
  (* First request:
     :method: GET
     :scheme: http
     :path: /
     :authority: www.example.com *)
  let hex_bytes = [|
    0x82; 0x86; 0x84; 0x41; 0x8c; 0xf1; 0xe3; 0xc2;
    0xe5; 0xf2; 0x3a; 0x6b; 0xa0; 0xab; 0x90; 0xf4;
    0xff
  |] in
  let buf = Cstruct.create (Array.length hex_bytes) in
  Array.iteri (fun i b -> Cstruct.set_uint8 buf i b) hex_bytes;
  let decoded = check_ok "RFC C.3.1" (H2_hpack.Decoder.decode decoder buf) in
  Alcotest.(check int) "four headers" 4 (List.length decoded);
  let method_h = List.find (fun h -> h.H2_hpack.name = ":method") decoded in
  Alcotest.(check string) ":method value" "GET" method_h.H2_hpack.value;
  let authority_h = List.find (fun h -> h.H2_hpack.name = ":authority") decoded in
  Alcotest.(check string) ":authority value" "www.example.com" authority_h.H2_hpack.value

(* ============================================================
   Default Table Size
   ============================================================ *)

let test_default_table_size () =
  Alcotest.(check int) "default table size" 4096 H2_hpack.default_table_size

(* ============================================================
   Empty Header Block
   ============================================================ *)

let test_empty_header_block () =
  let decoder = H2_hpack.Decoder.create 4096 in
  let buf = Cstruct.create 0 in
  let decoded = check_ok "empty block" (H2_hpack.Decoder.decode decoder buf) in
  Alcotest.(check int) "no headers" 0 (List.length decoded)

(* ============================================================
   Test Suite
   ============================================================ *)

let () =
  Alcotest.run "H2_hpack" [
    "huffman", [
      Alcotest.test_case "encode decode" `Quick test_huffman_encode_decode;
      Alcotest.test_case "common strings" `Quick test_huffman_common_strings;
    ];
    "static_table", [
      Alcotest.test_case "size" `Quick test_static_table_size;
      Alcotest.test_case "entries" `Quick test_static_table_entries;
      Alcotest.test_case "lookup" `Quick test_static_table_lookup;
    ];
    "encode_decode", [
      Alcotest.test_case "simple" `Quick test_encode_decode_simple;
      Alcotest.test_case "custom headers" `Quick test_encode_decode_with_custom_headers;
      Alcotest.test_case "sensitive" `Quick test_encode_decode_sensitive;
    ];
    "dynamic_table", [
      Alcotest.test_case "indexing" `Quick test_dynamic_table_indexing;
      Alcotest.test_case "separate contexts" `Quick test_encoder_decoder_separate_contexts;
    ];
    "rfc_examples", [
      Alcotest.test_case "C.2.1" `Quick test_rfc_c2_1;
      Alcotest.test_case "C.3.1" `Quick test_rfc_c3_1;
    ];
    "misc", [
      Alcotest.test_case "default table size" `Quick test_default_table_size;
      Alcotest.test_case "empty header block" `Quick test_empty_header_block;
    ];
  ]
