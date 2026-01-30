(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Tests for WebSocket module (RFC 6455) *)

module Websocket = Requests.Websocket
module Headers = Requests.Headers

(** Helper for string contains *)
let string_contains ~affix s =
  let alen = String.length affix in
  let slen = String.length s in
  if alen > slen then false
  else
    let rec check i =
      if i + alen > slen then false
      else if String.sub s i alen = affix then true
      else check (i + 1)
    in
    check 0

(** {1 Key Generation Tests} *)

let test_generate_key_length () =
  let key = Websocket.generate_key () in
  (* Base64 of 16 bytes = 24 characters (with padding) *)
  (* Actually 16 bytes -> ceil(16/3)*4 = 24 chars with padding,
     but base64 library might not pad. Let's check it decodes to 16 bytes *)
  let decoded = Base64.decode_exn key in
  Alcotest.(check int) "decoded length" 16 (String.length decoded)

let test_generate_key_unique () =
  let key1 = Websocket.generate_key () in
  let key2 = Websocket.generate_key () in
  Alcotest.(check bool) "keys are different" true (key1 <> key2)

let test_generate_key_valid_base64 () =
  let key = Websocket.generate_key () in
  (* Should not raise *)
  let _ = Base64.decode_exn key in
  Alcotest.(check pass) "valid base64" () ()

(** {1 Accept Computation Tests (RFC 6455 Section 4.2.2)} *)

let test_compute_accept_rfc_example () =
  (* RFC 6455 Section 1.3 example:
     Key: "dGhlIHNhbXBsZSBub25jZQ=="
     Accept: "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" *)
  let key = "dGhlIHNhbXBsZSBub25jZQ==" in
  let expected = "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" in
  let accept = Websocket.compute_accept ~key in
  Alcotest.(check string) "RFC example accept" expected accept

let test_validate_accept_correct () =
  let key = "dGhlIHNhbXBsZSBub25jZQ==" in
  let accept = "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" in
  Alcotest.(check bool) "valid accept" true
    (Websocket.validate_accept ~key ~accept)

let test_validate_accept_incorrect () =
  let key = "dGhlIHNhbXBsZSBub25jZQ==" in
  let accept = "wrongvalue" in
  Alcotest.(check bool) "invalid accept" false
    (Websocket.validate_accept ~key ~accept)

let test_compute_validate_roundtrip () =
  let key = Websocket.generate_key () in
  let accept = Websocket.compute_accept ~key in
  Alcotest.(check bool) "roundtrip validation" true
    (Websocket.validate_accept ~key ~accept)

(** {1 Protocol Negotiation Tests} *)

let test_parse_protocols_basic () =
  let protos = Websocket.parse_protocols "graphql-ws, graphql-transport-ws" in
  Alcotest.(check int) "count" 2 (List.length protos);
  Alcotest.(check bool) "has graphql-ws" true (List.mem "graphql-ws" protos);
  Alcotest.(check bool) "has graphql-transport-ws" true
    (List.mem "graphql-transport-ws" protos)

let test_parse_protocols_single () =
  let protos = Websocket.parse_protocols "chat" in
  Alcotest.(check (list string)) "single" ["chat"] protos

let test_parse_protocols_empty () =
  let protos = Websocket.parse_protocols "" in
  Alcotest.(check (list string)) "empty" [] protos

let test_protocols_to_string () =
  let protos = ["graphql-ws"; "graphql-transport-ws"] in
  let s = Websocket.protocols_to_string protos in
  Alcotest.(check string) "to_string" "graphql-ws, graphql-transport-ws" s

let test_select_protocol_match () =
  let offered = ["chat"; "superchat"] in
  let supported = ["superchat"; "chat"] in
  let selected = Websocket.select_protocol ~offered ~supported in
  (* Should select first from supported that is in offered *)
  Alcotest.(check (option string)) "selected" (Some "superchat") selected

let test_select_protocol_no_match () =
  let offered = ["chat"] in
  let supported = ["other"] in
  let selected = Websocket.select_protocol ~offered ~supported in
  Alcotest.(check (option string)) "no match" None selected

(** {1 Extension Parsing Tests} *)

let test_parse_extensions_basic () =
  let exts = Websocket.parse_extensions "permessage-deflate" in
  Alcotest.(check int) "count" 1 (List.length exts);
  Alcotest.(check string) "name" "permessage-deflate" (List.hd exts).name;
  Alcotest.(check int) "params count" 0 (List.length (List.hd exts).params)

let test_parse_extensions_with_params () =
  let exts = Websocket.parse_extensions
    "permessage-deflate; client_max_window_bits; server_no_context_takeover" in
  Alcotest.(check int) "count" 1 (List.length exts);
  let ext = List.hd exts in
  Alcotest.(check string) "name" "permessage-deflate" ext.name;
  Alcotest.(check int) "params count" 2 (List.length ext.params)

let test_parse_extensions_with_values () =
  let exts = Websocket.parse_extensions
    "permessage-deflate; client_max_window_bits=15" in
  let ext = List.hd exts in
  Alcotest.(check string) "name" "permessage-deflate" ext.name;
  match ext.params with
  | [(key, Some value)] ->
      Alcotest.(check string) "param key" "client_max_window_bits" key;
      Alcotest.(check string) "param value" "15" value
  | _ ->
      Alcotest.fail "Expected one param with value"

let test_parse_extensions_multiple () =
  let exts = Websocket.parse_extensions "permessage-deflate, x-custom" in
  Alcotest.(check int) "count" 2 (List.length exts);
  Alcotest.(check bool) "has permessage-deflate" true
    (Websocket.has_extension ~name:"permessage-deflate" exts);
  Alcotest.(check bool) "has x-custom" true
    (Websocket.has_extension ~name:"x-custom" exts)

let test_extensions_to_string () =
  let exts = [
    { Websocket.name = "permessage-deflate";
      params = [("client_max_window_bits", None)] }
  ] in
  let s = Websocket.extensions_to_string exts in
  Alcotest.(check string) "to_string"
    "permessage-deflate; client_max_window_bits" s

let test_get_extension_params () =
  let exts = Websocket.parse_extensions
    "permessage-deflate; client_max_window_bits=15" in
  match Websocket.get_extension_params ~name:"permessage-deflate" exts with
  | Some params ->
      Alcotest.(check int) "params count" 1 (List.length params)
  | None ->
      Alcotest.fail "Expected Some params"

(** {1 Upgrade Headers Tests} *)

let test_make_upgrade_headers_basic () =
  let key = "dGhlIHNhbXBsZSBub25jZQ==" in
  let headers = Websocket.make_upgrade_headers ~key () in
  Alcotest.(check (option string)) "Upgrade" (Some "websocket")
    (Headers.get `Upgrade headers);
  Alcotest.(check (option string)) "Connection" (Some "Upgrade")
    (Headers.get `Connection headers);
  Alcotest.(check (option string)) "Sec-WebSocket-Key" (Some key)
    (Headers.get `Sec_websocket_key headers);
  Alcotest.(check (option string)) "Sec-WebSocket-Version" (Some "13")
    (Headers.get `Sec_websocket_version headers)

let test_make_upgrade_headers_with_protocols () =
  let key = Websocket.generate_key () in
  let headers = Websocket.make_upgrade_headers ~key
    ~protocols:["graphql-ws"; "graphql-transport-ws"] () in
  match Headers.get `Sec_websocket_protocol headers with
  | Some proto ->
      Alcotest.(check bool) "contains graphql-ws" true
        (string_contains ~affix:"graphql-ws" proto)
  | None ->
      Alcotest.fail "Expected Sec-WebSocket-Protocol header"

let test_make_upgrade_headers_with_origin () =
  let key = Websocket.generate_key () in
  let headers = Websocket.make_upgrade_headers ~key
    ~origin:"https://example.com" () in
  Alcotest.(check (option string)) "Origin" (Some "https://example.com")
    (Headers.get `Origin headers)

(** {1 Upgrade Response Validation Tests} *)

let test_validate_response_success () =
  let key = "dGhlIHNhbXBsZSBub25jZQ==" in
  let accept = "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" in
  let headers = Headers.empty
    |> Headers.set `Upgrade "websocket"
    |> Headers.set `Connection "Upgrade"
    |> Headers.set `Sec_websocket_accept accept in
  let result = Websocket.validate_upgrade_response ~key ~status:101 ~headers in
  match result with
  | Ok () -> Alcotest.(check pass) "valid response" () ()
  | Error msg -> Alcotest.fail msg

let test_validate_response_wrong_status () =
  let key = Websocket.generate_key () in
  let headers = Headers.empty in
  let result = Websocket.validate_upgrade_response ~key ~status:200 ~headers in
  match result with
  | Error msg ->
      Alcotest.(check bool) "mentions 101" true
        (string_contains ~affix:"101" msg)
  | Ok () ->
      Alcotest.fail "Expected error for wrong status"

let test_validate_response_missing_upgrade () =
  let key = Websocket.generate_key () in
  let headers = Headers.empty
    |> Headers.set `Connection "Upgrade" in
  let result = Websocket.validate_upgrade_response ~key ~status:101 ~headers in
  match result with
  | Error msg ->
      Alcotest.(check bool) "mentions Upgrade" true
        (string_contains ~affix:"Upgrade" msg)
  | Ok () ->
      Alcotest.fail "Expected error for missing Upgrade"

let test_validate_response_wrong_accept () =
  let key = "dGhlIHNhbXBsZSBub25jZQ==" in
  let headers = Headers.empty
    |> Headers.set `Upgrade "websocket"
    |> Headers.set `Connection "Upgrade"
    |> Headers.set `Sec_websocket_accept "wrongvalue" in
  let result = Websocket.validate_upgrade_response ~key ~status:101 ~headers in
  match result with
  | Error msg ->
      Alcotest.(check bool) "mentions accept" true
        (string_contains ~affix:"Accept" msg ||
         string_contains ~affix:"accept" msg)
  | Ok () ->
      Alcotest.fail "Expected error for wrong accept"

(** {1 Test Suite} *)

let () =
  (* Initialize RNG for key generation *)
  Mirage_crypto_rng_unix.use_default ();
  Alcotest.run "WebSocket (RFC 6455)" [
    ("Key Generation", [
      Alcotest.test_case "Key length" `Quick test_generate_key_length;
      Alcotest.test_case "Keys unique" `Quick test_generate_key_unique;
      Alcotest.test_case "Valid base64" `Quick test_generate_key_valid_base64;
    ]);
    ("Accept Computation", [
      Alcotest.test_case "RFC example" `Quick test_compute_accept_rfc_example;
      Alcotest.test_case "Validate correct" `Quick test_validate_accept_correct;
      Alcotest.test_case "Validate incorrect" `Quick test_validate_accept_incorrect;
      Alcotest.test_case "Roundtrip" `Quick test_compute_validate_roundtrip;
    ]);
    ("Protocol Negotiation", [
      Alcotest.test_case "Parse basic" `Quick test_parse_protocols_basic;
      Alcotest.test_case "Parse single" `Quick test_parse_protocols_single;
      Alcotest.test_case "Parse empty" `Quick test_parse_protocols_empty;
      Alcotest.test_case "To string" `Quick test_protocols_to_string;
      Alcotest.test_case "Select match" `Quick test_select_protocol_match;
      Alcotest.test_case "Select no match" `Quick test_select_protocol_no_match;
    ]);
    ("Extension Parsing", [
      Alcotest.test_case "Basic" `Quick test_parse_extensions_basic;
      Alcotest.test_case "With params" `Quick test_parse_extensions_with_params;
      Alcotest.test_case "With values" `Quick test_parse_extensions_with_values;
      Alcotest.test_case "Multiple" `Quick test_parse_extensions_multiple;
      Alcotest.test_case "To string" `Quick test_extensions_to_string;
      Alcotest.test_case "Get params" `Quick test_get_extension_params;
    ]);
    ("Upgrade Headers", [
      Alcotest.test_case "Basic headers" `Quick test_make_upgrade_headers_basic;
      Alcotest.test_case "With protocols" `Quick test_make_upgrade_headers_with_protocols;
      Alcotest.test_case "With origin" `Quick test_make_upgrade_headers_with_origin;
    ]);
    ("Response Validation", [
      Alcotest.test_case "Success" `Quick test_validate_response_success;
      Alcotest.test_case "Wrong status" `Quick test_validate_response_wrong_status;
      Alcotest.test_case "Missing Upgrade" `Quick test_validate_response_missing_upgrade;
      Alcotest.test_case "Wrong accept" `Quick test_validate_response_wrong_accept;
    ]);
  ]
