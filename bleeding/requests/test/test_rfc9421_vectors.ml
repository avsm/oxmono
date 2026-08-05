(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** RFC 9421 Appendix B Test Vectors

    These tests verify our implementation against the official test vectors
    from {{:https://datatracker.ietf.org/doc/html/rfc9421#appendix-B}RFC 9421 Appendix B}. *)

module Signature = Requests.Signature
module Headers = Requests.Headers

(** Helper to run tests with an Eio clock *)
let with_clock f () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  f clock

(* ========================================================================= *)
(* RFC B.1 Test Keys                                                         *)
(* ========================================================================= *)

(** Base64url decode (RFC 4648 Section 5) *)
let base64url_decode s =
  (* Replace URL-safe chars with standard base64 chars and add padding *)
  let s = String.map (function '-' -> '+' | '_' -> '/' | c -> c) s in
  let pad = (4 - (String.length s mod 4)) mod 4 in
  let s = s ^ String.make pad '=' in
  Base64.decode_exn s

(** B.1.4 test-key-ed25519

    The Ed25519 key pair from RFC 9421 Appendix B.1.4.
    JWK format:
    {[
      {
        "kty": "OKP",
        "crv": "Ed25519",
        "kid": "test-key-ed25519",
        "d": "n4Ni-HpISpVObnQMW0wOhCKROaIKqKtW_2ZYb2p9KcU",
        "x": "JrQLj5P_89iXES9-vFgrIy29clF9CC_oPPsw3c5D0bs"
      }
    ]} *)
let test_key_ed25519_priv = base64url_decode "n4Ni-HpISpVObnQMW0wOhCKROaIKqKtW_2ZYb2p9KcU"
let test_key_ed25519_pub = base64url_decode "JrQLj5P_89iXES9-vFgrIy29clF9CC_oPPsw3c5D0bs"

(** B.1.5 test-shared-secret

    64 random bytes for HMAC-SHA256 testing.
    Base64: uzvJfB4u3N0Jy4T7NZ75MDVcr8zSTInedJtkgcu46YW4XByzNJjxBdtjUkdJPBtbmHhIDi6pcl8jsasjlTMtDQ== *)
let test_shared_secret =
  Base64.decode_exn "uzvJfB4u3N0Jy4T7NZ75MDVcr8zSTInedJtkgcu46YW4XByzNJjxBdtjUkdJPBtbmHhIDi6pcl8jsasjlTMtDQ=="

(* ========================================================================= *)
(* RFC B.2 Test Request                                                      *)
(* ========================================================================= *)

(** The test-request from RFC 9421 Appendix B.2:

    POST /foo?param=Value&Pet=dog HTTP/1.1
    Host: example.com
    Date: Tue, 20 Apr 2021 02:07:55 GMT
    Content-Type: application/json
    Content-Digest: sha-512=:WZDPaVn/7XgHaAy8pmojAkGWoRx2UFChF41A2svX+TaPm+AbwAgBWnrIiYllu7BNNyealdVLvRwEmTHWXvJwew==:
    Content-Length: 18

    {"hello": "world"} *)
let test_request_uri = Uri.of_string "https://example.com/foo?param=Value&Pet=dog"
let test_request_headers =
  Headers.empty
  |> Headers.set `Date "Tue, 20 Apr 2021 02:07:55 GMT"
  |> Headers.set `Content_type "application/json"
  |> Headers.set `Content_digest "sha-512=:WZDPaVn/7XgHaAy8pmojAkGWoRx2UFChF41A2svX+TaPm+AbwAgBWnrIiYllu7BNNyealdVLvRwEmTHWXvJwew==:"
  |> Headers.set `Content_length "18"

(* ========================================================================= *)
(* B.2.5 Signing a Request Using hmac-sha256                                 *)
(* ========================================================================= *)

(** Test case B.2.5: HMAC-SHA256 signature

    Signature base:
    {v
"date": Tue, 20 Apr 2021 02:07:55 GMT
"@authority": example.com
"content-type": application/json
"@signature-params": ("date" "@authority" "content-type");created=1618884473;keyid="test-shared-secret"
    v}

    Expected Signature-Input: sig-b25=("date" "@authority" "content-type");created=1618884473;keyid="test-shared-secret"
    Expected Signature: sig-b25=:pxcQw6G3AjtMBQjwo8XzkZf/bws5LelbaMk5rGIGtE8=:
*)
let test_b25_hmac_signature_base () =
  (* Build the expected signature base string exactly as the RFC specifies *)
  let expected_base =
    "\"date\": Tue, 20 Apr 2021 02:07:55 GMT\n" ^
    "\"@authority\": example.com\n" ^
    "\"content-type\": application/json\n" ^
    "\"@signature-params\": (\"date\" \"@authority\" \"content-type\");created=1618884473;keyid=\"test-shared-secret\""
  in
  (* Compute HMAC-SHA256 over the expected signature base *)
  let mac = Digestif.SHA256.hmac_string ~key:test_shared_secret expected_base in
  let sig_bytes = Digestif.SHA256.to_raw_string mac in
  let sig_b64 = Base64.encode_string sig_bytes in
  (* RFC expected signature *)
  let expected_sig = "pxcQw6G3AjtMBQjwo8XzkZf/bws5LelbaMk5rGIGtE8=" in
  Alcotest.(check string) "B.2.5 HMAC signature" expected_sig sig_b64

let test_b25_hmac_verify clock =
  (* Create verification context and verify the RFC's signature *)
  let key = Signature.Key.symmetric test_shared_secret in
  let headers =
    test_request_headers
    |> Headers.set `Signature_input "sig-b25=(\"date\" \"@authority\" \"content-type\");created=1618884473;keyid=\"test-shared-secret\""
    |> Headers.set `Signature "sig-b25=:pxcQw6G3AjtMBQjwo8XzkZf/bws5LelbaMk5rGIGtE8=:"
  in
  let context = Signature.Context.request ~method_:`POST ~uri:test_request_uri ~headers in
  match Signature.verify ~clock ~key ~label:"sig-b25" ~context ~headers () with
  | Error e ->
      Alcotest.fail ("B.2.5 HMAC verification failed: " ^ Signature.verify_error_to_string e)
  | Ok result ->
      Alcotest.(check (option string)) "keyid matches"
        (Some "test-shared-secret") result.keyid;
      Alcotest.(check int) "verified 3 components"
        3 (List.length result.verified_components)

(* ========================================================================= *)
(* B.2.6 Signing a Request Using ed25519                                     *)
(* ========================================================================= *)

(** Test case B.2.6: Ed25519 signature

    Signature base:
    {v
"date": Tue, 20 Apr 2021 02:07:55 GMT
"@method": POST
"@path": /foo
"@authority": example.com
"content-type": application/json
"content-length": 18
"@signature-params": ("date" "@method" "@path" "@authority" "content-type" "content-length");created=1618884473;keyid="test-key-ed25519"
    v}

    Expected Signature-Input: sig-b26=("date" "@method" "@path" "@authority" "content-type" "content-length");created=1618884473;keyid="test-key-ed25519"
    Expected Signature: sig-b26=:wqcAqbmYJ2ji2glfAMaRy4gruYYnx2nEFN2HN6jrnDnQCK1u02Gb04v9EDgwUPiu4A0w6vuQv5lIp5WPpBKRCw==:
*)
let test_b26_ed25519_signature_base () =
  (* Build the expected signature base string exactly as the RFC specifies *)
  let expected_base =
    "\"date\": Tue, 20 Apr 2021 02:07:55 GMT\n" ^
    "\"@method\": POST\n" ^
    "\"@path\": /foo\n" ^
    "\"@authority\": example.com\n" ^
    "\"content-type\": application/json\n" ^
    "\"content-length\": 18\n" ^
    "\"@signature-params\": (\"date\" \"@method\" \"@path\" \"@authority\" \"content-type\" \"content-length\");created=1618884473;keyid=\"test-key-ed25519\""
  in
  (* Sign with Ed25519 *)
  match Mirage_crypto_ec.Ed25519.priv_of_octets test_key_ed25519_priv with
  | Error _ -> Alcotest.fail "Invalid Ed25519 private key"
  | Ok priv_key ->
      let sig_bytes = Mirage_crypto_ec.Ed25519.sign ~key:priv_key expected_base in
      let sig_b64 = Base64.encode_string sig_bytes in
      (* RFC expected signature *)
      let expected_sig = "wqcAqbmYJ2ji2glfAMaRy4gruYYnx2nEFN2HN6jrnDnQCK1u02Gb04v9EDgwUPiu4A0w6vuQv5lIp5WPpBKRCw==" in
      Alcotest.(check string) "B.2.6 Ed25519 signature" expected_sig sig_b64

let test_b26_ed25519_verify clock =
  (* Create verification context and verify the RFC's signature *)
  let key = Signature.Key.ed25519 ~priv:test_key_ed25519_priv ~pub:test_key_ed25519_pub in
  let headers =
    test_request_headers
    |> Headers.set `Signature_input "sig-b26=(\"date\" \"@method\" \"@path\" \"@authority\" \"content-type\" \"content-length\");created=1618884473;keyid=\"test-key-ed25519\""
    |> Headers.set `Signature "sig-b26=:wqcAqbmYJ2ji2glfAMaRy4gruYYnx2nEFN2HN6jrnDnQCK1u02Gb04v9EDgwUPiu4A0w6vuQv5lIp5WPpBKRCw==:"
  in
  let context = Signature.Context.request ~method_:`POST ~uri:test_request_uri ~headers in
  match Signature.verify ~clock ~key ~label:"sig-b26" ~context ~headers () with
  | Error e ->
      Alcotest.fail ("B.2.6 Ed25519 verification failed: " ^ Signature.verify_error_to_string e)
  | Ok result ->
      Alcotest.(check (option string)) "keyid matches"
        (Some "test-key-ed25519") result.keyid;
      Alcotest.(check int) "verified 6 components"
        6 (List.length result.verified_components)

(* ========================================================================= *)
(* B.4 HTTP Message Transformations (Ed25519)                                *)
(* ========================================================================= *)

(** Test case B.4: Message transformation test

    Original request:
    {v
GET /demo?name1=Value1&Name2=value2 HTTP/1.1
Host: example.org
Date: Fri, 15 Jul 2022 14:24:55 GMT
Accept: application/json
Accept: */*
    v}

    Signature base:
    {v
"@method": GET
"@path": /demo
"@authority": example.org
"accept": application/json, */*
"@signature-params": ("@method" "@path" "@authority" "accept");created=1618884473;keyid="test-key-ed25519"
    v}

    Expected Signature: transform=:ZT1kooQsEHpZ0I1IjCqtQppOmIqlJPeo7DHR3SoMn0s5JZ1eRGS0A+vyYP9t/LXlh5QMFFQ6cpLt2m0pmj3NDA==:
*)
let test_b4_transform_signature_base () =
  let expected_base =
    "\"@method\": GET\n" ^
    "\"@path\": /demo\n" ^
    "\"@authority\": example.org\n" ^
    "\"accept\": application/json, */*\n" ^
    "\"@signature-params\": (\"@method\" \"@path\" \"@authority\" \"accept\");created=1618884473;keyid=\"test-key-ed25519\""
  in
  match Mirage_crypto_ec.Ed25519.priv_of_octets test_key_ed25519_priv with
  | Error _ -> Alcotest.fail "Invalid Ed25519 private key"
  | Ok priv_key ->
      let sig_bytes = Mirage_crypto_ec.Ed25519.sign ~key:priv_key expected_base in
      let sig_b64 = Base64.encode_string sig_bytes in
      let expected_sig = "ZT1kooQsEHpZ0I1IjCqtQppOmIqlJPeo7DHR3SoMn0s5JZ1eRGS0A+vyYP9t/LXlh5QMFFQ6cpLt2m0pmj3NDA==" in
      Alcotest.(check string) "B.4 Transform Ed25519 signature" expected_sig sig_b64

let test_b4_transform_verify clock =
  let key = Signature.Key.ed25519 ~priv:test_key_ed25519_priv ~pub:test_key_ed25519_pub in
  let uri = Uri.of_string "https://example.org/demo?name1=Value1&Name2=value2" in
  let headers =
    Headers.empty
    |> Headers.set `Date "Fri, 15 Jul 2022 14:24:55 GMT"
    |> Headers.set `Accept "application/json, */*"
    |> Headers.set `Signature_input "transform=(\"@method\" \"@path\" \"@authority\" \"accept\");created=1618884473;keyid=\"test-key-ed25519\""
    |> Headers.set `Signature "transform=:ZT1kooQsEHpZ0I1IjCqtQppOmIqlJPeo7DHR3SoMn0s5JZ1eRGS0A+vyYP9t/LXlh5QMFFQ6cpLt2m0pmj3NDA==:"
  in
  let context = Signature.Context.request ~method_:`GET ~uri ~headers in
  match Signature.verify ~clock ~key ~label:"transform" ~context ~headers () with
  | Error e ->
      Alcotest.fail ("B.4 Transform verification failed: " ^ Signature.verify_error_to_string e)
  | Ok result ->
      Alcotest.(check (option string)) "keyid matches"
        (Some "test-key-ed25519") result.keyid;
      Alcotest.(check int) "verified 4 components"
        4 (List.length result.verified_components)

(** Test that modified messages fail verification - B.4 example where
    method changed from GET to POST should fail *)
let test_b4_transform_modified_fails clock =
  let key = Signature.Key.ed25519 ~priv:test_key_ed25519_priv ~pub:test_key_ed25519_pub in
  let uri = Uri.of_string "https://example.org/demo?name1=Value1&Name2=value2" in
  let headers =
    Headers.empty
    |> Headers.set `Date "Fri, 15 Jul 2022 14:24:55 GMT"
    |> Headers.set `Accept "application/json, */*"
    |> Headers.set `Signature_input "transform=(\"@method\" \"@path\" \"@authority\" \"accept\");created=1618884473;keyid=\"test-key-ed25519\""
    |> Headers.set `Signature "transform=:ZT1kooQsEHpZ0I1IjCqtQppOmIqlJPeo7DHR3SoMn0s5JZ1eRGS0A+vyYP9t/LXlh5QMFFQ6cpLt2m0pmj3NDA==:"
  in
  (* Use POST instead of GET - should fail verification *)
  let context = Signature.Context.request ~method_:`POST ~uri ~headers in
  match Signature.verify ~clock ~key ~label:"transform" ~context ~headers () with
  | Error _ -> () (* Expected - signature should not verify *)
  | Ok _ -> Alcotest.fail "B.4 Modified message should have failed verification"

(** Test that authority modification fails verification *)
let test_b4_transform_authority_modified_fails clock =
  let key = Signature.Key.ed25519 ~priv:test_key_ed25519_priv ~pub:test_key_ed25519_pub in
  (* Changed authority from example.org to example.com *)
  let uri = Uri.of_string "https://example.com/demo?name1=Value1&Name2=value2" in
  let headers =
    Headers.empty
    |> Headers.set `Date "Fri, 15 Jul 2022 14:24:55 GMT"
    |> Headers.set `Accept "application/json, */*"
    |> Headers.set `Signature_input "transform=(\"@method\" \"@path\" \"@authority\" \"accept\");created=1618884473;keyid=\"test-key-ed25519\""
    |> Headers.set `Signature "transform=:ZT1kooQsEHpZ0I1IjCqtQppOmIqlJPeo7DHR3SoMn0s5JZ1eRGS0A+vyYP9t/LXlh5QMFFQ6cpLt2m0pmj3NDA==:"
  in
  let context = Signature.Context.request ~method_:`GET ~uri ~headers in
  match Signature.verify ~clock ~key ~label:"transform" ~context ~headers () with
  | Error _ -> () (* Expected - signature should not verify *)
  | Ok _ -> Alcotest.fail "B.4 Authority-modified message should have failed verification"

(* ========================================================================= *)
(* Signature Base Format Tests                                               *)
(* ========================================================================= *)

(** Test that we correctly format derived components *)
let test_derived_component_format () =
  let tests = [
    (Signature.Component.method_, "\"@method\"");
    (Signature.Component.authority, "\"@authority\"");
    (Signature.Component.path, "\"@path\"");
    (Signature.Component.query, "\"@query\"");
    (Signature.Component.target_uri, "\"@target-uri\"");
    (Signature.Component.status, "\"@status\"");
    (Signature.Component.query_param "Pet", "\"@query-param\";name=\"Pet\"");
  ] in
  List.iter (fun (comp, expected) ->
    let result = Signature.Component.to_identifier comp in
    Alcotest.(check string) ("Component: " ^ expected) expected result
  ) tests

(** Test header field component format *)
let test_field_component_format () =
  let tests = [
    (Signature.Component.field "content-type", "\"content-type\"");
    (Signature.Component.field "Content-Type", "\"content-type\"");  (* Should lowercase *)
    (Signature.Component.field_sf "accept", "\"accept\";sf");
    (Signature.Component.field_bs "x-custom", "\"x-custom\";bs");
  ] in
  List.iter (fun (comp, expected) ->
    let result = Signature.Component.to_identifier comp in
    Alcotest.(check string) ("Field: " ^ expected) expected result
  ) tests

(* ========================================================================= *)
(* Content-Digest Tests (RFC 9530 integration)                               *)
(* ========================================================================= *)

(** Verify Content-Digest from the RFC test-request:
    sha-512=:WZDPaVn/7XgHaAy8pmojAkGWoRx2UFChF41A2svX+TaPm+AbwAgBWnrIiYllu7BNNyealdVLvRwEmTHWXvJwew==: *)
let test_content_digest_rfc_body () =
  let body = "{\"hello\": \"world\"}" in
  let expected = "sha-512=:WZDPaVn/7XgHaAy8pmojAkGWoRx2UFChF41A2svX+TaPm+AbwAgBWnrIiYllu7BNNyealdVLvRwEmTHWXvJwew==:" in
  let computed = Signature.Content_digest.compute ~algorithm:`Sha512 ~body in
  Alcotest.(check string) "RFC test body Content-Digest" expected computed

let test_content_digest_verify_rfc () =
  let body = "{\"hello\": \"world\"}" in
  let digest = "sha-512=:WZDPaVn/7XgHaAy8pmojAkGWoRx2UFChF41A2svX+TaPm+AbwAgBWnrIiYllu7BNNyealdVLvRwEmTHWXvJwew==:" in
  match Signature.Content_digest.verify ~header:digest ~body with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("RFC Content-Digest verification failed: " ^ e)

(* ========================================================================= *)
(* Test Suite                                                                *)
(* ========================================================================= *)

let hmac_tests = [
  "B.2.5 HMAC signature base", `Quick, test_b25_hmac_signature_base;
  "B.2.5 HMAC verify", `Quick, with_clock test_b25_hmac_verify;
]

let ed25519_tests = [
  "B.2.6 Ed25519 signature base", `Quick, test_b26_ed25519_signature_base;
  "B.2.6 Ed25519 verify", `Quick, with_clock test_b26_ed25519_verify;
]

let transform_tests = [
  "B.4 Transform signature base", `Quick, test_b4_transform_signature_base;
  "B.4 Transform verify", `Quick, with_clock test_b4_transform_verify;
  "B.4 Modified method fails", `Quick, with_clock test_b4_transform_modified_fails;
  "B.4 Modified authority fails", `Quick, with_clock test_b4_transform_authority_modified_fails;
]

let component_tests = [
  "Derived component format", `Quick, test_derived_component_format;
  "Field component format", `Quick, test_field_component_format;
]

let digest_tests = [
  "RFC test body Content-Digest", `Quick, test_content_digest_rfc_body;
  "RFC Content-Digest verify", `Quick, test_content_digest_verify_rfc;
]

let () =
  Mirage_crypto_rng_unix.use_default ();
  Alcotest.run "RFC 9421 Test Vectors" [
    "B.2.5 HMAC-SHA256", hmac_tests;
    "B.2.6 Ed25519", ed25519_tests;
    "B.4 Transformations", transform_tests;
    "Components", component_tests;
    "Content-Digest", digest_tests;
  ]
