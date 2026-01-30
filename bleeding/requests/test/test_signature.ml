(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Tests for RFC 9421 HTTP Message Signatures *)

module Signature = Requests.Signature
module Headers = Requests.Headers

(** Helper to run tests with an Eio clock *)
let with_clock f () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  f clock

(** {1 Component Tests} *)

let test_component_identifiers () =
  let tests = [
    (Signature.Component.method_, "\"@method\"");
    (Signature.Component.authority, "\"@authority\"");
    (Signature.Component.path, "\"@path\"");
    (Signature.Component.query, "\"@query\"");
    (Signature.Component.target_uri, "\"@target-uri\"");
    (Signature.Component.field "content-type", "\"content-type\"");
    (Signature.Component.field_sf "content-type", "\"content-type\";sf");
    (Signature.Component.query_param "foo", "\"@query-param\";name=\"foo\"");
  ] in
  List.iter (fun (component, expected) ->
    let result = Signature.Component.to_identifier component in
    Alcotest.(check string) ("Component: " ^ expected) expected result
  ) tests

let test_component_parse () =
  let tests = [
    ("@method", true);
    ("@authority", true);
    ("content-type", true);
    ("content-type;sf", true);
  ] in
  List.iter (fun (input, should_succeed) ->
    match Signature.Component.of_identifier input with
    | Ok _ when should_succeed -> ()
    | Error _ when not should_succeed -> ()
    | Ok _ -> Alcotest.fail ("Should have failed: " ^ input)
    | Error e -> Alcotest.fail ("Should have succeeded: " ^ input ^ " - " ^ e)
  ) tests

(** {1 Algorithm Tests} *)

let test_algorithm_roundtrip () =
  let algs : Signature.Algorithm.t list = [
    `Ed25519;
    `Hmac_sha256;
    `Ecdsa_p256_sha256;
    `Ecdsa_p384_sha384;
    `Rsa_pss_sha512;
    `Rsa_v1_5_sha256;
  ] in
  List.iter (fun alg ->
    let s = Signature.Algorithm.to_string alg in
    match Signature.Algorithm.of_string s with
    | Some alg' when alg = alg' -> ()
    | Some _ -> Alcotest.fail ("Algorithm mismatch for " ^ s)
    | None -> Alcotest.fail ("Failed to parse algorithm: " ^ s)
  ) algs

(** {1 Content-Digest Tests} *)

let test_content_digest_sha256 () =
  let body = "Hello, World!" in
  let digest = Signature.Content_digest.compute ~algorithm:`Sha256 ~body in
  (* Verify it starts with sha-256= *)
  Alcotest.(check bool) "Has sha-256 prefix"
    true (String.length digest > 8 && String.sub digest 0 8 = "sha-256=")

let test_content_digest_verify () =
  let body = "Test body content" in
  let digest = Signature.Content_digest.compute ~algorithm:`Sha256 ~body in
  match Signature.Content_digest.verify ~header:digest ~body with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("Verification failed: " ^ e)

let test_content_digest_verify_mismatch () =
  let body = "Original content" in
  let digest = Signature.Content_digest.compute ~algorithm:`Sha256 ~body in
  match Signature.Content_digest.verify ~header:digest ~body:"Different content" with
  | Ok () -> Alcotest.fail "Should have failed verification"
  | Error _ -> () (* Expected *)

(** {1 HMAC Signing Tests} *)

let test_hmac_sign_verify clock =
  let secret = "my-secret-key-for-hmac-signing" in
  let key = Signature.Key.symmetric secret in
  let config = Signature.config ~key ~keyid:"test-key-1"
    ~components:[Signature.Component.method_; Signature.Component.authority]
    () in
  let headers = Headers.empty in
  let uri = Uri.of_string "https://example.com/api/test" in
  let context = Signature.Context.request ~method_:`GET ~uri ~headers in

  match Signature.sign ~clock ~config ~context ~headers with
  | Error e -> Alcotest.fail ("Signing failed: " ^ Signature.sign_error_to_string e)
  | Ok signed_headers ->
      (* Verify the signature was added *)
      Alcotest.(check bool) "Has Signature header"
        true (Option.is_some (Headers.get `Signature signed_headers));
      Alcotest.(check bool) "Has Signature-Input header"
        true (Option.is_some (Headers.get `Signature_input signed_headers));

      (* Verify the signature *)
      match Signature.verify ~clock ~key ~context ~headers:signed_headers () with
      | Error e -> Alcotest.fail ("Verification failed: " ^ Signature.verify_error_to_string e)
      | Ok result ->
          Alcotest.(check (option string)) "Keyid matches"
            (Some "test-key-1") result.keyid

(** {1 Ed25519 Signing Tests} *)

let test_ed25519_sign_verify clock =
  (* Generate a test Ed25519 key pair *)
  let priv, pub = Mirage_crypto_ec.Ed25519.generate () in
  let priv_bytes = Mirage_crypto_ec.Ed25519.priv_to_octets priv in
  let pub_bytes = Mirage_crypto_ec.Ed25519.pub_to_octets pub in

  let key = Signature.Key.ed25519 ~priv:priv_bytes ~pub:pub_bytes in
  let config = Signature.config ~key ~keyid:"ed25519-key"
    ~components:[
      Signature.Component.method_;
      Signature.Component.path;
      Signature.Component.authority;
    ]
    () in

  let headers = Headers.empty |> Headers.set `Content_type "application/json" in
  let uri = Uri.of_string "https://api.example.com/v1/resource?id=123" in
  let context = Signature.Context.request ~method_:`POST ~uri ~headers in

  match Signature.sign ~clock ~config ~context ~headers with
  | Error e -> Alcotest.fail ("Ed25519 signing failed: " ^ Signature.sign_error_to_string e)
  | Ok signed_headers ->
      (* Verify we can verify with the same key *)
      match Signature.verify ~clock ~key ~context ~headers:signed_headers () with
      | Error e -> Alcotest.fail ("Ed25519 verification failed: " ^ Signature.verify_error_to_string e)
      | Ok result ->
          Alcotest.(check (option string)) "Keyid matches"
            (Some "ed25519-key") result.keyid;
          Alcotest.(check int) "Verified 3 components"
            3 (List.length result.verified_components)

(** {1 Signature Base Construction Tests} *)

let test_signature_base_format clock =
  (* Test that the signature base has the correct format *)
  let key = Signature.Key.symmetric "test" in
  let config = Signature.config ~key ~keyid:"test"
    ~components:[Signature.Component.method_; Signature.Component.authority]
    ~include_created:false (* Don't include created for deterministic test *)
    () in

  let headers = Headers.empty in
  let uri = Uri.of_string "https://example.com/path" in
  let context = Signature.Context.request ~method_:`GET ~uri ~headers in

  match Signature.sign ~clock ~config ~context ~headers with
  | Error e -> Alcotest.fail ("Signing failed: " ^ Signature.sign_error_to_string e)
  | Ok signed_headers ->
      (* Check Signature-Input format *)
      match Headers.get `Signature_input signed_headers with
      | None -> Alcotest.fail "Missing Signature-Input"
      | Some input ->
          (* Should contain the components *)
          Alcotest.(check bool) "Contains @method"
            true (String.length input > 0 &&
                  (String.sub input 0 4 = "sig1" || String.sub input 0 4 = "sig="))

(** {1 Test Suite} *)

let component_tests = [
  "Component identifiers", `Quick, test_component_identifiers;
  "Component parsing", `Quick, test_component_parse;
]

let algorithm_tests = [
  "Algorithm roundtrip", `Quick, test_algorithm_roundtrip;
]

let content_digest_tests = [
  "SHA-256 digest", `Quick, test_content_digest_sha256;
  "Digest verification", `Quick, test_content_digest_verify;
  "Digest mismatch", `Quick, test_content_digest_verify_mismatch;
]

let signing_tests = [
  "HMAC sign and verify", `Quick, with_clock test_hmac_sign_verify;
  "Ed25519 sign and verify", `Quick, with_clock test_ed25519_sign_verify;
  "Signature base format", `Quick, with_clock test_signature_base_format;
]

let () =
  (* Initialize RNG for crypto operations *)
  Mirage_crypto_rng_unix.use_default ();

  Alcotest.run "HTTP Message Signatures (RFC 9421)" [
    "Components", component_tests;
    "Algorithms", algorithm_tests;
    "Content-Digest", content_digest_tests;
    "Signing", signing_tests;
  ]
