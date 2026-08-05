(*---------------------------------------------------------------------------
   Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
   SPDX-License-Identifier: MIT
  ---------------------------------------------------------------------------*)

(* Crowbar-based fuzz testing for JWT parsing *)

open Crowbar

(* Test that JWT parsing never crashes on arbitrary input *)
let test_parse_no_crash input =
  let _ = Jsonwt.parse input in
  ()

(* Test that JWT parsing in unsafe mode never crashes *)
let test_parse_unsafe_no_crash input =
  let _ = Jsonwt.parse_unsafe input in
  ()

(* Test that nested JWT parsing never crashes *)
let test_parse_nested_no_crash input =
  let _ = Jsonwt.parse_nested input in
  ()

(* Test header parsing never crashes *)
let test_header_parse_no_crash input =
  let _ = Jsonwt.Header.of_json input in
  ()

(* Test claims parsing never crashes *)
let test_claims_parse_no_crash input =
  let _ = Jsonwt.Claims.of_json input in
  ()

(* Test JWK parsing never crashes *)
let test_jwk_parse_no_crash input =
  let _ = Jsonwt.Jwk.of_json input in
  ()

(* Test algorithm parsing never crashes *)
let test_algorithm_parse_no_crash input =
  let _ = Jsonwt.Algorithm.of_string input in
  ()

(* Test base64url-like inputs (dots are JWT separators) *)
let test_jwt_structure input1 input2 input3 =
  let token = input1 ^ "." ^ input2 ^ "." ^ input3 in
  let _ = Jsonwt.parse token in
  ()

(* Test error printing never crashes *)
let () =
  let errors =
    [
      Jsonwt.Invalid_json "test";
      Jsonwt.Invalid_base64url "test";
      Jsonwt.Invalid_structure "test";
      Jsonwt.Invalid_header "test";
      Jsonwt.Invalid_claims "test";
      Jsonwt.Invalid_uri "test";
      Jsonwt.Duplicate_claim "test";
      Jsonwt.Unsupported_algorithm "test";
      Jsonwt.Algorithm_not_allowed "test";
      Jsonwt.Signature_mismatch;
      Jsonwt.Token_expired;
      Jsonwt.Token_not_yet_valid;
      Jsonwt.Invalid_issuer;
      Jsonwt.Invalid_audience;
      Jsonwt.Key_type_mismatch "test";
      Jsonwt.Unsecured_not_allowed;
      Jsonwt.Nesting_too_deep;
    ]
  in
  List.iter
    (fun e ->
      let _ = Format.asprintf "%a" Jsonwt.pp_error e in
      let _ = Jsonwt.error_to_string e in
      ())
    errors

let () =
  add_test ~name:"jwt: parse no crash" [ bytes ] test_parse_no_crash;
  add_test ~name:"jwt: parse_unsafe no crash" [ bytes ]
    test_parse_unsafe_no_crash;
  add_test ~name:"jwt: parse_nested no crash" [ bytes ]
    test_parse_nested_no_crash;
  add_test ~name:"jwt: header parse no crash" [ bytes ]
    test_header_parse_no_crash;
  add_test ~name:"jwt: claims parse no crash" [ bytes ]
    test_claims_parse_no_crash;
  add_test ~name:"jwt: jwk parse no crash" [ bytes ] test_jwk_parse_no_crash;
  add_test ~name:"jwt: algorithm parse no crash" [ bytes ]
    test_algorithm_parse_no_crash;
  add_test ~name:"jwt: structured input" [ bytes; bytes; bytes ]
    test_jwt_structure
