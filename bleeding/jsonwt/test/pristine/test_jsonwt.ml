(** JWT Library Tests

    Comprehensive tests derived from RFC 7519 (JSON Web Token) and RFC 7515
    (JSON Web Signature) specifications. *)

(* RFC 7515 Appendix A.1 symmetric key for HS256 *)
let rfc_hs256_key_b64 =
  "AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow"

(* RFC 7519 Section 3.1 example JWT (HS256) *)
let rfc_section3_1_token =
  "eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"

(* RFC 7519 Section 6.1 unsecured JWT *)
let rfc_section6_1_token =
  "eyJhbGciOiJub25lIn0.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ."

(* Helper to decode base64url to bytes *)
let b64url_decode s =
  (* Pad to multiple of 4 *)
  let pad =
    match String.length s mod 4 with
    | 0 -> ""
    | 2 -> "=="
    | 3 -> "="
    | _ -> failwith "invalid base64url"
  in
  (* Convert URL-safe chars to standard base64 *)
  let s = String.map (function '-' -> '+' | '_' -> '/' | c -> c) s in
  Base64.decode_exn (s ^ pad)

(* ============= Algorithm Tests ============= *)

let test_algorithm_roundtrip () =
  let open Jsonwt.Algorithm in
  let algs =
    [
      None; HS256; HS384; HS512; RS256; RS384; RS512; ES256; ES384; ES512; EdDSA;
    ]
  in
  List.iter
    (fun alg ->
      let s = to_string alg in
      match of_string s with
      | Ok alg' -> Alcotest.(check string) "roundtrip" s (to_string alg')
      | Error e -> Alcotest.fail (Jsonwt.error_to_string e))
    algs

let test_algorithm_unknown () =
  match Jsonwt.Algorithm.of_string "UNKNOWN" with
  | Error (Jsonwt.Unsupported_algorithm "UNKNOWN") -> ()
  | Error _ -> Alcotest.fail "Expected Unsupported_algorithm error"
  | Ok _ -> Alcotest.fail "Expected error for unknown algorithm"

(* ============= Header Tests ============= *)

let test_header_create () =
  let h = Jsonwt.Header.make ~typ:"JWT" Jsonwt.Algorithm.HS256 in
  Alcotest.(check (option string)) "typ" (Some "JWT") h.typ;
  Alcotest.(check bool) "alg" true (h.alg = Jsonwt.Algorithm.HS256)

let test_header_with_kid () =
  let h = Jsonwt.Header.make ~typ:"JWT" ~kid:"key-123" Jsonwt.Algorithm.RS256 in
  Alcotest.(check (option string)) "kid" (Some "key-123") h.kid;
  Alcotest.(check bool) "alg" true (h.alg = Jsonwt.Algorithm.RS256)

(* ============= Claims Tests ============= *)

let test_claims_builder () =
  let claims =
    Jsonwt.Claims.empty
    |> Jsonwt.Claims.set_iss "test-issuer"
    |> Jsonwt.Claims.set_sub "test-subject"
    |> Jsonwt.Claims.set_string "custom" "value"
    |> Jsonwt.Claims.build
  in
  Alcotest.(check (option string))
    "iss" (Some "test-issuer") (Jsonwt.Claims.iss claims);
  Alcotest.(check (option string))
    "sub" (Some "test-subject") (Jsonwt.Claims.sub claims);
  Alcotest.(check (option string))
    "custom" (Some "value")
    (Jsonwt.Claims.get_string "custom" claims)

let test_claims_with_timestamps () =
  let now = Ptime.of_float_s 1609459200. |> Option.get in
  (* 2021-01-01 00:00:00 UTC *)
  let exp = Ptime.of_float_s 1609545600. |> Option.get in
  (* 2021-01-02 00:00:00 UTC *)
  let claims =
    Jsonwt.Claims.empty |> Jsonwt.Claims.set_iat now
    |> Jsonwt.Claims.set_exp exp |> Jsonwt.Claims.set_nbf now
    |> Jsonwt.Claims.build
  in
  Alcotest.(check (option bool))
    "has exp" (Some true)
    (Option.map (fun _ -> true) (Jsonwt.Claims.exp claims));
  Alcotest.(check (option bool))
    "has iat" (Some true)
    (Option.map (fun _ -> true) (Jsonwt.Claims.iat claims));
  Alcotest.(check (option bool))
    "has nbf" (Some true)
    (Option.map (fun _ -> true) (Jsonwt.Claims.nbf claims))

let test_claims_audience_single () =
  let claims =
    Jsonwt.Claims.empty
    |> Jsonwt.Claims.set_aud [ "my-app" ]
    |> Jsonwt.Claims.build
  in
  Alcotest.(check (list string)) "aud" [ "my-app" ] (Jsonwt.Claims.aud claims)

let test_claims_audience_multiple () =
  let claims =
    Jsonwt.Claims.empty
    |> Jsonwt.Claims.set_aud [ "app1"; "app2"; "app3" ]
    |> Jsonwt.Claims.build
  in
  Alcotest.(check (list string))
    "aud" [ "app1"; "app2"; "app3" ] (Jsonwt.Claims.aud claims)

(* ============= Parse Tests ============= *)

let test_parse_invalid () =
  match Jsonwt.parse "not-a-jwt" with
  | Error (Jsonwt.Invalid_structure _) -> ()
  | Error _ -> Alcotest.fail "Expected Invalid_structure error"
  | Ok _ -> Alcotest.fail "Expected parse to fail"

let test_parse_malformed () =
  match Jsonwt.parse "a.b" with
  | Error (Jsonwt.Invalid_structure _) -> ()
  | Error _ -> Alcotest.fail "Expected Invalid_structure error"
  | Ok _ -> Alcotest.fail "Expected parse to fail with two parts"

let test_parse_invalid_base64 () =
  match Jsonwt.parse "!!!.@@@.###" with
  | Error (Jsonwt.Invalid_base64url _) -> ()
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Expected Invalid_base64url, got %s"
           (Jsonwt.error_to_string e))
  | Ok _ -> Alcotest.fail "Expected parse to fail with invalid base64"

(* ============= RFC 7519 Test Vectors ============= *)

(* RFC 7519 Section 6.1: Unsecured JWT *)
let test_rfc_unsecured_jwt_parse () =
  match Jsonwt.parse rfc_section6_1_token with
  | Ok jwt ->
      Alcotest.(check bool)
        "alg is none" true
        (jwt.header.alg = Jsonwt.Algorithm.None);
      Alcotest.(check (option string))
        "iss is joe" (Some "joe")
        (Jsonwt.Claims.iss jwt.claims);
      Alcotest.(check string) "signature is empty" "" jwt.signature
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Parse failed: %s" (Jsonwt.error_to_string e))

let test_rfc_unsecured_jwt_verify_rejected_by_default () =
  match Jsonwt.parse rfc_section6_1_token with
  | Ok jwt ->
      let key = Jsonwt.Jwk.symmetric "" in
      (* dummy key *)
      begin match Jsonwt.verify ~key jwt with
      | Error Jsonwt.Unsecured_not_allowed -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Expected Unsecured_not_allowed, got: %s"
               (Jsonwt.error_to_string e))
      | Ok () -> Alcotest.fail "Unsecured JWT should be rejected by default"
      end
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Parse failed: %s" (Jsonwt.error_to_string e))

let test_rfc_unsecured_jwt_verify_allowed_with_opt_in () =
  match Jsonwt.parse rfc_section6_1_token with
  | Ok jwt ->
      let key = Jsonwt.Jwk.symmetric "" in
      (* dummy key *)
      begin match Jsonwt.verify ~key ~allow_none:true jwt with
      | Ok () -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Verification failed: %s" (Jsonwt.error_to_string e))
      end
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Parse failed: %s" (Jsonwt.error_to_string e))

(* RFC 7519 Section 3.1: HS256 JWT *)
let test_rfc_hs256_jwt_parse () =
  match Jsonwt.parse rfc_section3_1_token with
  | Ok jwt ->
      Alcotest.(check bool)
        "alg is HS256" true
        (jwt.header.alg = Jsonwt.Algorithm.HS256);
      Alcotest.(check (option string)) "typ is JWT" (Some "JWT") jwt.header.typ;
      Alcotest.(check (option string))
        "iss is joe" (Some "joe")
        (Jsonwt.Claims.iss jwt.claims)
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Parse failed: %s" (Jsonwt.error_to_string e))

let test_rfc_hs256_jwt_verify () =
  match Jsonwt.parse rfc_section3_1_token with
  | Ok jwt ->
      let key_bytes = b64url_decode rfc_hs256_key_b64 in
      let key = Jsonwt.Jwk.symmetric key_bytes in
      begin match Jsonwt.verify ~key jwt with
      | Ok () -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Verification failed: %s" (Jsonwt.error_to_string e))
      end
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Parse failed: %s" (Jsonwt.error_to_string e))

let test_rfc_hs256_jwt_verify_wrong_key () =
  match Jsonwt.parse rfc_section3_1_token with
  | Ok jwt ->
      let wrong_key =
        Jsonwt.Jwk.symmetric "wrong-key-material-that-is-long-enough"
      in
      begin match Jsonwt.verify ~key:wrong_key jwt with
      | Error Jsonwt.Signature_mismatch -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Expected Signature_mismatch, got: %s"
               (Jsonwt.error_to_string e))
      | Ok () -> Alcotest.fail "Verification should fail with wrong key"
      end
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Parse failed: %s" (Jsonwt.error_to_string e))

(* ============= Claims Validation Tests ============= *)

let test_validate_expired_token () =
  let exp = Ptime.of_float_s 1300819380. |> Option.get in
  (* RFC example exp *)
  let now = Ptime.of_float_s 1400000000. |> Option.get in
  (* After exp *)
  let claims =
    Jsonwt.Claims.empty |> Jsonwt.Claims.set_exp exp |> Jsonwt.Claims.build
  in
  let header = Jsonwt.Header.make Jsonwt.Algorithm.None in
  let jwt = { Jsonwt.header; claims; signature = ""; raw = "" } in
  match Jsonwt.validate ~now jwt with
  | Error Jsonwt.Token_expired -> ()
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Expected Token_expired, got: %s"
           (Jsonwt.error_to_string e))
  | Ok () -> Alcotest.fail "Expected Token_expired error"

let test_validate_not_yet_valid_token () =
  let nbf = Ptime.of_float_s 1500000000. |> Option.get in
  let now = Ptime.of_float_s 1400000000. |> Option.get in
  (* Before nbf *)
  let claims =
    Jsonwt.Claims.empty |> Jsonwt.Claims.set_nbf nbf |> Jsonwt.Claims.build
  in
  let header = Jsonwt.Header.make Jsonwt.Algorithm.None in
  let jwt = { Jsonwt.header; claims; signature = ""; raw = "" } in
  match Jsonwt.validate ~now jwt with
  | Error Jsonwt.Token_not_yet_valid -> ()
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Expected Token_not_yet_valid, got: %s"
           (Jsonwt.error_to_string e))
  | Ok () -> Alcotest.fail "Expected Token_not_yet_valid error"

let test_validate_with_leeway () =
  let exp = Ptime.of_float_s 1300819380. |> Option.get in
  let now = Ptime.of_float_s 1300819390. |> Option.get in
  (* 10 seconds after exp *)
  let leeway = Ptime.Span.of_int_s 60 in
  (* 60 second leeway *)
  let claims =
    Jsonwt.Claims.empty |> Jsonwt.Claims.set_exp exp |> Jsonwt.Claims.build
  in
  let header = Jsonwt.Header.make Jsonwt.Algorithm.None in
  let jwt = { Jsonwt.header; claims; signature = ""; raw = "" } in
  match Jsonwt.validate ~now ~leeway jwt with
  | Ok () -> ()
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Expected validation to pass with leeway, got: %s"
           (Jsonwt.error_to_string e))

let test_validate_issuer_match () =
  let now = Ptime.of_float_s 1400000000. |> Option.get in
  let claims =
    Jsonwt.Claims.empty
    |> Jsonwt.Claims.set_iss "expected-issuer"
    |> Jsonwt.Claims.build
  in
  let header = Jsonwt.Header.make Jsonwt.Algorithm.None in
  let jwt = { Jsonwt.header; claims; signature = ""; raw = "" } in
  match Jsonwt.validate ~now ~iss:"expected-issuer" jwt with
  | Ok () -> ()
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Expected validation to pass, got: %s"
           (Jsonwt.error_to_string e))

let test_validate_issuer_mismatch () =
  let now = Ptime.of_float_s 1400000000. |> Option.get in
  let claims =
    Jsonwt.Claims.empty
    |> Jsonwt.Claims.set_iss "actual-issuer"
    |> Jsonwt.Claims.build
  in
  let header = Jsonwt.Header.make Jsonwt.Algorithm.None in
  let jwt = { Jsonwt.header; claims; signature = ""; raw = "" } in
  match Jsonwt.validate ~now ~iss:"expected-issuer" jwt with
  | Error Jsonwt.Invalid_issuer -> ()
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Expected Invalid_issuer, got: %s"
           (Jsonwt.error_to_string e))
  | Ok () -> Alcotest.fail "Expected Invalid_issuer error"

let test_validate_audience_match () =
  let now = Ptime.of_float_s 1400000000. |> Option.get in
  let claims =
    Jsonwt.Claims.empty
    |> Jsonwt.Claims.set_aud [ "app1"; "app2"; "my-app" ]
    |> Jsonwt.Claims.build
  in
  let header = Jsonwt.Header.make Jsonwt.Algorithm.None in
  let jwt = { Jsonwt.header; claims; signature = ""; raw = "" } in
  match Jsonwt.validate ~now ~aud:"my-app" jwt with
  | Ok () -> ()
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Expected validation to pass, got: %s"
           (Jsonwt.error_to_string e))

let test_validate_audience_mismatch () =
  let now = Ptime.of_float_s 1400000000. |> Option.get in
  let claims =
    Jsonwt.Claims.empty
    |> Jsonwt.Claims.set_aud [ "app1"; "app2" ]
    |> Jsonwt.Claims.build
  in
  let header = Jsonwt.Header.make Jsonwt.Algorithm.None in
  let jwt = { Jsonwt.header; claims; signature = ""; raw = "" } in
  match Jsonwt.validate ~now ~aud:"my-app" jwt with
  | Error Jsonwt.Invalid_audience -> ()
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Expected Invalid_audience, got: %s"
           (Jsonwt.error_to_string e))
  | Ok () -> Alcotest.fail "Expected Invalid_audience error"

(* ============= Algorithm Restriction Tests ============= *)

let test_algorithm_not_allowed () =
  match Jsonwt.parse rfc_section3_1_token with
  | Ok jwt ->
      let key_bytes = b64url_decode rfc_hs256_key_b64 in
      let key = Jsonwt.Jwk.symmetric key_bytes in
      (* Only allow HS384 and HS512, not HS256 *)
      let allowed_algs = [ Jsonwt.Algorithm.HS384; Jsonwt.Algorithm.HS512 ] in
      begin match Jsonwt.verify ~key ~allowed_algs jwt with
      | Error (Jsonwt.Algorithm_not_allowed "HS256") -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Expected Algorithm_not_allowed, got: %s"
               (Jsonwt.error_to_string e))
      | Ok () ->
          Alcotest.fail "Verification should fail when algorithm is not allowed"
      end
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Parse failed: %s" (Jsonwt.error_to_string e))

(* ============= Helper Function Tests ============= *)

let test_is_expired () =
  let exp = Ptime.of_float_s 1300819380. |> Option.get in
  let claims =
    Jsonwt.Claims.empty |> Jsonwt.Claims.set_exp exp |> Jsonwt.Claims.build
  in
  let header = Jsonwt.Header.make Jsonwt.Algorithm.None in
  let jwt = { Jsonwt.header; claims; signature = ""; raw = "" } in
  let now_before = Ptime.of_float_s 1300819370. |> Option.get in
  let now_after = Ptime.of_float_s 1300819390. |> Option.get in
  Alcotest.(check bool)
    "not expired before" false
    (Jsonwt.is_expired ~now:now_before jwt);
  Alcotest.(check bool)
    "expired after" true
    (Jsonwt.is_expired ~now:now_after jwt)

let test_time_to_expiry () =
  let exp = Ptime.of_float_s 1300819380. |> Option.get in
  let claims =
    Jsonwt.Claims.empty |> Jsonwt.Claims.set_exp exp |> Jsonwt.Claims.build
  in
  let header = Jsonwt.Header.make Jsonwt.Algorithm.None in
  let jwt = { Jsonwt.header; claims; signature = ""; raw = "" } in
  let now = Ptime.of_float_s 1300819370. |> Option.get in
  match Jsonwt.time_to_expiry ~now jwt with
  | Some span ->
      let seconds = Ptime.Span.to_float_s span |> int_of_float in
      Alcotest.(check int) "time to expiry" 10 seconds
  | None -> Alcotest.fail "Expected Some time to expiry"

let test_time_to_expiry_already_expired () =
  let exp = Ptime.of_float_s 1300819380. |> Option.get in
  let claims =
    Jsonwt.Claims.empty |> Jsonwt.Claims.set_exp exp |> Jsonwt.Claims.build
  in
  let header = Jsonwt.Header.make Jsonwt.Algorithm.None in
  let jwt = { Jsonwt.header; claims; signature = ""; raw = "" } in
  let now = Ptime.of_float_s 1300819390. |> Option.get in
  match Jsonwt.time_to_expiry ~now jwt with
  | None -> ()
  | Some _ -> Alcotest.fail "Expected None for expired token"

(* ============= Error Type Tests ============= *)

let test_error_to_string () =
  let errors =
    [
      (Jsonwt.Invalid_json "test", "Invalid JSON: test");
      (Jsonwt.Invalid_base64url "test", "Invalid base64url: test");
      (Jsonwt.Invalid_structure "test", "Invalid structure: test");
      (Jsonwt.Token_expired, "Token expired");
      (Jsonwt.Token_not_yet_valid, "Token not yet valid");
      (Jsonwt.Signature_mismatch, "Signature mismatch");
    ]
  in
  List.iter
    (fun (err, expected) ->
      let actual = Jsonwt.error_to_string err in
      Alcotest.(check string) "error string" expected actual)
    errors

(* ============= JWK Tests ============= *)

let test_jwk_symmetric () =
  (* Just verify that creating a symmetric key doesn't crash *)
  let _key = Jsonwt.Jwk.symmetric "my-secret-key" in
  ()

(* ============= Test Runner ============= *)

let () =
  Alcotest.run "Jsonwt"
    [
      ( "Algorithm",
        [
          Alcotest.test_case "roundtrip" `Quick test_algorithm_roundtrip;
          Alcotest.test_case "unknown" `Quick test_algorithm_unknown;
        ] );
      ( "Header",
        [
          Alcotest.test_case "create" `Quick test_header_create;
          Alcotest.test_case "with_kid" `Quick test_header_with_kid;
        ] );
      ( "Claims",
        [
          Alcotest.test_case "builder" `Quick test_claims_builder;
          Alcotest.test_case "timestamps" `Quick test_claims_with_timestamps;
          Alcotest.test_case "audience_single" `Quick
            test_claims_audience_single;
          Alcotest.test_case "audience_multiple" `Quick
            test_claims_audience_multiple;
        ] );
      ( "Parse",
        [
          Alcotest.test_case "invalid" `Quick test_parse_invalid;
          Alcotest.test_case "malformed" `Quick test_parse_malformed;
          Alcotest.test_case "invalid_base64" `Quick test_parse_invalid_base64;
        ] );
      ( "RFC 7519 Section 6.1 - Unsecured JWT",
        [
          Alcotest.test_case "parse" `Quick test_rfc_unsecured_jwt_parse;
          Alcotest.test_case "rejected_by_default" `Quick
            test_rfc_unsecured_jwt_verify_rejected_by_default;
          Alcotest.test_case "allowed_with_opt_in" `Quick
            test_rfc_unsecured_jwt_verify_allowed_with_opt_in;
        ] );
      ( "RFC 7519 Section 3.1 - HS256 JWT",
        [
          Alcotest.test_case "parse" `Quick test_rfc_hs256_jwt_parse;
          Alcotest.test_case "verify" `Quick test_rfc_hs256_jwt_verify;
          Alcotest.test_case "verify_wrong_key" `Quick
            test_rfc_hs256_jwt_verify_wrong_key;
        ] );
      ( "Claims Validation",
        [
          Alcotest.test_case "expired" `Quick test_validate_expired_token;
          Alcotest.test_case "not_yet_valid" `Quick
            test_validate_not_yet_valid_token;
          Alcotest.test_case "with_leeway" `Quick test_validate_with_leeway;
          Alcotest.test_case "issuer_match" `Quick test_validate_issuer_match;
          Alcotest.test_case "issuer_mismatch" `Quick
            test_validate_issuer_mismatch;
          Alcotest.test_case "audience_match" `Quick
            test_validate_audience_match;
          Alcotest.test_case "audience_mismatch" `Quick
            test_validate_audience_mismatch;
        ] );
      ( "Algorithm Restrictions",
        [ Alcotest.test_case "not_allowed" `Quick test_algorithm_not_allowed ]
      );
      ( "Helper Functions",
        [
          Alcotest.test_case "is_expired" `Quick test_is_expired;
          Alcotest.test_case "time_to_expiry" `Quick test_time_to_expiry;
          Alcotest.test_case "time_to_expiry_expired" `Quick
            test_time_to_expiry_already_expired;
        ] );
      ( "Error Types",
        [ Alcotest.test_case "to_string" `Quick test_error_to_string ] );
      ("JWK", [ Alcotest.test_case "symmetric" `Quick test_jwk_symmetric ]);
    ]
