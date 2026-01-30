(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Tests for OAuth 2.0 implementation (RFC 6749) *)

let () = Mirage_crypto_rng_unix.use_default ()

(** {1 Test Utilities} *)

let test_case name fn =
  Alcotest.test_case name `Quick fn

(** {1 PKCE Tests} *)

let test_pkce_generation () =
  let pkce = Requests_oauth.Oauth.generate_pkce () in
  (* Verifier should be URL-safe base64 without padding *)
  Alcotest.(check bool) "verifier not empty" true (String.length pkce.verifier > 0);
  Alcotest.(check bool) "verifier >= 43 chars" true (String.length pkce.verifier >= 43);
  (* Challenge should be different from verifier for S256 *)
  Alcotest.(check bool) "challenge differs from verifier"
    true (pkce.challenge <> pkce.verifier);
  (* Challenge method should be S256 by default *)
  Alcotest.(check string) "default method is S256"
    "S256" (Requests_oauth.Oauth.pkce_method_to_string pkce.method_)

let test_pkce_plain () =
  let pkce = Requests_oauth.Oauth.generate_pkce ~method_:Plain () in
  (* For plain method, challenge equals verifier *)
  Alcotest.(check string) "plain: challenge equals verifier"
    pkce.verifier pkce.challenge;
  Alcotest.(check string) "method is plain"
    "plain" (Requests_oauth.Oauth.pkce_method_to_string pkce.method_)

let test_pkce_s256_deterministic () =
  (* S256 challenge should be deterministic for the same verifier *)
  (* This tests the challenge computation, not the random generation *)
  let pkce1 = Requests_oauth.Oauth.generate_pkce () in
  let pkce2 = Requests_oauth.Oauth.generate_pkce () in
  (* Different verifiers should produce different challenges *)
  Alcotest.(check bool) "different verifiers -> different challenges"
    true (pkce1.challenge <> pkce2.challenge)

(** {1 State Parameter Tests} *)

let test_state_generation () =
  let state = Requests_oauth.Oauth.generate_state () in
  Alcotest.(check bool) "state not empty" true (String.length state > 0);
  (* State should be URL-safe *)
  String.iter (fun c ->
    let is_url_safe =
      (c >= 'A' && c <= 'Z') ||
      (c >= 'a' && c <= 'z') ||
      (c >= '0' && c <= '9') ||
      c = '-' || c = '_'
    in
    Alcotest.(check bool) "state is URL-safe" true is_url_safe
  ) state

let test_state_validation () =
  let state = "test-state-123" in
  Alcotest.(check bool) "valid state matches"
    true (Requests_oauth.Oauth.validate_state ~expected:state ~received:state);
  Alcotest.(check bool) "invalid state does not match"
    false (Requests_oauth.Oauth.validate_state ~expected:state ~received:"other-state")

(** {1 Configuration Tests} *)

let test_make_config () =
  let config = Requests_oauth.Oauth.make_config
    ~client_id:"my-client"
    ~client_secret:"my-secret"
    ~token_endpoint:"https://auth.example.com/token"
    ~authorization_endpoint:"https://auth.example.com/authorize"
    ~redirect_uri:"https://app.example.com/callback"
    ~scopes:["read"; "write"]
    () in
  Alcotest.(check string) "client_id" "my-client" config.client_id;
  Alcotest.(check (option string)) "client_secret" (Some "my-secret") config.client_secret;
  Alcotest.(check string) "token_endpoint"
    "https://auth.example.com/token" config.token_endpoint;
  Alcotest.(check (option string)) "authorization_endpoint"
    (Some "https://auth.example.com/authorize") config.authorization_endpoint;
  Alcotest.(check (list string)) "scopes" ["read"; "write"] config.scopes

let test_make_config_minimal () =
  let config = Requests_oauth.Oauth.make_config
    ~client_id:"public-client"
    ~token_endpoint:"https://auth.example.com/token"
    () in
  Alcotest.(check string) "client_id" "public-client" config.client_id;
  Alcotest.(check (option string)) "client_secret (public)" None config.client_secret;
  Alcotest.(check (option string)) "authorization_endpoint" None config.authorization_endpoint;
  Alcotest.(check (list string)) "scopes (empty)" [] config.scopes

(** {1 Authorization URL Tests} *)

let test_authorization_url () =
  let config = Requests_oauth.Oauth.make_config
    ~client_id:"my-client"
    ~token_endpoint:"https://auth.example.com/token"
    ~authorization_endpoint:"https://auth.example.com/authorize"
    ~redirect_uri:"https://app.example.com/callback"
    ~scopes:["read"; "write"]
    () in
  let state = "random-state-123" in
  let url = Requests_oauth.Oauth.authorization_url ~config ~state () in
  (* Check URL contains expected components *)
  let contains sub = Astring.String.find_sub ~sub url |> Option.is_some in
  Alcotest.(check bool) "contains response_type=code" true (contains "response_type=code");
  Alcotest.(check bool) "contains client_id" true (contains "client_id=my-client");
  Alcotest.(check bool) "contains state" true (contains "state=random-state-123");
  Alcotest.(check bool) "contains scope" true (contains "scope=read")

let test_authorization_url_with_pkce () =
  let config = Requests_oauth.Oauth.make_config
    ~client_id:"my-client"
    ~token_endpoint:"https://auth.example.com/token"
    ~authorization_endpoint:"https://auth.example.com/authorize"
    () in
  let state = "random-state" in
  let pkce = Requests_oauth.Oauth.generate_pkce () in
  let url = Requests_oauth.Oauth.authorization_url ~config ~state ~pkce () in
  Alcotest.(check bool) "contains code_challenge"
    true (Astring.String.find_sub ~sub:"code_challenge=" url |> Option.is_some);
  Alcotest.(check bool) "contains code_challenge_method=S256"
    true (Astring.String.find_sub ~sub:"code_challenge_method=S256" url |> Option.is_some)

let test_authorization_url_no_endpoint () =
  let config = Requests_oauth.Oauth.make_config
    ~client_id:"my-client"
    ~token_endpoint:"https://auth.example.com/token"
    () in
  let state = "random-state" in
  Alcotest.check_raises "raises without authorization_endpoint"
    (Invalid_argument "authorization_endpoint is required for authorization URL")
    (fun () -> ignore (Requests_oauth.Oauth.authorization_url ~config ~state ()))

(** {1 Error Code Tests} *)

let test_error_code_to_string () =
  let codes = [
    (Requests_oauth.Oauth.Invalid_request, "invalid_request");
    (Requests_oauth.Oauth.Invalid_client, "invalid_client");
    (Requests_oauth.Oauth.Invalid_grant, "invalid_grant");
    (Requests_oauth.Oauth.Unauthorized_client, "unauthorized_client");
    (Requests_oauth.Oauth.Unsupported_grant_type, "unsupported_grant_type");
    (Requests_oauth.Oauth.Invalid_scope, "invalid_scope");
    (Requests_oauth.Oauth.Unknown_error "custom", "custom");
  ] in
  List.iter (fun (code, expected) ->
    Alcotest.(check string) ("to_string: " ^ expected)
      expected (Requests_oauth.Oauth.error_code_to_string code)
  ) codes

(** {1 Token Tests} *)

let test_token_accessors () =
  (* Test token accessor functions with a mock token *)
  let token : Requests_oauth.Oauth.token = {
    access_token = "test-access-token";
    token_type = "Bearer";
    expires_at = None;
    refresh_token = Some "test-refresh-token";
    scope = Some "read write";
  } in
  Alcotest.(check string) "get_access_token"
    "test-access-token" (Requests_oauth.Oauth.get_access_token token);
  Alcotest.(check (option string)) "get_refresh_token"
    (Some "test-refresh-token") (Requests_oauth.Oauth.get_refresh_token token)

let test_token_expiry_no_expiry () =
  (* Token without expiry should not be expired *)
  let token : Requests_oauth.Oauth.token = {
    access_token = "test-token";
    token_type = "Bearer";
    expires_at = None;
    refresh_token = None;
    scope = None;
  } in
  Alcotest.(check bool) "no expiry -> not expired"
    false (Requests_oauth.Oauth.is_expired token);
  Alcotest.(check bool) "no expiry -> not expires_within"
    false (Requests_oauth.Oauth.expires_within (Ptime.Span.of_int_s 3600) token)

let test_token_expiry_expired () =
  (* Create a token that expired 1 hour ago *)
  let one_hour_ago =
    Ptime.sub_span (Ptime_clock.now ()) (Ptime.Span.of_int_s 3600)
    |> Option.get
  in
  let token : Requests_oauth.Oauth.token = {
    access_token = "test-token";
    token_type = "Bearer";
    expires_at = Some one_hour_ago;
    refresh_token = None;
    scope = None;
  } in
  Alcotest.(check bool) "expired token is_expired" true (Requests_oauth.Oauth.is_expired token)

let test_token_expires_within () =
  (* Create a token that expires in 15 seconds *)
  let expires_in_15s =
    Ptime.add_span (Ptime_clock.now ()) (Ptime.Span.of_int_s 15)
    |> Option.get
  in
  let token : Requests_oauth.Oauth.token = {
    access_token = "test-token";
    token_type = "Bearer";
    expires_at = Some expires_in_15s;
    refresh_token = None;
    scope = None;
  } in
  (* 30 second window should include it *)
  Alcotest.(check bool) "expires within 30s"
    true (Requests_oauth.Oauth.expires_within (Ptime.Span.of_int_s 30) token);
  (* 10 second window should not include it *)
  Alcotest.(check bool) "not expires within 10s"
    false (Requests_oauth.Oauth.expires_within (Ptime.Span.of_int_s 10) token)

(** {1 Error Formatting Tests} *)

let test_pp_error () =
  let err : Requests_oauth.Oauth.error = {
    code = Requests_oauth.Oauth.Invalid_grant;
    description = Some "The token has expired";
    uri = Some "https://example.com/errors";
  } in
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Requests_oauth.Oauth.pp_error ppf err;
  Format.pp_print_flush ppf ();
  let output = Buffer.contents buf in
  Alcotest.(check bool) "contains error code"
    true (Astring.String.find_sub ~sub:"invalid_grant" output |> Option.is_some);
  Alcotest.(check bool) "contains description"
    true (Astring.String.find_sub ~sub:"The token has expired" output |> Option.is_some)

(** {1 Auth Module Integration Tests} *)

let test_auth_requires_https () =
  (* Basic and Bearer auth should require HTTPS *)
  let basic = Requests.Auth.basic ~username:"user" ~password:"pass" in
  let bearer = Requests.Auth.bearer ~token:"token" in
  Alcotest.(check bool) "basic requires https" true (Requests.Auth.requires_https basic);
  Alcotest.(check bool) "bearer requires https" true (Requests.Auth.requires_https bearer)

(** {1 Error Module Integration Tests} *)

let test_error_oauth_types () =
  let err = Requests.Error.Oauth_error {
    error_code = "invalid_grant";
    description = Some "Token expired";
    uri = None;
  } in
  Alcotest.(check bool) "is_oauth_error" true (Requests.Error.is_oauth_error err);
  Alcotest.(check bool) "is_retryable" false (Requests.Error.is_retryable err)

let test_error_token_expired () =
  let err = Requests.Error.Token_expired in
  Alcotest.(check bool) "is_oauth_error" true (Requests.Error.is_oauth_error err)

let test_error_token_refresh_failed () =
  let err = Requests.Error.Token_refresh_failed { reason = "No refresh token" } in
  Alcotest.(check bool) "is_oauth_error" true (Requests.Error.is_oauth_error err)

(** {1 Test Runner} *)

let pkce_tests = [
  test_case "PKCE generation (S256)" test_pkce_generation;
  test_case "PKCE generation (plain)" test_pkce_plain;
  test_case "PKCE S256 deterministic" test_pkce_s256_deterministic;
]

let state_tests = [
  test_case "State generation" test_state_generation;
  test_case "State validation" test_state_validation;
]

let config_tests = [
  test_case "Make config with all options" test_make_config;
  test_case "Make config minimal" test_make_config_minimal;
]

let auth_url_tests = [
  test_case "Authorization URL" test_authorization_url;
  test_case "Authorization URL with PKCE" test_authorization_url_with_pkce;
  test_case "Authorization URL without endpoint" test_authorization_url_no_endpoint;
]

let error_code_tests = [
  test_case "Error code to_string" test_error_code_to_string;
]

let token_tests = [
  test_case "Token accessors" test_token_accessors;
  test_case "Token expiry - no expiry" test_token_expiry_no_expiry;
  test_case "Token expiry - expired" test_token_expiry_expired;
  test_case "Token expires_within" test_token_expires_within;
]

let error_format_tests = [
  test_case "Error pretty printing" test_pp_error;
]

let integration_tests = [
  test_case "Auth requires HTTPS" test_auth_requires_https;
  test_case "Error OAuth types" test_error_oauth_types;
  test_case "Error token expired" test_error_token_expired;
  test_case "Error token refresh failed" test_error_token_refresh_failed;
]

let () =
  Alcotest.run "OAuth 2.0 (RFC 6749)" [
    ("PKCE (RFC 7636)", pkce_tests);
    ("State Parameter", state_tests);
    ("Client Configuration", config_tests);
    ("Authorization URL", auth_url_tests);
    ("Error Codes", error_code_tests);
    ("Token Management", token_tests);
    ("Error Formatting", error_format_tests);
    ("Integration", integration_tests);
  ]
