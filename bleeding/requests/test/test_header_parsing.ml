(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Tests for Header_parsing module *)

module Header_parsing = Requests.Header_parsing
module Headers = Requests.Headers
module Method = Requests.Method

(** {1 Content-Range Tests (RFC 9110 Section 14.4)} *)

let test_content_range_basic () =
  let result = Header_parsing.parse_content_range "bytes 0-499/1234" in
  match result with
  | Some cr ->
      Alcotest.(check string) "unit" "bytes" cr.unit;
      Alcotest.(check (option (pair int64 int64))) "range"
        (Some (0L, 499L)) cr.range;
      Alcotest.(check (option int64)) "complete_length"
        (Some 1234L) cr.complete_length
  | None ->
      Alcotest.fail "Expected Some content_range"

let test_content_range_unsatisfied () =
  let result = Header_parsing.parse_content_range "bytes */1234" in
  match result with
  | Some cr ->
      Alcotest.(check string) "unit" "bytes" cr.unit;
      Alcotest.(check (option (pair int64 int64))) "range" None cr.range;
      Alcotest.(check (option int64)) "complete_length"
        (Some 1234L) cr.complete_length
  | None ->
      Alcotest.fail "Expected Some content_range for unsatisfied"

let test_content_range_unknown_length () =
  let result = Header_parsing.parse_content_range "bytes 0-499/*" in
  match result with
  | Some cr ->
      Alcotest.(check string) "unit" "bytes" cr.unit;
      Alcotest.(check (option (pair int64 int64))) "range"
        (Some (0L, 499L)) cr.range;
      Alcotest.(check (option int64)) "complete_length" None cr.complete_length
  | None ->
      Alcotest.fail "Expected Some content_range for unknown length"

let test_content_range_to_string () =
  let cr = Header_parsing.make_content_range ~start:0L ~end_:499L ~complete_length:1234L in
  let s = Header_parsing.content_range_to_string cr in
  Alcotest.(check string) "to_string" "bytes 0-499/1234" s

let test_content_range_unsatisfied_to_string () =
  let cr = Header_parsing.make_unsatisfied_range ~complete_length:1234L in
  let s = Header_parsing.content_range_to_string cr in
  Alcotest.(check string) "unsatisfied to_string" "bytes */1234" s

let test_content_range_invalid () =
  let invalid_inputs = [
    "";
    "invalid";
    "0-499/1234";  (* missing unit *)
    "bytes 0-499";  (* missing length *)
  ] in
  List.iter (fun input ->
    let result = Header_parsing.parse_content_range input in
    Alcotest.(check bool) (Printf.sprintf "Invalid: %S" input)
      true (Option.is_none result)
  ) invalid_inputs

(** {1 If-Range Tests (RFC 9110 Section 13.1.5)} *)

let test_if_range_etag_strong () =
  let result = Header_parsing.parse_if_range "\"abc123\"" in
  match result with
  | Some (If_range_etag etag) ->
      Alcotest.(check string) "strong etag" "\"abc123\"" etag
  | _ ->
      Alcotest.fail "Expected If_range_etag for strong etag"

let test_if_range_etag_weak () =
  let result = Header_parsing.parse_if_range "W/\"abc123\"" in
  match result with
  | Some (If_range_etag etag) ->
      Alcotest.(check string) "weak etag" "W/\"abc123\"" etag
  | _ ->
      Alcotest.fail "Expected If_range_etag for weak etag"

let test_if_range_date () =
  let result = Header_parsing.parse_if_range "Sun, 06 Nov 1994 08:49:37 GMT" in
  match result with
  | Some (If_range_date date) ->
      Alcotest.(check string) "date" "Sun, 06 Nov 1994 08:49:37 GMT" date
  | _ ->
      Alcotest.fail "Expected If_range_date"

let test_if_range_is_etag () =
  let etag = Header_parsing.if_range_of_etag "\"test\"" in
  let date = Header_parsing.if_range_of_date "Sun, 06 Nov 1994 08:49:37 GMT" in
  Alcotest.(check bool) "etag is_etag" true (Header_parsing.if_range_is_etag etag);
  Alcotest.(check bool) "date is_etag" false (Header_parsing.if_range_is_etag date);
  Alcotest.(check bool) "etag is_date" false (Header_parsing.if_range_is_date etag);
  Alcotest.(check bool) "date is_date" true (Header_parsing.if_range_is_date date)

(** {1 Allow Tests (RFC 9110 Section 10.2.1)} *)

let test_allow_parse_basic () =
  let methods = Header_parsing.parse_allow "GET, HEAD, PUT" in
  Alcotest.(check int) "count" 3 (List.length methods);
  Alcotest.(check bool) "has GET" true (List.mem `GET methods);
  Alcotest.(check bool) "has HEAD" true (List.mem `HEAD methods);
  Alcotest.(check bool) "has PUT" true (List.mem `PUT methods)

let test_allow_parse_single () =
  let methods = Header_parsing.parse_allow "OPTIONS" in
  Alcotest.(check int) "count" 1 (List.length methods);
  Alcotest.(check bool) "has OPTIONS" true (List.mem `OPTIONS methods)

let test_allow_parse_with_spaces () =
  let methods = Header_parsing.parse_allow "GET,  HEAD,   POST" in
  Alcotest.(check int) "count" 3 (List.length methods);
  Alcotest.(check bool) "has GET" true (List.mem `GET methods);
  Alcotest.(check bool) "has HEAD" true (List.mem `HEAD methods);
  Alcotest.(check bool) "has POST" true (List.mem `POST methods)

let test_allow_to_string () =
  let methods = [`GET; `HEAD; `PUT] in
  let s = Header_parsing.allow_to_string methods in
  Alcotest.(check string) "to_string" "GET, HEAD, PUT" s

let test_allow_contains () =
  let allow = "GET, HEAD, PUT" in
  Alcotest.(check bool) "contains GET" true
    (Header_parsing.allow_contains `GET allow);
  Alcotest.(check bool) "contains POST" false
    (Header_parsing.allow_contains `POST allow)

(** {1 Authentication-Info Tests (RFC 9110 Section 11.6.3)} *)

let test_auth_info_full () =
  let info = Header_parsing.parse_authentication_info
    "nextnonce=\"abc123\", qop=auth, rspauth=\"xyz789\", cnonce=\"client\", nc=00000001" in
  Alcotest.(check (option string)) "nextnonce" (Some "abc123") info.nextnonce;
  Alcotest.(check (option string)) "qop" (Some "auth") info.qop;
  Alcotest.(check (option string)) "rspauth" (Some "xyz789") info.rspauth;
  Alcotest.(check (option string)) "cnonce" (Some "client") info.cnonce;
  Alcotest.(check (option string)) "nc" (Some "00000001") info.nc

let test_auth_info_nextnonce_only () =
  let info = Header_parsing.parse_authentication_info "nextnonce=\"newone\"" in
  Alcotest.(check (option string)) "nextnonce" (Some "newone") info.nextnonce;
  Alcotest.(check bool) "has_nextnonce" true (Header_parsing.has_nextnonce info);
  Alcotest.(check (option string)) "qop" None info.qop

let test_auth_info_empty () =
  let info = Header_parsing.parse_authentication_info "" in
  Alcotest.(check (option string)) "nextnonce" None info.nextnonce;
  Alcotest.(check bool) "has_nextnonce" false (Header_parsing.has_nextnonce info)

(** {1 Retry-After Tests (RFC 9110 Section 10.2.3)} *)

let test_retry_after_seconds () =
  let result = Header_parsing.parse_retry_after "120" in
  match result with
  | Some (Retry_after_seconds s) ->
      Alcotest.(check int) "seconds" 120 s
  | _ ->
      Alcotest.fail "Expected Retry_after_seconds"

let test_retry_after_date () =
  let result = Header_parsing.parse_retry_after "Fri, 31 Dec 1999 23:59:59 GMT" in
  match result with
  | Some (Retry_after_date d) ->
      Alcotest.(check string) "date" "Fri, 31 Dec 1999 23:59:59 GMT" d
  | _ ->
      Alcotest.fail "Expected Retry_after_date"

let test_retry_after_to_seconds () =
  let ra = Header_parsing.Retry_after_seconds 60 in
  Alcotest.(check (option int)) "to_seconds" (Some 60)
    (Header_parsing.retry_after_to_seconds ra)

let test_retry_after_date_to_seconds () =
  (* Test with known date: Sun, 06 Nov 1994 08:49:37 GMT *)
  (* Unix timestamp for that date is 784111777 *)
  let ra = Header_parsing.Retry_after_date "Sun, 06 Nov 1994 08:49:37 GMT" in
  (* Use a "now" of 784111700, so we expect 77 seconds *)
  let now = 784111700.0 in
  let result = Header_parsing.retry_after_to_seconds ~now ra in
  Alcotest.(check (option int)) "future date" (Some 77) result

let test_retry_after_past_date_to_seconds () =
  (* Test past date: returns 0 when date is in the past *)
  (* Sun, 06 Nov 1994 08:49:37 GMT has timestamp 784111777 *)
  let ra = Header_parsing.Retry_after_date "Sun, 06 Nov 1994 08:49:37 GMT" in
  let now = 900000000.0 in  (* A timestamp after 1994 *)
  let result = Header_parsing.retry_after_to_seconds ~now ra in
  Alcotest.(check (option int)) "past date returns 0" (Some 0) result

let test_retry_after_date_no_now () =
  (* Without now parameter, returns None for date values *)
  let ra = Header_parsing.Retry_after_date "Sun, 06 Nov 2033 08:49:37 GMT" in
  let result = Header_parsing.retry_after_to_seconds ra in
  Alcotest.(check (option int)) "no now returns None" None result

(** {1 Accept-Ranges Tests (RFC 9110 Section 14.3)} *)

let test_accept_ranges_bytes () =
  let ar = Header_parsing.parse_accept_ranges "bytes" in
  Alcotest.(check bool) "supports bytes" true
    (Header_parsing.supports_byte_ranges ar)

let test_accept_ranges_none () =
  let ar = Header_parsing.parse_accept_ranges "none" in
  Alcotest.(check bool) "supports none" false
    (Header_parsing.supports_byte_ranges ar)

let test_accept_ranges_case_insensitive () =
  let ar1 = Header_parsing.parse_accept_ranges "BYTES" in
  let ar2 = Header_parsing.parse_accept_ranges "Bytes" in
  Alcotest.(check bool) "BYTES supports" true
    (Header_parsing.supports_byte_ranges ar1);
  Alcotest.(check bool) "Bytes supports" true
    (Header_parsing.supports_byte_ranges ar2)

(** {1 Cache-Status Tests (RFC 9211)} *)

let test_cache_status_basic () =
  let entries = Header_parsing.parse_cache_status "Cloudflare; hit" in
  Alcotest.(check int) "count" 1 (List.length entries);
  let entry = List.hd entries in
  Alcotest.(check string) "cache_id" "Cloudflare" entry.cache_id;
  Alcotest.(check (option bool)) "hit" (Some true) entry.hit

let test_cache_status_fwd () =
  let entries = Header_parsing.parse_cache_status "CDN; fwd=uri-miss; stored" in
  Alcotest.(check int) "count" 1 (List.length entries);
  let entry = List.hd entries in
  Alcotest.(check string) "cache_id" "CDN" entry.cache_id;
  Alcotest.(check (option bool)) "stored" (Some true) entry.stored;
  match entry.fwd with
  | Some Header_parsing.Fwd_uri_miss -> Alcotest.(check pass) "fwd" () ()
  | _ -> Alcotest.fail "Expected Fwd_uri_miss"

let test_cache_status_multiple () =
  let entries = Header_parsing.parse_cache_status "Origin, CDN; hit, Edge; fwd=stale" in
  Alcotest.(check int) "count" 3 (List.length entries);
  Alcotest.(check bool) "is_hit" true (Header_parsing.cache_status_is_hit entries)

let test_cache_status_with_params () =
  let entries = Header_parsing.parse_cache_status "CDN; ttl=3600; fwd-status=200" in
  Alcotest.(check int) "count" 1 (List.length entries);
  let entry = List.hd entries in
  Alcotest.(check (option int)) "ttl" (Some 3600) entry.ttl;
  Alcotest.(check (option int)) "fwd_status" (Some 200) entry.fwd_status

let test_cache_status_to_string () =
  let entry = {
    Header_parsing.cache_id = "CDN";
    hit = Some true;
    fwd = None;
    fwd_status = None;
    stored = None;
    collapsed = None;
    ttl = Some 3600;
    key = None;
    detail = None;
  } in
  let s = Header_parsing.cache_status_entry_to_string entry in
  Alcotest.(check bool) "contains hit" true (String.length s > 0)

(** {1 Content-Digest Tests (RFC 9530)} *)

let test_digest_parse_sha256 () =
  let digests = Header_parsing.parse_digest_header "sha-256=:YWJjZGVm:" in
  Alcotest.(check int) "count" 1 (List.length digests);
  let dv = List.hd digests in
  Alcotest.(check bool) "is sha256" true (dv.algorithm = Header_parsing.Sha256);
  Alcotest.(check string) "digest" "YWJjZGVm" dv.digest

let test_digest_parse_multiple () =
  let digests = Header_parsing.parse_digest_header "sha-256=:abc:, sha-512=:xyz:" in
  Alcotest.(check int) "count" 2 (List.length digests)

let test_digest_compute_sha256 () =
  let content = "hello world" in
  let digest = Header_parsing.compute_sha256 content in
  (* SHA-256 of "hello world" is known *)
  Alcotest.(check bool) "not empty" true (String.length digest > 0)

let test_digest_compute_sha512 () =
  let content = "hello world" in
  let digest = Header_parsing.compute_sha512 content in
  Alcotest.(check bool) "not empty" true (String.length digest > 0)

let test_digest_validate () =
  let content = "test content" in
  let dv = Header_parsing.make_content_digest content in
  let valid = Header_parsing.validate_digest ~digests:[dv] content in
  Alcotest.(check bool) "validates" true valid

let test_digest_validate_mismatch () =
  let dv = Header_parsing.make_content_digest "original" in
  let valid = Header_parsing.validate_digest ~digests:[dv] "modified" in
  Alcotest.(check bool) "does not validate" false valid

let test_digest_to_string () =
  let dv = { Header_parsing.algorithm = Header_parsing.Sha256; digest = "abc123" } in
  let s = Header_parsing.digest_value_to_string dv in
  Alcotest.(check string) "format" "sha-256=:abc123:" s

let test_digest_strongest () =
  let digests = [
    { Header_parsing.algorithm = Header_parsing.Sha256; digest = "a" };
    { Header_parsing.algorithm = Header_parsing.Sha512; digest = "b" };
  ] in
  match Header_parsing.get_strongest_digest digests with
  | Some d -> Alcotest.(check bool) "is sha512" true (d.algorithm = Header_parsing.Sha512)
  | None -> Alcotest.fail "Expected Some digest"

(** {1 HSTS Tests (RFC 6797)} *)

let test_hsts_basic () =
  match Header_parsing.parse_hsts "max-age=31536000" with
  | Some hsts ->
      Alcotest.(check int64) "max_age" 31536000L hsts.max_age;
      Alcotest.(check bool) "include_subdomains" false hsts.include_subdomains;
      Alcotest.(check bool) "preload" false hsts.preload
  | None ->
      Alcotest.fail "Expected Some hsts"

let test_hsts_full () =
  match Header_parsing.parse_hsts "max-age=63072000; includeSubDomains; preload" with
  | Some hsts ->
      Alcotest.(check int64) "max_age" 63072000L hsts.max_age;
      Alcotest.(check bool) "include_subdomains" true hsts.include_subdomains;
      Alcotest.(check bool) "preload" true hsts.preload
  | None ->
      Alcotest.fail "Expected Some hsts"

let test_hsts_case_insensitive () =
  match Header_parsing.parse_hsts "Max-Age=31536000; INCLUDESUBDOMAINS" with
  | Some hsts ->
      Alcotest.(check int64) "max_age" 31536000L hsts.max_age;
      Alcotest.(check bool) "include_subdomains" true hsts.include_subdomains
  | None ->
      Alcotest.fail "Expected Some hsts"

let test_hsts_missing_max_age () =
  let result = Header_parsing.parse_hsts "includeSubDomains; preload" in
  Alcotest.(check bool) "is None" true (Option.is_none result)

let test_hsts_to_string () =
  let hsts = Header_parsing.make_hsts ~max_age:31536000L ~include_subdomains:true () in
  let s = Header_parsing.hsts_to_string hsts in
  Alcotest.(check bool) "contains max-age" true
    (String.length s > 0 && String.sub s 0 8 = "max-age=")

let test_hsts_is_enabled () =
  Alcotest.(check bool) "enabled" true
    (Header_parsing.hsts_is_enabled Header_parsing.hsts_one_year_subdomains);
  Alcotest.(check bool) "disabled" false
    (Header_parsing.hsts_is_enabled Header_parsing.hsts_disable)

let test_hsts_presets () =
  Alcotest.(check int64) "one_year max_age" 31536000L
    Header_parsing.hsts_one_year_subdomains.max_age;
  Alcotest.(check bool) "preload has preload" true
    Header_parsing.hsts_preload.preload;
  Alcotest.(check int64) "disable max_age" 0L
    Header_parsing.hsts_disable.max_age

(** {1 Test Suite} *)

let () =
  Alcotest.run "Header Parsing" [
    ("Content-Range", [
      Alcotest.test_case "Basic parsing" `Quick test_content_range_basic;
      Alcotest.test_case "Unsatisfied range" `Quick test_content_range_unsatisfied;
      Alcotest.test_case "Unknown length" `Quick test_content_range_unknown_length;
      Alcotest.test_case "To string" `Quick test_content_range_to_string;
      Alcotest.test_case "Unsatisfied to string" `Quick test_content_range_unsatisfied_to_string;
      Alcotest.test_case "Invalid inputs" `Quick test_content_range_invalid;
    ]);
    ("If-Range", [
      Alcotest.test_case "Strong ETag" `Quick test_if_range_etag_strong;
      Alcotest.test_case "Weak ETag" `Quick test_if_range_etag_weak;
      Alcotest.test_case "Date" `Quick test_if_range_date;
      Alcotest.test_case "Type predicates" `Quick test_if_range_is_etag;
    ]);
    ("Allow", [
      Alcotest.test_case "Basic parsing" `Quick test_allow_parse_basic;
      Alcotest.test_case "Single method" `Quick test_allow_parse_single;
      Alcotest.test_case "With extra spaces" `Quick test_allow_parse_with_spaces;
      Alcotest.test_case "To string" `Quick test_allow_to_string;
      Alcotest.test_case "Contains" `Quick test_allow_contains;
    ]);
    ("Authentication-Info", [
      Alcotest.test_case "Full parsing" `Quick test_auth_info_full;
      Alcotest.test_case "Nextnonce only" `Quick test_auth_info_nextnonce_only;
      Alcotest.test_case "Empty" `Quick test_auth_info_empty;
    ]);
    ("Retry-After", [
      Alcotest.test_case "Seconds" `Quick test_retry_after_seconds;
      Alcotest.test_case "Date" `Quick test_retry_after_date;
      Alcotest.test_case "To seconds" `Quick test_retry_after_to_seconds;
      Alcotest.test_case "Date to seconds" `Quick test_retry_after_date_to_seconds;
      Alcotest.test_case "Past date to seconds" `Quick test_retry_after_past_date_to_seconds;
      Alcotest.test_case "Date without now" `Quick test_retry_after_date_no_now;
    ]);
    ("Accept-Ranges", [
      Alcotest.test_case "Bytes" `Quick test_accept_ranges_bytes;
      Alcotest.test_case "None" `Quick test_accept_ranges_none;
      Alcotest.test_case "Case insensitive" `Quick test_accept_ranges_case_insensitive;
    ]);
    ("Cache-Status", [
      Alcotest.test_case "Basic parsing" `Quick test_cache_status_basic;
      Alcotest.test_case "Forward reason" `Quick test_cache_status_fwd;
      Alcotest.test_case "Multiple entries" `Quick test_cache_status_multiple;
      Alcotest.test_case "With params" `Quick test_cache_status_with_params;
      Alcotest.test_case "To string" `Quick test_cache_status_to_string;
    ]);
    ("Content-Digest", [
      Alcotest.test_case "Parse SHA-256" `Quick test_digest_parse_sha256;
      Alcotest.test_case "Parse multiple" `Quick test_digest_parse_multiple;
      Alcotest.test_case "Compute SHA-256" `Quick test_digest_compute_sha256;
      Alcotest.test_case "Compute SHA-512" `Quick test_digest_compute_sha512;
      Alcotest.test_case "Validate match" `Quick test_digest_validate;
      Alcotest.test_case "Validate mismatch" `Quick test_digest_validate_mismatch;
      Alcotest.test_case "To string" `Quick test_digest_to_string;
      Alcotest.test_case "Get strongest" `Quick test_digest_strongest;
    ]);
    ("HSTS", [
      Alcotest.test_case "Basic parsing" `Quick test_hsts_basic;
      Alcotest.test_case "Full parsing" `Quick test_hsts_full;
      Alcotest.test_case "Case insensitive" `Quick test_hsts_case_insensitive;
      Alcotest.test_case "Missing max-age" `Quick test_hsts_missing_max_age;
      Alcotest.test_case "To string" `Quick test_hsts_to_string;
      Alcotest.test_case "Is enabled" `Quick test_hsts_is_enabled;
      Alcotest.test_case "Presets" `Quick test_hsts_presets;
    ]);
  ]
