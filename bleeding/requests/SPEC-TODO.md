# HTTP RFC Specification Compliance TODO

This document tracks RFC compliance issues identified in the ocaml-requests library.
Generated from comprehensive analysis against RFC 9110, 9111, 9112, 7235, 7617, 7616, 6750, 6265, 3986, 2818, and 8446.

## Current Compliance Summary

| RFC | Area | Compliance | Notes |
|-----|------|------------|-------|
| RFC 9110 | HTTP Semantics | 95%+ | Excellent - all methods, status codes, headers |
| RFC 9112 | HTTP/1.1 Syntax | 90%+ | Excellent - security validation complete |
| RFC 9111 | HTTP Caching | 85%+ | Good - full age calc, heuristic freshness, Vary |
| RFC 7617/6750/7616 | Authentication | 90%+ | Excellent - userhash, auth-int, bearer form |
| RFC 6265 | Cookies | 70-80% | Good - delegated to Cookeio |
| RFC 3986 | URI | 95%+ | Excellent - inlined with full parsing |

---

## Section 1: URI Library Inlining ✓ COMPLETE

**Goal:** Inline the third_party/uri library into requests, replacing Angstrom-based parsing with Eio.Buf_read combinators for consistency with the HTTP parsing stack.

**Current Status:** IMPLEMENTED in `lib/uri.ml` and `lib/uri.mli`

The URI library has been fully inlined with string-based parsing (no Angstrom dependency):
- [x] All URI parsers implemented (scheme, authority, path, query, fragment)
- [x] IPv4 and IPv6 address parsing
- [x] Percent encoding/decoding with component-specific character sets
- [x] Path normalization and dot segment removal
- [x] Reference resolution per RFC 3986 Section 5.2
- [x] Scheme-specific normalization (HTTP/HTTPS defaults)
- [x] Host case normalization (lowercase)

---

## Section 2: P0 - Security Critical ✓ COMPLETE

### 2.1 Bare CR Validation (RFC 9112) ✓

**RFC Reference:** RFC 9112 Section 2.2

> "A recipient that receives whitespace between the start-line and the first header field MUST either reject the message as invalid or..."
> "bare CR must be rejected"

**Current Status:** IMPLEMENTED in `lib/http_read.ml`

The `validate_no_bare_cr` function validates all relevant areas:
- [x] Add bare CR validation in `status_line` parsing
- [x] Add bare CR validation in `header_line` parsing
- [x] Add bare CR validation in chunked extension parsing

### 2.2 Chunk Size Overflow Protection ✓

**RFC Reference:** RFC 9112 Section 7.1

**Current Status:** IMPLEMENTED in `lib/http_read.ml`

The `chunk_size` function limits hex digits to 16 (`max_chunk_size_hex_digits`):
- [x] Add length check before parsing chunk size
- [x] Protection in both `chunk_size` and `Chunked_body_source.read_chunk_size`

### 2.3 Request Smuggling Prevention ✓

**RFC Reference:** RFC 9112 Section 6.3

> "If a message is received with both a Transfer-Encoding and a Content-Length header field, the Transfer-Encoding overrides the Content-Length."

**Current Status:** IMPLEMENTED in `lib/http_read.ml`

- [x] Transfer-Encoding takes precedence over Content-Length
- [x] Add explicit logging/warning when both present
- [ ] Consider rejecting requests with conflicting headers in strict mode

---

## Section 3: P1 - High Priority ✓ COMPLETE

### 3.1 Content-Length Validation ✓

**RFC Reference:** RFC 9110 Section 8.6

> "Any Content-Length field value greater than or equal to zero is valid."

**Current Status:** IMPLEMENTED in `lib/http_read.ml`

- [x] Reject negative Content-Length values explicitly (in `parse_content_length`)
- [x] Validate Content-Length matches actual body length for responses (raises `Content_length_mismatch`)
- [x] Add `content_length_mismatch` error type (in `lib/error.ml`)

### 3.2 Age Header Calculation ✓

**RFC Reference:** RFC 9111 Section 4.2.3

> "age_value = delta-seconds"
> "The Age header field conveys the sender's estimate of the time since the response was generated"

**Current Status:** IMPLEMENTED in `lib/cache_control.ml`

Full RFC 9111 Section 4.2.3 calculation with `age_inputs` type and `calculate_age` function:
- [x] Add full RFC 9111 Section 4.2.3 age calculation
- [x] Track `request_time` and `response_time` in cache entries
- [x] Add `is_fresh` function using calculated age vs max-age

### 3.3 Heuristic Freshness ✓

**RFC Reference:** RFC 9111 Section 4.2.2

> "A cache MAY calculate a heuristic expiration time"
> "a typical setting of this value might be 10% of the time since the response's Last-Modified field value"

**Current Status:** IMPLEMENTED in `lib/cache_control.ml`

- [x] Add `heuristic_freshness` function
- [x] Use 10% of (now - Last-Modified) as default (`default_heuristic_fraction`)
- [ ] Add Warning 113 "Heuristic expiration" for stale responses
- [x] Add configurable `max_heuristic_age` parameter (`default_max_heuristic_age`)

### 3.4 Digest Auth Enhancements ✓

**RFC Reference:** RFC 7616 Section 3.4

**Current Status:** IMPLEMENTED in `lib/auth.ml`

- [x] Add `userhash` parameter support (in `digest_challenge` and `build_digest_header`)
- [x] Add SHA-256 support (`hash_string` function)
- [x] Add `auth-int` qop (in `compute_digest_response` with body hash)
- [ ] Add `nextnonce` handling for pipelining
- [x] Add `stale=true` handling (`digest_is_stale` function)

### 3.5 Bearer Token Form Parameter ✓

**RFC Reference:** RFC 6750 Section 2.2

> "Clients MAY use the form-encoded body parameter access_token"

**Current Status:** IMPLEMENTED in `lib/auth.ml`

- [x] Add `Bearer_form of { token : string }` variant to auth type
- [x] Serialize as `access_token=TOKEN` via `get_bearer_form_body`
- [x] `is_bearer_form` predicate and `bearer_form` constructor

---

## Section 4: P2 - Medium Priority (Mostly Complete)

### 4.1 Warning Header (Deprecated but Present)

**RFC Reference:** RFC 9111 Section 5.5 (obsoleted)

**Note:** Warning header is obsolete in HTTP but may still be received.

- [ ] Parse Warning header values if present in responses
- [ ] Generate Warning 110 "Response is Stale" when serving stale cached content
- [ ] Generate Warning 112 "Disconnected operation" when offline

### 4.2 Vary Header Support ✓

**RFC Reference:** RFC 9111 Section 4.1

> "A cache MUST use the Vary header field to select the representation"

**Current Status:** IMPLEMENTED in `lib/cache.ml`

- [x] Parse Vary header from responses (`parse_vary` function)
- [x] Match Vary headers for cache lookup (`vary_matches` function)
- [x] Store request headers needed for Vary matching (`vary_headers` in entry type)

### 4.3 Connection Header Parsing ✓

**RFC Reference:** RFC 9110 Section 7.6.1

> "the connection option 'close' signals that the sender is going to close the connection after the current request/response"

**Current Status:** IMPLEMENTED in `lib/headers.ml`

- [x] Parse full comma-separated Connection header values (`parse_connection_header`)
- [x] Remove hop-by-hop headers listed in Connection (`remove_hop_by_hop`)
- [x] Handle `Connection: keep-alive` for HTTP/1.0 (`connection_keep_alive`)
- [x] Handle `Connection: close` (`connection_close`)

### 4.4 Transfer-Encoding Validation ✓

**RFC Reference:** RFC 9112 Section 6.1

> "A server MUST NOT apply a transfer coding to a response to a HEAD request"

**Current Status:** IMPLEMENTED in `lib/http_read.ml`

- [x] Log warning for Transfer-Encoding in response to HEAD (`validate_no_transfer_encoding`)
- [x] Log warning for Transfer-Encoding in 1xx, 204, 304 responses
- [ ] Add test cases for invalid Transfer-Encoding responses

### 4.5 Host Header Validation ✓

**RFC Reference:** RFC 9110 Section 7.2

> "A client MUST send a Host header field in all HTTP/1.1 request messages"

**Current Status:** IMPLEMENTED in `lib/http_write.ml`

- [x] Automatically add Host header from URI if not present
- [x] Verify Host header matches URI authority (logs warning if mismatch)
- [x] Handle Host header for CONNECT requests (uses authority-form host:port)

---

## Section 5: P3 - Low Priority / Nice to Have

### 5.1 Trailer Headers ✓

**RFC Reference:** RFC 9110 Section 6.5

> "Trailer allows the sender to include additional fields at the end of a chunked message"

**Current Status:** IMPLEMENTED in `lib/http_read.ml`

- [x] Parse Trailer header (`parse_trailers` function, lines 315-348)
- [x] Collect trailer fields after final chunk
- [x] Validate trailers don't include forbidden fields (`forbidden_trailer_headers` list)
- [x] Log warnings for forbidden headers and skip them

### 5.2 TE Header ✓

**RFC Reference:** RFC 9110 Section 10.1.4

> "The TE header field describes what transfer codings... the client is willing to accept"

**Current Status:** IMPLEMENTED in `lib/headers.ml` and `lib/header_name.ml`

- [x] TE header type support (`Te` variant in header_name.ml)
- [x] Send `TE: trailers` when trailers are supported (`Headers.te_trailers` function)
- [x] Generic `Headers.te` function for other TE values
- [ ] Parse TE header from incoming requests (server-side, not needed for client)

### 5.3 Expect Continue Timeout ✓

**RFC Reference:** RFC 9110 Section 10.1.1

> "A client that will wait for a 100 (Continue) response before sending the request content SHOULD use a reasonable timeout"

**Current Status:** IMPLEMENTED in `lib/expect_continue.ml` and `lib/timeout.ml`

- [x] Add configurable timeout for 100 Continue wait (`Timeout.t.expect_100_continue`)
- [x] Default to reasonable timeout (1.0 second)
- [x] Timeout implementation using `Eio.Time.with_timeout_exn` in `http_client.ml`
- [x] On timeout, sends body anyway per RFC 9110 recommendation

### 5.4 Method Properties Enforcement ✓

**RFC Reference:** RFC 9110 Section 9

**Current Status:** IMPLEMENTED across multiple modules

- [x] Method properties defined (`is_safe`, `is_idempotent`, `is_cacheable` in `method.ml`)
- [x] Cache only stores GET/HEAD responses (`is_cacheable_method` in `cache.ml`)
- [x] Retry only retries idempotent methods (GET, HEAD, PUT, DELETE, OPTIONS, TRACE in `retry.ml`)
- [x] Debug logging when method prevents caching or retry
- [x] Configurable `strict_method_semantics` option in `Retry.config` (raises error on violation)

### 5.5 URI Normalization for Comparison

**RFC Reference:** RFC 3986 Section 6.2

- [ ] Add `Uri.equivalent` function for comparison after normalization
- [ ] Case-insensitive scheme and host
- [ ] Normalize empty path to "/" for http/https
- [ ] Remove default port numbers

### 5.6 Internationalized Resource Identifiers (IRI)

**RFC Reference:** RFC 3987

- [ ] Add `Uri.of_iri` for IRI to URI conversion
- [ ] Handle UTF-8 encoding in path and query
- [ ] Percent-encode non-ASCII characters

---

## Section 6: Cookie Compliance (Delegated to Cookeio)

The library delegates cookie handling to the Cookeio library. These items should be verified in that library:

- [ ] Verify Cookeio handles `SameSite` attribute per RFC 6265bis
- [ ] Verify `__Host-` and `__Secure-` cookie prefixes
- [ ] Verify Public Suffix List usage for domain matching
- [ ] Verify cookie path matching rules

---

## Section 7: Implementation Order

### Phase 1: Security Fixes (P0) ✓ COMPLETE
1. ✓ Bare CR validation
2. ✓ Chunk size overflow protection
3. ✓ Request smuggling logging

### Phase 2: URI Library Inlining ✓ COMPLETE
1. ✓ Inlined URI library with string-based parsing
2. ✓ Pct module (percent encoding)
3. ✓ Path module (normalization)
4. ✓ Reference resolution and canonicalization

### Phase 3: Core RFC 9111 Compliance ✓ COMPLETE
1. ✓ Age calculation per Section 4.2.3
2. ✓ Heuristic freshness per Section 4.2.2
3. ✓ Vary header support

### Phase 4: Authentication Enhancements ✓ COMPLETE
1. ✓ Digest auth userhash
2. ✓ Digest auth auth-int qop
3. ✓ Bearer form parameter

### Phase 5: Edge Cases and Polish ✓ COMPLETE
1. ✓ Transfer-Encoding validation
2. ✓ Connection header parsing
3. ✓ Trailer header support
4. ✓ Method property enforcement
5. ✓ Host header validation
6. ✓ TE header support
7. ✓ Expect 100-continue timeout

---

## Previously Completed Fixes

| Priority | Issue | RFC | Status |
|----------|-------|-----|--------|
| P0 | Bare CR validation | RFC 9112 Section 2.2 | FIXED |
| P0 | Chunk size overflow protection | RFC 9112 Section 7.1 | FIXED |
| P0 | Request smuggling prevention | RFC 9112 Section 6.3 | FIXED |
| P1 | Content-Length negative validation | RFC 9110 Section 8.6 | FIXED |
| P1 | Full age calculation | RFC 9111 Section 4.2.3 | FIXED |
| P1 | Heuristic freshness | RFC 9111 Section 4.2.2 | FIXED |
| P1 | Digest auth userhash | RFC 7616 Section 3.4 | FIXED |
| P1 | Digest auth auth-int qop | RFC 7616 Section 3.4 | FIXED |
| P1 | Bearer token form parameter | RFC 6750 Section 2.2 | FIXED |
| P2 | Vary header support | RFC 9111 Section 4.1 | FIXED |
| P2 | Connection header parsing | RFC 9110 Section 7.6.1 | FIXED |
| P2 | Transfer-Encoding validation | RFC 9112 Section 6.1 | FIXED |
| Major | URI library inlining | RFC 3986 | FIXED |
| P2 | Host header validation | RFC 9110 Section 7.2 | FIXED |
| P3 | Trailer headers | RFC 9110 Section 6.5 | FIXED |
| P3 | TE header support | RFC 9110 Section 10.1.4 | FIXED |
| P3 | Expect 100-continue timeout | RFC 9110 Section 10.1.1 | FIXED |
| P3 | Method properties enforcement | RFC 9110 Section 9 | FIXED |
| P2 | CONNECT authority-form | RFC 9112 Section 3.2.3 | FIXED |
| P3 | strict_method_semantics option | RFC 9110 Section 9.2.2 | FIXED |
| High | 303 redirect method change | RFC 9110 Section 15.4.4 | FIXED |
| High | obs-fold header handling | RFC 9112 Section 5.2 | FIXED |
| High | Basic auth username validation | RFC 7617 Section 2 | FIXED |
| Medium | Close-delimited body reading | RFC 9112 Section 6.3 | FIXED |
| Medium | Retry-After HTTP-date format | RFC 9110 Section 10.2.3 | FIXED |
| Medium | 407 proxy auth auto-retry | RFC 7235 Section 3.2 | FIXED |
| Medium | 417 Expectation Failed retry | RFC 9110 Section 10.1.1 | FIXED |
| Low | Asterisk-form OPTIONS | RFC 9112 Section 3.2.4 | FIXED |
| Low | Accept-Language header builder | RFC 9110 Section 12.5.4 | FIXED |

---

## Section 8: Feature Roadmap (Non-RFC)

These are feature enhancements not tied to specific RFC compliance:

### 8.1 Protocol Extensions
- [ ] HTTP/2 support (RFC 9113 - spec present in spec/)
- [ ] Unix domain socket support

### 8.2 Security Enhancements
- [ ] Certificate/public key pinning

### 8.3 API Improvements
- [ ] Request/response middleware system
- [ ] Progress callbacks for uploads/downloads
- [ ] Request cancellation

### 8.4 Testing
- [ ] Expand unit test coverage for individual modules
- [ ] Add more edge case tests for HTTP date parsing
- [ ] Add test cases for invalid Transfer-Encoding responses

### 8.5 Documentation
- [ ] Add troubleshooting guide to README

---

## Notes

- The library intentionally does not implement cache storage (RFC 9111) as it provides utilities for applications to build their own caching layer
- SOCKS5 proxy support is declared but not implemented - this is a feature gap, not a compliance issue
- SHA-512-256 for Digest auth is not implemented due to complexity of the special initialization vectors required
- HTTP/2 and HTTP/3 are out of scope for this library (HTTP/1.1 only)
