# HTTP/1.1 RFC Compliance Analysis for httpz

This document compares the httpz implementation against the HTTP/1.1 RFCs (7230-7235) and identifies missing or unimplemented features.

## Reference Documents

- `spec/rfc7230.txt` - Message Syntax and Routing
- `spec/rfc7231.txt` - Semantics and Content
- `spec/rfc7232.txt` - Conditional Requests
- `spec/rfc7233.txt` - Range Requests
- `spec/rfc7234.txt` - Caching
- `spec/rfc7235.txt` - Authentication

---

## RFC 7230: Message Syntax and Routing

### Implemented Features

| Section | Feature | Status | Notes |
|---------|---------|--------|-------|
| 2.6 | HTTP-version parsing | ✅ | HTTP/1.0 and HTTP/1.1 |
| 3.1.1 | Request-line parsing | ✅ | METHOD SP target SP version CRLF |
| 3.2 | Header field parsing | ✅ | name: OWS value OWS CRLF |
| 3.2.3 | Whitespace (OWS/RWS) | ✅ | SP and HTAB handled |
| 3.2.6 | Field value components | ✅ | Token chars validated |
| 3.3.1 | Transfer-Encoding: chunked | ✅ | Full chunk parsing with trailers |
| 3.3.2 | Content-Length parsing | ✅ | With overflow protection |
| 3.3.3 | Message body length | ✅ | CL and TE precedence |
| 3.5 | Bare CR detection | ✅ | Security: smuggling prevention |
| 4.1 | Chunked transfer coding | ✅ | Size, data, extensions, trailers |
| 4.1.2 | Trailer validation | ✅ | Forbidden trailers rejected |
| 5.4 | Host header requirement | ✅ | Required for HTTP/1.1 |
| 6.1 | Connection header | ✅ | keep-alive/close |
| 6.3 | Persistence default | ✅ | HTTP/1.1=keep-alive, 1.0=close |

### Missing/Incomplete Features

| Section | Feature | Priority | Description |
|---------|---------|----------|-------------|
| 3.2.4 | obs-fold handling | Medium | Multi-line header values (deprecated but must be parseable) |
| 3.3.1 | Multiple Transfer-Encodings | High | e.g., "gzip, chunked" - only final chunked is parsed |
| 4.2 | Compression codings | Low | gzip, compress, deflate decoding (typically done at app layer) |
| 5.3 | Request-target forms | Medium | Only origin-form tested; absolute-form, authority-form, asterisk-form |
| 5.7 | Message forwarding | N/A | Proxy functionality out of scope for parser |
| 6.7 | Upgrade mechanism | Low | Protocol switching (WebSocket etc.) |

### Security Considerations (Section 9)

| Issue | Status | Notes |
|-------|--------|-------|
| Response splitting | ✅ | CRLF injection detection |
| Request smuggling | ✅ | Bare CR, ambiguous framing checks |
| Header size limits | ✅ | Configurable max_header_size |
| Content-Length limits | ✅ | Configurable max_content_length |

---

## RFC 7231: Semantics and Content

### Implemented Features

| Section | Feature | Status | Notes |
|---------|---------|--------|-------|
| 4.1 | GET | ✅ | |
| 4.2 | HEAD | ✅ | |
| 4.3 | POST | ✅ | |
| 4.4 | PUT | ✅ | |
| 4.5 | DELETE | ✅ | |
| 4.6 | CONNECT | ✅ | |
| 4.7 | OPTIONS | ✅ | |
| 4.8 | TRACE | ✅ | |
| 4.3.5 | PATCH | ✅ | (RFC 5789) |
| 5.1.1 | Expect: 100-continue | ✅ | Parsed and cached in req.expect_continue |
| 6.x | Status codes | ✅ | Comprehensive coverage (see Res.status) |

### Missing/Incomplete Features

| Section | Feature | Priority | Description |
|---------|---------|----------|-------------|
| 3.4 | Content negotiation | Low | Accept, Accept-Language, etc. (app-layer concern) |
| 5.1.2 | Max-Forwards | Low | For TRACE/OPTIONS (proxy feature) |
| 5.3.1 | Quality values (q=) | Medium | Parsing weight values in Accept headers |
| 5.5 | Request context headers | Low | From, Referer, User-Agent semantics |
| 6.4.1 | 300 Multiple Choices | Low | Status code not in Res.status |
| 6.4.4 | 305 Use Proxy | N/A | Deprecated |
| 7.1.1.1 | Date header format | Medium | RFC 5322 date parsing |
| 7.1.4 | Vary header | Low | Cache variation (proxy/cache concern) |
| 7.4 | Server/User-Agent | Low | Header generation |

### Recommended Additions

1. **Date header parsing** - For conditional requests and caching
2. **Quality value parsing** - `parse_quality : Span.t -> float` for Accept-* headers

---

## RFC 7232: Conditional Requests

### Implemented Features

| Section | Feature | Status | Notes |
|---------|---------|--------|-------|
| 2.2 | Last-Modified (header name) | ✅ | Recognized in Header_name.t |
| 2.3 | ETag (header name) | ✅ | Recognized in Header_name.t |
| 3.1 | If-Match (header name) | ✅ | Recognized in Header_name.t |
| 3.2 | If-None-Match (header name) | ✅ | Recognized in Header_name.t |
| 3.3 | If-Modified-Since (header name) | ✅ | Recognized in Header_name.t |
| 3.4 | If-Unmodified-Since (header name) | ✅ | Recognized in Header_name.t |
| 4.1 | 304 Not Modified | ✅ | In Res.status |
| 4.2 | 412 Precondition Failed | ✅ | In Res.status |

### Missing/Incomplete Features

| Section | Feature | Priority | Description |
|---------|---------|----------|-------------|
| 2.1 | Validator comparison | High | Strong vs weak comparison functions |
| 2.2.2 | Last-Modified date parsing | High | HTTP-date parsing to timestamp |
| 2.3.2 | ETag parsing | High | Parse entity-tag with weak indicator (W/) |
| 3 | Precondition evaluation | Medium | Evaluation order and logic |
| 3.5 | If-Range | Medium | Range request conditional |

### Recommended Additions

```ocaml
(** Entity tag with optional weak indicator. *)
type etag = { weak : bool; tag : string }

(** Parse ETag header value.
    Examples: "xyzzy", W/"xyzzy", "" *)
val parse_etag : Base_bigstring.t -> Span.t -> etag option

(** Strong comparison (RFC 7232 Section 2.3.2). *)
val etag_strong_eq : etag -> etag -> bool

(** Weak comparison (RFC 7232 Section 2.3.2). *)
val etag_weak_eq : etag -> etag -> bool

(** Parse HTTP-date (RFC 7231 Section 7.1.1.1). *)
val parse_http_date : Base_bigstring.t -> Span.t -> float option
```

---

## RFC 7233: Range Requests

### Implemented Features

| Section | Feature | Status | Notes |
|---------|---------|--------|-------|
| 2.1 | Range header name | ✅ | Recognized in Header_name.t |
| 3.1 | Content-Range header name | ✅ | Recognized in Header_name.t |
| 4.1 | 206 Partial Content | ✅ | In Res.status |
| 4.4 | 416 Range Not Satisfiable | ✅ | In Res.status |

### Missing/Incomplete Features

| Section | Feature | Priority | Description |
|---------|---------|----------|-------------|
| 2.1 | Range header parsing | High | Parse "bytes=start-end" syntax |
| 2.3 | Accept-Ranges header | Medium | Writing Accept-Ranges: bytes |
| 3.1 | Content-Range writing | High | Writing Content-Range: bytes start-end/total |
| 3.4 | If-Range | Medium | Conditional range requests |
| 4.1 | 206 with multipart | Low | multipart/byteranges responses |
| 5.4.2 | Multiple ranges | Low | Coalescing overlapping ranges |

### Recommended Additions

```ocaml
(** Byte range specification. *)
type byte_range =
  | Range of int64 * int64     (** bytes=start-end *)
  | Suffix of int64            (** bytes=-suffix *)
  | From of int64              (** bytes=start- *)

(** Parse Range header value. Returns list of ranges.
    Example: "bytes=0-499, 1000-1499" -> [Range(0,499); Range(1000,1499)] *)
val parse_range : Base_bigstring.t -> Span.t -> byte_range list option

(** Write Content-Range header: "Content-Range: bytes start-end/total\r\n" *)
val write_content_range : Base_bigstring.t -> off:int -> start:int64 -> end_:int64 -> total:int64 -> int

(** Write Accept-Ranges header: "Accept-Ranges: bytes\r\n" *)
val write_accept_ranges : Base_bigstring.t -> off:int -> int
```

---

## RFC 7234: Caching

### Implemented Features

| Section | Feature | Status | Notes |
|---------|---------|--------|-------|
| 5.1 | Age header name | ✅ | Recognized in Header_name.t |
| 5.2 | Cache-Control header name | ✅ | Recognized in Header_name.t |
| 5.3 | Expires header name | ✅ | Recognized in Header_name.t |

### Missing/Incomplete Features

| Section | Feature | Priority | Description |
|---------|---------|----------|-------------|
| 1.2.1 | Cache validation | Low | Freshness calculation (cache implementation) |
| 5.2.1 | Cache-Control directives | Medium | Parse max-age, no-cache, no-store, etc. |
| 5.4 | Pragma header | Low | HTTP/1.0 compatibility |
| 5.5 | Warning header | Low | Deprecated in newer specs |

### Recommended Additions (if caching support desired)

```ocaml
(** Cache-Control directive. *)
type cache_directive =
  | Max_age of int
  | No_cache of string list option  (** field-names or None *)
  | No_store
  | No_transform
  | Only_if_cached
  | Must_revalidate
  | Public
  | Private of string list option
  | Proxy_revalidate
  | S_maxage of int
  | Extension of string * string option

(** Parse Cache-Control header value. *)
val parse_cache_control : Base_bigstring.t -> Span.t -> cache_directive list
```

---

## RFC 7235: Authentication

### Implemented Features

| Section | Feature | Status | Notes |
|---------|---------|--------|-------|
| 4.1 | WWW-Authenticate header name | ✅ | Recognized in Header_name.t |
| 4.2 | Authorization header name | ✅ | Recognized in Header_name.t |
| 3.1 | 401 Unauthorized | ✅ | In Res.status |

### Missing/Incomplete Features

| Section | Feature | Priority | Description |
|---------|---------|----------|-------------|
| 3.2 | 407 Proxy Auth Required | Medium | Status code missing |
| 4.3 | Proxy-Authenticate | Medium | Header name not recognized |
| 4.4 | Proxy-Authorization | Medium | Header name not recognized |
| 2.1 | Challenge parsing | Low | auth-scheme + params/token68 |
| 2.2 | Credentials parsing | Low | Basic, Bearer, etc. |

### Recommended Additions

```ocaml
(* In Header_name.t *)
| Proxy_authenticate
| Proxy_authorization

(* In Res.status *)
| Proxy_authentication_required  (** 407 *)

(** Authentication scheme and parameters. *)
type auth_challenge = {
  scheme : string;  (** "Basic", "Bearer", etc. *)
  params : (string * string) list;  (** realm, charset, etc. *)
}

(** Parse WWW-Authenticate or Proxy-Authenticate value. *)
val parse_auth_challenge : Base_bigstring.t -> Span.t -> auth_challenge option
```

---

## Implementation Priority Matrix

### High Priority (Core HTTP functionality)

| Feature | RFC | Effort | Impact |
|---------|-----|--------|--------|
| Multiple Transfer-Encodings | 7230 §3.3.1 | Medium | Correctness for compressed+chunked |
| ETag parsing (with W/) | 7232 §2.3 | Low | Conditional request support |
| Range header parsing | 7233 §2.1 | Medium | Partial content support |
| HTTP-date parsing | 7231 §7.1.1 | Medium | Conditional requests, caching |

### Medium Priority (Better compliance)

| Feature | RFC | Effort | Impact |
|---------|-----|--------|--------|
| obs-fold handling | 7230 §3.2.4 | Low | Legacy compatibility |
| Request-target forms | 7230 §5.3 | Low | Proxy support |
| Validator comparison | 7232 §2.1 | Low | Correct conditional semantics |
| Content-Range writing | 7233 §3.1 | Low | Range response support |
| Accept-Ranges writing | 7233 §2.3 | Trivial | Signal range support |
| Proxy auth headers | 7235 §4.3-4.4 | Trivial | Header recognition |
| 407 status code | 7235 §3.2 | Trivial | Proxy authentication |

### Low Priority (Application-layer concerns)

| Feature | RFC | Effort | Impact |
|---------|-----|--------|--------|
| Content negotiation | 7231 §3.4 | High | Usually app-specific |
| Quality value parsing | 7231 §5.3.1 | Low | Accept header weights |
| Cache-Control parsing | 7234 §5.2 | Medium | Cache implementation |
| Auth challenge parsing | 7235 §2.1 | Medium | Auth scheme specific |
| Compression codings | 7230 §4.2 | High | Usually handled by middleware |

---

## Recommended Implementation Plan

### Phase 1: Core Compliance (High Priority)

1. **Add HTTP-date parsing** (`lib/date.ml`)
   - RFC 7231 Section 7.1.1.1 defines three formats
   - IMF-fixdate: `Sun, 06 Nov 1994 08:49:37 GMT`
   - RFC 850: `Sunday, 06-Nov-94 08:49:37 GMT`
   - ANSI C asctime: `Sun Nov  6 08:49:37 1994`

2. **Add ETag parsing** (`lib/etag.ml`)
   - Parse weak indicator (W/)
   - Implement strong and weak comparison

3. **Add Range header parsing** (`lib/range.ml`)
   - Parse byte-range-spec: `bytes=start-end`
   - Handle suffix-range: `bytes=-500`
   - Handle open-ended: `bytes=9500-`
   - Return list of ranges

4. **Enhance Transfer-Encoding handling**
   - Parse comma-separated list
   - Validate "chunked" is last
   - Track other encodings for application layer

### Phase 2: Response Improvements

5. **Add missing response writers** (`lib/res.ml`)
   - `write_content_range`
   - `write_accept_ranges`
   - `write_etag`
   - `write_last_modified`

6. **Add missing status codes**
   - 407 Proxy Authentication Required
   - 300 Multiple Choices (optional)

7. **Add missing header names**
   - `Proxy_authenticate`
   - `Proxy_authorization`
   - `If_range`
   - `Accept_ranges`

### Phase 3: Edge Cases

8. **obs-fold handling**
   - Detect and unfold multi-line headers
   - Convert to single line internally

9. **Request-target validation**
   - Validate absolute-form for proxies
   - Validate authority-form for CONNECT
   - Validate asterisk-form for OPTIONS

### Phase 4: Optional Extensions

10. **Cache-Control parsing** (if caching support needed)
11. **Authentication parsing** (if auth support needed)
12. **Content negotiation** (if needed by applications)

---

## Testing Recommendations

For each implemented feature, add tests covering:

1. **Positive cases** - Valid inputs parse correctly
2. **Edge cases** - Empty values, maximum lengths, boundaries
3. **Security cases** - Malformed inputs, injection attempts
4. **Interop cases** - Real-world examples from browsers/servers

Test vectors should be derived from:
- RFC examples
- Real-world HTTP traffic captures
- Known CVE test cases (for security features)

---

## Summary

The httpz library has excellent coverage of RFC 7230 (message syntax) with strong security features. The main gaps are:

1. **Conditional requests (RFC 7232)** - Header names recognized but no parsing/comparison
2. **Range requests (RFC 7233)** - Status codes present but no Range header parsing
3. **Some edge cases** - obs-fold, multiple Transfer-Encodings, request-target forms

The library is well-suited for high-performance HTTP parsing. Most "missing" features are application-layer concerns (caching, auth, content negotiation) that users would implement on top of the parsed headers.
