# RFC 6265 Compliance TODO

This document tracks deviations from [RFC 6265](https://datatracker.ietf.org/doc/html/rfc6265) (HTTP State Management Mechanism) and missing features in ocaml-cookeio.

## High Priority

### 1. Public Suffix Validation (Section 5.3, Step 5)

**Status:** ✅ IMPLEMENTED

The RFC requires rejecting cookies with domains that are "public suffixes" (e.g., `.com`, `.co.uk`) to prevent domain-wide cookie attacks.

**Implementation:**
- Uses the `publicsuffix` library which embeds the Mozilla Public Suffix List at build time
- Validates Domain attribute in `of_set_cookie_header` before creating the cookie
- Rejects cookies where Domain is a public suffix (e.g., `.com`, `.co.uk`, `.github.io`)
- Allows cookies where the request host exactly matches the public suffix domain
- IP addresses bypass PSL validation (per RFC 6265 Section 5.1.3)
- Cookies without Domain attribute (host-only) are always allowed

**Security impact:** Prevents attackers from setting domain-wide cookies that would affect all sites under a TLD.

---

## Medium Priority

### 2. IP Address Domain Matching (Section 5.1.3)

**Status:** ✅ IMPLEMENTED

The RFC specifies that domain suffix matching should only apply to host names, not IP addresses.

**Implementation:**
- Uses the `ipaddr` library to detect IPv4 and IPv6 addresses
- IP addresses require exact match only (no suffix matching)
- Hostnames continue to support subdomain matching when `host_only = false`

---

### 3. Expires Header Date Format (Section 4.1.1)

**Status:** Wrong format

**Current behavior:** Outputs RFC3339 format (`2021-06-09T10:18:14+00:00`)

**RFC requirement:** Use `rfc1123-date` format (`Wed, 09 Jun 2021 10:18:14 GMT`)

**Location:** `cookeio.ml:447-448`

**Fix:** Implement RFC1123 date formatting for Set-Cookie header output.

---

### 4. Cookie Ordering in Header (Section 5.4, Step 2)

**Status:** ✅ IMPLEMENTED

When generating Cookie headers, cookies are sorted:
1. Cookies with longer paths listed first
2. Among equal-length paths, earlier creation-times listed first

**Implementation:** `get_cookies` function in `cookeio_jar.ml` uses `compare_cookie_order` to sort cookies before returning them.

---

### 5. Creation Time Preservation (Section 5.3, Step 11.3)

**Status:** ✅ IMPLEMENTED

When replacing an existing cookie (same name/domain/path), the creation-time of the old cookie is preserved.

**Implementation:** `add_cookie` and `add_original` functions in `cookeio_jar.ml` use `preserve_creation_time` to retain the original creation time when updating an existing cookie.

---

### 6. Default Path Computation (Section 5.1.4)

**Status:** Not implemented (caller responsibility)

The RFC specifies an algorithm for computing default path when Path attribute is absent:
1. If uri-path is empty or doesn't start with `/`, return `/`
2. If uri-path contains only one `/`, return `/`
3. Return characters up to (but not including) the rightmost `/`

**Suggestion:** Add `default_path : string -> string` helper function.

---

## Low Priority

### 7. Storage Limits (Section 6.1)

**Status:** Not implemented

RFC recommends minimum capabilities:
- At least 4096 bytes per cookie
- At least 50 cookies per domain
- At least 3000 cookies total

**Suggestion:** Add configurable limits with RFC-recommended defaults.

---

### 8. Excess Cookie Eviction (Section 5.3)

**Status:** Not implemented

When storage limits are exceeded, evict in priority order:
1. Expired cookies
2. Cookies sharing domain with many others
3. All cookies

Tiebreaker: earliest `last-access-time` first (LRU).

---

### 9. Two-Digit Year Parsing (Section 5.1.1)

**Status:** Minor deviation

**RFC specification:**
- Years 70-99 → add 1900
- Years 0-69 → add 2000

**Current code** (`cookeio.ml:128-130`):
```ocaml
if year >= 0 && year <= 68 then year + 2000
else if year >= 69 && year <= 99 then year + 1900
```

**Issue:** Year 69 is treated as 1969, but RFC says 70-99 get 1900, implying 69 should get 2000.

---

## Compliant Features

The following RFC requirements are correctly implemented:

- [x] Case-insensitive attribute name matching (Section 5.2)
- [x] Leading dot removal from Domain attribute (Section 5.2.3)
- [x] Max-Age takes precedence over Expires (Section 5.3, Step 3)
- [x] Secure flag handling (Section 5.2.5)
- [x] HttpOnly flag handling (Section 5.2.6)
- [x] Cookie date parsing with multiple format support (Section 5.1.1)
- [x] Session vs persistent cookie distinction (Section 5.3)
- [x] Last-access-time updates on retrieval (Section 5.4, Step 3)
- [x] Host-only flag for domain matching (Section 5.3, Step 6)
- [x] Path matching algorithm (Section 5.1.4)
- [x] IP address domain matching - exact match only (Section 5.1.3)
- [x] Cookie ordering in headers - longer paths first, then by creation time (Section 5.4, Step 2)
- [x] Creation time preservation when replacing cookies (Section 5.3, Step 11.3)
- [x] Public suffix validation - rejects cookies for TLDs like .com (Section 5.3, Step 5)

---

## Extensions Beyond RFC 6265

These features are implemented but not part of RFC 6265:

| Feature | Specification |
|---------|---------------|
| SameSite | RFC 6265bis (draft) |
| Partitioned | CHIPS proposal |
| Mozilla format | De facto standard |

---

## References

- [RFC 6265](https://datatracker.ietf.org/doc/html/rfc6265) - HTTP State Management Mechanism
- [RFC 6265bis](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis) - Updated cookie spec (draft)
- [Public Suffix List](https://publicsuffix.org/) - Mozilla's public suffix database
- [CHIPS](https://developer.chrome.com/docs/privacy-sandbox/chips/) - Cookies Having Independent Partitioned State
