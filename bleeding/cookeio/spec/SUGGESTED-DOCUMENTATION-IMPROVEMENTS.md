# Specific Documentation Improvement Suggestions

This document provides concrete, ready-to-implement suggestions for enhancing RFC documentation in the cookeio codebase.

## 1. Main Module Header Enhancement (cookeio.mli)

### Add Standards References Section

Add this to the end of the main module documentation (after line 48):

```ocaml
    {2 Standards and References}

    This library implements and references the following IETF specifications:

    {ul
    {- {{:https://datatracker.ietf.org/doc/html/rfc6265}RFC 6265} -
       HTTP State Management Mechanism (April 2011) - Primary specification}
    {- {{:https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis}RFC 6265bis} -
       Cookies: HTTP State Management Mechanism (Draft) - SameSite attribute and modern updates}
    {- {{:https://datatracker.ietf.org/doc/html/rfc1034#section-3.5}RFC 1034 Section 3.5} -
       Domain Names - Preferred Name Syntax for domain validation}
    {- {{:https://datatracker.ietf.org/doc/html/rfc2616#section-2.2}RFC 2616 Section 2.2} -
       HTTP/1.1 - Token syntax definition}
    {- {{:https://datatracker.ietf.org/doc/html/rfc1123#section-5.2.14}RFC 1123 Section 5.2.14} -
       Internet Host Requirements - Date format (rfc1123-date)}}

    Additional standards:
    {ul
    {- {{:https://publicsuffix.org/}Mozilla Public Suffix List} - Registry
       of public suffixes for cookie domain validation per RFC 6265 Section 5.3 Step 5}}

    See the {{:../RFC-TODO.md}RFC Compliance Tracker} for implementation status
    of all RFC 6265 requirements.
```

## 2. Enhanced Function Documentation

### A. `normalize_year` function (cookeio.ml:425)

**Location**: `lib/core/cookeio.ml:425`

**Current:**
```ocaml
  (** Normalize abbreviated years per RFC 6265.

      Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.1} RFC 6265 Section 5.1.1}:
      - Years 70-99 map to 1970-1999
      - Years 0-69 map to 2000-2069

      ...
```

**Suggested Enhancement:**
```ocaml
  (** Normalize abbreviated years per RFC 6265 Section 5.1.1.

      Implements two-digit year expansion per
      {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.1} RFC 6265 Section 5.1.1}:
      - Years 70-99 → add 1900 to get 1970-1999
      - Years 0-69 → add 2000 to get 2000-2069

      This algorithm provides a 100-year window (1970-2069) for interpreting
      two-digit years in cookie Expires attributes. The cutoff at year 70
      was chosen to handle legacy cookies while supporting dates through 2069.

      Note: RFC 6265 recommends that servers generate only four-digit years
      in the rfc1123-date format to avoid ambiguity. This function exists
      for parsing legacy cookies that use two-digit years.

      @param year Two-digit year (0-99)
      @return Four-digit year (1970-2069)

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.1> RFC 6265 Section 5.1.1 - Dates *)
```

### B. `strip_leading_dot` function (cookeio.ml:391)

**Location**: `lib/core/cookeio.ml:391`

**Suggested Addition:**
```ocaml
  (** Remove leading dot from domain attribute per RFC 6265 Section 5.2.3.

      Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.3} RFC 6265 Section 5.2.3},
      "If the attribute-value is empty, the behavior is undefined. However,
      the user agent SHOULD ignore the cookie-av entirely." Additionally,
      "Ignore any leading %x2E ('.') character" in the Domain attribute value.

      This normalization ensures that ".example.com" and "example.com" are
      treated identically for cookie domain matching purposes. The leading dot
      was used historically but has no semantic meaning in RFC 6265.

      @param domain Domain attribute value (may have leading dot)
      @return Domain with leading dot removed if present

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.3> RFC 6265 Section 5.2.3 - The Domain Attribute *)
```

### C. `is_token_char` function (cookeio.ml:117)

**Location**: `lib/core/cookeio.ml:117-124`

**Current:**
```ocaml
  (** Check if a character is a valid RFC 2616 token character.

      Per RFC 6265, cookie-name must be a token as defined in RFC 2616 Section 2.2:
      ...
```

**Suggested Enhancement:**
```ocaml
  (** Check if a character is a valid RFC 2616 token character.

      Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1} RFC 6265 Section 4.1.1},
      cookie-name must be a "token" as defined in
      {{:https://datatracker.ietf.org/doc/html/rfc2616#section-2.2} RFC 2616 Section 2.2}.

      RFC 2616 defines a token as:
      {[ token = 1*<any CHAR except CTLs or separators> ]}

      Separators are: {[ ( ) < > @ , ; : \ " / [ ] ? = { } SP HT ]}

      This means tokens consist of visible ASCII characters (33-126) excluding
      control characters (0-31, 127) and the 19 separator characters listed above.

      Valid token characters: ASCII 33-126 except separators
      Invalid: control chars (0-31, 127), space (32), separators

      @param c Character to test
      @return true if [c] is a valid token character

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1> RFC 6265 Section 4.1.1 - Syntax
      @see <https://datatracker.ietf.org/doc/html/rfc2616#section-2.2> RFC 2616 Section 2.2 - Basic Rules *)
```

## 3. Enhanced Validation Module Documentation

### Add Section Overview to Validate Module (cookeio.mli:273)

**Location**: `lib/core/cookeio.mli:264-272`

**Current:**
```ocaml
(** {1 RFC 6265 Validation}

    Validation functions for cookie names, values, and attributes per
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1} RFC 6265 Section 4.1.1}.
    ...
```

**Suggested Enhancement:**
```ocaml
(** {1 RFC 6265 Validation}

    Validation functions for cookie names, values, and attributes per
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1} RFC 6265 Section 4.1.1}.

    These functions implement the syntactic requirements from RFC 6265 to ensure
    cookies conform to the specification before being sent in HTTP headers.
    All validation failures return detailed error messages citing the specific
    RFC requirement that was violated.

    {2 Validation Philosophy}

    Per RFC 6265 Section 4, there is an important distinction between:
    - **Server requirements** (Section 4.1): Strict syntax for generating Set-Cookie headers
    - **User agent requirements** (Section 5): Lenient parsing for receiving Set-Cookie headers

    These validation functions enforce the {b server requirements}, ensuring that
    cookies generated by this library conform to RFC 6265 syntax. When parsing
    cookies from HTTP headers, the library may be more lenient to maximize
    interoperability with non-compliant servers.

    {2 Character Set Requirements}

    RFC 6265 restricts cookies to US-ASCII characters with specific exclusions:
    - Cookie names: RFC 2616 tokens (no CTLs, no separators)
    - Cookie values: cookie-octet characters (0x21, 0x23-0x2B, 0x2D-0x3A, 0x3C-0x5B, 0x5D-0x7E)
    - Domain values: RFC 1034 domain name syntax or IP addresses
    - Path values: Any character except CTLs and semicolon

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1> RFC 6265 Section 4.1.1 - Syntax *)
```

## 4. Cookie Jar Module Enhancements

### A. Add Algorithm Documentation to `compare_cookie_order` (cookeio_jar.ml:312)

**Location**: `lib/jar/cookeio_jar.ml:312-318`

**Current:**
```ocaml
(** Compare cookies for ordering per RFC 6265 Section 5.4, Step 2.
    ...
```

**Suggested Enhancement:**
```ocaml
(** Compare cookies for ordering per RFC 6265 Section 5.4, Step 2.

    Implements the cookie ordering algorithm from
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.4} RFC 6265 Section 5.4 Step 2}:

    {[
    The user agent SHOULD sort the cookie-list in the following order:
    - Cookies with longer paths are listed before cookies with shorter paths.
    - Among cookies that have equal-length path fields, cookies with earlier
      creation-times are listed before cookies with later creation-times.
    ]}

    This ordering ensures that more specific cookies (longer paths) take
    precedence over less specific cookies when both match a request URL.
    Among equally-specific cookies, older cookies are preferred to maintain
    stable behavior across requests.

    The RFC uses "SHOULD" (not "MUST") because the server cannot rely on
    any particular cookie ordering, but this ordering is recommended for
    predictable behavior.

    @param c1 First cookie
    @param c2 Second cookie
    @return Negative if c1 < c2, zero if equal, positive if c1 > c2

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.4> RFC 6265 Section 5.4 - The Cookie Header *)
```

### B. Enhance `path_matches` Documentation (cookeio_jar.ml:95)

**Location**: `lib/jar/cookeio_jar.ml:95-106`

**Current:**
```ocaml
(** Check if a request path matches a cookie path.

    Per RFC 6265 Section 5.1.4, a request-path path-matches a given cookie-path if:
    ...
```

**Suggested Enhancement:**
```ocaml
(** Check if a request path matches a cookie path.

    Implements the path-matching algorithm from
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.4} RFC 6265 Section 5.1.4}.

    Per Section 5.1.4, a request-path path-matches a given cookie-path if
    at least one of the following conditions holds:

    1. The cookie-path and request-path are identical (exact match)
    2. The cookie-path is a prefix of request-path, AND the last character
       of cookie-path is "/" (directory match)
    3. The cookie-path is a prefix of request-path, AND the first character
       of request-path that is not in cookie-path is "/" (subdirectory match)

    Examples:
    - Cookie path "/" matches all request paths
    - Cookie path "/docs" matches "/docs", "/docs/", "/docs/Web/", but NOT "/documents"
    - Cookie path "/docs/" matches "/docs/", "/docs/Web/", but NOT "/docs" or "/documents"

    This algorithm prevents cookies from leaking across directory boundaries.
    A cookie set for "/admin" should not be sent to "/administration".

    @param request_path Request path from the URI
    @param cookie_path Cookie's path attribute
    @return true if request_path matches cookie_path per RFC 6265

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.4> RFC 6265 Section 5.1.4 - Paths and Path-Match *)
```

## 5. Implementation Comment Enhancements

### A. Max-Age Handling (cookeio.ml:564)

**Location**: `lib/core/cookeio.ml:564`

**Current:**
```ocaml
          (* Handle negative values as 0 per RFC 6265 *)
```

**Suggested Enhancement:**
```ocaml
          (* Handle negative values as 0 per RFC 6265 Section 5.2.2.
             The RFC states: "If delta-seconds is less than or equal to zero,
             let expiry-time be the earliest representable date and time."
             We treat this as immediate expiration by setting Max-Age to 0. *)
```

### B. Creation Time Preservation (cookeio_jar.ml:198)

**Location**: `lib/jar/cookeio_jar.ml:198`

**Current:**
```ocaml
     per RFC 6265 Section 5.3, Step 11.3 *)
```

**Suggested Enhancement:**
```ocaml
     per RFC 6265 Section 5.3, Step 11.3, which states:
     "If the newly created cookie was received from a 'non-HTTP' API and
     there is already a cookie in the cookie store with the same name,
     domain, and path as the newly created cookie, then the creation-time
     of the newly created cookie should be the same as the creation-time
     of the old-cookie."

     This ensures stable cookie ordering in the Cookie header when cookies
     are updated, per Section 5.4 Step 2 ordering requirements. *)
```

## 6. README.md Enhancement

### Current README Section (Lines 1-16)

Replace with:

```markdown
# Cookeio - HTTP Cookie Management for OCaml

Cookeio is an OCaml library for managing HTTP cookies, implementing
[RFC 6265](https://datatracker.ietf.org/doc/html/rfc6265) (HTTP State
Management Mechanism) with support for modern extensions from
[RFC 6265bis](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis).

## Overview

HTTP cookies are a mechanism defined in [RFC 6265](https://datatracker.ietf.org/doc/html/rfc6265)
that allows "server side connections to store and retrieve information on the
client side." Originally designed to enable persistent client-side state for
web applications, cookies are essential for storing user preferences, session
data, shopping cart contents, and authentication tokens.

This library provides a complete cookie jar implementation following
[RFC 6265](https://datatracker.ietf.org/doc/html/rfc6265) while integrating
with OCaml's [Eio](https://github.com/ocaml-multicore/eio) for efficient
asynchronous operations.

## Standards Compliance

This library provides full [RFC 6265](https://datatracker.ietf.org/doc/html/rfc6265)
compliance including:

- ✅ Cookie parsing and serialization ([Section 4](https://datatracker.ietf.org/doc/html/rfc6265#section-4))
- ✅ Domain and path matching ([Section 5.1](https://datatracker.ietf.org/doc/html/rfc6265#section-5.1))
- ✅ Storage model implementation ([Section 5.3](https://datatracker.ietf.org/doc/html/rfc6265#section-5.3))
- ✅ Cookie ordering in headers ([Section 5.4](https://datatracker.ietf.org/doc/html/rfc6265#section-5.4))
- ✅ Public Suffix List validation ([Section 5.3 Step 5](https://datatracker.ietf.org/doc/html/rfc6265#section-5.3))
- ✅ IP address domain matching ([Section 5.1.3](https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.3))
- ✅ Creation time preservation ([Section 5.3 Step 11.3](https://datatracker.ietf.org/doc/html/rfc6265#section-5.3))

See [RFC-TODO.md](RFC-TODO.md) for detailed compliance status and known deviations.

### Modern Cookie Attributes

In addition to RFC 6265, this library supports modern cookie attributes:

- **SameSite** (`Strict`, `Lax`, `None`) - Cross-site request protection from
  [RFC 6265bis Section 5.4.7](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis#section-5.4.7)
- **Partitioned** - CHIPS (Cookies Having Independent Partitioned State) for
  privacy-preserving third-party cookies
```

## 7. Error Message Enhancement

### Example: Validation Error Messages

Consider enhancing error messages with clickable RFC links for better developer experience:

**Current (cookeio.ml:164):**
```ocaml
               "Cookie name %S contains invalid characters: %s. RFC 6265 requires \
                valid RFC 2616 token characters only."
```

**Suggested:**
```ocaml
               "Cookie name %S contains invalid characters: %s. \
                RFC 6265 Section 4.1.1 requires cookie names to be valid \
                RFC 2616 tokens (https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1). \
                Tokens cannot contain control characters or separators."
```

## 8. Test Documentation Enhancement

### Add Test Suite Overview (test_cookeio.ml)

Add at the top of the test file, after imports:

```ocaml
(** {1 RFC 6265 Compliance Test Suite}

    This test suite validates cookeio's compliance with
    {{:https://datatracker.ietf.org/doc/html/rfc6265}RFC 6265}
    (HTTP State Management Mechanism).

    {2 Test Organization}

    Tests are organized by RFC section:

    - {b Section 5.1.3}: Domain matching ({!test_ip_address_domain_matching})
    - {b Section 5.1.4}: Path matching ({!test_path_matching})
    - {b Section 5.3}: Storage model ({!test_creation_time_preservation})
    - {b Section 5.3 Step 5}: Public suffix validation ({!test_public_suffix_validation})
    - {b Section 5.4 Step 2}: Cookie ordering ({!test_cookie_ordering})
    - {b Section 4.1.1}: Syntax validation ({!test_rfc6265_validation})

    {2 Validation Philosophy}

    Each test includes:
    - Reference to specific RFC section being tested
    - Clear test case description
    - Expected behavior per RFC requirements
    - Edge cases and error conditions

    See {{:../RFC-TODO.md}RFC-TODO.md} for implementation status of all
    RFC 6265 requirements. *)
```

## Implementation Priority

1. **Immediate**: Add Standards References section to main module (Section 1)
2. **High**: Enhance key function documentation (Section 2)
3. **Medium**: Improve README with RFC links (Section 6)
4. **Low**: Enhanced implementation comments (Section 5)
5. **Optional**: Error message improvements (Section 7)

All changes maintain backward compatibility and only enhance documentation.

---

**Document Version**: 1.0
**Date**: 2025-12-11
**Purpose**: Specific, actionable documentation improvements
