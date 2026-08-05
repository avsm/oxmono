# RFC Documentation Analysis and Improvement Plan

## Current State Assessment

### ✅ Excellent Current Documentation

Your cookeio library already has **outstanding RFC documentation**. The interface files (`cookeio.mli` and `cookeio_jar.mli`) demonstrate best practices with:

1. **Comprehensive RFC 6265 citations** - Nearly every function, type, and module includes appropriate RFC 6265 section references
2. **Proper OCamldoc formatting** - Using `{{:URL}RFC XXXX Section N}` format consistently
3. **Section-specific links** - Functions link to exact RFC sections (e.g., Section 5.3, Section 5.4)
4. **Clear explanations** - Documentation explains *why* RFC requirements exist, not just *what* they are
5. **Detailed validation docs** - The `Validate` module clearly cites RFC requirements for each validation function

### 📋 RFCs Referenced in the Codebase

| RFC | Status | Description | Referenced In |
|-----|--------|-------------|---------------|
| **RFC 6265** | ✅ Downloaded | HTTP State Management Mechanism (Cookies) | Throughout codebase |
| **RFC 6265bis** | ❌ Missing | Updated Cookie Spec (Draft) | SameSite attribute docs |
| **RFC 1034** | ❌ Missing | Domain Names - Concepts and Facilities | Domain validation |
| **RFC 2616** | ❌ Missing | HTTP/1.1 (obsoleted by RFC 7230-7235) | Token definition |
| **RFC 1123** | ❌ Missing | Internet Host Requirements | Date format |
| **RFC 3339** | ❌ Missing | Date and Time on the Internet | Date parsing |

## Recommendations

### 1. Download Missing RFC Specifications

Add the following RFCs to the `spec/` directory:

```bash
# RFC 6265bis (latest draft)
curl -o spec/rfc6265bis-22.txt https://www.ietf.org/archive/id/draft-ietf-httpbis-rfc6265bis-22.txt

# RFC 1034 - Domain Names
curl -o spec/rfc1034.txt https://datatracker.ietf.org/doc/html/rfc1034.txt

# RFC 2616 - HTTP/1.1 (or RFC 7230 for the modern version)
curl -o spec/rfc2616.txt https://datatracker.ietf.org/doc/html/rfc2616.txt

# RFC 1123 - Internet Host Requirements
curl -o spec/rfc1123.txt https://datatracker.ietf.org/doc/html/rfc1123.txt

# RFC 3339 - Date and Time on the Internet
curl -o spec/rfc3339.txt https://datatracker.ietf.org/doc/html/rfc3339.txt
```

### 2. Documentation Improvements

#### A. README.md Enhancement

The current README.md lacks RFC references. Suggested improvements:

```markdown
# Cookeio - HTTP Cookie Management for OCaml

Cookeio is an OCaml library for managing HTTP cookies, implementing
{{:https://datatracker.ietf.org/doc/html/rfc6265}RFC 6265}
(HTTP State Management Mechanism) with support for modern extensions from
{{:https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis}RFC 6265bis}.

## Standards Compliance

This library provides complete RFC 6265 compliance including:

- Cookie parsing and serialization per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-4}Section 4}
- Domain and path matching per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.1}Section 5.1}
- Storage model implementation per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3}Section 5.3}
- Public Suffix List validation per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3}Section 5.3 Step 5}

### Modern Cookie Attributes

- **SameSite** - Cross-site request protection ({{:https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis#section-5.4.7}RFC 6265bis Section 5.4.7})
- **Partitioned** - CHIPS support for privacy-preserving third-party cookies
```

#### B. Implementation File Comments

Some implementation comments in `cookeio.ml` and `cookeio_jar.ml` could benefit from RFC section references. Examples:

**Current (cookeio.ml:564):**
```ocaml
(* Handle negative values as 0 per RFC 6265 *)
```

**Improved:**
```ocaml
(* Handle negative values as 0 per RFC 6265 Section 5.2.2, which states
   that Max-Age values of zero or less cause immediate cookie expiration *)
```

**Current (cookeio_jar.ml:374):**
```ocaml
(* Sort cookies per RFC 6265 Section 5.4, Step 2: *)
```

**Improved:**
```ocaml
(* Sort cookies per RFC 6265 Section 5.4, Step 2:
   1. Cookies with longer paths listed before cookies with shorter paths
   2. Among equal-length paths, earlier creation-times listed first
   This ensures consistent cookie ordering in HTTP headers *)
```

#### C. Add RFC Context to Error Messages

Consider adding RFC section references to validation error messages for better developer experience:

**Current:**
```ocaml
Error "Cookie name is empty; RFC 6265 requires at least one character"
```

**Enhanced:**
```ocaml
Error "Cookie name is empty; RFC 6265 Section 4.1.1 requires cookie-name \
       to be a non-empty token (see https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1)"
```

### 3. RFC Cross-References

Consider adding a "References" section to your main module documentation that lists all related RFCs:

```ocaml
(** {2 Standards and References}

    This library implements the following IETF specifications:

    {ul
    {- {{:https://datatracker.ietf.org/doc/html/rfc6265}RFC 6265} -
       HTTP State Management Mechanism (April 2011)}
    {- {{:https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis}RFC 6265bis} -
       Updated Cookie Specification (Draft) - SameSite attribute}
    {- {{:https://datatracker.ietf.org/doc/html/rfc1034}RFC 1034} -
       Domain Names (Section 3.5 for domain name syntax)}
    {- {{:https://datatracker.ietf.org/doc/html/rfc2616}RFC 2616} -
       HTTP/1.1 (Section 2.2 for token syntax)}
    {- {{:https://datatracker.ietf.org/doc/html/rfc1123}RFC 1123} -
       Internet Host Requirements (date format)}}

    Additional standards:
    {ul
    {- {{:https://publicsuffix.org/}Public Suffix List} - Mozilla's registry
       of public suffixes for cookie domain validation}} *)
```

### 4. RFC Compliance Testing Documentation

Your test file `test_cookeio.ml` has excellent RFC compliance tests. Consider adding a test suite summary that maps tests to RFC sections:

```ocaml
(** {1 RFC 6265 Compliance Test Suite}

    This test suite validates compliance with RFC 6265:

    - Domain matching (Section 5.1.3): {!test_ip_address_domain_matching}
    - Path matching (Section 5.1.4): {!test_path_matching}
    - Cookie ordering (Section 5.4 Step 2): {!test_cookie_ordering}
    - Creation time preservation (Section 5.3 Step 11.3): {!test_creation_time_preservation}
    - Public suffix validation (Section 5.3 Step 5): {!test_public_suffix_validation}

    See RFC-TODO.md for implementation status of all RFC requirements. *)
```

### 5. Specific Function Documentation Improvements

#### Function: `normalize_year` (cookeio.ml:425)

**Current:**
```ocaml
(** Normalize abbreviated years per RFC 6265. *)
```

**Improved:**
```ocaml
(** Normalize abbreviated years per RFC 6265 Section 5.1.1.

    Two-digit year conversion follows RFC 6265 Section 5.1.1:
    - Years 70-99 → add 1900 (1970-1999)
    - Years 0-69 → add 2000 (2000-2069)

    This algorithm handles the Y2K transition and provides a 100-year window
    centered on the year 2000 for date interpretation.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.1> RFC 6265 Section 5.1.1 - Dates *)
```

#### Function: `strip_leading_dot` (cookeio.ml:391)

**Current:**
```ocaml
(** Remove leading dot from domain attribute. *)
```

**Improved:**
```ocaml
(** Remove leading dot from domain attribute per RFC 6265 Section 5.2.3.

    The RFC requires that any leading dot in the Domain attribute value be
    ignored. This normalization ensures consistent domain matching behavior
    regardless of whether the Set-Cookie header includes a leading dot.

    Example: ".example.com" and "example.com" are treated identically.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.3> RFC 6265 Section 5.2.3 - The Domain Attribute *)
```

### 6. Documentation Build Integration

Add a check to your CI/build process to ensure RFC files are present:

```bash
# In your build script or CI
for rfc in rfc6265.txt rfc1034.txt rfc2616.txt rfc1123.txt rfc3339.txt; do
  if [ ! -f "spec/$rfc" ]; then
    echo "Warning: spec/$rfc missing. Documentation may reference unavailable specifications."
  fi
done
```

## Summary

Your project already demonstrates **excellent RFC documentation practices**. The suggested improvements are minor enhancements that would:

1. Make all referenced RFCs locally available for offline development
2. Add RFC context to a few remaining undocumented areas
3. Enhance error messages with RFC references for better developer experience
4. Create a centralized RFC reference section in documentation

The current state is already production-ready and sets a high bar for RFC compliance documentation in OCaml projects.

## Priority Order

1. **High Priority**: Download missing RFCs to `spec/` directory
2. **Medium Priority**: Add References section to main module documentation
3. **Low Priority**: Enhance internal implementation comments
4. **Optional**: Add RFC references to error messages

---

**Document Status**: Initial Analysis
**Date**: 2025-12-11
**Reviewer**: RFC Documentation Specialist
