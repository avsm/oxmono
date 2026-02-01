# JWT and CWT Implementation TODO

RFC 7519 (JWT) and RFC 8392 (CWT) compliance tracking for jsonwt.

## Implementation Status

### JSON Web Token (JWT) - RFC 7519
- [x] Type definitions for all registered claims
- [x] Type definitions for JOSE header parameters
- [x] Type definitions for algorithms
- [x] Base64url encoding/decoding
- [x] Basic JWT structure parsing (3-part split)
- [x] JSON parsing with jsont
- [x] Signature creation and verification (HMAC, ECDSA, EdDSA)
- [x] Claims validation
- [x] JWK parsing and serialization
- [x] Structured error types
- [x] Comprehensive tests (30 tests passing)

### CBOR Web Token (CWT) - RFC 8392
- [x] Type definitions for COSE algorithms (HMAC, ECDSA, EdDSA)
- [x] Type definitions for COSE keys (Symmetric, Ec2, Okp)
- [x] Claims with integer keys (1=iss, 2=sub, 3=aud, 4=exp, 5=nbf, 6=iat, 7=cti)
- [x] CBOR encoding via cbort library
- [x] COSE_Mac0 structure (MACed CWT)
- [x] COSE_Sign1 structure (Signed CWT)
- [x] Signature creation and verification (HMAC, ECDSA, EdDSA)
- [x] Claims validation (same as JWT)
- [x] Structured error types
- [x] Comprehensive tests (28 tests passing)

### CBOR Codec - RFC 8949
- [x] Low-level CBOR encoding primitives (Cbor_rw)
- [x] Major type constants and encoding functions
- [x] Integer encoding (positive and negative)
- [x] Float encoding (IEEE 754 double precision)
- [x] Text string and byte string encoding
- [x] Array and map encoding
- [x] Tag encoding
- [x] Simple value encoding (false, true, null, undefined)
- [x] Comprehensive tests with RFC 8949 Appendix A vectors (46 tests passing)

---

## Completed Phases

### Phase 0: Error Types and Core Infrastructure - DONE

- [x] Structured error type with all RFC-compliant variants
- [x] `pp_error` and `error_to_string` functions
- [x] StringOrURI validation (validates URIs per RFC 3986)

### Phase 1: JSON Parsing with jsont - DONE

- [x] Header JSON codec (`Header.of_json`, `Header.to_json`)
- [x] Claims JSON codec with strict mode for duplicate detection
- [x] JWK JSON codec for all key types (Oct, RSA, EC, OKP)

### Phase 2: Signature Operations - PARTIALLY DONE

- [x] **HMAC Signatures** (HS256, HS384, HS512) - using digestif
- [x] **ECDSA Signatures** (ES256, ES384, ES512) - using mirage-crypto-ec
- [x] **EdDSA Signatures** (Ed25519) - using mirage-crypto-ec
- [x] **Unsecured JWT** ("none") - with explicit `~allow_none:true` opt-in
- [x] **Nested JWT Support** - `parse_nested` with max_depth protection
- [ ] **RSA Signatures** (RS256, RS384, RS512) - STUBBED, needs JWK-to-RSA key parsing

### Phase 3: Claims Validation - DONE

- [x] Time-based claims (exp, nbf) with leeway support
- [x] Issuer validation (iss)
- [x] Audience validation (aud)
- [x] `is_expired` helper function
- [x] `time_to_expiry` helper function

### Phase 4: Full JWT Creation Flow - DONE

- [x] `create` function for signing JWTs
- [x] Algorithm/key type validation

### Phase 5: Tests - DONE (104 tests passing)

#### JWT Tests (30 tests)

**RFC Test Vectors:**
- [x] RFC 7519 Section 3.1 HS256 JWT
- [x] RFC 7519 Section 6.1 Unsecured JWT

**Algorithm Coverage:**
- [x] HS256 sign/verify
- [x] HS384 sign/verify
- [x] HS512 sign/verify
- [ ] RS256 sign/verify (stubbed)
- [ ] RS384 sign/verify (stubbed)
- [ ] RS512 sign/verify (stubbed)
- [x] ES256 sign/verify
- [x] ES384 sign/verify
- [x] ES512 sign/verify
- [x] EdDSA sign/verify
- [x] none (unsecured) with opt-in

**Validation Tests:**
- [x] Expired token rejection
- [x] Not-yet-valid token rejection
- [x] Issuer mismatch rejection
- [x] Audience mismatch rejection
- [x] Leeway handling

**Error Cases:**
- [x] Invalid base64url
- [x] Invalid JSON
- [x] Wrong number of parts
- [x] Signature mismatch
- [x] Algorithm not in allowed list
- [x] Unsecured JWT without allow_none

#### CWT Tests (28 tests)

**RFC Test Vectors:**
- [x] RFC 8392 Appendix A claims timestamps
- [x] RFC 8392 example values (hex test vectors)

**Algorithm Coverage:**
- [x] HMAC_256_64 (alg=4)
- [x] HMAC_256 (alg=5)
- [x] HMAC_384 (alg=6)
- [x] HMAC_512 (alg=7)
- [x] ES256 (alg=-7)
- [x] ES384 (alg=-35)
- [x] ES512 (alg=-36)
- [x] EdDSA (alg=-8)

**COSE Key Tests:**
- [x] Symmetric key creation
- [x] Ed25519 key creation
- [x] P-256 key creation
- [x] Key ID (kid) support

**Claims Tests:**
- [x] Claims builder
- [x] Timestamp claims (exp, nbf, iat)
- [x] Single and multiple audience
- [x] CWT ID (cti)
- [x] CBOR serialization

**Validation Tests:**
- [x] Expired token rejection
- [x] Not-yet-valid token rejection
- [x] Issuer validation
- [x] Audience validation
- [x] Leeway handling

#### CBOR Tests (46 tests)

**RFC 8949 Appendix A Test Vectors:**
- [x] Unsigned integers (0-1000000000000)
- [x] Negative integers (-1 to -1000)
- [x] Booleans and null
- [x] Floats (1.0, 1.1, -4.1, 1.0e+300, Infinity, NaN)
- [x] Text strings (empty, ASCII, UTF-8 with Unicode)
- [x] Byte strings
- [x] Arrays (empty, nested, 25 items)
- [x] Maps (empty, int keys, string keys, nested)
- [x] Tags (epoch timestamp)
- [x] Constants (major types, simple values, additional info)

---

## Remaining Work

### JWT: RSA Signatures (RS256, RS384, RS512)

**Status:** Stubbed - returns `Key_type_mismatch "RSA signing/verification not yet implemented"`

**Required:**
1. Implement JWK-to-RSA key parsing for `n`, `e` (public) and `d`, `p`, `q`, `dp`, `dq`, `qi` (private) fields
2. Use `mirage-crypto-pk` for RSASSA-PKCS1-v1_5 signatures
3. Add tests with RFC test vectors

### CWT: CBOR Decoding

**Status:** Not implemented - current implementation only encodes CWTs

**Required:**
1. Add CBOR decoding functions to Cbor_rw module
2. Implement `Cwt.parse` to decode CWT from CBOR bytes
3. Add tests with RFC 8392 Appendix A encoded test vectors

### CWT: COSE Key Encoding/Decoding

**Status:** Keys are created in memory but not serialized

**Required:**
1. Add `Cose_key.to_cbor` and `Cose_key.of_cbor` functions
2. Follow RFC 9052 Section 7 (COSE Key) format
3. Add tests with RFC test vectors

### CWT: CWT Tag Support

**Status:** Partial - encodes COSE structures but not outer CWT tag

**Required:**
1. Add support for CWT tag (61) wrapping per RFC 8392 Section 2
2. Add support for optional outer COSE tag per RFC 9052

### Future Work (Not in Current Scope)

1. **JWK Set (JWKS)**: RFC 7517 Section 5 support for multiple keys
   - Useful for key rotation and fetching keys from JWKS endpoints
   - Example: `/.well-known/jwks.json`
   - Consider adding `Jwks.t` type and `Jwks.find_key : kid:string -> Jwks.t -> Jwk.t option`

2. **JWE Support**: RFC 7516 JSON Web Encryption
   - Required for encrypted JWTs (as opposed to signed JWTs)
   - Lower priority unless needed for specific use cases

---

## Design Decisions (Implemented)

1. **StringOrURI validation**: YES - Validates that `iss`/`sub` values containing ":" are valid URIs per RFC 3986.

2. **Duplicate claims**: STRICT MODE - Rejects JWTs with duplicate claim names by default. `~strict:false` allows lenient parsing.

3. **"none" algorithm**: REQUIRE OPT-IN - `~allow_none:bool` parameter to `verify` (default false). Unsecured JWTs rejected unless explicitly allowed.

4. **Error types**: STRUCTURED - Proper error variant type for pattern matching and error handling.

5. **Algorithm allowlist**: YES - `~allowed_algs` parameter to `verify`, defaulting to all algorithms (except none).

6. **Clock source**: EXPLICIT - Always requires `~now:Ptime.t` parameter. No implicit system clock usage.

7. **Nested JWTs**: YES - Support via `parse_nested` with `~max_depth` protection (default 2).

---

## File Summary

| File | Lines | Description |
|------|-------|-------------|
| `lib/jsonwt.ml` | ~1010 | JWT implementation |
| `lib/jsonwt.mli` | ~480 | JWT interface with RFC documentation |
| `lib/cwt.ml` | ~760 | CWT implementation |
| `lib/cwt.mli` | ~400 | CWT interface with RFC documentation |
| `cbort/cbor_rw.ml` | ~200 | Low-level CBOR encoding primitives |
| `cbort/cbor_rw.mli` | ~200 | CBOR encoding interface |
| `cbort/cbort.ml` | ~300 | CBOR codec for jsont types |
| `cbort/cbort.mli` | ~100 | CBOR codec interface |
| `test/test_jsonwt.ml` | ~440 | 30 JWT tests |
| `test/test_cwt.ml` | ~500 | 28 CWT tests |
| `test/test_cbor.ml` | ~320 | 46 CBOR encoding tests |

---

## References

### JWT (JSON Web Token)
- [RFC 7519](https://datatracker.ietf.org/doc/html/rfc7519) - JSON Web Token (JWT)
- [RFC 7515](https://datatracker.ietf.org/doc/html/rfc7515) - JSON Web Signature (JWS)
- [RFC 7517](https://datatracker.ietf.org/doc/html/rfc7517) - JSON Web Key (JWK)
- [RFC 7518](https://datatracker.ietf.org/doc/html/rfc7518) - JSON Web Algorithms (JWA)
- [RFC 8037](https://datatracker.ietf.org/doc/html/rfc8037) - CFRG Elliptic Curve (EdDSA)

### CWT (CBOR Web Token)
- [RFC 8392](https://datatracker.ietf.org/doc/html/rfc8392) - CBOR Web Token (CWT)
- [RFC 9052](https://datatracker.ietf.org/doc/html/rfc9052) - CBOR Object Signing and Encryption (COSE) Structures
- [RFC 9053](https://datatracker.ietf.org/doc/html/rfc9053) - CBOR Object Signing and Encryption (COSE) Algorithms
- [RFC 8949](https://datatracker.ietf.org/doc/html/rfc8949) - Concise Binary Object Representation (CBOR)
