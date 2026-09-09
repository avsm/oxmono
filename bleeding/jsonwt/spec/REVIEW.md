# JWT RFC review, 2026-09-09

## Verdict

The repaired `jsonwt` library is suitable for signature verification in the
OCaml spindle. Spindle now uses it instead of its own JWT parser
and OpenSSL binding. Independently signed malformed tokens are rejected, and
real local PDS tokens complete the Docker job lifecycle.

This is a source review and regression exercise for the profile below, not a
cryptographic certification or a claim to implement all JOSE specifications.
The separate `jsonwt.cwt` library has its own [CWT review](CWT-REVIEW.md).

## Scope and provenance

Reviewed `lib/jsonwt.ml`, its public interface and `lib/crypto_stubs.c`, then
reviewed the authentication integration in `../spindle/lib/auth.ml`. Compared
with the original sibling copy recorded in [UPSTREAM.md](../UPSTREAM.md).
The original NIST EC verification and security failures are intentionally not
preserved by the differential tests.

The accepted wire profile is a compact, signed JWT containing a UTF-8 JSON
claims object. JWE, nested JWTs, detached or unencoded payloads, critical JOSE
extensions, RSA operations and certificate-bound JWKs are unsupported. Unknown
noncritical header members are ignored and never drive key discovery.

The bundled RFC texts, source URLs and hashes are in [README.md](README.md).
RFC 9864 updates the algorithm guidance in RFCs 7518 and 8037. `EdDSA` remains
available for explicitly selected legacy Ed25519 interoperability. Its newer
`Ed25519` identifier and Ed448 are not implemented. Spindle uses ES256K.

## Requirements and findings

### Compact JWS: RFC 7515 sections 4, 5, 7 and appendix C

The parser requires exactly three compact components and JSON objects for the
header and claims. It rejects duplicate members, including escaped spellings
of the same name, before selecting an algorithm. No lenient parsing API remains.
Any `crit` member is rejected because the library implements no JOSE extensions.
`b64` is rejected even when true. RFC 7797 section 7 also forbids unencoded
payloads in JWTs. Nested `cty=JWT` is rejected.

Base64url accepts only the unpadded URL alphabet and canonical trailing bits,
consistent with RFC 4648 sections 3.5 and 5. Verification consumes the original
signed bytes. Abstract tokens prevent a caller from replacing raw bytes while
retaining different parsed claims. Header re-encoding is never substituted for
the original signing input during verification.

Evidence: upstream RFC 7519 section 3.1 HMAC vector, strict-header and canonical
encoding regressions, independent signatures, tampered signatures, and the
compiler test rejecting record forgery.

### Keys: RFC 7517 section 4 and RFC 7518 section 6

JWK import rejects duplicates, malformed recognized metadata, invalid points,
incorrect coordinate/scalar widths and private/public disagreement. It binds
EC and OKP keys to their algorithm. HMAC imports without `alg` remain unbound
until explicitly configured. Operations enforce `alg`, `use=sig` and any
`key_ops` restriction. Certificate parameters are rejected because their key
and usage consistency cannot be checked by this implementation.

Unknown noncertificate JWK members do not affect cryptographic policy. Key
import never fetches certificates or trusts a key merely because it was found
inside a token. `Jwk.to_json` includes private material when the supplied key
contains it. Key confidentiality is a caller responsibility.

Evidence: duplicate/unknown algorithm, metadata, invalid coordinate/curve,
operation restriction, roundtrip and mismatched private/public key tests.

### Signatures: RFC 7518 sections 3.2, 3.4 and 3.6

HMAC uses the selected SHA-2 function, requires keys at least as long as its
hash output and compares signatures with Eqaf. Key entropy cannot be inferred
from length. The API documentation requires randomly generated key material.

NIST ECDSA keys now include the missing SEC1 uncompressed-point prefix. JOSE
signatures use fixed-width `r || s`: 64, 96 or 132 bytes. Verification rejects
other lengths. The Mirage Crypto backend validates curve points and uses
RFC 6979 deterministic signing with RNG-backed masking. `none` requires
explicit opt-in for both creation and verification, with an empty signature.

Evidence: 1500 HMAC differential cases in both signing directions, independent
OpenSSL verification fixtures, all supported native signing roundtrips, invalid
length/tampering checks and unsecured-token policy tests. This reviews backend
use and input validation, not a fresh proof of backend curve arithmetic.

### Ed25519: RFC 8037 sections 2 and 3.1, RFC 8032, RFC 9864

Only the Ed25519 OKP subtype is supported. Public and private keys have the
required widths and must agree. The backend verifies the original signing
input with its Ed25519 routine. RFC 9864 deprecates the polymorphic `EdDSA`
identifier. Applications should not treat its presence in `Algorithm.all` as a
recommendation for new protocols. Every verification requires an allowlist.

Evidence: an independent OpenSSL Ed25519 fixture, malformed signatures and
key pairs, and native signing. Fully specified Ed25519 identifiers remain
outside the advertised algorithm matrix.

### ES256K: RFC 8812 sections 3.1 and 3.2

The key must be EC/secp256k1 and the algorithm ES256K. JWK coordinates are
exactly 32 bytes each. The additional SEC1 constructor accepts the compressed
form carried in PLC multibase keys and stores a validated uncompressed point.
It rejects infinity, invalid encodings and points outside the curve. Signature
verification requires exactly 64 bytes, converts `r || s` to DER and uses
OpenSSL EVP with SHA-256 and secp256k1. ES256K signing is unsupported.

The C bindings bound every borrowed string range. DER storage is sized above
the maximum encoding of two 32-byte integers. All EVP, EC and BIGNUM objects
are freed on success and failure. No global mutable crypto context is shared.

Evidence: independent compressed/uncompressed public-key fixtures, malformed
points and signatures, verification across four domains, and local PDS
interoperability through spindle.

### Claims: RFC 7519 sections 4, 5, 6, 7 and 11

Registered claims retain their original JSON values. Present malformed claims
are errors instead of disappearing. NumericDates must be finite numbers within
Ptime's range. Fractions are retained, expiration is exclusive and not-before
is inclusive. Leeway is nonnegative. StringOrURI values use RFC 3986 validation
without changing case, escaping or other signed claim values. Audience arrays
must contain strings only. Duplicate members are rejected at every object.

JSON uses Jsont and RFC 8259 UTF-8 decoding. The default token limit is 8192
bytes, decoded objects are limited to 64 KiB and nesting to 32 levels before
recursive decoding. Binary64 JSON-number precision and Ptime's representable
range are explicit implementation limits.

`validate` checks claims when present and caller-supplied issuer and audience.
It cannot choose required application claims, token type, identity trust,
maximum lifetime or replay policy. `verify_and_validate` verifies the signature
before applying claim checks. `parse` alone never authenticates anything.

Evidence: string/null/out-of-range dates, expiration equality, fractional dates,
not-before and leeway boundaries, audience types, URI identity, duplicate
members, size/depth limits and malformed ATP credential metadata tests.

### JWT BCP: RFC 8725 section 3

Cryptographic operations require a caller allowlist and a key bound to exactly
one algorithm. The implementation checks key kind, curve, operation and
algorithm together. Unsupported nested and encrypted operations are rejected.
Explicit application typing and issuer-key binding remain at the service
boundary. JSONWT never follows `jku`, embedded `jwk`, `x5u` or token-controlled
key URLs. No compression or password derivation is implemented.

Spindle's application profile was updated on 2026-09-09. It accepts ES256K
and ES256 `#atproto` Multikeys, binds the document ID and key controller to
the verified issuer, checks exact method and audience, and permits the
specific `#tangled_spindle` audience fragment. Bare spindle DIDs remain
accepted for existing Tangled clients. Both relative and absolute verification
method IDs are supported.

## Application profile and replay prevention

The current [ATProto service authentication specification][atproto] requires
`jti` and recommends one-use tokens. Spindle now requires a nonempty bounded
nonce and consumes `(issuer, jti)` in SQLite only after signature verification.
Concurrent reuse and reuse after process restart fail. Nonces expire with the
token; capacity exhaustion fails closed. Forged tokens cannot reserve nonces.

Spindle requires integral `iat` and `exp`, bounds future issuance by 30 seconds,
and limits both lifetime and remaining validity to one hour. The PDS's normal
short-lived service tokens are used by the integration clients. The one-hour
ceiling remains an explicit application policy, broader than the suggested
60-second lifetime. The library provides token primitives; storage and replay
policy correctly remain in the service.

See [spindle's compatibility review](../../spindle/PARITY.md) and
[deployment guide](../../spindle/DEPLOYMENT.md). Signed fixtures now cover both
curves and identity binding; Docker tests use actual local PDS-issued tokens,
restart replay rejection, knot pushes and Tangled's CI API.

[atproto]: https://atproto.com/specs/xrpc#inter-service-authentication-jwt

## Reproduction and allocation evidence

Run the build, forced tests and deterministic Crowbar command in the project
README. Run `python3 bleeding/spindle/testbed/run.py test` for integration.
The testbed contacts the local PDS/PLC and Git fixture, with image/package
fetches during setup. It does not require the live ATP network.

Observed checks: 30 upstream JWT tests, five security regression groups,
1500 bidirectional HMAC cases, NIST/Ed25519 signing, HMAC and ES256K across four
domains, compiler rejection of token forgery, 42 signed spindle auth cases,
and ATP XRPC tests. CWT evidence is recorded separately in its review.

Docker checks passed real authentication, checkout, metadata and directory
listing, CBOR log streaming, rejection cases, failed checkout, discovery,
filtering, pagination, cancellation, completed-job recovery and interruption
recovery. Crowbar exercises six parser properties with 10000 cases each.

For one small pre-parsed HS256 token, 1000 verification iterations measured
2104 OCaml heap bytes per pristine verification and 88 after the port. This
excludes OpenSSL native allocations and is not a whole-request benchmark.
Delimiter and nesting scanners are compiler-checked allocation-free. Parsing
and crypto verification as a whole still allocate.

The scoped Dune formatting checks pass for build files. OCaml formatting is
blocked by the installed 0.29.0 formatter versus the projects' 0.28.1 setting
and OxCaml syntax support. Changed code was formatted by hand under the root
repository instructions. The formatter configuration was not changed.
