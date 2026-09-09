# JSONWT

Bounded compact JWT parsing, signing and verification for OxCaml. The `jsonwt`
library uses immutable strings and has no Cstruct dependency. It performs no
network access or key discovery. See [the RFC review](spec/REVIEW.md) for the
supported profile and [provenance](UPSTREAM.md) for the imported sources.

## Algorithms

| JOSE name | Key | Sign | Verify |
| --- | --- | --- | --- |
| HS256, HS384, HS512 | HMAC, at least 32, 48, 64 random bytes | Yes | Yes |
| ES256, ES384, ES512 | P-256, P-384, P-521 | Yes | Yes |
| ES256K | secp256k1 | No | Yes |
| EdDSA (legacy) | Ed25519 | Yes | Yes |
| none | Unsecured, explicit opt-in | Yes | Yes |

RFC 9864 deprecates the legacy `EdDSA` identifier. The new `Ed25519` identifier
is not yet implemented.

RSA identifiers are recognized but operations are unsupported. JWE, nested
JWTs, detached or unencoded payloads, JOSE extensions, certificate-bound JWKs
and JWK sets are outside this library's profile.

## Verification

Supply a trusted key and an explicit algorithm allowlist. HMAC keys must also
be bound to one algorithm with `Jwk.with_alg`, or with `alg` in the input JWK.
EC and Ed25519 constructors bind the algorithm from the curve. `Jwk.of_json`
validates key material and enforces supplied `alg`, `use` and `key_ops`.
It does not establish who owns the key.

```ocaml
let verify_service_token ~public_sec1 ~now ~issuer ~audience raw =
  let ( let* ) = Result.bind in
  let* key = Jsonwt.Jwk.secp256k1_pub public_sec1 in
  let* token = Jsonwt.parse raw in
  let* () = Jsonwt.verify_and_validate ~key ~now
      ~allowed_algs:[Jsonwt.Algorithm.ES256K]
      ~iss:issuer ~aud:audience token in
  Ok (Jsonwt.claims token)
```

The example checks the signature, issuer, audience and any expiration or
not-before value present. Applications must require their necessary claims,
check `typ`, constrain `iat` and lifetime, and enforce custom claims and replay
policy. Parsed claims are untrusted until these checks succeed. Spindle's
[authentication module](../spindle/lib/auth.ml) supplies its service policy.

`parse` defaults to an 8192-byte compact-token limit. Decoded JSON objects are
limited to 64 KiB and 32 levels of nesting. Duplicate object members, malformed
registered claims, noncanonical base64url and unknown critical extensions are
errors. Fractional NumericDates are accepted within Ptime's range. JSON numbers
use binary64 precision. `exp = now` is expired with zero leeway.

Tokens and claims are abstract. Access them with `header`, `claims`, `raw` and
`signature`. `encode` preserves the original signed serialization exactly.
Claim builders replace existing members and reject invalid values at `build`.
Signing uses `create ~header ~claims ~key ()`. Unsecured creation requires
`~allow_none:true`. Unsecured verification also requires `None` in the explicit
allowlist. ECDSA signing requires an initialized Mirage Crypto RNG for masking.

## OxCaml and allocation

The public token, claim and key types carry `immutable_data`. Parsing,
verification, validation and the secp256k1 public-key constructor are checked
`portable`. Signing and general JWK import retain the backend's nonportable
interface.

The compact parser retains the original string and its signed-prefix length.
Delimiter and nesting scans use `let mutable` and compiler-checked
`[@zero_alloc]`. URI validation uses Uriz's local interface. HMAC and ES256K
borrow the original signed prefix in OpenSSL. Other ECDSA verifiers hash a
string slice. Ed25519 still copies that prefix for its backend API. JSON and
base64 decoding allocate, and OpenSSL allocates native crypto contexts.

## Build and test

Use the monorepo's `5.2.0+ox` switch and OpenSSL 3 development headers.

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check \
  @bleeding/jsonwt/all
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check --force \
  bleeding/jsonwt/test
opam exec --switch=5.2.0+ox -- dune exec --profile release-check \
  bleeding/jsonwt/fuzz/fuzz_jsonwt.exe -- -r 10000 -s 7519
```

Tests include unchanged upstream source snapshots for HMAC differential checks,
independent OpenSSL signatures, malformed-token regressions, compiler rejection
of token record forgery and verification across four domains. Fixture generators
retain public keys only. Regenerating fixtures changes their random signatures.

## Legacy CWT

The unchanged CWT implementation is a separate `jsonwt.cwt` library, exposed
as `Jsonwt_cwt`. Its existing tests still run. Its Cstruct/COSE dependencies and
security limitations are separate from JWT. CWT has not received this RFC
security review and is not suitable on this review's evidence for an
untrusted-token authentication boundary. Spindle does not link it.
