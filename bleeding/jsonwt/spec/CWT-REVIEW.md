# CWT RFC review, 2026-09-09

## Verdict and usage

The repaired library supports bounded COSE_Sign1 and COSE_Mac0 verification
with explicit key and algorithm policy. Its original authentication bypasses
and parser allocation failures have regression coverage. Use it only with an
application profile that supplies trusted keys, requires appropriate claims
and handles token purpose, lifetime and replay. This is source review and
interoperability testing, not cryptographic certification.

No production consumer in this tree uses CWT. Searches for `Jsonwt_cwt`,
`Jsonwt.Cwt` and `jsonwt.cwt` find implementation, documentation, tests and
fuzzing only. Spindle links JWT and uses ATProto service JWTs. CWT is not an
alternative token format for those endpoints.

The original files and their hashes are recorded in [UPSTREAM.md](../UPSTREAM.md).
The unchanged CWT snapshot remains a test input for differential checks.
Bundled specifications and their source URLs are in [README.md](README.md).

## Findings and repairs

- RFC 9052 sections 4.4 and 6.3: authenticate transmitted bytes. Verification
  serialized parsed claims again. Reordering, duplicate insertion and malformed
  expiry injection could preserve acceptance without a new MAC. Retain the
  original payload and protected bytes. Independently MACed reordered and
  indefinite maps verify. Unsigned mutations fail.

- RFC 8392 sections 6 and 7: identify protection. Tag 61 was rejected, but a
  Sign1 tag on an HMAC envelope was accepted. Accept outer CWT tag only around
  the correct COSE tag. Match the tag to the permitted algorithm. RFC A.3 and
  A.4 signatures now must verify.

- RFC 9052 sections 3.1 and 9: validate headers. Critical extensions and
  duplicate algorithms were ignored. An unprotected key ID overrode a protected
  one. Require typed header maps and a protected algorithm. Reject duplicate or
  overlapping labels, crit, countersignatures and IVs. Key IDs must be byte
  strings.

- RFC 9052 section 7.1: enforce key policy. alg and key_ops were ignored, and
  unknown algorithms disappeared on import. Require an algorithm binding and
  verification allowlist. Enforce recognized nonempty key_ops, key family and
  operation. Reject unknown algorithms and unsupported Base IVs.

- RFC 9053 sections 2, 3.1 and 7: validate keys. Invalid points, inconsistent
  private/public keys and empty HMAC keys were accepted. Reuse reviewed JWK
  point validation. Require hash-sized HMAC keys and exact signature lengths.
  Test all supported algorithms against OpenSSL or independent HMAC fixtures.

- RFC 8392 sections 2 and 3: preserve claim meaning. Wrong claim types,
  fractional dates and invalid UTF-8 were discarded or accepted. Creation and
  parsing disagreed about expiry and kid. Validate registered types, finite
  dates, StringOrURI and byte-string cti. Preserve fractional values and
  original strings. Protect emitted key IDs. Builders replace existing labels.

- RFC 8392 section 3.1.4: expiration is exclusive. exp equal to now passed.
  Reject equality with zero leeway. Check fractional exp/nbf boundaries and
  reject negative leeway.

- RFC 8949 sections 3, 5 and 10: bounded valid CBOR. Trailing bytes were
  ignored, integer conversions raised, and declared lengths allocated before
  checking input. A nine-byte input requesting 4 GiB aborted a memory-limited
  process. Use a private bounded codec. Check available bytes before copying,
  reject reserved encodings, validate each UTF-8 chunk, bound nesting and item
  counts, and consume the complete input. Regression tests include the tiny
  hostile lengths.

The generic Cbort decoder remains unchanged. CWT uses its immutable CBOR value
model, with its own decoder and encoder. This review does not make Cbort's
unbounded generic decoder suitable for untrusted input. The production CWT
code has no Cstruct dependency. Pristine JWT tests still require Cstruct.

## Supported profile and limits

- Sign1 supports ES256/P-256, ES384/P-384, ES512/P-521 and EdDSA/Ed25519.
  Mac0 supports HMAC 256/64, 256/256, 384/384 and 512/512.
  HMAC keys must contain at least 32, 48 or 64 random bytes respectively.
  RFC 9864 deprecates these polymorphic EC/EdDSA identifiers. Fully specified
  replacements are not implemented, so these are compatibility algorithms.
- Encryption, nested protection, multiple signatures or recipients, detached
  payloads, external AAD and compressed EC key coordinates are unsupported.
  Every critical extension and countersignature is rejected. Unknown
  noncritical headers do not drive key discovery. An unprotected kid remains
  an untrusted hint, and verification never establishes ownership of a key.
- Parsing defaults to 8192 bytes, with a hard 64 KiB cap. Each CBOR decode has
  depth 32 and 4096-item limits. All maps require integer or text labels.
  Claim integer labels must fit native integers. CBOR integer values preserve
  the full unsigned 64-bit argument range. Larger integers need explicit tags
  in custom claims. Registered NumericDates reject tags, NaN, infinity and
  values outside Ptime's range. Time accessors use binary64 precision.
- Definite and indefinite input lengths and valid nonminimal integers are
  accepted. Generated Sig_structure and MAC_structure use definite lengths
  and minimal arguments as RFC 9052 section 9 requires. Authentication uses
  original protected/payload byte strings regardless of their encoding.
- Registered claims are optional at library level. Applications must require
  their issuer, audience, expiry, issuance time and purpose. cti is preserved
  but neither generated nor remembered for replay detection.

Parsed values and keys are abstract and immutable. Parsing, validation and
verification have portable interfaces proven by captures in domain closures.
The decoder cursor is local. General key import and signing keep their
backend's nonportable interface. ECDSA signing needs an initialized RNG.
Decoding strings, encoding signing structures and cryptographic contexts still
allocate. This is not a zero-allocation token implementation.

## Verification

The commands in the project README run these checks under OxCaml minus-39:

- 45 adapted upstream CWT tests, including successful verification of RFC 8392
  A.3 ES256 and A.4 tagged HMAC 256/64. The RFC's AES-CCM key is correctly
  rejected because encryption is unsupported.
- Four security groups covering wire mutations, claims, headers/keys and CBOR
  syntax/resource limits.
- 2000 HMAC differential cases against pristine CWT in both directions,
  including byte identity for their shared canonical profile.
- NIST-curve and Ed25519 signing, independent public-only OpenSSL fixtures,
  tampered signatures, and all four signature algorithms across four domains.
  Portable HMAC parsing and verification also share immutable values.
- Four CWT Crowbar properties, 10000 cases each with seed 8392, alongside the
  six JWT properties. Inputs exercise complete tokens, claims, keys and
  structured COSE envelopes without crashes.

The integrated JWT/spindle Docker suite passes PDS-issued authentication,
checkout, metadata, listing, CBOR logs, rejection cases, cancellation and
restart recovery. PLC resolution is also tested over the local TLS gateway
with the test CA. That integration uses JWT only.
