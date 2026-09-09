# Source provenance

This project was already imported into `bleeding/jsonwt` by oxmono commit
`dece62175`. Before these repairs, `lib/jsonwt.ml` and `lib/jsonwt.mli` were
byte-identical to `../monopampam/ocaml-jsonwt`.

The full sibling revision is recorded below. Original files retain their
copyright notices and ISC license. The existing fuzz driver carries its own
MIT notice.

```
Sibling import: d24430fd7f622250a14efde60f76119d4f000393
jsonwt.ml:  d3275098b9b20cf3ee5a38c566463086d85c12daa8d474b0bb1d72889b00a199
jsonwt.mli: 9781e99933dad7f4bf4cb4302cdbc22998a9adcde4e17bb005f61dafa1af70ba
```

`test/pristine` holds unchanged copies of these two files and the original JWT
test source. Dune compiles the snapshots under `Jsonwt_pristine` with an empty
CWT module because the JWT differential does not use CWT. Snapshots are test
inputs, not installed libraries.

## Local changes

- `Json`, `Header`, `Claims`, base64 and compact parsing enforce bounded strict
  decoding. Tokens and keys become abstract. The RFC review names each repaired
  security invariant and its regression tests.
- `Jwk` validates key material, algorithm and operation metadata. SEC1 encoding
  for the original NIST curves is corrected. Unsupported RSA stubs are removed.
- `crypto_stubs.c` adds OpenSSL 3 ES256K verification and point validation,
  derived from spindle's former verifier. HMAC uses a fresh EVP context and
  borrows its input prefix. Other algorithms retain the existing backends.
- Portable signatures, immutable kinds, local URI parsing, mutable local
  counters and an unboxed delimiter tuple target OxCaml minus-39.
- `cwt/jsonwt_cwt.ml` and `.mli` are the original CWT sources moved unchanged.
  Their public path changes from `Jsonwt.Cwt` to `Jsonwt_cwt` in `jsonwt.cwt`.
- Original JWT tests are adapted to abstract accessors, explicit algorithm
  policy and `create ... ()`. CWT tests initialize the backend RNG and use the
  new module path. New tests cover the deliberately stricter behavior.
- Build metadata, documentation and bundled RFCs describe the supported
  profile. Certificate-bound JWKs and JOSE extensions are explicitly rejected.

## Refresh checks

1. Compare the sibling source with `test/pristine` before importing changes.
   Do not replace the snapshots just to remove a differential failure.
2. Preserve token/claim agreement, key policy and parsing bounds. Inspect
   upstream changes against `spec/REVIEW.md` and the pinned RFC texts.
3. Run the JSONWT, ATP XRPC and spindle build and forced test aliases. Run
   the deterministic Crowbar command in the README and the spindle Docker
   testbed. Do not use the live ATP network for interoperability tests.
4. Compare supported HMAC behavior against the pristine source in both signing
   directions. Keep deliberate stricter failures in the security tests.
   EC verification is checked against OpenSSL, since pristine EC verification
   contains the defect repaired here.
5. Update provenance, RFC hashes and the review when the supported profile or
   crypto backend changes. Never retain generated private fixture keys.
