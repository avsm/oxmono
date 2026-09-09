# Specification sources

RFC texts are unmodified copies from RFC Editor. JWT-related texts were
retrieved on 2026-09-09. The CWT/CBOR/COSE texts were retained from the original
import. `SHA256SUMS` pins the exact local bytes. RFC copyright and license
notices remain in each document.

The [review](REVIEW.md) describes the selected JWT profile. Inclusion here does
not claim full implementation of a document. RFC 9864 updates RFCs 7518, 8037
and 9053. The separate [CWT review](CWT-REVIEW.md) covers a bounded Sign1/Mac0 profile.

- [RFC 3986](https://www.rfc-editor.org/rfc/rfc3986.txt):
  URI syntax used for StringOrURI claims.
  Local copy: [rfc3986.txt](rfc3986.txt).
- [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648.txt):
  Base64url alphabet and canonical pad bits.
  Local copy: [rfc4648.txt](rfc4648.txt).
- [RFC 6979](https://www.rfc-editor.org/rfc/rfc6979.txt):
  Deterministic ECDSA backend reference.
  Local copy: [rfc6979.txt](rfc6979.txt).
- [RFC 7515](https://www.rfc-editor.org/rfc/rfc7515.txt):
  Compact JWS and protected header processing.
  Local copy: [rfc7515.txt](rfc7515.txt).
- [RFC 7517](https://www.rfc-editor.org/rfc/rfc7517.txt):
  JWK syntax and policy metadata.
  Local copy: [rfc7517.txt](rfc7517.txt).
- [RFC 7518](https://www.rfc-editor.org/rfc/rfc7518.txt):
  HMAC and NIST ECDSA algorithms and key formats.
  Local copy: [rfc7518.txt](rfc7518.txt).
- [RFC 7519](https://www.rfc-editor.org/rfc/rfc7519.txt):
  JWT claims and validation.
  Local copy: [rfc7519.txt](rfc7519.txt).
- [RFC 7797](https://www.rfc-editor.org/rfc/rfc7797.txt):
  Unencoded JWS payload extension, rejected for JWT.
  Local copy: [rfc7797.txt](rfc7797.txt).
- [RFC 8032](https://www.rfc-editor.org/rfc/rfc8032.txt):
  Ed25519 backend reference.
  Local copy: [rfc8032.txt](rfc8032.txt).
- [RFC 8037](https://www.rfc-editor.org/rfc/rfc8037.txt):
  Legacy EdDSA/OKP JOSE representation.
  Local copy: [rfc8037.txt](rfc8037.txt).
- [RFC 8259](https://www.rfc-editor.org/rfc/rfc8259.txt):
  UTF-8 JSON syntax and interoperability.
  Local copy: [rfc8259.txt](rfc8259.txt).
- [RFC 8392](https://www.rfc-editor.org/rfc/rfc8392.txt):
  CWT claims and protection.
  Local copy: [rfc8392.txt](rfc8392.txt).
- [RFC 8725](https://www.rfc-editor.org/rfc/rfc8725.txt):
  JWT security best current practice.
  Local copy: [rfc8725.txt](rfc8725.txt).
- [RFC 8812](https://www.rfc-editor.org/rfc/rfc8812.txt):
  secp256k1 JWK and ES256K signatures.
  Local copy: [rfc8812.txt](rfc8812.txt).
- [RFC 8949](https://www.rfc-editor.org/rfc/rfc8949.txt):
  CBOR syntax, validity and resource limits.
  Local copy: [rfc8949.txt](rfc8949.txt).
- [RFC 9052](https://www.rfc-editor.org/rfc/rfc9052.txt):
  COSE structures, headers and key policy.
  Local copy: [rfc9052.txt](rfc9052.txt).
- [RFC 9053](https://www.rfc-editor.org/rfc/rfc9053.txt):
  COSE algorithms and key parameters.
  Local copy: [rfc9053.txt](rfc9053.txt).
- [RFC 9864](https://www.rfc-editor.org/rfc/rfc9864.txt):
  Updates JWA/EdDSA algorithm guidance.
  Local copy: [rfc9864.txt](rfc9864.txt).

Verify local files with `sha256sum -c SHA256SUMS` in this directory. Refresh
from the exact source URLs, inspect RFC updates and errata at RFC Editor, and
update the review and hashes together. Published texts are retained verbatim,
without folding errata into them.
