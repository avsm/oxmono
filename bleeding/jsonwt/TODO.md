# Remaining work

- Review `jsonwt.cwt` independently against its COSE/CWT RFCs before using it
  for authentication. The JWT review does not cover that implementation.
- Add RFC 9864 fully specified Ed25519 identifiers with explicit key binding
  and independent fixtures before recommending Ed25519 for new protocols.
- Add ES256K signing only with independent signing vectors and a reviewed
  private-key API. Spindle currently needs verification only.
- Consider a slice-aware Ed25519 backend and bounded slice base64 decoding if
  profiles show their copies matter. Keep signed bytes and parsed claims tied.

RSA, JWE, nested JWTs, JOSE extensions, certificate handling and JWK sets are
outside the current supported profile. No stub is advertised as implemented.
