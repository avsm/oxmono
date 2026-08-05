# jsonwt

JSON Web Token (JWT) and CBOR Web Token (CWT) implementation for OCaml.

## Overview

A type-safe implementation of JWT (RFC 7519) and CWT (RFC 8392) with full support for:

- **JWT parsing and creation** - Compact serialization format
- **Signature verification** - HMAC, RSA, ECDSA, and EdDSA algorithms
- **Claims validation** - Expiration, not-before, issuer, audience checks
- **JSON Web Key (JWK)** - Key representation per RFC 7517
- **Nested JWTs** - Recursive parsing with depth limits
- **CBOR Web Tokens** - RFC 8392 for constrained environments

## Installation

```
opam install jsonwt
```

## Usage

### Parsing and Verifying a JWT

```ocaml
let token_string = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..." in
match Jsonwt.parse token_string with
| Ok jwt ->
    let key = Jsonwt.Jwk.symmetric "secret-key" in
    (match Jsonwt.verify ~key jwt with
    | Ok () -> print_endline "Valid signature"
    | Error e -> print_endline (Jsonwt.error_to_string e))
| Error e -> print_endline (Jsonwt.error_to_string e)
```

### Creating a JWT

```ocaml
let header = Jsonwt.Header.make ~typ:"JWT" Jsonwt.Algorithm.HS256 in
let claims = Jsonwt.Claims.(
  empty
  |> set_iss "https://example.com"
  |> set_sub "user123"
  |> set_exp (Ptime.of_float_s 1700000000.0 |> Option.get)
  |> build
) in
let key = Jsonwt.Jwk.symmetric "secret-key" in
match Jsonwt.create ~header ~claims ~key with
| Ok jwt -> print_endline (Jsonwt.encode jwt)
| Error e -> print_endline (Jsonwt.error_to_string e)
```

## Supported Algorithms

| Algorithm | Description |
|-----------|-------------|
| HS256/384/512 | HMAC with SHA-2 |
| RS256/384/512 | RSASSA-PKCS1-v1_5 with SHA-2 |
| ES256/384/512 | ECDSA with P-256/384/521 |
| EdDSA | Ed25519 signatures |
| none | Unsecured (requires explicit opt-in) |

## References

- [RFC 7519](https://datatracker.ietf.org/doc/html/rfc7519) - JSON Web Token (JWT)
- [RFC 7515](https://datatracker.ietf.org/doc/html/rfc7515) - JSON Web Signature (JWS)
- [RFC 7517](https://datatracker.ietf.org/doc/html/rfc7517) - JSON Web Key (JWK)
- [RFC 7518](https://datatracker.ietf.org/doc/html/rfc7518) - JSON Web Algorithms (JWA)
- [RFC 8392](https://datatracker.ietf.org/doc/html/rfc8392) - CBOR Web Token (CWT)

## License

ISC
