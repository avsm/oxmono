# kdf - Key Derivation Functions

## Vendored base (2026-10-03)

Based on `v1.1.2`, commit
`6e2cad01ef7305ba6562a1a626d77e2da130970e`. See [../upstreams.json](../upstreams.json)
for the repository and import scope.

The refresh bounds scrypt allocation before constructing its working buffer.
The existing portable interfaces and implementation annotations are retained.

This repository provides multiple already specified key derivation functions in
and for OCaml:

- [scrypt](https://tools.ietf.org/html/rfc7914),
- [PBKDF 1 and 2 as defined by PKCS#5](https://tools.ietf.org/html/rfc2898),
- and [HKDF](https://tools.ietf.org/html/rfc5869).

## Documentation

[API Documentation](https://robur-coop.github.io/kdf/doc)

## Installation

`opam install kdf` will install the latest released version.

