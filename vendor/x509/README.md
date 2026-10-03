## Vendored base (2026-10-03)

Based on `v1.2.0-10-g6f4baca`, commit
`6f4bacaec0454bbd8f2d89966cba3b202ef4f729`. See [../upstreams.json](../upstreams.json)
for the repository and import scope.

The refresh includes typed distinguished names, SAN-only service identities,
name-constraint union semantics and PKCS#12 iteration limits. Local portable
decoders, validation and fresh empty sets are retained. The new attribute
value helpers and comparisons carry compiler-checked portable annotations,
and the internal encoding functor requires immutable encoding values. HTTPz
and Fetch certificate tests construct CN values with `Common_name.v`.

## X.509 - Public Key Infrastructure purely in OCaml

v1.2.0
X.509 is a public key infrastructure used mostly on the Internet.  It consists
of certificates which include public keys and identifiers, signed by an
authority.  Authorities must be exchanged over a second channel to establish the
trust relationship.  This library implements most parts of
[RFC5280](https://tools.ietf.org/html/rfc5280) and
[RFC6125](https://tools.ietf.org/html/rfc6125). The
[Public Key Cryptography Standards (PKCS)](https://en.wikipedia.org/wiki/PKCS)
defines encoding and decoding in ASN.1 DER and PEM format, which is also
implemented by this library - namely PKCS 1, PKCS 7, PKCS 8, PKCS 9 and PKCS 10.

Read our [Usenix Security 2015 paper](https://www.usenix.org/conference/usenixsecurity15/technical-sessions/presentation/kaloper-mersinjak).

## Documentation

[API documentation](https://mirleft.github.io/ocaml-x509/doc)

## Installation

`opam install x509` will install this library.
