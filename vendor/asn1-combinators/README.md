# asn1-combinators — Embed typed ASN.1 grammars in OCaml

## Vendored base (2026-10-03)

Based on `v0.3.3`, commit
`2dc40ec4db5bad9fc0e4a45331c88a0b43d16131`. See [../upstreams.json](../upstreams.json)
for the repository and import scope.

The refresh adopts upstream domain-safe error formatting. The existing portable
ASN.1 combinators, decoders and error functions are retained.

v0.3.3

asn1-combinators is a library for expressing ASN.1 in OCaml. Skip the notation
part of ASN.1, and embed the abstract syntax directly in the language. These
abstract syntax representations can be used for parsing, serialization, or
random testing.

The only ASN.1 encodings currently supported are BER and DER.

asn1-combinators is distributed under the ISC license.

## Documentation

`asn.mli`, [online][doc].

[doc]: https://mirleft.github.io/ocaml-asn1-combinators/doc

[![Build Status](https://travis-ci.org/mirleft/ocaml-asn1-combinators.svg?branch=master)](https://travis-ci.org/mirleft/ocaml-asn1-combinators)

