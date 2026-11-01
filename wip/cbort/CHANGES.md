# Unreleased

- Fix string skipping and reject overflowing lengths, invalid indefinite
  headers and reserved simple values. `read_int` rejects signed overflow.

- Reduce temporary allocation in CBOR header and integer decoding while
  preserving the public API. Requires OxCaml on OCaml 5.2 or later.
