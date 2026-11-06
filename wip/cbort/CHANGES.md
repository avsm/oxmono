# Unreleased

- Grow long byte and text strings as input arrives instead of allocating
  the declared length, so a forged header fails with `End_of_file`
  rather than exhausting memory.

- Clarify opam descriptions of package functionality and interfaces.

- Fix string skipping and reject overflowing lengths, invalid indefinite
  headers and reserved simple values. `read_int` rejects signed overflow.

- Reduce temporary allocation in CBOR header and integer decoding while
  preserving the public API. Requires OxCaml on OCaml 5.2 or later.
