## base64 - Base64 encoding for OCaml

This is base64 3.5.2, vendored from https://github.com/mirage/ocaml-base64 and
patched for OxCaml portability.

The code patch touches `base64.ml` and `base64.mli` only:

* `type alphabet` holds two immutable `string` lookup tables rather than two
  mutable `int array` tables, so the top-level `default_alphabet` and
  `uri_safe_alphabet` can be read from a `portable` function without a
  contention error. The decode table stores the sentinel `'\xff'` where the
  upstream array stored `-1`.
* The three `%caml_bytes_set16u`, `%caml_string_get16u` and `%bswap16`
  primitives carry `@@ portable` in their type. An `external` declared in a
  structure is nonportable otherwise, and every encode and decode path reaches
  one of them.
* `base64.mli` is annotated `@@ portable`, and `type alphabet` is given the
  `immutable_data` kind.

Encoding and decoding behaviour, including padding handling, bounds checks and
the error messages, is unchanged. The `base64.rfc2045` sublibrary is vendored
unpatched.

What else differs from the upstream distribution:

* `base64.opam` is the upstream file with the `with-test` dependencies, the
  `dune subst` build step and `x-maintenance-intent` dropped, since the
  workspace builds this copy directly, and with a line in the description
  recording the patch.
* `dune`, `rfc2045/dune`, `dune-project` and `test/` are written for this
  workspace rather than taken from upstream.
* Upstream's `CHANGES.md` is not vendored. The release this copy came from is
  named in `base64.opam` and in the header of `base64.mli`.

`test/` pins the RFC 4648 vectors and the portability of the interface. Dune
skips aliases under a vendored directory, so `dune runtest` does not reach it
and it must be run by name:

    dune exec vendor/base64/test/test_base64.exe

Because that test is inert in an ordinary build, `avsm/arod/test/test_base64.ml`
holds a smaller copy of the same two guarantees, outside `vendor/` and so under
`dune runtest`. A re-vendor that drops the patch fails there.
