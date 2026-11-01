# SQLite3 binding provenance

The binding base is upstream tag `5.3.1`, commit
`4c8892f6dd4a4ff2ba35f6e1f94a0403a4b9c010` in
<https://github.com/mmottl/sqlite3-ocaml>.

Compared on 2026-11-01 against the original OxMono import
`5ab071c5db3356032ee0d1831b7e59ffe344acb9:bleeding/sqlite3`:

- `lib/sqlite3.ml` differs only by the added `interrupt` external.
- `lib/sqlite3_stubs.c` differs only by `caml_sqlite3_interrupt`.
- `lib/sqlite3.mli` adds the corresponding interrupt documentation.
- The licence and `test/test_fun.ml` match byte for byte.

Local additions bundle SQLite, provide the Eio wrapper and adapt the build.
Later patches add OxCaml modes and callback constraints. Re-vendoring must
retain those adaptations and check the binding, Eio and portability tests.
The bundled SQLite version is recorded in `vendor/sqlite3/VERSION`; it is
separate from the OCaml binding version.
