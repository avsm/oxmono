# Console import

Imported from `../../../../samoht/monopampam/ocaml-console` at monorepo commit
`771b958d4c43456d56780c1bd35edab5719626ca` (2026-10-02). The source
license and notices are retained. `lib/`, its Eio adapter, the upstream README,
and the runnable unit tests are included.

## Local changes

- The upstream `matrix.glyph` dependency is unavailable in this workspace.
  `lib/glyph.ml` supplies the three grapheme operations used by `Width`, using
  `uuseg`, `uucp`, and `uutf`. The width tests cover the substitution.
- `Color.hex` parses its three or six ASCII hex digits locally. This avoids
  linking a second `Ascii` module alongside the private one in
  `vendor/ocaml-codec`.
- The optional `console.vte` adapter, examples, fuzz harness, cram tests, and
  tests requiring `matrix.vte` are omitted. The retained core and Eio tests run
  under `samoht/console/test`.
- The Dune and opam dependency declarations reflect these changes.

Refresh the source from the recorded upstream revision or a reviewed newer
revision, reapply the local changes, then run:

```sh
dune build samoht/console
dune runtest --force samoht/console/test
```

The CLI entry points use `Console_eio.setup` for terminal detection and colour.
Bushel, Sortal, Tessabot, and OwnTracks render selected list commands as tables
on a terminal and keep their previous text formats when output is redirected.
