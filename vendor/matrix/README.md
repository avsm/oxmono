# Matrix terminal primitives

Imported the `matrix/lib/{ansi,glyph,grid,vte,pty}` subset of
https://github.com/invariant-hq/mosaic at tag `0.1.0`, commit
`bbaec9a2b49eccc7be958df1a3fc3f53443787b8`, on 2026-11-01. Those directories
are mapped directly to this directory. LICENSE is retained. Other Mosaic
packages, examples and tests are omitted. This is the terminal toolkit,
separate from the repository's `matrix-chat` package.

Local adaptations: a standalone Dune package and portable annotations on the
pure Unicode lookups and grapheme iterator. The segmenter's two constant
lookup arrays become pattern matches so portable closures capture no mutable
arrays. Per-call segmenter state remains private to the caller.

On re-vendoring, retain the subset and Unicode tables together, reapply the
annotations and lookup changes, update `../upstreams.json`, and run Console
and K7's explicit test aliases plus `@u/avsm/arod/test/console_port/runtest`.
The latter compares segmentation with the unmodified Uuseg implementation.
