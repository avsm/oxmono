# HTTP stack synchronization

This record describes the last recorded imports, not equivalence with current
upstream tips. The [HTTP guide](../../docs/http.md) covers usage.

## Recorded sources

- Stack baseline: `avsm/oxcaml-httpz`
  `2eb1eb0b8ef2a6f300b44e514b8826ade53dc02b` (2026-09-07), incorporating
  stock `avsm/ocaml-httpz` through `72e4541`.
- Selective DAV synchronization: OxMono
  `e2531cf8f01344cdc53ff51a80d19d198e8f8507`, standalone OxCaml
  `9d59fac5d8f877969824d030c26c44d2898df98d`, stock OCaml
  `9960477b34025a8ce257f5c921109f6310f83505` (2026-09-08).
- Subsequent selective stock import:
  `0e56f3fd7462159dac82c537ca55f765bdc3b1c5` (2026-09-08), adding DAV
  collection objects and href helpers. See [DAV synchronization](dav/SYNC.md).

Independent vendor revisions belong in
[vendor/upstreams.json](../../vendor/upstreams.json), not in this record.
The old comparison counts and dated test transcripts remain in Git history.

## Path mapping

| Standalone | OxMono |
| --- | --- |
| `httpz/` | `bleeding/httpz/` |
| `fetch/` | `bleeding/fetch/` |
| `proffer/` | `bleeding/proffer/` |
| `example/` | `example/` |
| `test/release/` | `bleeding/fetch/test/release/` |
| `test/dependencies/` | HTTPz dependency tests and Fetch facade tests |

## Adaptations to preserve

- `httpz.uri` adapts the shared `Uriz.t` from `vendor/ocaml-uri`. Replacing
  it with standalone's private URI implementation breaks shared type identity.
- Portable Jsont/Bytesrw codecs use portable callbacks and factory defaults.
  Portable Duration, Mtime and Cmarkit interfaces remove the need for the
  standalone boundary assertions. Check current interfaces before copying casts.
- Eio, TLS/X.509 and the other vendors are synchronized independently. Preserve
  Eio's portable domain callbacks and local Cstruct interfaces. Never replace
  the vendor tree wholesale as part of an HTTP sync.
- Keep OxMono's platform backends and middleware, including Fetch's macOS,
  command-line and signature libraries. Projects have separate Dune/opam
  manifests and additional integration tests.
- Keep `httpz.websocket`, added locally after the recorded stack baseline.
- Preserve the portable Fetch URL and IDNA changes needed by Matrix. See
  [Matrix import notes](../matrix/OXMONO.md) and
  [Uunf provenance](../../vendor/uunf/README.md).
- The retired HTTPz router, Eio server and static-server executable must not
  return. Proffer owns the server API and its HTTPz backend.

## Update procedure

1. Record source revisions and compare mapped files individually. Preserve
   local packages, dependency identities and adaptations above.
2. Build the HTTP packages and examples with `--profile release-check`. Force
   their scoped test aliases and run affected consumer tests. Check dependency
   consumers against both bytecode and native interfaces where supported.
3. Run the relevant protocol fuzz targets and configured integration fixtures.
   Record skipped checks separately. DAV has its own `dav/check_sync.py`.
4. Review changed public interfaces, package manifests and documentation. Update
   this record and the relevant vendor records with the exact imported bases.

Use an OxCaml-compatible odoc for API documentation. Historical successful runs
are not evidence for a later synchronization.
