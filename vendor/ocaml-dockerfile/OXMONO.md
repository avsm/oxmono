# OxMono import

Imported dockerfile.8.3.9 at `e13814efe2e53f9b7cb344e20ce13c6c705a0259`.
`OXMONO.json` records the release archive URL and checksums. Every retained
source was compared against the checksum-verified release archive on
2026-10-03. Sources match except for the adaptations below.

## Scope and adaptations

Dockerfile and Dockerfile-opam libraries and tests. The CLI and its package declaration are omitted from this import.
The Dune project declares the recorded upstream version for generated opam
metadata and ox snapshot versioning.

## Refresh

1. Fetch the recorded release and the proposed replacement. Compare their
   library sources and dependency declarations.
2. Preserve the documented build scope and compiler adaptations. Verify the
   downloaded archive checksum and record its commit in `OXMONO.json` and
   `../upstreams.json`.
3. Build `@bleeding/d10/all` and `@bleeding/osdist/all` with `release-check`.
   Run `dune runtest --profile release-check --force bleeding/d10 bleeding/osdist`.
   Use the `5.2.0+ox` switch. The HTTP regressions need loopback sockets.
4. Run the explicit vendor test aliases listed in
   `../../docs/oi-library-import.md`. Recursive vendor aliases skip tests.
