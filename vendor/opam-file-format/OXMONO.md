# OxMono import

Imported opam-file-format.2.2.0 at `59d9a6b585ac3fd141004f155a459e78f39993ac`.
`OXMONO.json` records the release archive URL and checksums. Every retained
source was compared against the checksum-verified release archive on
2026-10-03. Sources match except for the adaptations below.

## Scope and adaptations

Upstream library, parser generator and tests, unchanged.

## Refresh

1. Fetch the recorded release and the proposed replacement. Compare their
   library sources and dependency declarations.
2. Preserve the documented build scope and compiler adaptations. Verify the
   downloaded archive checksum and record its commit in `OXMONO.json` and
   `../upstreams.json`.
3. Build `@bleeding/oi-libs/all` with `release-check` and run
   `dune runtest --profile release-check --force bleeding/oi-libs`.
   Use the `5.2.0+ox` switch. The HTTP regressions need loopback sockets.
4. Run the explicit vendor test aliases listed in
   `../../bleeding/oi-libs/OXMONO.md`. Recursive vendor aliases skip tests.
