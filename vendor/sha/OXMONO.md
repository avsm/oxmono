# OxMono import

Imported sha.1.15.4 at `c743398abee8f822fc0d12f229121e431d60dd5d`.
`OXMONO.json` records the release archive URL and checksums. Every retained
source was compared against the checksum-verified release archive on
2026-10-03. Sources match except for the adaptations below.

## Scope and adaptations

Upstream SHA C bindings, unchanged. Optional OUnit2 tests excluded from workspace traversal. SHA algorithms exercised by the oi-libs opam differential fixture.

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
