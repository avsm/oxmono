# OxMono import

Imported swhid_core.0.1 at `1eff590933290be2b565306c89a28b2537a0659f`.
`OXMONO.json` records the release archive URL and checksums. Every retained
source was compared against the checksum-verified release archive on
2026-10-03. Sources match except for the adaptations below.

## Scope and adaptations

Upstream library and tests, unchanged.

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
