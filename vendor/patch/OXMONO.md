# OxMono import

Imported patch.3.1.2 at `1b977a11d5e3c80c325aed6ea8a85a9801cbe57c`.
`OXMONO.json` records the release archive URL and checksums. Every retained
source was compared against the checksum-verified release archive on
2026-10-03. Sources match except for the adaptations below.

## Scope and adaptations

Library and tests. The opatch CLI is omitted from this import.

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
