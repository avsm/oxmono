# OxMono import

Imported `lib/osrel` from oi 0.14.2 at
`ca8c59ff26bd7350e909324dc804982f0e4cee5b`.
[oxmono/upstream.json](oxmono/upstream.json) records the upstream scope.

The library implementation is unchanged. The standalone Dune project declares
the `osrel` package. Platform normalization and detection tests live in `test/`.

The [shared import review](../../docs/oi-library-import.md) records dependency
provenance and validation. On refresh, compare the recorded `lib/osrel` sources,
retain the package metadata and local tests, and run the README's scoped checks.
