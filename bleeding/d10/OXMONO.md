# OxMono import

Imported `lib/d10`, `lib/d10ir` and `test/d10` from oi 0.14.2 at
`ca8c59ff26bd7350e909324dc804982f0e4cee5b`. The IR command library is excluded.
[oxmono/upstream.json](oxmono/upstream.json) records the upstream scope.

`lib/` provides `d10` and `ir/` provides `d10.ir` in the same opam package.
Local adaptations use Fetch/Curl, preserve child environments, adapt Jsont
codecs for OxCaml and restore cached prefixes through the relocation pass.
The existing install-file handler is public. Tests cover layers, locks,
HTTP behavior and IR compatibility. `test/compat/opam.expected` preserves the
stock-OCaml differential baseline for the vendored opam libraries.

The executor also supports cached permanent prefixes and single-node execution
with deferred recipe preparation. Prefix assembly detaches writable copies,
restores dependencies in order and compares file contents and modes for layer
capture. Saved recipes include the executed node and its source archive.
Staging and uncached user-prefix execution remain supported. Tests in
`test/executor` cover both policies, recipe replay, PATH isolation, dependency
immutability, failed capture and parallel prefix restoration.

The [shared import review](../../docs/oi-library-import.md) details these
adaptations, caller locking and cache immutability requirements. On refresh,
map upstream `lib/d10` to `lib/` and `lib/d10ir` to `ir/`, preserve those
adaptations and local tests, and run the README's scoped checks and the
explicit vendor suites listed in the review.
