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

`ir/makefile.ml` and `test/unit/test_makefile.ml` are adapted from
`lib/cmd/makefile_export.ml` and `test/oi/test_makefile_export.ml` at the same
upstream commit. The exporter now lives in d10.ir and names its shell helpers
`d10-build-node.sh` and `d10-install.sh`. Local changes rebase source and opam
metadata paths, preserve `TMPDIR` and `OI_STATIC`, clear failed layers before
retrying, rebase prepared substitution files, and resolve deferred scalar opam
configuration values from staged dependencies. Ox's distribution tests
exercise standalone builds and installation without ox or opam.

Public interfaces document caller locking, cache identities, source archive
preparation and executor policies. Direct dependencies are declared explicitly.
Retain these contracts and the platform-key regression tests when refreshing.
