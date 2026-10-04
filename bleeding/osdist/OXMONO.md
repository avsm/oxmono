# OxMono import

Imported `lib/osdist` and the library cases from `test/oi/test_osdist.ml`
from oi 0.14.2 at `ca8c59ff26bd7350e909324dc804982f0e4cee5b`.
[oxmono/upstream.json](oxmono/upstream.json) records the upstream scope.

Jsont defaults use factories for the workspace API. Fifteen library tests
are retained. Four oi CLI signing-key tests are excluded. Distribution targets
retain upstream defaults, including the stock-OCaml Alpine builder image.
Packaging generation is tested, but OxCaml deployment and native installation
are not established by these tests.

The [shared import review](../../docs/oi-library-import.md) records dependency
provenance and validation. On refresh, compare `lib/osdist`, retain the Jsont
adaptations, re-extract only the library tests and run the README's scoped checks.
