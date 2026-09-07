# Vendored libraries

These sources carry local OxCaml patches and build-system adaptations.
[upstreams.json](upstreams.json) records the upstream repository, branch and
exact base revision for each directory. The revision identifies the upstream
snapshot incorporated into the local port; local source files may differ.
Package version constraints are not a record of that snapshot.
`base_version` is the result of `git describe --tags --always`, which can
identify a development snapshot after a release. `scope` records partial
imports, directory mappings and rewrites where needed.

## Check upstream tips

From the repository root, run:

```sh
python3 vendor/check-upstreams.py
python3 vendor/check-upstreams.py eio cstruct tls
python3 vendor/check-upstreams.py --json > /tmp/vendor-tips.json
```

The script needs Python 3 and Git. It queries repositories concurrently with
`git ls-remote`, without downloading source trees or changing the manifest or
working tree. `--jobs` controls concurrency and `--timeout` bounds each query.
It also checks that every vendor directory has a manifest entry.

`branch: "HEAD"` follows the repository's default branch. An explicit branch
name can be recorded when maintaining a particular upstream branch.

| Status | Meaning |
| --- | --- |
| `current` | The recorded base equals the advertised branch tip. |
| `changed` | The tip differs; fetch its history to determine ancestry and review the changes. |
| `unknown-base` | No verified upstream base has been recorded yet. |
| `error` | The query failed or the requested branch was absent. |

Exit status is 0 when all selected bases are current, 1 for changed or unknown
bases, and 2 for query or configuration errors. JSON output includes full
commit IDs and errors. A failed query never counts as up to date.

The checker itself has offline integration tests using temporary Git repositories:

```sh
python3 vendor/test_check_upstreams.py
```

## During OxCaml merges and vendor refreshes

1. Run the checker before merging a new compiler or refreshing dependencies.
   Read each affected vendor's README and provenance notes, including any
   source-directory mappings or omitted upstream components in the manifest.
2. Fetch the recorded base and new branch tip into a temporary checkout. Review
   the upstream changes and merge them against the local port using the recorded
   base. Preserve portability and contention boundaries, local buffer lifetimes,
   allocation checks and monorepo package identities.
3. Keep local changes and partial backports separate from the upstream base.
   Advance `revision` and `base_version` only after incorporating the complete
   upstream change for the vendored component. Update its README/provenance
   notes and the manifest's verification date at the same time.
4. Build with the repository's switch and compiler allocation checks:

   ```sh
   opam exec --switch=5.2.0+ox -- dune build --profile release-check @all
   ```

5. Run the affected vendors' explicit test aliases and downstream integration
   tests. The root `(vendored_dirs vendor)` causes recursive aliases such as
   `@vendor/eio/runtest` to skip vendored tests. Address each test directory
   directly, for example `@@vendor/eio/lib_eio_linux/tests/runtest`, or run its
   test executable. MDX stanzas must include the complete vendored package
   closure in their dependencies to avoid mixing installed and local interfaces.
   Optional test dependencies and platform-specific tests need separate checks.
6. Run the checker again and `git diff --check`, then review the resulting diff
   and commit the source changes together with their provenance updates.

Eio's detailed import and patch history is in [eio/VENDORED.md](eio/VENDORED.md).
Libraries embedded inside HTTPz are tracked separately in
[bleeding/httpz/VENDORED.md](../bleeding/httpz/VENDORED.md).

## 2026-09-07 refresh and validation

The refresh verified 37 bases against their upstream default-branch tips. Refreshes
include Eio after 1.5, Cstruct 6.3, Cmarkit after 0.4, Syndic after 1.8, and
newer Bytesrw, Gmap, Htmlit, X.509, Xmlm and Zarith changes. Upstream changes
outside a component's documented scope were reviewed without adding the
omitted packages. The Uriz rewrite retains its package identity and exposes
upstream's new `path_unencoded` name through its existing decoded-path API.

The subsequent Uri migration removed the unused `cohttp-eio` vendor and its
external Cohttp/Uri dependencies. The manifest now covers 36 directories.
Consumers use the shared `Uriz.t`, including Fetch's signature context, and
Uriz provides `with_query_params` and HTTP `canonicalize` operations for this
port. Its [migration notes](ocaml-uri/README.md#compatibility-with-uri) describe
the compatibility rules and deliberate correctness differences.

Validation with `opam exec --switch=5.2.0+ox --` and `release-check` passed:

- Full workspace `@all`, plus HTTPz, Fetch, Proffer, ActivityPub, ATP, OpenAPI,
  Arod and Bushel regression aliases.
- Explicit native test aliases for Bytesrw, Cstruct, Gmap, Htmlit, Ptime, Uriz,
  X.509, Xmlm, the Digestif C backend, and the local Syndic regression.
- Zarith's tests, including the new upstream byte-conversion tests in both
  native code and bytecode; Eio's new connection and process-environment APIs
  on both Linux and POSIX backends.
- The checker integration tests and a final live check of all 37 upstream tips.

Some optional suites remain outside that passing set:

- Eio's upstream MDX transcripts include stock-compiler type output and
  callbacks rejected by the portable domain boundary. Some stanzas also load
  installed dependencies instead of the local closure; the README suite
  requires the unavailable `kcas` package. The main tests stanza now declares
  its vendored closure, and `tests/test_upstream_refresh.ml` directly exercises
  the updated APIs on both Linux backends.
- Syndic's legacy live-feed suite requires `ocplib-json-typed`. Its sources
  now use `Uriz`; the local `@@vendor/syndic/runtest` regression covers
  the new relaxed parser and retained author fallback without network fixtures.
- Digestif's alternative OCaml backend is not yet compatible with the port's
  portable virtual interface and OxCaml's local-aware byte helpers. The default
  C backend passes its 685 digest tests.
