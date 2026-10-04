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

## 2026-10-03 refresh and validation

All 41 vendor remotes were checked. The following 12 bases were updated,
retaining their local OxCaml patches and documented import scopes. Versions
with a commit suffix include reviewed changes after the named release.

| Vendor | Updated base | Changes |
| --- | --- | --- |
| asn1-combinators | `v0.3.3` | Domain-safe error formatting. |
| bytesrw | `v0.4.1-1-g2883530` | Writer positions, writer callbacks and slice formatting. |
| checkseum | `v0.5.3-3-gfef8888` | FreeBSD 32-bit header fix. |
| cmarkit | `v0.4.0-8-g247a041` | Public unique heading ID generator. |
| decompress | `v1.6.1-1-gd0e4478` | 32-bit support and Windows binary I/O. |
| digestif | `v1.3.1-16-g35e5c1c` | XOR bounds and OCaml hash counter fixes. |
| jsonm | `v1.0.2-2-g8582f4d` | Source-location and deprecation documentation. |
| kdf | `v1.1.2` | Scrypt allocation bound. |
| mirage-crypto | `v2.4.1-2-g5cf7fc9` | Domain-safe errors and Jsont test fixtures. |
| tls | `v2.1.3-9-g5913e4c` | X.509 1.2 support and handshake validation fixes. |
| uunf | `v18.0.0` | Unicode 18 normalization data. |
| x509 | `v1.2.0-10-g6f4baca` | Typed names, SAN-only identities and validation fixes. |

The final check reports 39 bases at their upstream tips. Eio remains at the
requested 1.6 release. Mirage Crypto stops before ARC4 removal, which breaks
the current X.509 PKCS#12 implementation. Later Mirage Crypto commits must
be reconsidered with an X.509 update that handles that removal.

The DS4 and Apple Foundation Models entries now use their distinct Tangled
repository URLs. DS4 previously pointed at the Apple Foundation Models
repository. Both copies are current. The unchanged `ocaml-codec` base was
verified through SSH because its HTTPS remote requires authentication.

Validation with `5.2.0+ox` and `release-check` passed:

- HTTPz, Fetch, Proffer, Arod, Bushel, Sortal, Matrix and JMAP consumer tests,
  including the existing Markdown goldens and portability guards.
- X.509's 2,218 tests, TLS's 477 unit tests and 22 key-derivation tests,
  Digestif's 685 C-backend tests, and the HKDF, PBKDF and scrypt suites.
  The same 2,218 X.509 tests also pass on pristine upstream with OCaml 5.5.0.
- Mirage Crypto's domain-error regression, 70 symmetric-cipher tests and
  70 elliptic-curve tests. The domain-safe formatter permits restoring the
  upstream GCM and CCM invalid-input messages.
- Bytesrw writer-position and callback regressions on both the local port
  and pristine upstream. Jsonm's 5,013 differential comparisons and Uunf's
  84,356 comparisons pass. Cmarkit's four Markdown fixtures produce identical
  HTML with pristine upstream in strict/extended and safe/unsafe modes.

Validation limits:

- The full workspace `dune build` passes on Linux after adding unsupported
  platform fallbacks for Apple Speech, Foundation Models, and DS4 Metal, and
  installing the missing Bonsai dependencies. Live Arod route capture needs a
  local configuration and data corpus, which are absent here. Checked-in
  rendering goldens pass.
- Checkseum's bibliography Adler-32 test fails identically in both backends
  before and after this refresh. An isolated build of the unchanged base
  reproduces the same values. The other 20 cases pass in each backend.
- Decompress's direct suite needs `bstr`. Mirage Crypto's PK suite needs
  `randomconv`; its new Wycheproof decoder captures nonportable `Ohex`
  operations through this workspace's portable Jsont interface. The ASN.1
  recursive-combinator test also has an existing OxCaml mode mismatch.
- Digestif's optional OCaml backend and the platform-specific Windows and
  32-bit paths were not run. Formatting checks retain the existing limitations
  of stock ocamlformat on OxCaml sources and unrelated Dune formatting diffs.

## 2026-09-07 refresh and validation

The refresh verified 37 bases against their upstream default-branch tips. Refreshes
include Eio after 1.5, Cstruct 6.3, Cmarkit after 0.4, Syndic after 1.8, and
newer Bytesrw, Gmap, Htmlit, X.509, Xmlm and Zarith changes. Upstream changes
outside a component's documented scope were reviewed without adding the
omitted packages. The Uriz rewrite retains its package identity and exposes
upstream's new `path_unencoded` name through its existing decoded-path API.

The subsequent Uri migration removed the unused `cohttp-eio` vendor and its
external Cohttp/Uri dependencies. The manifest then covered 36 directories.
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

Jsonm 1.0.2 was added on 2026-09-08 for JMAP's I-JSON validation. Its
[port notes](jsonm/README.md) record the release base, compiler-checked
annotations and differential test against pristine sources. With the earlier
Uunf import, the manifest now records 38 vendors.

## oi library dependencies

The [oi library import](../docs/oi-library-import.md) adds opam core/format,
opam-file-format, ocamlgraph, SHA, swhid_core, patch and Dockerfile sources.
Their individual `OXMONO.md` and `OXMONO.json` files record release checksums,
compiler adaptations and scoped validation.

## Runner solver libraries

`opam-0install` 0.6.0 and `0install-solver` 2.18 supply the in-process solver
for ox. Only generic solver libraries are retained. Implementations match the
checksum-verified upstream archives. Directory/switch contexts and application
CLIs are omitted, so the library depends on opam-format without opam-state.
See each directory's `OXMONO.md` and `OXMONO.json` for scope and provenance.
Validate through `dune runtest avsm/ox --force` and
`dune build --profile release-check @avsm/ox/all`.

Upstream source, changelog and license whitespace is preserved. The two new
solver imports contribute eight unchanged whitespace diagnostics to
`git diff --check`.
