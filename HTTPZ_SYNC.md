# Synchronizing HTTPz

HTTPz, Fetch and Proffer are synchronized from `avsm/oxcaml-httpz` commit
`2eb1eb0b8ef2a6f300b44e514b8826ade53dc02b` on 2026-09-07. This ports stock
`avsm/ocaml-httpz` through commit `72e4541`, including the preceding audit fixes,
Duration cleanup, media consolidation and Proffer wrapper rename. [HTTPZ.md](HTTPZ.md) introduces the libraries and examples.

## 2026-09-08 WebDAV package synchronization

The reviewed monorepo implementation was committed first as
`e2531cf8f01344cdc53ff51a80d19d198e8f8507`. Its final subpackage layout is now
synchronized with standalone OxCaml `9d59fac5d8f877969824d030c26c44d2898df98d` and
stock OCaml `9960477b34025a8ce257f5c921109f6310f83505`. These are selective DAV
ports; the broader stack-history baseline above is unchanged.

The former standalone `davz` package is replaced by `httpz.dav` / `Httpz_dav`.
Its corrected XML codec is private and matches `vendor/xmlm`; its only public
library dependency is `httpz.uri`. `fetch.dav` / `Fetch_dav` owns the client.
`proffer.dav` / `Proffer_dav` re-exports that client with identical types and
exceptions, preserving the dependency direction from Proffer to Fetch to HTTPz.
All public XML encoding and namespace values belong to `Httpz_dav`.

The [DAV synchronization guide](bleeding/httpz/dav/SYNC.md) documents the shared
paths and stock adaptations. Check all 38 source, test, fixture and documentation
files, plus private/vendor XML parity, with:

```sh
python3 bleeding/httpz/dav/check_sync.py
```

Both standalone trees pass full `@all @install @runtest` builds, using
`5.2.0+ox --profile release-check` and stock `5.5.0` respectively. The monorepo
passes HTTPz/Fetch/Proffer installation targets, HTTPz and Proffer suites,
DAV protocol and client tests, the facade identity test and explicit vendor
Xmlm regressions. The three trees pass Apache HTTP/HTTPS client workflows and
the opt-in smoke test's success and injected-failure cleanup in one disposable
fixture. No live-account access was needed for this port.

Stock documentation builds with existing reference warnings. OxCaml `@doc`
resolves to the stock 5.5.0 `odoc` binary in this environment and rejects OxCaml
typed artifacts, including unchanged core/vendor modules. That optional check
needs an OxCaml-compatible documentation tool.

## 2026-09-07 vendor branch tips

The 37 monorepo vendors were checked and refreshed against their own upstream
default branches. This is now tracked separately from the standalone HTTP stack
revision: [vendor/upstreams.json](vendor/upstreams.json) records each exact base
and import scope, and [vendor/README.md](vendor/README.md) describes the tip
checker, merge procedure and validation results.
The later Uriz migration removed the unused `cohttp-eio` vendor; 36 remain.

## 2026-09-07 Eio refresh

Eio was refreshed directly from `ocaml-multicore/eio` main at
`0ee73e48b566e7cd09cd3c1fc08ef1da199558b0` (`v1.5-5-g0ee73e4`). This brings in
the complete 1.5 release and the subsequent `Process.Env` API. The OxCaml
portability boundaries, shared Cstruct adaptations and direct portable Mtime
calls are preserved. [vendor/eio/VENDORED.md](vendor/eio/VENDORED.md) records
the current base and local patch history.

## 2026-09-07 monorepo cleanup

Compared the mapped source trees against standalone `2eb1eb0`; the shared wire,
Fetch and Proffer implementations were synchronized. The remaining legacy
router/server was an intentionally retained monorepo extension, with the
permanent caching proxy as its last application consumer. Upstream OxMono
`464375d58` removed that proxy, after which `httpz.route`, `httpz.eio_server`,
the static-server executable and router-only tests/benchmarks were removed.
Wire parser portability and allocation checks remain. The disabled Core_bench
target and its otherwise unused package dependencies were removed as well.

The preceding upstream commits vendored portable Duration and Mtime. Proffer's
cache now calls `Duration.to_f` directly, and Eio's clock implementation uses
Mtime without the former compatibility assertions. Public signatures and time
conversion semantics are unchanged.

The current tree comparison, preserved extensions and test evidence are in
[HTTPZ_COMPARISON.md](HTTPZ_COMPARISON.md). Historical source-review records are
retained and identified as snapshots.

## 2026-09-07 Proffer wrapper names

Proffer's private Site fields and accessor now use `run_with_wrappers` and
`has_wrappers`; the direct handler runner is `run_without_wrappers`. Dispatch,
responder/test variables and related comments use the same wrapper terminology.
The changed Proffer files match the standalone OxCaml port. Request behavior,
public interfaces, locality, portability and allocation annotations are unchanged.

Validation: `opam exec --switch=5.2.0+ox -- dune build --profile release-check
@all @bleeding/proffer/runtest` passes, including all 55 wrapper checks.
The standalone port also passes its allocation/mode/concurrency audit.

## 2026-09-07 consolidated media and tests

The HTTP wire library now owns `Httpz.Syntax` and `Httpz.Diagnostic`; their
private Dune libraries are removed. The single `httpz.media` library depends on
wire and includes `Httpz_media.Json` and `Httpz_media.Markdown`, including the
bounded JSON reader. The former JSON reader and codec sublibraries are removed.
Structured media errors use `Json.Error`. Fetch and Proffer retain the `Media`,
`Json` and `Markdown` names. The underlying Jsont/Cmarkit value types remain.
The name remains `httpz.media` so the library belongs to the `httpz` package.

Media, cookie, TLS, Punycode and public-suffix tests are collected in
`bleeding/httpz/test`; Proffer's JSON and Markdown tests are collected in
`bleeding/proffer/test`. Test dependencies and fixtures move with them, and the
private Markdown sanitizer regression retains its test-only source copy.
The exponential parser regression remains deferred at the maintainer's request.

OxMono retains its shared Uriz adapter, platform backends, portable dependencies,
local producers, unboxed values and allocation annotations. Its bounded JSON
reader retains the native structural scan and its portable Markdown calls.

Validation with `5.2.0+ox` and `--profile release-check` passes:

- Full workspace `@all` and HTTPz, Fetch and Proffer install targets.
- HTTPz, Fetch, Proffer, ActivityPub and Arod test suites.
- Shared URI, Cstruct and Eio suites, and all four HTTPz fuzz targets.
- `git diff --check`.

The standalone port also passes its allocation/mode/concurrency audit. Backend
hot paths retain zero heap allocation; the existing 405 path retains 160 bytes.
Stock installed bytecode/native consumers and fresh documentation builds pass.
JMAP was updated in `2146f97`. JMAP and Matrix build/test suites pass against the
stock source; Matrix's live homeserver test is skipped without
`MATRIX_TEST_HOMESERVER`.

## 2026-09-07 duration API cleanup

Removed the `Fetch.Duration` and `Proffer.Duration` aliases. Callers use the
external `Duration` module directly and declare `duration` in their Dune
libraries. Examples, tests and documentation follow the same API. Duration
types, conversions and OxCaml portability boundaries are unchanged.

The platform backends, command-line configuration, ActivityPub and Arod now
name the dependency explicitly in Dune and their generated opam manifests.
The full workspace build, HTTP install targets, HTTP tests, ActivityPub tests
and Arod tests pass under `5.2.0+ox` with `--profile release-check`.

## 2026-09-06 backend extraction and audit follow-ups

Proffer Backend now owns responder lifetime and error containment. Private
Conditional, Response and Dispatch modules own preconditions, transport outcomes
and routing; Etag owns field matching. Backend shrinks from 654 to 133 lines.
The extraction retains local values, global field modalities, unboxed operations,
portable interfaces and compiler-checked zero-allocation contracts. Hot entry
points are explicitly inlined, with the same callback and cold 405 allocation
boundaries as before.

The synchronization also includes raw duration validation, Retry-After floor
handling, `Fetch.get_as ?limit`, weak BLAKE2b-256 cache validators, extended
multipart filename UTF-8 validation, indexed multipart parameter names, the PSL
generator completeness floor, and directional-control diagnostic escaping.
Cookie jars gain `?oversized`; `httpz-cookiecat` selects strict missing/oversized
handling and exits 2 on read failure. Its existing runtime dependency on
`eio_main` is now reflected in the HTTPz package manifest. Public interfaces
document bounded JSON, SSE and Curl's inherited protocol limitations.

OxMono retains its shared Uriz adapter and URI type identity, native template
slots, Base-map cache implementation, portable dependencies, platform backends,
application integrations and build profiles. Larger URI association lists gain
randomized indexing while small lists avoid building a table.

The standalone Cmarkit exponential-parser regression is temporarily disabled in
`bleeding/fetch/test/release/dune`, following the stock and standalone OxCaml
trees. Its executable and ordinary Markdown tests remain. This regression is
deferred at the maintainer's request; the vendored Cmarkit fix is unchanged.

Validation uses `opam exec --switch=5.2.0+ox -- dune` with
`--profile release-check`:

- HTTPz, Proffer and Fetch `@install` and `@all` aliases, plus `@example/all`.
- Tests in all three HTTP trees, `vendor/ocaml-uri`, `vendor/cstruct`,
  `vendor/eio`, `bleeding/apubt` and `avsm/arod`, including shared URI identity
  and public facade consumers.
- Full workspace `@all` and all four `@bleeding/httpz/fuzz/fuzz` targets.
- `git diff --check`.

All of these checks pass. Existing compiler alerts and warnings in the wider
workspace remain non-fatal.

The manual `bleeding/fetch/test/release/proffer_backend.exe` benchmark reports
zero heap bytes per request for GET, HEAD, 304, 412 and routed GET; the cold 405
path retains 160 bytes per request. A 500,000-request sample measured 68, 73,
87, 92, 96 and 195 ns/request respectively. Timings are observations, not test
thresholds. The standalone sync record contains the alternating before/after
comparison.

## Directory mapping

| `oxcaml-httpz` | OxMono |
|---|---|
| `httpz/` | `bleeding/httpz/` |
| `fetch/` | `bleeding/fetch/` |
| `proffer/` | `bleeding/proffer/` |
| `example/` | `example/` |
| `test/release/` | `bleeding/fetch/test/release/` |
| `test/dependencies/` | `bleeding/httpz/test/dependencies/` and `bleeding/fetch/test/dependencies/facades/` |
| `vendor/NAME/` | `vendor/NAME/` |

OxMono-only packages and fuzz targets remain in these trees. The shared HTTP
examples live under the top-level `example/` directory. Server handlers receive
clock operations through their environment so the examples also compile
against the standalone portable Eio interfaces. Synchronize individual files
rather than replacing destination directories.

## Monorepo adaptations

- `httpz.uri` adapts the external portable `Uriz.t`, preserving type
  compatibility for existing OxMono consumers. Its private adapter exposes
  the standalone component names, templates and IP classifier. The shared
  `vendor/ocaml-uri` implementation receives the parser, query and local
  allocation improvements while retaining its span, canonical-parser and
  `Raw` APIs. Fetch/signature now takes `Uriz.t` directly from the request URL;
  its public context no longer depends on opam `Uri.t`.
- Findlib dependencies are explicit in the HTTP projects, including the
  monorepo-only platform backends, benchmarks and examples.
  URI remains separate; the combined media library depends on wire.
- Timeout, retry, pacing and cache policies use the installed `duration`
  package, now vendored with portable annotations. Fetch/main and Fetch/macos expose typed durations. CLI and
  ActivityPub float boundaries validate before creating a client and keep
  positive subnanosecond values positive. Arod's cache and confinement tests
  follow the current Proffer contract.
- Curl uses libcurl's native headers, trailers, framing and content decoding,
  with libcurl 7.83 or later. Fetch/httpz retains strict protocol checks.
  The foreign-domain guard test deliberately crosses the static portability
  boundary to exercise Curl's runtime rejection without weakening its API.
- Proffer calls the vendored portable Duration interface directly. Eio likewise
  uses the portable Mtime interface without compatibility assertions.
- OxMono retains its broader Eio and Ptime ports. The exact Eio upstream base
  and local patches are recorded in [vendor/eio/VENDORED.md](vendor/eio/VENDORED.md).
  Eio's local Cstruct changes
  are merged while keeping the portable `Flow.copy_string` implementation.
  TLS/Eio retains decrypted records as strings with an offset for partial
  reads, avoiding Cstruct copies. The TLS/X.509 closure and Eio Resource/Flow
  boundary contain no `Obj.magic_portable`.
- Bytesrw and Jsont retain their portable ports, with upstream bases tracked
  in the vendor manifest. Jsont additionally
  exposes `String_map.create ()` for freshly owned maps. Monorepo codecs and
  generators use factory defaults and portable callbacks. APub's URI fields
  now use `Uriz.t`; callers parse them with `Uriz.of_string_exn`. Multibase
  encoding tables are immutable so ATP codecs can call it portably.
- Vendored Cmarkit includes the nested-link parser correction from upstream
  commit `6a64f63`; its provenance is recorded in `vendor/cmarkit/README.md`.
  Its portable interfaces let the HTTP adapters omit standalone boundary casts.
- Fetch/macos closes each response's child switch on EOF, explicit close or
  failure. Its lifecycle regression runs on Linux; NSURLSession network
  behaviour requires macOS.

The enabled HTTPz benchmarks cover wire parsing and field/chunk operations;
Proffer benchmarks cover routing and backend dispatch.

## Updating and checking

1. Record the standalone revision and start from a clean OxMono snapshot.
2. Compare the mapped files, retaining the adaptations above and OxMono-only
   packages. Check callers when shared dependency interfaces change.
3. Use the `5.2.0+ox` switch and `--profile release-check` to build the HTTP
   install targets, all three HTTP directory aliases and `@example/all`. Run
   their tests together with the external URI, Cstruct and Eio tests. Run all
   four HTTPz fuzz targets and check the wider workspace for integration failures.
4. Review the diff and update this record before committing.

For the earlier committed synchronization, the full workspace build, HTTP
install targets, HTTP tests and four fuzz targets passed under `release-check`.
External URI, Cstruct, Eio, ActivityPub and Arod tests passed for that sync.
Twelve installed Findlib consumers
compiled and ran in bytecode and native code, including shared URI type
identity. Dependency exclusion checks passed, including no Checkseum or
Decompress in Curl.

The unified example tree and HTTP install targets build under `release-check`.
All 31 example programs ran, including the documented server/client pair.
The HTTPS example was exercised with a local HTTP URL. Guide snippets match
the compiled source files, and local README links and run paths resolve.

Rendered API documentation requires an OxCaml-compatible odoc, which is not
available in this switch. The unchanged ATP syntax suite requires the absent
`bleeding/atp/vendor/atproto/interop-test-files/syntax/` fixtures.
