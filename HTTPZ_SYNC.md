# Synchronizing HTTPz

HTTPz, Fetch, Proffer and their portable dependencies were synchronized from
`avsm/oxcaml-httpz` commit `00ef85b0910ac3adf20ad89436cc4abe19ac1b81`
on 2026-09-06. [HTTPZ.md](HTTPZ.md) introduces the libraries, their
dependencies and complete client/server examples.

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
  `Raw` APIs. Fetch/signature retains its bridge to the existing `Uri.t` API.
- Findlib dependencies are explicit in the HTTP projects, including the
  monorepo-only router, server, platform backends, benchmarks and examples.
  URI, media and bounded Jsont libraries remain independent of the HTTP
  transport and wire parser where their interfaces permit it.
- Timeout, retry, pacing and cache policies use the installed `duration`
  package. Fetch/main and Fetch/macos expose typed durations. CLI and
  ActivityPub float boundaries validate before creating a client and keep
  positive subnanosecond values positive. Arod's cache and confinement tests
  follow the current Proffer contract.
- Curl uses libcurl's native headers, trailers, framing and content decoding,
  with libcurl 7.83 or later. Fetch/httpz retains strict protocol checks.
  The foreign-domain guard test deliberately crosses the static portability
  boundary to exercise Curl's runtime rejection without weakening its API.
- Proffer's portable cache constructor retains the standalone assertion
  around `Duration.to_f`. Duration 0.3.1 implements it as pure arithmetic over
  immutable values; re-audit that boundary when updating Duration.
- OxMono retains its broader Eio and Ptime ports. Eio's local Cstruct changes
  are merged while keeping the portable `Flow.copy_string` implementation.
  TLS/Eio retains decrypted records as strings with an offset for partial
  reads, avoiding Cstruct copies. The TLS/X.509 closure and Eio Resource/Flow
  boundary contain no `Obj.magic_portable`.
- Bytesrw and Jsont use the standalone portable versions. Jsont additionally
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

The legacy `bleeding/httpz/bench/bench_httpz.exe` target remains disabled:
its installed Core_bench/Async closure uses a different Cstruct build.
The other HTTPz benchmarks remain enabled.

## Updating and checking

1. Record the standalone revision and start from a clean OxMono snapshot.
2. Compare the mapped files, retaining the adaptations above and OxMono-only
   packages. Check callers when shared dependency interfaces change.
3. Use the `5.2.0+ox` switch and `--profile release-check` to build the HTTP
   install targets, all three HTTP directory aliases and `@example/all`. Run
   their tests together with the external URI, Cstruct and Eio tests. Run all
   four HTTPz fuzz targets and check the wider workspace for integration failures.
4. Review the diff and update this record before committing.

The full workspace build, HTTP install targets, HTTP tests and four fuzz
targets passed under `release-check`. External URI, Cstruct, Eio, ActivityPub
and Arod tests passed for this sync. Twelve installed Findlib consumers
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
