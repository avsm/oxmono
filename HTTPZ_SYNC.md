# Synchronizing HTTPz

HTTPz, Fetch, Proffer and their portable dependencies were synchronized from
`avsm/oxcaml-httpz` commit `c6fda0e7ebea6ec869ceb71cb051cd3eaed08e97`
on 2026-09-05. [HTTPZ_RELEASE.md](HTTPZ_RELEASE.md) records the API changes
and release scope.

## Directory mapping

| `oxcaml-httpz` | OxMono |
|---|---|
| `httpz/` | `bleeding/httpz/` |
| `fetch/` | `bleeding/fetch/` |
| `proffer/` | `bleeding/proffer/` |
| `test/release/` | `bleeding/fetch/test/release/` |
| `vendor/NAME/` | `vendor/NAME/` |

OxMono-only packages, examples and fuzz targets remain in these trees.
Synchronize individual files rather than replacing destination directories.

## Monorepo adaptations

- HTTPz's namespaced URI API adapts the external portable `Uriz.t`, preserving
  type compatibility for existing OxMono consumers. The external implementation
  receives the corresponding parser and query updates; its older names remain
  available. Fetch/signature bridges to its existing `Uri.t` interface.
- OxMono retains its broader Eio and Ptime ports. Eio's local Cstruct changes
  are merged while keeping the portable `Flow.copy_string` implementation.
  The TLS/X.509 closure and Eio Resource/Flow boundary contain no
  `Obj.magic_portable`.
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
   install targets and all three HTTP directory aliases, then run their tests
   together with the external URI, Cstruct and Eio tests. Run all four HTTPz
   fuzz targets and check the wider workspace for integration failures.
4. Review the diff and update this record before committing.

The full workspace build and the HTTP, codec, generator and application checks
passed for this sync. The unchanged ATP syntax suite requires the absent
`bleeding/atp/vendor/atproto/interop-test-files/syntax/` fixtures.
