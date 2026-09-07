# HTTP stack comparison and cleanup

Compared on 2026-09-07 with `/home/avsm2/src/git/avsm/oxcaml-httpz`, clean branch
`main` at `2eb1eb0b8ef2a6f300b44e514b8826ade53dc02b`. The monorepo already records
that exact source revision in [HTTPZ_SYNC.md](HTTPZ_SYNC.md). Its latest upstream
merge includes OxMono `464375d58`, which removes `httpz-perma-proxy`, and the
preceding portable Duration/Mtime imports.

The shared runtime is synchronized. The obsolete server was a deliberately
retained monorepo extension, not an indication that the wire parser or Fetch
implementation was behind.

## Source inventory

These are byte comparisons of tracked files at corresponding relative paths,
after the cleanup. Package manifests live at different levels in the two
repositories, so “monorepo only” includes local packaging and relocated tests.

| Standalone path | Monorepo path | Identical | Different | Standalone only | Monorepo only |
| --- | --- | ---: | ---: | ---: | ---: |
| `httpz/` | `bleeding/httpz/` | 115 | 12 | 2 | 37 |
| `fetch/` | `bleeding/fetch/` | 58 | 4 | 0 | 53 |
| `proffer/` | `bleeding/proffer/` | 58 | 2 | 0 | 9 |
| `example/` | `example/` | 96 | 1 | 0 | 1 |

Before cleanup, HTTPz had 47 monorepo-only files, Fetch had 54, and Proffer's
only shared-file difference was its README. The extra Proffer difference is
removal of the now-unnecessary Duration assertion.

All five dependency-consumer source files are identical after mapping their
locations. The Facades Dune file has monorepo dependencies. The five files in
standalone `test/release/` are also identical under
`bleeding/fetch/test/release/`.

## Intentional differences to preserve

- **Shared URI identity.** Standalone HTTPz owns private `Uriz` and scanner
  implementations. OxMono's `httpz.uri` adapts `vendor/ocaml-uri` so its values
  remain `Uriz.t`, shared with existing applications. Its `Uri` and scanner
  adapters account for the two standalone-only files and most shared HTTPz
  source differences. Templates and IP handling use those adapted module names.
  Replacing this directory wholesale would duplicate the implementation and
  break the established type identity.
- **Portable dependencies.** OxMono has broader Eio, Ptime and Cmarkit
  annotations. The Markdown adapter therefore calls Cmarkit directly, without
  standalone's casts. Jsont adds `String_map.create ()` for fresh ownership.
  The newly vendored Duration and Mtime packages likewise eliminate Proffer's
  duration conversion cast and Eio's Mtime wrapper.
- **Eio is synchronized separately.** After the comparison exposed the older
  base, OxMono's [Eio vendor](vendor/eio/VENDORED.md) was refreshed directly from
  upstream main at `v1.5-5-g0ee73e4`. It now includes `Net.connect ?bind_to
  ?options`, `Sockopt.settings` and `Process.Env`. OxMono's `Domain_manager.run`
  still enforces a portable callback and exposes an explicit `unsafe_run`.
  The Curl foreign-domain regression intentionally casts its test closure to
  exercise the runtime guard beneath that static boundary.
- **Monorepo Fetch extensions.** `fetch-main`, `fetch-macos`, NSURLSession
  bindings, `fetch-cmdliner` and `fetch-signature` are absent from standalone.
  They remain live packages. Shared Fetch frontend and HTTPz/Curl implementation
  files are byte-identical; test dependencies include the monorepo's URI,
  Base64 and vendored time packages.
- **Additional coverage.** OxMono retains cookie/date/Punycode fuzzers, parser
  portability checks, shared-URI identity tests, wire benchmarks and the early
  Proffer portability spikes. Standalone's root `audit/` programs and runtime
  allocation/concurrency audit are not mirrored here. Compiler allocation
  checks and the existing monorepo release regressions remain available.
- **Packaging and documentation.** Each monorepo HTTP project has its own
  `dune-project` and opam files. The example project has a local project file;
  all example programs match standalone. README differences describe these
  paths and extensions. Historical audit/RFC records remain local.

The shared TLS/X.509 dependency closure matched standalone at the initial
comparison. The subsequent [vendor refresh](vendor/README.md) updates all 37
monorepo vendors against their own upstream branch tips, adding differences in
that closure as well. [vendor/upstreams.json](vendor/upstreams.json) now records
the exact bases and import scopes. The later Uriz migration removes the unused
`cohttp-eio` vendor, leaving 36. Ptime retains its different directory layout;
Duration, Mtime and Cmarkit remain monorepo vendors rather than embedded
standalone vendors. Do not replace the entire vendor directory as a
synchronization shortcut.

## Removed

The permanent caching proxy was the last application using `Httpz_route` and
`Httpz_eio_server`. Its upstream deletion made these removals possible:

- `bleeding/httpz/route/` and the `httpz.route` library.
- `bleeding/httpz/eio_server/` and the `httpz.eio_server` library.
- The `httpz-eio-server` static-server executable in `bleeding/httpz/bin/`.
- The obsolete router regression and router portions of the allocation benchmark.
  Parser portability still runs across two domains; Proffer retains routing
  and backend tests and benchmarks.
- The disabled Core_bench executable and HTTPz's unused Core, Core_bench,
  ppx_jane and eio_posix package dependencies.
- A tracked `.DS_Store`; the root ignore list now excludes it.

No live source or Dune dependency refers to the retired libraries. Old migration
plans in `docs/superpowers/plans/` remain historical records. The root Proffer
sketch and HTTPz README now describe the current architecture.

Twenty previously ignored CSS/HTML outputs from the removed TW package became
untracked after the first upstream merge. They were preserved under
`/tmp/oxmono-retired-tw-x3lipaba/bleeding/tw/` and removed from the working tree.

## Integration fixes and validation

The newly vendored Mtime must be included in Fetch's MDX package closures;
otherwise MDX loads installed Mtime against vendored Eio and fails interface
checks. All four Fetch MDX stanzas now declare it.

The full workspace build also found Bushel's remaining uses of the earlier
OpenAPI shapes. Its PeerTube adapter now reads typed IDs/UUIDs directly and
flattens optional nullable descriptions/dates. Its Karakeep adapter similarly
flattens summary presence. This preserves the application's existing metadata
representation while using the corrected generated codecs.

Validation uses `opam exec --switch=5.2.0+ox -- dune` with
`--profile release-check` so compiler allocation checks remain enabled:

- Full workspace `@all`, which includes the examples and enabled benchmarks.
- HTTPz, Fetch, Proffer and OpenAPI regression aliases, including MDX tests.
- Eio, Duration and Mtime aliases, plus Arod and Bushel integration tests.
- A source/build-reference scan for the retired libraries and `git diff --check`.

The standalone checkout was read without modification or fetching newer remote
history. This comparison covers its supplied local revision. Its separate
runtime audit was inventoried but not imported or run as part of this cleanup.
