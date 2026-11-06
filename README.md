# OxMono

OxMono contains OCaml libraries and applications built with OxCaml. Libraries
under development live in `wip/`; user applications live in `u/<owner>/`.

## Repository layout

- [`wip/`](wip/) contains shared projects, including Httpz, Proffer, Fetch, ox
  and [Duraml job queues](wip/duraml/README.md).
- [`u/avsm/`](u/avsm/) contains applications such as Sortal, Bushel and Arod.
- [`vendor/`](vendor) contains dependency sources with OxCaml adaptations.
- [`examples/`](examples/README.md) contains the HTTP server and client examples.
- [`u/etap/taposaur/`](u/etap/taposaur/README.md) contains paper archive readers, SQLite
  search, inspection commands and a read-only Proffer browser.

Each package declares its dependencies in a `dune-project` and an `.opam`
file. Public OCaml interfaces are documented in `.mli` files. Dune builds
local libraries and vendored dependencies together across the workspace.

## Development

Read [AGENTS.md](AGENTS.md) for contribution and verification rules and
[TODO.md](TODO.md) for open work. Package documentation lives beside its code.

## Forks and user builds

Use `ox init --user NAME` to create an independent entry under `u/`. Build it
with `ox build --user NAME`, or create an install tree with
`ox dist bundle --user NAME --output dist/NAME`. Each owner has a Dockerfile
for Linux amd64 and arm64. Shared development happens in `wip/` and imported
ports remain in `vendor/`. See the [ox guide](wip/ox/README.md#monorepo-owners-and-releases)
for bootstrapping, owner isolation and source opam releases.

## HTTP examples

Start with the [HTTP guide](docs/http.md). It runs a Proffer server, then a Fetch
client against it. The [example index](examples/README.md) covers routing,
forms, streaming, authentication, JSON and request policies.

The examples live under `examples/proffer/` and `examples/fetch/`. Later Fetch
examples use the Proffer server in `examples/localhost/`, linked as the
example-only Dune library `localhost`.

## Setup

Use an OxCaml switch with the OxCaml opam repository configured. The commands
below select the switch named `5.4.0+ox9`. The root `oxmono.opam` installs the
external development dependencies. Sources under `vendor/` and the other
workspace projects supply the local libraries.

```sh
eval "$(opam env --switch=5.4.0+ox9)"
opam install . --deps-only
dune build --profile release-check @example/all
```

The root dependency list includes test, fuzzing and MDX tools unconditionally
because `dune build @all` builds their executables. It excludes libraries
provided by the workspace. Update it when adding an external dependency to
a local or vendored project. Individual package `.opam` files describe their
standalone dependencies.

Native builds also require PROJ, Blosc, Zstd, GMP, zlib, libcurl and the
OpenBLAS/LAPACKE libraries used by Nx. Opam's system dependencies cover Debian
and Homebrew. For manual installation on Debian or Ubuntu:

```sh
sudo apt install pkg-config libproj-dev proj-data libblosc-dev libzstd-dev \
  libgmp-dev zlib1g-dev libcurl4-openssl-dev libopenblas-dev liblapacke-dev
```

On macOS with Homebrew:

```sh
brew install pkg-config proj c-blosc zstd gmp curl openblas lapack libomp
export PKG_CONFIG_PATH="$(brew --prefix openblas)/lib/pkgconfig:$(brew --prefix lapack)/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
```

## Build and test

Build the workspace with allocation contracts checked, including
`[@zero_alloc opt]`, using `-O3`.

```sh
dune build --profile release-check @all
```

Build selected applications or run the HTTP tests with explicit targets.

```sh
dune build --profile release-check u/avsm/sortal u/avsm/bushel u/avsm/arod
dune runtest --profile release-check wip/httpz wip/proffer wip/fetch
```

The HTTP examples compile as part of `@all`. Their READMEs give commands to
run each program. Server examples continue running until stopped with Ctrl-C.

The unreleased [Matrix SDK](wip/matrix/OXMONO.md) and
[Zulip bindings](wip/zulip/OXMONO.md) live under `wip/`. Their import
notes record upstream revisions, local adaptations and scoped test commands.

The [OpenRouter client](wip/openrouter/README.md) provides Eio-native
chat, vision inputs, function tools and streaming over Fetch.

The [mqttz client and codecs](wip/mqttz/README.md) support MQTT 3.1.1 and
5.0 with borrowed byte payloads, Eio TCP/TLS and a Docker interoperability suite.

The [OwnTracks client](wip/owntracks/README.md) uses mqttz for location
messages and exports live tracks and Recorder history as GeoJSON.

The [IMAP client](wip/imap/README.md) provides a pure protocol library,
an Eio client, a SQLite store and a durable IMAP to Maildir synchronizer with
the `imap-sync` command.

The [Maildir library](wip/maildir/README.md) stores messages in the
Dovecot Maildir layout for the IMAP synchronizer and other Eio programs.

The unreleased [JMAP client](wip/jmap/OXMONO.md),
[IDKit contact and calendar libraries](wip/idk/OXMONO.md), and
[JSON Pointer library](vendor/ocaml-json-pointer/OXMONO.md) retain their upstream
source layouts and record local adaptations beside them.

The [Crowthebot Matrix assistant](u/avsm/crowthebot/README.md) uses OpenRouter,
profile-local SQLite authority and context, and Sortal OPML blogroll tools.

## Dependencies

The workspace uses opam dependencies and the sources under `vendor/`.
Vendored package READMEs describe their interfaces and local changes.
Generating API documentation requires an OxCaml-compatible `odoc`.

[SYNC.md](wip/httpz/SYNC.md) records the source revision and update procedure
for the HTTP libraries.

The [osrel](wip/osrel/README.md), [d10](wip/d10/README.md) and
[osdist](wip/osdist/README.md) projects are unreleased libraries maintained here.
They provide platform detection, binary layers, build recipes and distro
packaging. The [oi import review](docs/oi-library-import.md) records their provenance and
OxCaml adaptations. The [ox plan](docs/ox-plan.md) describes the workspace roadmap.

The [ox runner](wip/ox/README.md) builds and runs committed monorepo packages from opam
metadata, with generated snapshot versions and local day10 layers. It builds
the OxCaml toolchain from source without invoking opam or creating switches.

The [DS4 bindings](wip/ds4/README.md) provide local DeepSeek, GLM and Qwen
inference, with DeepSeek V4.1 Flash 1017 preferred when downloaded.
