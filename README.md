# OxMono

OxMono contains OCaml libraries and applications built with OxCaml. Libraries
under development live in `bleeding/`; application projects live in `avsm/`.

## Repository layout

- [`bleeding/`](bleeding/) contains libraries, including Httpz, Proffer and Fetch.
- [`samoht/`](samoht/) contains imported libraries adapted to this workspace.
- [`avsm/`](avsm/) contains applications such as Sortal, Bushel and Arod.
- [`vendor/`](vendor/) contains dependency sources with OxCaml adaptations.
- [`example/`](example/README.md) contains the HTTP server and client examples.

Each package declares its dependencies in a `dune-project` and an `.opam`
file. Public OCaml interfaces are documented in `.mli` files. Dune builds
local libraries and vendored dependencies together across the workspace.

## HTTP examples

Start with the [HTTP guide](HTTPZ.md). It runs a Proffer server, then a Fetch
client against it. The [example index](example/README.md) covers routing,
forms, streaming, authentication, JSON and request policies.

The examples live under `example/proffer/` and `example/fetch/`. Later Fetch
examples use the Proffer server in `example/localhost/`, linked as the
example-only Dune library `localhost`.

## Setup

Use an OxCaml switch with the OxCaml opam repository configured. The commands
below select the switch named `5.2.0+ox`. The root `oxmono.opam` installs the
external development dependencies. Sources under `vendor/` and the other
workspace projects supply the local libraries.

```sh
eval "$(opam env --switch=5.2.0+ox)"
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

Build the workspace with allocation contracts checked.

```sh
dune build --profile release-check @all
```

Build selected applications or run the HTTP tests with explicit targets.

```sh
dune build --profile release-check avsm/sortal avsm/bushel avsm/arod
dune runtest --profile release-check bleeding/httpz bleeding/proffer bleeding/fetch
```

The HTTP examples compile as part of `@all`. Their READMEs give commands to
run each program. Server examples continue running until stopped with Ctrl-C.

The unreleased [Matrix SDK](bleeding/matrix/OXMONO.md) and
[Zulip bindings](bleeding/zulip/OXMONO.md) live under `bleeding/`. Their import
notes record upstream revisions, local adaptations and scoped test commands.

The [OpenRouter client](bleeding/openrouter/README.md) provides Eio-native
chat, vision inputs, function tools and streaming over Fetch.

The [mqttz client and codecs](bleeding/mqttz/README.md) support MQTT 3.1.1 and
5.0 with borrowed byte payloads, Eio TCP/TLS and a Docker interoperability suite.

The [OwnTracks client](bleeding/owntracks/README.md) uses mqttz for location
messages and exports live tracks and Recorder history as GeoJSON.

The [IMAP client](bleeding/imap/README.md) provides a pure protocol library,
an Eio client, a SQLite store and a durable IMAP to Maildir synchronizer with
the `imap-sync` command.

The [Maildir library](bleeding/maildir/README.md) stores messages in the
Dovecot Maildir layout for the IMAP synchronizer and other Eio programs.

The unreleased [JMAP client](bleeding/jmap/OXMONO.md),
[IDKit contact and calendar libraries](bleeding/idk/OXMONO.md), and
[JSON Pointer library](bleeding/json-pointer/OXMONO.md) retain their upstream
source layouts and record local adaptations beside them.

The [Crowthebot Matrix assistant](avsm/crowthebot/README.md) uses OpenRouter,
profile-local SQLite authority and context, and Sortal OPML blogroll tools.

## Dependencies

The workspace uses opam dependencies and the sources under `vendor/`.
Vendored package READMEs describe their interfaces and local changes.
Generating API documentation requires an OxCaml-compatible `odoc`.

[HTTPZ_SYNC.md](HTTPZ_SYNC.md) records the source revision and update procedure
for the HTTP libraries.

The [osrel](bleeding/osrel/README.md), [d10](bleeding/d10/README.md) and
[osdist](bleeding/osdist/README.md) projects provide platform detection,
binary layers, build recipes and distro packaging. The
[oi import review](docs/oi-library-import.md) records their provenance and
OxCaml adaptations. The [ox plan](docs/ox-plan.md) describes the workspace roadmap.

The [ox runner](avsm/ox/README.md) builds and runs committed monorepo packages from opam
metadata, with generated snapshot versions and local day10 layers. It builds
the OxCaml toolchain from source without invoking opam or creating switches.

The [DS4 bindings](bleeding/ds4/README.md) provide local DeepSeek, GLM and Qwen
inference, with DeepSeek V4.1 Flash 1017 preferred when downloaded.
