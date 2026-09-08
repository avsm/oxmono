# OxMono

OxMono contains OCaml libraries and applications built with OxCaml. Libraries
under development live in `bleeding/`; application projects live in `avsm/`.

## Repository layout

- [`bleeding/`](bleeding/) contains libraries, including Httpz, Proffer and Fetch.
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

Use an OxCaml 5.2 switch with Dune and the package dependencies installed.
The commands below select the switch named `5.2.0+ox`. Dependency declarations
are in the `.opam` files under `bleeding/` and `avsm/`; the sources under
`vendor/` supply the workspace's adapted libraries.

```sh
eval "$(opam env --switch=5.2.0+ox)"
dune build --profile release-check @example/all
```

Zarr and Tessera also require the system PROJ, Blosc and Zstd libraries.
On Debian or Ubuntu, install their development packages.

```sh
sudo apt install pkg-config libproj-dev proj-data libblosc-dev libzstd-dev
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
