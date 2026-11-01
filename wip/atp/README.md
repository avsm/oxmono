# ocaml-atp

OCaml libraries and CLI tools for the [AT Protocol](https://atproto.com/), the
decentralized social networking protocol powering Bluesky and other
applications.

## Status

This is a work in progress, but functional enough for personal use. The core
libraries implement AT Protocol primitives, and CLI tools for Bluesky, Tangled,
and Standard.Site are included.

Significant inspiration (and some code such as Hermes for RPC and the MST) came
from [futur.blue/pegasus](https://tangled.org/futur.blue/pegasus) which has a
more complete PDS implementation. The appropriate MPL license has been
preserved where that code is used.

The [implementation review](REVIEW.md) documents tested fixes and open issues.
The [Tangled CLI guide](bin/tangled/README.md) covers repository, CI and access
commands. The [lexicon manifest](lexicons/tangled/README.md) pins the complete
Tangled schema set. The Docker testbed runs independently of live ATP services.
MST mutation and generated-schema validation are incomplete; this is not yet a
validated repository implementation.

XRPC clients can reuse a caller-owned Fetch capability:

```ocaml
let client = Xrpc.Client.of_fetch ~service:"https://pds.example"
  ~max_response_bytes:(16 * 1024 * 1024) fetch
```

Configure timeouts, network restrictions, and retries on `fetch`. JSON and binary
responses are bounded; JSON also requires a JSON Content-Type. Writes reject
redirects. Use `procedure_unit` for endpoints without an output schema.
Credentials are scoped to the service, and a retained authenticated client cannot
continue after logout or an account change. Session files use private atomic
replacement under `$XDG_CONFIG_HOME` (default `~/.config`).

Use the default `Standard` CID format for current AT Protocol CAR/CBOR data.
The optional `Atproto` format implements the bundled experimental draft and is
not the current interoperable encoding.

## Installation

Add the aoah-opam-repo overlay and install:

```sh
opam repo add aoah https://tangled.org/anil.recoil.org/aoah-opam-repo.git
opam install atp bsky tangled standard-site
```

Or pin directly from git:

```sh
opam pin add -y https://tangled.org/@anil.recoil.org/ocaml-atp.git
```

## Packages

### Core Libraries

| Package | Description |
|---------|-------------|
| `atp` | IPLD implementation with CID, DAG-CBOR, MST, CAR format support |
| `atp-xrpc` | XRPC client for AT Protocol PDS communication |
| `xrpc-auth` | Authentication helpers for CLI applications |

### Code Generation

| Package | Description |
|---------|-------------|
| `hermest` | Lexicon code generator for OCaml |
| `hermest-cli` | CLI for the hermest generator |

### Generated Lexicon Libraries

| Package | Description |
|---------|-------------|
| `atp-lexicon-atproto` | `com.atproto.*` types |
| `atp-lexicon-bsky` | `app.bsky.*` types (Bluesky) |
| `atp-lexicon-tangled` | `sh.tangled.*` and `org.tangled.temp.*` types |
| `atp-lexicon-standard-site` | `site.standard.*` types |

### Application Libraries and CLIs

| Package | Description |
|---------|-------------|
| `bsky` | Bluesky client library and CLI |
| `tangled` | Tangled git collaboration platform client and CLI |
| `standard-site` | Blog/publication management client and CLI |

## Requirements

- OCaml >= 5.1
- Dune >= 3.20

## Usage

### Bluesky CLI

```sh
# Login (credentials stored in XDG config directory)
bsky auth login alice.example

# Post to your timeline
bsky post "Hello from OCaml!"

# Inspect authentication and profiles
bsky auth status
```

### Tangled CLI

```sh
# Login
tangled auth login --handle alice.example

# List repositories
tangled repo list
```

### Standard Site CLI

```sh
# Login
standard-site auth login alice.example

# Manage your blog posts
standard-site document list
```

## Building from Source

```sh
git clone https://tangled.org/@anil.recoil.org/ocaml-atp.git
cd ocaml-atp
opam install . --deps-only
dune build
```

## License

ISC + MPL for bits from Pegasus
