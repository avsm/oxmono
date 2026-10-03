# Changes

## Unreleased

- Add `Tangled.Api.list_artifacts` and `artifact_version`, which list the
  release artifacts of a repository and read the version from a file name.

- Read `$bytes` base64 with or without padding and write it without, as the
  data model specifies. Records with a bytes field, such as a Tangled
  artifact's tag, no longer fail to decode.

- Add `Xrpc.Identity`, which resolves a handle to a DID and a DID to the
  personal data server that holds its records.

- Refresh all 231 Tangled lexicons and add current CI, repository identity,
  access-control, issue, key, record and complete API commands to the CLI.
- Generate subscriptions and nullable fields correctly, fail on invalid input
  schemas, and decode concatenated DAG-CBOR values without losing stream bytes.

- Encode DAG-CBOR text and byte strings larger than 4 KiB without overflowing
  the encoder buffer, preserving streaming writer order.

- Decode JWT credential metadata with JSONWT's strict bounded parser, rejecting
  duplicate or malformed registered claims before making refresh decisions.

- Add a reproducible Tangled lexicon audit and a local PLC/PDS/Jetstream Docker
  setup with persistent development accounts and an end-to-end record test.

## v0.1.0 (2025-01-15)

Initial release.

### Core Libraries

- `atp`: IPLD implementation with CID, DAG-CBOR, MST, CAR format support
- `atp-xrpc`: XRPC client for AT Protocol PDS communication
- `xrpc-auth`: Authentication helpers for CLI applications

### Code Generation

- `hermest`: Lexicon code generator for OCaml
- `hermest-cli`: CLI for the hermest generator

### Generated Lexicon Libraries

- `atp-lexicon-atproto`: `com.atproto.*` types
- `atp-lexicon-bsky`: `app.bsky.*` types (Bluesky)
- `atp-lexicon-tangled`: `sh.tangled.*` types
- `atp-lexicon-standard-site`: `site.standard.*` types

### Application Libraries and CLIs

- `bsky`: Bluesky client library and CLI
- `tangled`: Tangled git collaboration platform client and CLI
- `standard-site`: Blog/publication management client and CLI
