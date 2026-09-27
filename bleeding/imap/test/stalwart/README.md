# Stalwart IMAP interoperability oracle

This opt-in fixture runs the independent Rust Stalwart server in Docker. The
default image is pinned by digest to `stalwartlabs/stalwart:v0.15.5`:
`sha256:dcf575db2d53d9ef86d6ced8abe4ba491984659a0f8862cc6079ee7b41c3c568`.
The image's `org.opencontainers.image.revision` is
`9aecfc1dfd53a87c8918a6a98123c50af2001998`, matching the local
`../stalwart` tag `v0.15.5`. Its startup log reports `Stalwart Server v0.15.5`.
This release uses the static TOML configuration supported by that tag; the
fixture's memory directory contains one synthetic user, and RocksDB data lives
in a container tmpfs. Only the IMAP listener is published, at an ephemeral
loopback host port. Each test creates and deletes its own mailbox, using unique
synthetic messages.

The live OCaml suite checks baseline authentication and mailbox selection,
UIDPLUS APPENDUID identity, exact fetched RFC822 bytes, CONDSTORE MODSEQ and
conditional STORE conflict/acceptance, QRESYNC selection with an advancing
checkpoint, and durable bridge import and upload with byte-for-byte checks.
If the server advertises the independent draft `OBJECTID+` capability, the
same suite explicitly enables it and checks the CREATE receipt, STATUS and
identifier-based SELECT against the same compound account/mailbox identity,
then fetches the message identifiers. Set
`IMAP_STALWART_OBJECTID_PLUS_REQUIRED=1` to make
absence of that capability a test failure. The pinned v0.15.5 image does not
advertise it. The separate [v0.16 fixture](../stalwart_v16/README.md) requires
and tests OBJECTID+ over certificate-pinned IMAPS.
That fixture also exercises the durable SQLite guard against a mailbox-name
replacement after rename.

The pinned Stalwart release **does not advertise CRAM-MD5**. Its pre-auth
CAPABILITY lists `AUTH=PLAIN`, `AUTH=OAUTHBEARER`, and `AUTH=XOAUTH2`; the
Stalwart v0.15.5 Rust IMAP authentication handler only accepts those methods.
The test verifies that the OCaml client rejects an explicit CRAM-MD5 attempt
before sending credentials. This fixture opts into plaintext PLAIN solely on
the isolated Docker loopback connection. The separate Dovecot fixture exercises
successful and rejected CRAM-MD5 authentication and bridge sync.

```sh
name="imap-stalwart-$$"
export IMAP_STALWART_PORT=0
trap 'bleeding/imap/test/stalwart/down.sh "$name"' EXIT
eval "$(bleeding/imap/test/stalwart/up.sh "$name")"
IMAP_STALWART_REQUIRED=1 opam exec --switch=5.2.0+ox -- \
  dune runtest --force bleeding/imap/test/stalwart
```

The scripts label containers and refuse to reuse or remove an unrelated name.
`IMAP_STALWART_IMAGE` can override the digest for an explicit comparison run.
The fixed credentials are synthetic and only suitable for this isolated test.

Relevant upstream sources: [Stalwart v0.15.5 default configuration](https://github.com/stalwartlabs/stalwart/blob/v0.15.5/resources/config/config.toml),
[IMAP authentication handler](https://github.com/stalwartlabs/stalwart/blob/v0.15.5/crates/imap/src/op/authenticate.rs),
and [Docker image entrypoint](https://github.com/stalwartlabs/stalwart/blob/v0.15.5/resources/docker/entrypoint.sh).
