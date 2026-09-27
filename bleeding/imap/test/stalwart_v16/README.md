# Stalwart v0.16 OBJECTID+ interoperability fixture

This opt-in fixture pins Stalwart v0.16.23 by Docker digest
`sha256:be215678796691bc39bdda918ecc50d14a9032a099a1d1950e51950aec7e2592`.
It bootstraps a fresh server, creates one synthetic account in recovery mode,
then runs the built-in IMAPS listener on an ephemeral loopback port. The test
pins the temporary self-signed certificate and requires `OBJECTID+`. The live
suite also binds the compound mailbox identity in SQLite, renames that mailbox,
creates a replacement under its old name, and verifies the next scan refuses
to publish the replacement while the original message remains intact.

```sh
name="imap-stalwart-v16-$$"
eval "$(bleeding/imap/test/stalwart_v16/up.sh "$name")"
trap 'bleeding/imap/test/stalwart_v16/down.sh "$name" "$IMAP_STALWART_V16_DIR"' EXIT
IMAP_STALWART_REQUIRED=1 IMAP_STALWART_OBJECTID_PLUS_REQUIRED=1 \
  opam exec --switch=5.2.0+ox -- dune runtest --force bleeding/imap/test/stalwart
```

`up.sh` needs Docker, Python 3, and OpenSSL. It prints only shell exports;
the generated server admin password is never emitted. The fixed account
password is synthetic and confined to this isolated test. `down.sh` checks
the container label and temporary directory marker before cleanup.

The v0.16 bootstrap and registry provisioning use Stalwart's
[bootstrap mode](https://stalw.art/docs/configuration/bootstrap-mode/),
[Bootstrap object](https://stalw.art/docs/ref/object/bootstrap/), and
[Account object](https://stalw.art/docs/ref/object/account/).
