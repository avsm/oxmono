# IMAP for OCaml and Eio

The `imap` package is an IMAP4rev1 and IMAP4rev2 client with a durable IMAP
to Maildir synchronizer and the [`imap-sync`](bin/README.md) command.

## Layout

- `lib/protocol` is the library `imap`, module `Imap`. It holds the
  protocol values, response framing and parsing, command encoders, the
  extension vocabularies and the scan and reconciliation planners, and
  performs no I/O.
- `lib/eio` is the library `imap.eio`, module `Imap_eio`. It holds
  credentials and endpoints, authenticated connections, mailbox selection
  leases, a connection pool and the `Mailbox` strategy layer. Its
  implementation library `imap_eio_core` is private to the package and is
  used directly only by low-level tests.
- `lib/store` is the library `imap.store`, module `Imap_store`. It keeps
  SQLite cursors and UID snapshots, the pair and operation journal, and a
  content-addressed archive of message bodies.
- `lib/sync` is the library `imap.sync`, whose modules live under
  `Imap_sync`. `Engine` scans, archives and appends, `Bridge` runs a cycle
  in both directions, `Flags` and `Deletion` reconcile one pair, `Plan`
  previews a cycle offline, `Repair` holds the operator repairs and `Watch`
  reacts to IDLE. Every online call takes an `Imap_sync.Ctx.t`, and every
  call reports one `Imap_sync.Error.t`.
- `bin/` holds the `imap-sync` executable and its library `imap_cli`.
- The sibling [`maildir`](../maildir/README.md) package stores local
  messages and depends on no IMAP library.

`imap` depends only on `mail-flag`. `imap.eio` and `imap.store` each depend
on `imap` and not on each other. `imap.sync` depends on all of them and on
`maildir`. A program that speaks IMAP without synchronizing links
`imap.eio` alone.

The odoc pages in [`doc/`](doc/) describe the libraries, a client session
and a durable sync cycle. Their programs are the compiled examples in
[`test/examples/`](test/examples/), which the build compiles and never runs.
The local RFC corpus is in [`spec/`](spec/).

## Client

`Imap_eio.Transport.v` names an endpoint, with implicit TLS on port 993 by
default, required STARTTLS, or clear text for a trusted test fixture.
`Imap_eio.Auth` holds a password or an OAUTHBEARER token. Its automatic
mechanism prefers advertised SASL PLAIN over TLS, then CRAM-MD5, then LOGIN,
and a failed mechanism is never retried as another. PLAIN, LOGIN and
OAUTHBEARER require TLS unless the caller opts into insecure transport.
`Imap_eio.Client.connect` authenticates and then enables IMAP4rev2 on a
server that offers both revisions, UTF8=ACCEPT on an IMAP4rev1 session, and
QRESYNC, each when offered.

`Client.with_mailbox` selects a mailbox and holds the selection lease
across its callback. The `Selected.t` handle expires when the callback
returns, and a `Client` call on the same connection from inside the
callback is `Error (State _)` without sending anything. Commands are
serialized across fibers. A mailbox that reports UIDNOTSTICKY is refused,
since its UIDs cannot support durable pairing. Mailbox names are UTF-8 and
are sent as modified UTF-7 on a session without UTF-8 mode.

An operation that exists only because of an extension lives in a submodule
named for it. Its `require`, or `enable` for a mode, checks the capability
once and returns a witness that the operations take in place of the lease
or the client. A missing extension is `Error (Unsupported c)` and an
unconfirmed mode `Error (Not_enabled c)`, both before anything is sent. The
lease submodules are `Condstore`, `Qresync`, `Uidplus`, `Move`, `Binary`,
`Searchres`, `Sort`, `Esort`, `Thread`, `Partial`, `Messagelimit`,
`Uidbatches`, `Notify` and `Idle`. The connection submodules are `Acl`,
`Quota`, `Metadata`, `Notify`, `Multiappend` and `Compress`, and the modes
`Objectid_plus` and `Uidonly`.

`Selected.fetch` takes up to 1,000 UIDs and a list of `Imap.Fetch_item.t`
and returns one row per reported UID in request order. `Selected.fetch_to`
streams the exact `BODY.PEEK[]` bytes of one message into a sink, whose
output is provisional until the call returns `Ok ()`. `Client.append`
sends one message, and `Client.Multiappend.append_many` a batch. A result
of `Error (Uncertain _)` means the server may have applied the command, so
reconcile before retrying. `Imap_eio.Mailbox` chooses the commands for a
search, a fetch, a store, a move, a change scan and a wait from what the
server offers, for a client that is not a syncer, and reports the strategy
with every result. `Imap_eio.Pool` bounds a set of authenticated
connections. Give a long IDLE wait a connection outside the pool.

## Store and sync

`Imap_store` publishes a mailbox inventory through a disk-backed stage and
advances the cursor and the UID snapshot in one compare-and-swap
transaction. Its journal records each sync mutation before it is sent, and
its blob store streams exact bytes into synced, content-addressed SHA-256
files. Collecting orphan blobs with `Imap_store.Blob.reap_orphans_iter`
requires every writer of the blob directory to be stopped.

`Imap_sync.Bridge.copy_once` runs one cycle between a mailbox and a Maildir.
It publishes a complete remote inventory, stages a complete Maildir
inventory, copies unpaired messages both ways, reconciles the flags of each
pair and applies an opt-in deletion policy. It never pairs messages because
their bytes match and never replays a mutation whose outcome is uncertain.
Such a mutation stays pending until a later complete inventory proves its
outcome or an operator repairs it with `Imap_sync.Repair`. A pair the cycle
cannot settle safely is held with a durable conflict and counted in the
receipt. The cycle holds the Maildir writer lease, which every other
Maildir writer must also take. `Imap_sync.Watch.run` reconnects for each
scan and IDLE wait and scans whenever the selected state differs from the
published cursor.

## Tests

Tests live under `test/` and run with

    dune build @bleeding/imap/runtest

Live-server suites skip without their environment variables. The
[Cyrus oracle](test/oracle/README.md), the
[Dovecot fixture](test/dovecot/README.md), the
[Stalwart fixture](test/stalwart/README.md) and the
[Stalwart v0.16 fixture](test/stalwart_v16/README.md) run the client and
the bridge against real servers. The
[resource-scale regression](test/scale/README.md) pages a large Maildir and
a durable SQLite journal.
