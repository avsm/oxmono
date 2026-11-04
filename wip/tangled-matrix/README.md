# tangled-matrix

A Matrix bot that reports Tangled CI. For each pipeline of the repositories
it serves, it keeps one message in the repository's room, edited in place
as the pipeline's workflows start, change and finish:

```
🔄 alice.example/demo: push refs/heads/main 1a2b3c4 running, 1 of 3 passed
✅ 5.5.1:x86_64:Debian-13
❌ 4.14.2:x86_64:Debian-13: Failed: Build failed
🔄 5.5.1:arm64:Debian-13
https://tangled.org/did:plc:…/demo/pipelines/3m7cd…
```

It runs apart from any spindle and uses only the public spindle API that
Tangled's own pages use, so it works with any spindle.

## Running it

```sh
tangled-matrix --bot @ci:example.org --password-file bot.password \
  --account alice.example --state-dir /var/lib/tangled-matrix
```

| Option | Meaning |
| --- | --- |
| `--bot USER` | The bot's Matrix user ID. |
| `--password-file FILE` | Its password, for the first login and for creating cross-signing keys. |
| `--account HANDLE` | A Tangled account whose bindings the bot honours. Repeat it for more. |
| `--state-dir DIR` | Where the session, keys and messages are kept. `tangled-matrix-state` by default. |
| `--recovery-key-file FILE` | The account's recovery key, to verify a new device of an account that already has cross-signing keys. |
| `--homeserver URL` | The homeserver, found through `.well-known/matrix/client` by default. |
| `--interval SECONDS` | How often each spindle is polled, 15 by default. |
| `--edit-interval SECONDS` | How often a message may be edited, 10 by default. |
| `--appview URL` | Where pipelines are linked to, `https://tangled.org` by default. |
| `--allow-unencrypted` | Also post in rooms without end-to-end encryption. |

## Binding a repository to a room

A maintainer invites the bot to the room, then runs, in a clone of the
repository:

```sh
tangled pipeline matrix set --bot @ci:example.org --room '#ci:example.org'
```

This writes an `org.recoil.tangled.matrix` record to the maintainer's
account, keyed by the repository's DID, naming the bot and the room's ID.
The bot reads the bindings of each `--account` every minute. It honours a
binding only when its author owns the repository or is one of the
collaborators its knot records, and only for a repository that names a
spindle. The operator's list of accounts is what stops a stranger from
pointing their own repository at a room the bot is in.

## The bot's device

The bot logs in once and keeps its session, Olm account and Megolm
sessions in `DIR/matrix`, so every restart is the same device. If its
access token stops working, it logs in again as the same device.

The first time a device runs, it signs itself with the account's
self-signing key, so that other people's clients show its messages as sent
by a verified device:

- For an account without cross-signing keys, it creates them, uploading
  them with the password, and sets up secret storage and key backup. It
  writes the new recovery key to `DIR/recovery-key` and logs where. Keep a
  copy elsewhere.
- For an account with cross-signing keys, it recovers them from secret
  storage with `--recovery-key-file`, or with `DIR/recovery-key` if it made
  one. Without either, it runs unverified and says so.

A file `DIR/verified-DEVICE` records that a device is verified.

## What it sends

The bot joins every room it is invited to. It sends to a room only once
it has synced, has joined the room and knows the room is encrypted, so
that nothing meant for an encrypted room goes out in the clear. Messages are
`m.notice`s, so they do not ping anyone, with an HTML body. Edits are
`m.replace` events, sent at most every `--edit-interval` seconds per
pipeline. The last edit always shows how the pipeline ended.

Which message reports which pipeline is kept in `DIR/messages.json`, so a
restart goes on editing the same messages, and pipelines that were running
are looked up again.

## Security

Nothing in `DIR` is encrypted at rest, because the Matrix SDK keeps its
keys in plain files. The directory and its files are made readable only by
their owner. Run the bot as its own user.

## Limits

- Spindles publish no stream of pipeline changes, so a change shows up
  within `--interval` seconds.
- Bindings made by collaborators are read only from `--account` accounts.
- Messages waiting for a room are held in memory, so a restart before the
  room is ready sends the pipeline's state from then on.

## Checking against a homeserver

`dune runtest` checks rendering, the message store and binding permissions.
`test/live.exe HOMESERVER USER PASSWORD-FILE STATE-DIR ROOM` drives the bot
against a real homeserver, such as the Matrix SDK's throwaway Synapse
(`wip/matrix/test/integration/synapse.sh up`). It connects, sends a message
to `ROOM` and edits it, then connects again. Run it with an empty
`STATE-DIR` for an account without cross-signing keys, then again after
deleting `STATE-DIR/matrix` and `STATE-DIR/verified-*`, to check that a new
device recovers the keys with the saved recovery key.
