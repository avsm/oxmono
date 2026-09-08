# Examples

Each directory holds one program, its `dune` file and a README that walks
through it. The examples are about using the libraries. The numbered and
lettered directories form a tutorial over `matrix-chat.eio` and `matrix-chat.bot`. It
starts with a login, adds sending, receiving, sessions, encryption,
verification and key backup, and ends with a series of bots. The `p-`, `r-`
and `u-` directories cover the other libraries on their own: `matrix-chat.proto`,
the result-returning `matrix-chat.client`, and `matrix-chat.ui`.

The examples build with the rest of the tree. From the repository root:

<pre><code><b>$ dune build @all</b>
<b>$ dune exec -- example/1-login/login.exe</b>
</code></pre>

They have no `dune-project` of their own because two of the libraries they
depend on, `fetch` and `httpz`, are not yet released to opam. The repository
README lists the pins.

<br>

# Tutorial

- [`1-login`](1-login#folders-and-files) logs in with a password, prints the
  user id and device id, and logs out.
- [`2-send`](2-send#folders-and-files) sends one text message to a room.
- [`3-sync`](3-sync#folders-and-files) runs the sync loop and prints messages
  as they arrive.
- [`4-echo`](4-echo#folders-and-files) answers every message in a room, using
  the sync loop alone.
- [`5-rooms`](5-rooms#folders-and-files) creates a room, invites a user, sets
  the topic, lists the members and leaves.
- [`6-media`](6-media#folders-and-files) uploads a file, sends it to a room
  and downloads it again.
- [`7-profile`](7-profile#folders-and-files) stores the session on disk so
  that later runs need no password.
- [`8-cli`](8-cli#folders-and-files) adds the `matrix-chat.cli` terms, so the
  program has `--homeserver`, `--profile`, `-v` and exit statuses.
- [`9-encrypt`](9-encrypt#folders-and-files) creates an encrypted room and
  exchanges a message each way.
- [`a-verify`](a-verify#folders-and-files) verifies another device by
  comparing emoji.
- [`b-backup`](b-backup#folders-and-files) backs up room keys under a
  recovery key and restores them on a new device.
- [`c-oauth`](c-oauth#folders-and-files) logs in through a browser with
  OAuth 2.0.
- [`d-bot`](d-bot#folders-and-files) is the echo bot as a `matrix-chat.bot`
  plugin, with a profile, encryption and a command line.
- [`e-commands`](e-commands#folders-and-files) answers `!commands` with
  arguments, generates `!help`, and restricts some commands to room
  administrators.
- [`f-store`](f-store#folders-and-files) remembers facts per room in the
  plugin store, and keeps them across restarts.
- [`g-membership`](g-membership#folders-and-files) greets members who join,
  notes those who leave, and reports changes to the room name and topic.
- [`h-notify`](h-notify#folders-and-files) sends one message and exits with a
  status, for use from cron.

<br>

# `matrix-chat.proto`, offline

- [`p-events`](p-events#folders-and-files) builds event content, encodes it
  to JSON and decodes it back, with typed identifiers throughout.
- [`p-sync`](p-sync#folders-and-files) decodes a saved `/sync` response and
  walks its rooms, state and timelines.

<br>

# `matrix-chat.client`, the result-returning layer

- [`r-mock`](r-mock#folders-and-files) drives the client against a mock HTTP
  backend, inspects the requests it makes and handles the `Error.t` it returns.
- [`r-errors`](r-errors#folders-and-files) tells transport failures, Matrix
  error codes and decoding failures apart, and retries the ones worth retrying.

<br>

# `matrix-chat.ui`, reactive models

- [`u-room-list`](u-room-list#folders-and-files) runs a `Runtime` and prints
  the room list as a snapshot followed by diffs.
- [`u-timeline`](u-timeline#folders-and-files) opens a room timeline over a
  SQLite event cache, pages back through history and follows new items.

<br>

# A homeserver to try them against

Every transcript in these READMEs was recorded against the Synapse the
integration suite uses. `test/integration/synapse.sh up` starts it in Docker
with open registration at `http://127.0.0.1:8008`, and a user is registered
with one `POST` to `/_matrix/client/v3/register` using the `m.login.dummy`
flow. Any homeserver that allows password login works for the tutorial, and
the encryption and verification examples need a second client, for which the
`omatrix` program in `bin/omatrix` serves.
