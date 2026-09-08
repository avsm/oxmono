# OCaml Zulip

OCaml bindings for the Zulip Server 12.2 API using
[Fetch](https://github.com/avsm/httpz) with the httpz backend, Eio, Jsont, and
Xdge. Local integration tests run against a pinned Zulip Server 12.2 image.

Start with the [example tutorial](example/README.md): a small bot that says
hello, then echo, commands, rooms, plugins, and persistent
state. Each step is a runnable program with an explanation. The
[offline mock](example/r-mock/README.md) runs without a Zulip account.

| Dune library | Purpose |
| --- | --- |
| `zulip` | Pure protocol types, distinct IDs, Jsont codecs and search narrows |
| `zulip.eio` | Fetch transport, profiles, endpoint helpers and live event queues |
| `zulip.bot` | Composable handlers, commands, rooms, send handles and plugin state |
| `zulip.bot.cli` | Cmdliner entry points for bots and one-shot programs |

[API_COVERAGE.md](API_COVERAGE.md) maps endpoint families and Python SDK helpers
to their canonical OCaml implementations. Endpoint modules such as `Messages`,
`Attachments`, `Channels`, and `Scheduled_messages` own their respective
operations. Public identifiers modeled by the library use distinct
`Zulip.Id.*` types.
Extensible response fields remain available at explicit `raw` or `extensions`
boundaries, and future enum values remain representable where Zulip can add
values. `Client.request` also supports additional endpoints.

## Build and test

Requires OCaml 5.5 and Dune 3.21 or newer. Install the dependencies listed in
[dune-project](dune-project). During development, a sibling httpz checkout can
be linked into the workspace:

```sh
ln -s ../ocaml-httpz ocaml-httpz  # only if the link is not already present
dune build zulip.install
dune build @example/all
dune runtest test
```

Build browsable API documentation with `dune build @doc-new` using Dune 3.24
and odoc 3. This includes dependency documentation, so links to Jsont and the
OCaml standard library resolve. Open
`_build/default/_doc_new/html/docs/local/zulip/index.html` after the build.

The ordinary test suite uses local fixtures and Fetch/Eio mocks. The real-server
suite is explicit and needs Docker Compose:

```sh
test/integration/zulip.sh run
```

This starts a pinned Zulip stack, creates local users/bots/channels, runs the
OCaml scenarios, and removes its containers and data. No external Zulip account
is required. The first run downloads images. See the
[integration guide](test/integration/README.md) for interactive servers, logs,
ports and cleanup.

## Credentials and profiles

Named JSON profiles live at
`$XDG_CONFIG_HOME/zulip/profiles/<name>.json`, defaulting to
`~/.config/zulip/profiles/<name>.json`:

```json
{
  "site": "https://eeg.zulipchat.com",
  "email": "your-bot-email",
  "api_key": "your-api-key"
}
```

The profile directory and files must grant no group or other access.
The profile writer creates directories with mode 0700 and files with mode 0600,
and replaces files atomically. You can import the standard `zuliprc` downloaded
from Zulip:

```sh
dune exec example/1-hello/hello.exe -- --profile eeg --zuliprc ~/Downloads/zuliprc
```

Later runs need only `--profile eeg`. The importer reads the `[api]` section's
`site`, `email` and `key` fields, without INI interpolation or the old custom
`[bot]` section. It is a deliberately limited reader, not a general INI parser.

Configuration precedence is explicit `--site`/`--email`/`--api-key` options,
then `ZULIP_SITE`/`ZULIP_EMAIL`/`ZULIP_API_KEY`, then the selected profile.
Malformed existing profiles are reported. Xdge's `ZULIP_CONFIG_DIR` and
`ZULIP_DATA_DIR` overrides select base directories; Xdge appends `zulip`.

## Client

```ocaml
let () =
  Eio_main.run @@ fun env ->
  let open Zulip_eio in
  let profile = Profile.resolve ~fs:env#fs "eeg" |> Error.or_raise in
  let transport = Transport.v env in
  let client =
    Client.create ~transport ~auth:(Profile.auth profile) () |> Error.or_raise
  in
  let id =
    Messages.send_channel client ~channel:"general" ~topic:"OCaml"
      ~content:"Hello from OCaml!" () |> Error.or_raise
  in
  Format.printf "Sent message %a@." Zulip.Id.Message.pp id
```

Endpoint helpers return results with structured API, HTTP, JSON, transport,
timeout and configuration errors. The client scopes Basic credentials to the
configured origin and bounds decoded bodies. The transport separates long polls
from ordinary requests. Automatic transport retries are limited to GET/HEAD.
Mutations are not replayed after ambiguous failures. Cleartext authentication requires the
explicit `Client.create ~allow_insecure:true` used by local tests.

`Messages.get_messages` has separate range and selected-ID modes. A range
requires nonnegative `num_before` and `num_after`; its anchor defaults to
`Newest`, and `Date "2005-04-18"` selects the first message at or after an
ISO 8601 date or datetime. A `message_ids` query omits the anchor and counts.
Zulip returns only requested messages that exist, are accessible, and match an
optional narrow.

## Bots

```ocaml
open Zulip_bot

let spec =
  Bot.v ()
  |> Bot.command ~name:"ping" ~doc:"check that the bot is alive"
       (fun _ command -> ignore (Event.reply command.message.envelope "pong"))
  |> Bot.help

let () = Zulip_bot_cli.Main.run ~name:"zulip-ping" spec ()
```

The interface follows the Matrix bot library's `spec -> spec` plugin approach.
Commands, help, filters, error hooks and typed plugin state compose without
depending on the collector. This prepares for a shared Matrix/Zulip adapter;
the two libraries still have distinct types.

Bots answer one-to-one DMs and explicit mentions by default, ignoring their own
messages and other known bots. Replies preserve a channel's topic or a group DM's
participants. Room handlers run in order, with bounded queues and a fixed worker
budget; different rooms can proceed concurrently. Overload backpressures event
collection. `Sent.await` returns `` `Done outcome `` after the send reaches a
terminal outcome, or `` `Timed_out `` when only the observation deadline expires.
A timeout leaves the send running. An `Indeterminate` terminal outcome means
Zulip may already have accepted it, so the application must decide how to
recover. The sender retries explicit rate-limit rejections only. Releasing the
context switch wakes blocked queue admissions, cancels queued sends, and marks
in-flight sends indeterminate. `Bot.stop` requests shutdown of the collector and
room workers while leaving the context sender alive.

The CLI persists plugin values independently at
`$XDG_DATA_HOME/zulip/profiles/<name>/plugins.json`. File updates are atomic within
one process. Delivery is live: restarts register fresh queues, and expired
queues report recovery before re-registering. Pending events and sends are not
persisted; reconnects can lose events. Handlers should not assume exactly-once
side effects.

Follow the [tutorial](example/README.md) from the first bot, or jump to:

- [Commands and help](example/3-commands/README.md):
  `dune exec example/3-commands/commands.exe -- --profile eeg`
- [Persistent room counter](example/6-state/README.md):
  `dune exec example/6-state/state.exe -- --profile eeg`
- [One-shot account inspection](example/r-client/README.md):
  `dune exec example/r-client/client.exe -- --profile eeg`

The webhook adapter validates Zulip's token/trigger/message envelope and
dispatches into an existing bot. The application supplies its HTTP server;
replies use the same queued API-send path. See the generated API documentation
for lifecycle and routing details.

ISC license; see [LICENSE.md](LICENSE.md).

For Zulip Cloud deployment, see the [hosted bot guide](HOSTED_BOTS.md) and
[read-only preflight](example/r-preflight/README.md). The
[implementation plan](HOSTED_BOT_PLAN.md) records the hosted-bot and Python SDK
parity scope.
