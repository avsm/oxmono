# Run a bot on Zulip Cloud

The initial deployment target is `https://eeg.zulipchat.com`. The same executable
works with another current Zulip organization. Develop with the
[local Docker server](test/integration/README.md), then use a separate hosted
profile. The library does not need a public listening port for its event queue.

## Public-web validation

An anonymous HTTPS check on 2026-09-06 read three messages each from the
public `Blogs` and `Tessera` channels using the library's Fetch/httpz transport
and decoded all six with `Zulip.Message.jsont`. The server reported version
`12.0-763-ga2f1c1534f`, feature level 507, and enabled public-web access.

The public web route is `GET /json/messages` with an explicit
`streams:web-public` narrow, optionally combined with a channel filter.
The corresponding `/api/v1` channel-list request requires authentication.
This check used no credentials, cookies, event queues or mutations.
Authenticated client and bot validation remains a separate step below.

## Create the account and check access

Create a **Generic bot** in the organization's bot settings and download its
zuliprc. A generic bot runs in your process and connects with its API key.
An outgoing-webhook bot uses a different delivery setup. Organization policy
may restrict bot creation; subscribe the bot to the channels it should read.
See Zulip's [bot setup](https://zulip.com/help/add-a-bot-or-integration) and
[running bots](https://zulip.com/help/running-bots) documentation.

Store the downloaded file privately and import it into a named profile:

```sh
chmod 600 /path/to/zuliprc
dune exec example/r-preflight/preflight.exe -- \
  --profile eeg --zuliprc /path/to/zuliprc
```

This checks authentication, the server version, the account and its subscribed
channels using GET requests. It does not post a message or register a queue.
Check that the printed site is `https://eeg.zulipchat.com` and that this is the
intended bot. Subsequent commands need only `--profile eeg`.

Profiles are private JSON files under
`$XDG_CONFIG_HOME/zulip/profiles/NAME.json` (normally
`~/.config/zulip/profiles/NAME.json`). Explicit CLI credentials override
`ZULIP_SITE`, `ZULIP_EMAIL`, `ZULIP_API_KEY`, which override the selected profile.
Avoid putting API keys in shell history; the zuliprc import is convenient for
local use. TLS uses system trust; custom trust or client certificates can be
provided through `Transport.v ~https` or an injected Fetch capability.

## First exchange

Run the echo tutorial:

```sh
dune exec example/2-echo/echo.exe -- --profile eeg
```

After the connected log appears, send the bot a one-to-one DM from your human
account. It should echo the text. In a dedicated test channel that the bot can
access, start a message with a mention selected using Zulip autocomplete and
then some text. Check that the reply stays in that topic. The commands tutorial
accepts a mention followed by `!ping`:

```sh
dune exec example/3-commands/commands.exe -- --profile eeg
```

Run one example at a time. The defaults process one-to-one DMs and messages
where the bot is mentioned; they ignore the bot itself and other known bots.
`Bot.v ~all_messages:true` broadens activation. The runner refreshes account
names and bot classification from the initial user snapshot and later events.
ID-bearing mentions continue to work after a rename.

## State, delivery and supervision

Start with [plugins](example/5-plugins/README.md) and
[persistent state](example/6-state/README.md). Each plugin uses its own namespace
and a Jsont codec. State lives in
`$XDG_DATA_HOME/zulip/profiles/NAME/plugins.json` (normally
`~/.local/share/zulip/profiles/NAME/plugins.json`); keep that directory across restarts.
Writes are atomic and serialized inside one process. Use one process per
profile/state file; there is no cross-process state locking.

Event delivery is live. The runner reconnects after transient errors, honors
Retry-After, and registers a replacement for an expired queue. It reports
`Recovering` and `Live` through `Bot.on_sync`. It does not replay history missed
while stopped or after queue expiry. Zulip describes snapshot and queue
semantics in [queue registration](https://zulip.com/api/register-queue).

`Event.reply` returns a queued send handle. Use `Sent.await` when delivery must
be observed. It returns `Done outcome` for a terminal send outcome and
`Timed_out` when the observation deadline expires. An explicit rate-limit
rejection can be retried; a lost response after a possible write yields
`Indeterminate error` as the terminal outcome inside `Done`, without
resending blindly. Application side effects should tolerate duplicates.
`Timed_out` leaves the send running.
Releasing the context switch wakes blocked admissions, settles queued sends as
`Cancelled`, and settles in-flight sends as `Indeterminate None`.

Build the executable once and supervise it with your normal service manager.
For example, after installing the executable as `/opt/zulip-bot/echo.exe` and
importing the profile for the `zulipbot` Unix account:

```ini
[Unit]
Description=Zulip echo bot
After=network-online.target
Wants=network-online.target

[Service]
User=zulipbot
Environment=XDG_CONFIG_HOME=/var/lib/zulipbot/config
Environment=XDG_DATA_HOME=/var/lib/zulipbot/data
ExecStart=/opt/zulip-bot/echo.exe --profile eeg
Restart=on-failure
RestartSec=10
TimeoutStopSec=15

[Install]
WantedBy=multi-user.target
```

Import the profile with those same XDG directories before starting the service.
SIGINT and SIGTERM request shutdown. Authentication/configuration errors and
malformed queue responses fail the CLI, so check its logs if supervision keeps
restarting it. Queues and workers are bounded; handlers in one channel run in
order across its topics, while independent conversations can proceed together.

## Lower-level use

`Event_queue.register ~options` controls the initial snapshot and event stream.
`Initial_state` has optional typed accessors; absent snapshot families differ
from empty collections. `Settings.get state Settings.Twenty_four_hour_time`
uses the same typed key as a settings update. `Event_queue.iter` and
`iter_messages` provide callbacks without the bot activation rules.

For manual polling, `Event_queue.get_events` returns a `Batch.t` without
advancing the queue cursor. Accept the events first, then call
`Event_queue.ack queue batch`. The optional `~count` acknowledges a cumulative
prefix of that batch, so a consumer can commit only the accepted prefix.

Use `Zulip.Event_payload.of_event` for typed protocol views. In a bot's custom
callback, `Zulip_bot.Event.payload` provides those views directly. Unsupported
kinds retain their raw JSON; malformed supported payloads are reported separately.
The raw response and generic `Client.request_json` remain available for newer
server fields or endpoints.

An outgoing-webhook adapter can call `Webhook.handle`; map `Stopped` to a
retryable HTTP response, such as 503. Arrange authentication and a public HTTPS
listener in the host application. The queue-based tutorials need neither.

For local Docker use only, the shared CLI accepts `--allow-insecure-http`.
That flag permits credentials over HTTP; it does not disable HTTPS certificate
checking and is unnecessary for the hosted profile.
