# Learn Zulip by example

Start with a bot that says hello, then add one idea at a time. Each directory
contains a small runnable program, its `dune` file, and a README explaining the
code and what to try. This follows Dream's tutorial style and the Matrix
library's examples. All examples build as part of this repository; they share
its `dune-project` and dependencies.

Want to try something without an account? [Run the offline mock](r-mock/README.md).

## Tutorial

| Example | What you learn |
| --- | --- |
| [1 · Hello](1-hello/README.md) | Register a handler and run a bot |
| [2 · Echo](2-echo/README.md) | Read a message and reply to its conversation |
| [3 · Commands](3-commands/README.md) | Handle `!ping`, arguments, and generated help |
| [4 · Rooms](4-rooms/README.md) | Understand channels, topics, and direct messages |
| [5 · Plugins](5-plugins/README.md) | Reuse an ordinary OCaml function to compose a bot |
| [6 · State](6-state/README.md) | Keep a counter across restarts with a Jsont codec |

## Standalone recipes

| Example | What you learn |
| --- | --- |
| [Preflight](r-preflight/README.md) | Check hosted authentication and channel access without posting |
| [Client](r-client/README.md) | Query an account's subscribed channels and exit |
| [Send](r-send/README.md) | Send one message to a chosen channel and topic |
| [Saved snippets](r-snippets/README.md) | Create, edit, list, and delete reusable Markdown content |
| [Mock](r-mock/README.md) | Supply a Fetch backend and use the client without a server |

## Set up once

Build the repository with the dependencies in the [main README](../README.md).
Run all commands below from the repository root:

```sh
dune build example
```

For the tutorial, use a Zulip bot account and download its standard `zuliprc`
credentials file. It contains the `[api]` fields `site`, `email`, and `key`, as
shown in [zuliprc.example](zuliprc.example). The `site` is your organization's
HTTPS address. Keep the downloaded file outside your source tree.

Start the first example and import those credentials into a profile named
`tutorial`:

```sh
dune exec example/1-hello/hello.exe -- \
  --profile tutorial --zuliprc ~/Downloads/zuliprc
```

This creates `~/.config/zulip/profiles/tutorial.json` with private permissions.
Later runs only need `--profile tutorial`. XDG environment variables can move
the profile directory; see [profiles](../README.md#credentials-and-profiles).

Send the bot a **one-to-one direct message** from your own user account. Use
`hello` for the first example. Stop the program with **Ctrl-C** before starting
the next one, and keep using the same profile. Run one tutorial bot at a time:
two processes using the same account can both answer the same incoming message.

## How to address the bot

The examples answer ordinary messages and commands in one-to-one DMs. In a
channel or group DM, insert a mention of the bot with Zulip's autocomplete at
the start of the message, then write the text or command. For example, insert
the bot's mention followed by `!ping`. The bot must have access to that channel.

The mention uses the account's **Zulip display name**. Changing the executable
name does not rename the account. The profile name only selects
local credentials and state.

Replies go to the same channel and topic, or to the same DM participants. The
default bot ignores its own messages and messages from other known bots. A
leading `!` introduces a command; plain message handlers receive other text.

Examples show expected exchanges, rather than recorded sessions from a hosted
organization. Most queue their replies with `Event.reply` and keep running.
Queueing is not confirmation of delivery; [the send recipe](r-send/README.md)
explains how to distinguish `` `Done outcome `` from an observation timeout
before exiting.

## Local tests and further reading

The [mock recipe](r-mock/README.md) needs neither credentials nor Docker. For
real-server tests without an external organization, run the integration suite:

```sh
test/integration/zulip.sh run
```

That command provisions its own accounts on a local HTTP server and exercises
the client and bot runtime. The tutorial CLI also supports the local server with `--allow-insecure-http`.
The suite exercises a real tutorial process using isolated fixture credentials.
See the [hosted bot guide](../HOSTED_BOTS.md) when moving to Zulip Cloud.
See the [Docker guide](../test/integration/README.md) for setup and resource use.

Build the [API documentation](../doc/index.mld) with
`dune build @doc-new` using Dune 3.24 and odoc 3, or look up an endpoint in the
[coverage inventory](../API_COVERAGE.md). The numbered tutorial ends with a
composable bot whose plugin values survive restarts. Event replay during
downtime and a shared Matrix/Zulip adapter remain separate concerns.
