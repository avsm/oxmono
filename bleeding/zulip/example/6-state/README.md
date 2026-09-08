# 6 · State

The `!count` command increments a counter and saves it. Stop and restart the
program with the same profile, and the next command continues where it left off.

```ocaml
let room = Room.key command.message.envelope.room in
let result =
  Plugin_store.update (Bot.plugin_store bot) ~room ~plugin:"tutorial.counter"
    ~key:"value" Jsont.int (fun old -> Option.value ~default:0 old + 1)
```

[`Plugin_store.update`](../../lib/zulip_bot/plugin_store.mli) reads the old
value, computes the new value, and saves it as one serialized operation. A new
counter has no value (`None`), so the function starts from zero. `Jsont.int`
describes how to read and write the integer as JSON.

The key has three parts: the plugin's name, the room, and `"value"`. Another
plugin can use its own `"value"` without colliding with this counter. The full
[handler](state.ml) replies with the count on success and reports a storage
error if saving fails.

```sh
dune exec example/6-state/state.exe -- --profile tutorial
```

With a fresh counter in a one-to-one DM:

```text
You: !count
Bot: Count: 1
You: !count
Bot: Count: 2
```

Press Ctrl-C, run the same command again, then send `!count`. The reply is
`Count: 3`. Try a channel with a leading mention: it has a separate counter.
Two topics in that same channel share the counter because `Room.key` identifies
the channel. A group DM has its own counter too.

By default the values live in
`~/.local/share/zulip/profiles/tutorial/plugins.json`. XDG settings can move
this directory. Keep one process writing a profile's store at a time; atomic
file replacement does not provide a lock between independent processes.

Persistent values do not make delivery persistent. The bot registers a fresh
event queue after a restart, so it does not count commands sent while offline.
A successful state update and its reply are separate operations: a missing
reply does not prove the counter was unchanged.

That completes the tutorial. Next, use the [client recipe](../r-client/README.md)
to query Zulip, the [send recipe](../r-send/README.md) for a one-shot program,
or the [mock recipe](../r-mock/README.md) to work without a server.

[Previous](../5-plugins/README.md) · [All examples](../README.md) ·
[Source](state.ml) · [Build file](dune)
