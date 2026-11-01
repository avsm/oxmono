# 4 · Rooms

The `!where` command describes its conversation. A `Room.t` represents either
a channel or a direct-message conversation; its reply destination also retains
the incoming topic or DM participants.

```ocaml
let room = command.message.envelope.room in
let description =
  if Room.is_direct room then
    Printf.sprintf "A direct message with %d participants."
      (List.length (Room.participants room))
  else
    Printf.sprintf "A channel, in topic %S."
      (Option.value ~default:"" (Room.topic room))
```

The full [handler](rooms.ml) replies with this description and `Room.key room`,
a stable string identifying the conversation.

```sh
dune exec example/4-rooms/rooms.exe -- --profile tutorial
```

Try `!where` in a one-to-one DM. The participant count includes your account
and the bot, so it is two. Try it again in a group DM, starting with a mention;
the reply goes to the full group.

Then mention the bot followed by `!where` in two topics of the same channel.
The topic in each reply changes, while the `channel:…` conversation key stays
the same. This distinction matters for persistent state: a key scoped by
`Room.key` belongs to the whole channel, across its topics.

Handlers run in order within each room. Other rooms can proceed concurrently.
For a channel, that ordering includes messages across all its topics.

Next: [5 · Plugins](../5-plugins/README.md) puts a reusable handler in its own module.

[Previous](../3-commands/README.md) · [All examples](../README.md) ·
[Source](rooms.ml) · [Room API](../../lib/zulip_bot/room.mli) · [Build file](dune)
