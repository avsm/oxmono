# 3 · Commands

This bot answers `!ping`, repeats `!echo TEXT`, and lists its commands with
`!help`.

```ocaml
let spec =
  Bot.v ()
  |> Bot.command ~name:"ping" ~doc:"check that the bot is alive" ping
  |> Bot.command ~name:"echo" ~args:"TEXT" ~doc:"repeat your text" echo
  |> Bot.help
```

[`Bot.command`](../../lib/zulip_bot/bot.mli) takes a name without the `!`
prefix. Its handler receives an [`Event.command`](../../lib/zulip_bot/event.mli):
`command.args` is the remaining text, and `command.message.envelope` is where
to reply. The `~args` label documents the syntax in help; the handler still
checks whether required arguments are present.

For example, the echo handler is:

```ocaml
let echo _ (command : Event.command) =
  let reply =
    if command.args = "" then "Usage: !echo TEXT" else command.args
  in
  ignore (Event.reply command.message.envelope reply)
```

```sh
dune exec example/3-commands/commands.exe -- --profile tutorial
```

Try these in a one-to-one DM:

```text
You: !ping
Bot: pong
You: !echo two words
Bot: two words
You: !echo
Bot: Usage: !echo TEXT
```

`!help` lists `ping`, `echo`, and `help`, including the descriptions supplied in
the spec. An unknown command gets a suggestion to try `!help`. Plain text has
no handler in this example, so it gets no reply. In a channel or group DM,
commands still need a leading mention of the bot.

Next: [4 · Rooms](../4-rooms/README.md) examines the conversation behind a reply.

[Previous](../2-echo/README.md) · [All examples](../README.md) ·
[Source](commands.ml) · [Build file](dune)
