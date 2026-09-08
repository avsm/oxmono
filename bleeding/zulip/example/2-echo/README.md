# 2 · Echo

Change the fixed greeting to a reply built from the incoming message:

```ocaml
let spec =
  Bot.v ()
  |> Bot.on_message (fun _ message ->
      ignore (Event.reply message.envelope ("You said: " ^ message.body)))
```

[`message.body`](../../lib/zulip_bot/event.mli) is the text supplied to the
handler. The runner requests Markdown from Zulip and removes a leading mention
of the bot before dispatching it. The reply is also Markdown, so formatting in
the original text is retained.

```sh
dune exec example/2-echo/echo.exe -- --profile tutorial
```

Expected exchange in a one-to-one DM:

```text
You: tea, please
Bot: You said: tea, please
```

Now try the same text in a channel the bot can access, beginning with a real
mention selected through Zulip's autocomplete. The reply appears in that
channel's same topic. In a group DM, begin with the mention too; the reply
includes everyone in that conversation.

The envelope already carries the destination. The handler does not need to
look up channel names or reconstruct a list of DM recipients.

Next: [3 · Commands](../3-commands/README.md) gives the bot named actions.

[Previous](../1-hello/README.md) · [All examples](../README.md) ·
[Source](echo.ml) · [Build file](dune)
