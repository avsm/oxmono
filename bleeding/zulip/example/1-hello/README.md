# 1 · Hello

This is a complete bot. Send it `hello` in a one-to-one DM and it replies
`Hello!`.

```ocaml
open Zulip_bot

let spec =
  Bot.v ()
  |> Bot.on_message (fun _ message ->
      ignore (Event.reply message.envelope "Hello!"))

let () = Zulip_bot_cli.Main.run ~name:"hello" spec ()
```

[`Bot.v`](../../lib/zulip_bot/bot.mli) makes an empty specification.
`Bot.on_message` adds a function to call when a plain message arrives. The
first argument, unused here, is the running bot. The second is the message.
[`Event.reply`](../../lib/zulip_bot/event.mli) uses its envelope to send the
answer back to the right place. `ignore` discards the queued send's handle.

[`Main.run`](../../lib/zulip_bot_cli/main.mli) provides the command line, opens
the chosen profile, and runs the bot until you press Ctrl-C. You can focus on
the handler while it manages the live event queue.

After the [one-time profile setup](../README.md#set-up-once), run:

```sh
dune exec example/1-hello/hello.exe -- --profile tutorial
```

Expected exchange in a DM:

```text
You: hello
Bot: Hello!
```

Try changing `"Hello!"`, stop the program, and run it again. The reply changes
without changing how the bot connects or receives messages. Use plain `hello`
here: a leading `!` invokes command parsing. Commands are introduced in
[step 3](../3-commands/README.md).

Next: [2 · Echo](../2-echo/README.md) reads the incoming text.

[All examples](../README.md) · [Source](hello.ml) · [Build file](dune)
