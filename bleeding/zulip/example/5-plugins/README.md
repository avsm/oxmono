# 5 · Plugins

A plugin is an ordinary function of type `Bot.spec -> Bot.spec`. It takes a
specification, adds behavior, and returns the new specification. In fact,
`Bot.command` and `Bot.on_message` already produce plugins when given handlers.

This example puts a configurable command in [greeting.ml](greeting.ml):

```ocaml
let command ~name ~greeting : Bot.plugin =
  Bot.command ~name ~args:"[NAME]" ~doc:("say " ^ greeting)
    (fun _ command ->
      let name = if command.args = "" then "there" else command.args in
      ignore
        (Event.reply command.message.envelope
           (Printf.sprintf "%s, %s!" greeting name)))
```

The program in [plugins.ml](plugins.ml) uses it twice:

```ocaml
let spec =
  Bot.v ()
  |> Greeting.command ~name:"hello" ~greeting:"Hello"
  |> Greeting.command ~name:"bye" ~greeting:"Goodbye"
  |> Bot.help
```

```sh
dune exec example/5-plugins/plugins.exe -- --profile tutorial
```

Expected exchange in a one-to-one DM:

```text
You: !hello Ada
Bot: Hello, Ada!
You: !bye
Bot: Goodbye, there!
```

The greeting module knows nothing about profiles or the command-line runner.
Copy it into another bot and apply it to that bot's spec. Use distinct command
names when composing plugins; handlers are registered in pipeline order and
matching handlers all run.

This is also the shape used by `matrix.bot`. The two libraries currently have
their own types, but the separation between application behavior and the runner
is the same. A plugin using these Zulip modules is not yet a Matrix plugin.

Next: [6 · State](../6-state/README.md) lets a plugin remember a value.

[Previous](../4-rooms/README.md) · [All examples](../README.md) ·
[Greeting interface](greeting.mli) · [Bot API](../../lib/zulip_bot/bot.mli) ·
[Build file](dune)
