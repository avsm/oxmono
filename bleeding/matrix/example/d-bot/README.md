# `d-bot`

<br>

`bot` is the echo bot of [`4-echo`](../4-echo#folders-and-files) rewritten
as a `matrix-chat.bot` plugin. It takes `--homeserver`, `--username`,
`--password-file` and `--profile`, joins every room it is invited to, and
answers every plain message with `you said: ` followed by the message body.

<br>

`4-echo` logs in, runs the sync loop and dispatches events by hand, the same
plumbing every bot needs regardless of what it does with a message. The
`matrix-chat.bot` framework takes that plumbing off the programmer, credentials,
the sync loop, joining invited rooms and saving encryption state on exit,
and leaves a plugin to state only what it reacts to and how, as a value that
a program can also compose with other plugins.

```ocaml
module Bot = Matrix_bot.Bot
module Room = Matrix_bot.Room
module Event = Matrix_bot.Event
module Main = Matrix_bot.Main

let plugin : Bot.plugin =
  Bot.on_message (fun _ (message : Event.message) ->
      ignore
        (Room.send_notice message.envelope.room
           ("you said: " ^ message.content.body)))

let spec = plugin (Bot.v ~name:"bot" ())

let () =
  Main.run ~name:"bot" ~doc:"An echo bot built from a matrix-chat.bot plugin"
    (Cmdliner.Term.const spec)
```

<br>

[`Bot.on_message`](../../lib/matrix_bot/bot.mli) registers a handler that
runs on every [`Event.message`](../../lib/matrix_bot/event.mli), a plain
message that is not a command. The handler reads the room to answer through
from the event's `envelope`, and replies with
[`Room.send_notice`](../../lib/matrix_bot/room.mli), an `m.notice`, which a
client answering messages is required to ignore, so that two bots dropped
into the same room do not answer each other for ever.

[`Bot.v ~name:"bot" ()`](../../lib/matrix_bot/bot.mli) builds a spec with no
handlers, and `plugin` applied to it adds the one above. A plugin always has
type `spec -> spec`, which is how several of them compose, threading the
same value through, as the `matrix-bot` program in `bin/matrix-bot` does
with six. `auto_join` defaults to `true`, so `Bot.v` here also accepts every
invitation without a handler asking for it.

[`Main.run`](../../lib/matrix_bot/main.mli) takes the spec as a
`Cmdliner.Term.t` rather than a plain value, so a bot with flags of its own
can build a different spec per invocation. It never returns. It adds
`--homeserver`, `--username`, `--password-file`, `--profile` and the
log-level flags, evaluates them, connects, and runs until `SIGINT`,
`SIGTERM`, or a handler calls `Bot.stop`. The encryption state is saved on
the way out in every case.

<br>

A run, with the bot invited to a room and a message sent from another user.

<pre><code><b>$ export MATRIX_HOMESERVER=http://127.0.0.1:8008 MATRIX_USERNAME=examplebot MATRIX_PASSWORD=botpassword123</b>
<b>$ dune exec -- example/d-bot/bot.exe -v</b>
bot.exe: [INFO] Logging in as examplebot
bot.exe: [INFO] Login successful: user_id=@examplebot:localhost device_id=OYIWJAGMSS
bot.exe: [INFO] Joined !otNeeMDvmKAohuqZXy:localhost on an invite
</code></pre>

The other user's view of the room after sending `hello, bot!`.

```json
{"sender": "@human:localhost", "content": {"body": "hello, bot!", "msgtype": "m.text"}}
{"sender": "@examplebot:localhost", "content": {"body": "you said: hello, bot!", "msgtype": "m.notice"}}
```

`SIGINT` stops the bot and exits with status 0.

<br>

**Next:** [`e-commands`](../e-commands#folders-and-files) adds commands with
arguments and generated help.

**See also:** the `matrix-bot` program in `bin/matrix-bot` composes five
plugins on the same `Bot.spec`.

<br>

[Up to the example index](../#readme)
