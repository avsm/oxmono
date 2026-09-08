# `e-commands`

<br>

`commands` answers `!ping`, `!roll [N]`, `!react`, `!topic TEXT` and `!help`
in every room it has joined, and points an unrecognised `!word` at `!help`.
It takes `--homeserver`, `--username`, `--password-file`, `--profile` and
`-v` from the library and keeps its session and encryption state under a
profile, as [`7-profile`](../7-profile#folders-and-files) describes.

<br>

A room mixes ordinary conversation with lines meant for a bot, and nothing
in the `m.room.message` event tells the two apart. The `!` prefix is the
convention `matrix-chat.bot` uses to mark the second kind, so a bot can watch a
busy room without treating every sentence as an instruction, and a person
typing to another person is never mistaken for one addressing the bot.

```ocaml
module Bot = Matrix_bot.Bot
module Room = Matrix_bot.Room
module Event = Matrix_bot.Event
module Args = Matrix_bot.Args
module Context = Matrix_bot.Context
module Main = Matrix_bot.Main

let ping _ (c : Event.command) = ignore (Event.reply c.message.envelope "pong")

let random bot =
  Matrix_client.Client.random
    (Matrix_eio.Client.base (Context.client (Bot.context bot)))

let roll bot (c : Event.command) =
  let sides =
    match Args.find_word c 0 with None -> Some 6 | Some _ -> Args.find_int c 0
  in
  match sides with
  | Some sides when sides >= 1 ->
      let bytes = Matrix_client.Random.generate (random bot) 2 in
      let n = (Char.code bytes.[0] * 256) + Char.code bytes.[1] in
      ignore
        (Event.reply c.message.envelope (string_of_int ((n mod sides) + 1)))
  | Some _ | None -> ignore (Event.reply c.message.envelope "usage: !roll [N]")

let thumbs_up _ (c : Event.command) =
  ignore (Event.react c.message.envelope "\xf0\x9f\x91\x8d" (* 👍 *))

let topic _ (c : Event.command) =
  if String.equal c.args "" then
    ignore (Event.reply c.message.envelope "usage: !topic TEXT")
  else
    match Room.set_topic c.message.envelope.room c.args with
    | Ok () -> ignore (Event.react c.message.envelope "\xe2\x9c\x85" (* ✅ *))
    | Error error ->
        ignore
          (Event.reply c.message.envelope
             (Format.asprintf "cannot set the topic: %a" Matrix_client.Error.pp
                error))

let unknown _ (c : Event.command) =
  ignore
    (Event.reply c.message.envelope
       (Printf.sprintf "unknown command !%s, try !help" c.name))

let spec =
  Bot.v ~name:"commands" ()
  |> Bot.command ~name:"ping" ~doc:"answer pong, as a reply" ping
  |> Bot.command ~name:"roll" ~args:"[N]"
       ~doc:"roll an N-sided die, N defaults to 6" roll
  |> Bot.command ~name:"react" ~doc:"react to the command with a thumbs up"
       thumbs_up
  |> Bot.command ~name:"topic" ~args:"TEXT" ~doc:"set the room topic"
       ~admin:true topic
  |> Bot.help
  |> Bot.on_unknown_command unknown

let () =
  Main.run ~name:"commands"
    ~doc:"A bot with commands, arguments and generated help"
    (Cmdliner.Term.const spec)
```

<br>

[`Bot.command`](../../lib/matrix_bot/bot.mli) registers a handler under a
name, without the prefix. `~args` fills in a command's arguments in
[`Bot.help`](../../lib/matrix_bot/bot.mli)'s reply, and `~doc` its one-line
description. `help` is registered last so that it lists every command that
came before it.

[`Args.find_word`](../../lib/matrix_bot/args.mli) reads the first word of
`!roll`'s arguments, and its absence stands for six sides. When a word is
present, [`Args.find_int`](../../lib/matrix_bot/args.mli) reads it as the
number of sides, and a word that is not a positive integer gets the usage
line back instead of a roll.
[`Event.reply`](../../lib/matrix_bot/event.mli) sends the result as a notice
in reply to the command.

`!react` and a successful `!topic` both call
[`Event.react`](../../lib/matrix_bot/event.mli), which annotates the command
with an emoji instead of sending a further message. `!topic` calls
[`Room.set_topic`](../../lib/matrix_bot/room.mli), and any failure,
including the bot's own power level being too low, is reported by
`Event.reply` instead of the reaction.

`!topic` is registered `~admin:true`, so
[`Bot.command`](../../lib/matrix_bot/bot.mli) checks the sender's power
level, a number a room's `m.room.power_levels` state assigns each member,
against the spec's `admin_level`, 50 by default, and answers a sender below
it with a refusal before the handler runs. The room's creator holds 100 by
default and a plain member holds 0, and only a member who can already
change `m.room.power_levels` can raise another member's, the bot's own
device included. [`Bot.on_unknown_command`](../../lib/matrix_bot/bot.mli)
replaces the default reply to a `!word` no handler claims.

<br>

`alice` created the room and invited the bot, so she holds power level 100
and the bot holds 0 until she raises it. `bob`, also invited, holds 0.

<pre><code><b>$ export MATRIX_HOMESERVER=http://127.0.0.1:8008 MATRIX_USERNAME=examplebot MATRIX_PASSWORD=botpassword123</b>
<b>$ dune exec -- example/e-commands/commands.exe -v</b>
commands.exe: [INFO] Logging in as examplebot
commands.exe: [INFO] Login successful: user_id=@examplebot:localhost device_id=OYIWJAGMSS
Logged in as @examplebot:localhost, device OYIWJAGMSS; the session is saved under 'default'
commands.exe: [INFO] Joined !sdwGEpiDWKcytnlekh:localhost on an invite
</code></pre>

`alice` raises the bot's power level to 50 from her client and tries
`!topic` again. The room's view of the exchange:

```json
{"sender": "@alice:localhost", "content": {"body": "!help", "msgtype": "m.text"}}
{"sender": "@examplebot:localhost", "content": {"body": "!ping - answer pong, as a reply\n!roll [N] - roll an N-sided die, N defaults to 6\n!react - react to the command with a thumbs up\n!topic TEXT - set the room topic\n!help - list the commands", "msgtype": "m.notice"}}
{"sender": "@alice:localhost", "content": {"body": "!roll 20", "msgtype": "m.text"}}
{"sender": "@examplebot:localhost", "content": {"body": "16", "msgtype": "m.notice"}}
{"sender": "@bob:localhost", "content": {"body": "!topic castle in the clouds", "msgtype": "m.text"}}
{"sender": "@examplebot:localhost", "content": {"body": "you need to be a moderator to do that", "msgtype": "m.notice"}}
{"sender": "@alice:localhost", "content": {"body": "!topic castle in the clouds", "msgtype": "m.text"}}
{"sender": "@examplebot:localhost", "content": {"key": "✅"}}
{"sender": "@alice:localhost", "content": {"body": "!bogus", "msgtype": "m.text"}}
{"sender": "@examplebot:localhost", "content": {"body": "unknown command !bogus, try !help", "msgtype": "m.notice"}}
```

`SIGINT` stops the bot, saves the encryption machine to the profile and
exits 0.

<br>

**Next:** [`f-store`](../f-store#folders-and-files) remembers facts per room
in the plugin store, and keeps them across restarts.

**See also:** [`d-bot`](../d-bot#folders-and-files) is the plain echo bot
this one is built from. The `matrix-bot` program in `bin/matrix-bot`
composes a similar `commands` plugin among six others.

<br>

[Up to the example index](../#readme)
