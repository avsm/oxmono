# `f-store`

<br>

`store` remembers one piece of text per room, across restarts. `!remember
KEY TEXT` stores TEXT under KEY for the room the command was sent in,
`!recall KEY` prints it back, `!forget KEY` removes it, `!facts` lists the
keys held for the room, and `!count` increments and prints a per-room
counter. It takes `--homeserver`, `--username`, `--password-file`,
`--profile` and `-v` from the library and keeps its session and encryption
state under a profile, as [`7-profile`](../7-profile#folders-and-files)
describes.

<br>

A bot process restarts, redeploys and gets killed by the machine it runs on,
and a room does not stop existing between one run and the next. A bot that
keeps what it learns only in memory forgets everything a restart loses, so
anything meant to outlive the process, a fact a room asked it to remember, a
running count, a setting a room chose, needs somewhere on disk to live.
`matrix-chat.bot` gives every plugin a scoped corner of one such file for this.

`recall`, which mirrors `remember` but calls `Plugin_store.find`, `forget`,
which calls `Plugin_store.remove`, and `facts`, which calls
`Plugin_store.keys`, are left out below.

```ocaml
module Bot = Matrix_bot.Bot
module Room = Matrix_bot.Room
module Event = Matrix_bot.Event
module Plugin_store = Matrix_bot.Plugin_store
module Main = Matrix_bot.Main

let remember bot (c : Event.command) =
  match String.index_opt c.args ' ' with
  | None -> ignore (Event.reply c.message.envelope "usage: !remember KEY TEXT")
  | Some i -> (
      let key = String.sub c.args 0 i in
      let text =
        String.trim (String.sub c.args (i + 1) (String.length c.args - i - 1))
      in
      if String.equal text "" then
        ignore (Event.reply c.message.envelope "usage: !remember KEY TEXT")
      else
        match
          Plugin_store.set (Bot.plugin_store bot)
            ~room:(Room.id c.message.envelope.room)
            ~plugin:"store" ~key Jsont.string text
        with
        | Ok () ->
            ignore (Event.react c.message.envelope "\xe2\x9c\x85" (* ✅ *))
        | Error error ->
            ignore
              (Event.reply c.message.envelope
                 ("cannot remember that: " ^ Plugin_store.error_to_string error))
      )

let count bot (c : Event.command) =
  match
    Plugin_store.update (Bot.plugin_store bot)
      ~room:(Room.id c.message.envelope.room)
      ~plugin:"store" ~key:"count" Jsont.int (function
      | None -> 1
      | Some n -> n + 1)
  with
  | Ok n -> ignore (Event.reply c.message.envelope (string_of_int n))
  | Error error ->
      ignore
        (Event.reply c.message.envelope
           ("cannot update the counter: " ^ Plugin_store.error_to_string error))

let spec =
  Bot.v ~name:"store" ()
  |> Bot.command ~name:"remember" ~args:"KEY TEXT"
       ~doc:"remember TEXT under KEY in this room" remember
  |> Bot.command ~name:"recall" ~args:"KEY"
       ~doc:"print what is remembered under KEY" recall
  |> Bot.command ~name:"forget" ~args:"KEY"
       ~doc:"forget what is remembered under KEY" forget
  |> Bot.command ~name:"facts" ~doc:"list the keys remembered in this room"
       facts
  |> Bot.command ~name:"count" ~doc:"increment and print a per-room counter"
       count
  |> Bot.help

let () =
  Main.run ~name:"store"
    ~doc:"A bot that remembers per-room facts across restarts"
    (Cmdliner.Term.const spec)
```

<br>

[`Plugin_store.set`](../../lib/matrix_bot/plugin_store.mli) writes `text`
under `key`, scoped to `~plugin:"store"` and, with `~room`, to the room the
command came from, so the same key in a different room holds a different
value. [`Jsont.string`](https://erratique.ch/software/jsont) is the codec
that turns `text` into the JSON the store keeps.
[`Plugin_store.find`](../../lib/matrix_bot/plugin_store.mli) reads it back
with the same codec, and `Ok None` becomes the "not remembered" reply.
[`Plugin_store.remove`](../../lib/matrix_bot/plugin_store.mli), behind
`forget`, and [`Plugin_store.keys`](../../lib/matrix_bot/plugin_store.mli),
behind `facts`, are scoped the same way.

[`Plugin_store.update`](../../lib/matrix_bot/plugin_store.mli) reads the
current value, passes it to a function and stores the result, with no other
fiber able to read or write the same key in between. `count`'s function
takes `None` to `1` and any other value to its successor, so a key that has
never been set behaves like zero.

[`Bot.plugin_store`](../../lib/matrix_bot/bot.mli) is the `state.json` file
in the profile directory. Every value in it is JSON, and a key is named by
the plugin that wrote it and, when the write named a room, by that room, so
one plugin's keys cannot collide with another's and a room's facts do not
leak into a different room. The file is rewritten whole on every write, and
a bot started again with the same `--profile` opens the same file and reads
back whatever the previous run left there.

<pre><code><b>$ export MATRIX_HOMESERVER=http://127.0.0.1:8008 MATRIX_USERNAME=examplebot MATRIX_PASSWORD=botpassword123</b>
<b>$ dune exec -- example/f-store/store.exe --profile demo -v</b>
store.exe: [INFO] Logging in as examplebot
store.exe: [INFO] Login successful: user_id=@examplebot:localhost device_id=SEVEFDNJTT
Logged in as @examplebot:localhost, device SEVEFDNJTT; the session is saved under 'demo'
store.exe: [INFO] Joined !lqKykfccnMqtbCleYu:localhost on an invite
</code></pre>

`alice` sends `!recall office` before anything is remembered, remembers it,
and recalls it again. The room's view:

```json
{"sender": "@alice:localhost", "content": {"body": "!recall office", "msgtype": "m.text"}}
{"sender": "@examplebot:localhost", "content": {"body": "office is not remembered", "msgtype": "m.notice"}}
{"sender": "@alice:localhost", "content": {"body": "!remember office Building 4, room 12", "msgtype": "m.text"}}
{"sender": "@examplebot:localhost", "content": {"key": "✅"}}
{"sender": "@alice:localhost", "content": {"body": "!recall office", "msgtype": "m.text"}}
{"sender": "@examplebot:localhost", "content": {"body": "Building 4, room 12", "msgtype": "m.notice"}}
```

`Ctrl-C` stops the bot, saves the state file and exits 0. Starting it again
with the same profile finds the room already joined and reuses the session.

<pre><code><b>$ dune exec -- example/f-store/store.exe --profile demo -v</b>
store.exe: [INFO] Reusing the session of profile 'demo' (@examplebot:localhost, device SEVEFDNJTT)
store.exe: [INFO] Joined !lqKykfccnMqtbCleYu:localhost on an invite
</code></pre>

`alice` sends `!recall office` again, and the fact from the first run is
still there:

```json
{"sender": "@alice:localhost", "content": {"body": "!recall office", "msgtype": "m.text"}}
{"sender": "@examplebot:localhost", "content": {"body": "Building 4, room 12", "msgtype": "m.notice"}}
```

<br>

**Next:** [`g-membership`](../g-membership#folders-and-files) reacts to who
is in the room and what it is called.

**See also:** [`e-commands`](../e-commands#folders-and-files) is where the
commands and `!help` used here were introduced.

<br>

[Up to the example index](../#readme)
