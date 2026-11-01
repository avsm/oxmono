# matrix-bot

<br>

`matrix-bot` is one executable built from five `matrix-chat.bot` plugins and a
one-shot notifier. Each `--name` flag adds one plugin to the same bot, so
several run together in one room over one sync loop. `--notify` is the
exception, it sends one message and exits instead of running a bot. Where
[`d-bot`](../d-bot) built one plugin into one program,
this directory composes several.

| Plugin | What it exercises in `matrix-chat.bot` |
| --- | --- |
| [`Echo`](echo.mli) | `Bot.on_message` and `Room.send_notice`, the whole of a bot in two lines |
| [`Commands`](commands.mli) | `Bot.command` with arguments and doc, `Bot.help` generated from them, `Event.reply` and `Event.react`, a state event through `Room.set_topic`, and `~admin:true` |
| [`Welcome`](welcome.mli) | `Bot.on_membership`, `Bot.on_room_state` and the `Event.Profile` event, typed state so no bot parses `m.room.member` |
| [`Moderator`](moderator.mli) | `Room.redact` and `Room.kick`, and `Bot.plugin_store` for strike counts that outlive the process |
| [`Logger`](logger.mli) | `Room.backfill` from the `Joined` handler, and `Bot.on` printing every `Event.t` as it arrives |
| [`Notify`](notify.mli) | not a plugin, `Bot.run` with a spec that sends once, awaits the `Sent.t` and calls `Bot.stop` |

<br>

`--echo` takes `--prefix TEXT`, what it puts before a body, defaulting to
`you said: `. `--moderator` takes `--words WORD`, repeatable and defaulting
to `badger` and `spoiler`, and `--strikes N`, defaulting to 3. `--commands`,
`--welcome` and `--logger` take no flags of their own. `--notify ROOM
--body TEXT` joins `ROOM`, sends `TEXT`, prints the event id on standard
output and exits, ignoring every plugin flag. Every invocation also takes
`--homeserver`, `--username`, `--password-file`, `--profile` and `-v`,
repeatable for `Logs` at info and then debug.

The first run needs `--username` and a password, every later run needs only
the profile, and `--help=plain` documents every flag, trimmed here to its
top.

<pre><code><b>$ export MATRIX_PASSWORD=botpassword</b>
<b>$ dune exec matrix-bot -- --username examplebot --profile examplebot \
    --echo --commands --welcome -v</b>
<b>$ dune exec matrix-bot -- --help=plain</b>
NAME
       matrix-bot - Matrix bots built from matrix-chat.bot plugins

OPTIONS
       --commands
           Answer !ping, !roll, !react, !topic and !help.

       --echo
           Repeat every message back.

       --moderator
           Redact forbidden words, warn, and kick on the last strike.

       --notify=ROOM
           Send one message to ROOM, print its event id and exit.
</code></pre>

`--profile NAME` names a directory under
`$XDG_DATA_HOME/matrix/profiles/NAME/`, the same one
[`omatrix`](../omatrix) uses. It holds `session.json`,
the access token and the device id, `crypto_state.json`,
`one_time_keys.json` and the Megolm and Olm session files for this device,
`state.json`, the `Bot.plugin_store` holding each room's cursor and
`--moderator`'s strike counts, and `events.sqlite3`, the event store a
restarted bot reads its own history back from.

`Bot.plugin_store` is a file, so a restart is not a fresh start.
`--moderator` keeps its strike counts, and each room's cursor says what the
bot already handled. The default backlog policy, `Skip`, then passes over
what arrived while the bot was down and moves the cursor to it, so a
restarted bot does not answer its own backlog.

<br>

**See also:** [`d-bot`](../d-bot) is the tutorial's
version of `Echo`, a single plugin in one file.
