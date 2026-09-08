# `h-notify`

<br>

`notify` sends one message to a room and exits, the shape a cron job wants.
Besides the usual `--homeserver`, `--username`, `--password-file` and
`--profile`, it takes `--room ROOM_ID` and `--body TEXT`. It prints the sent
message's event id on standard output and exits 0, or prints the failure on
standard error and exits 1.

<br>

Sending into a room is not a one-off HTTP call a script can make on its own.
A device has to be a member of the room, hold a session, and, if the room is
encrypted, hold the room's current Megolm session, all of which normally
come from running the sync loop first. `notify` still has to bring up that
machinery for one message, which is why it behaves like any other bot up to
the point of sending, and only its exit as soon as the message lands sets it
apart.

```ocaml
module Bot = Matrix_bot.Bot
module Room = Matrix_bot.Room
module Sent = Matrix_bot.Sent
module Main = Matrix_bot.Main
module Id = Matrix_proto.Id

let notify ~room_id ~body bot room =
  if Id.Room_id.equal (Room.id room) room_id then (
    (match Sent.await (Room.send_text room body) with
    | Sent.Sent event_id -> print_endline (Id.Event_id.to_string event_id)
    | Sent.Failed (Some error) ->
        Printf.eprintf "notify: %s\n%!" (Matrix_client.Error.to_string error)
    | Sent.Failed None -> Printf.eprintf "notify: the send failed\n%!"
    | Sent.Cancelled -> Printf.eprintf "notify: the send was cancelled\n%!"
    | Sent.Timed_out -> Printf.eprintf "notify: the send timed out\n%!");
    Bot.stop bot)

let build room_id body =
  Bot.v ~name:"notify" () |> Bot.on_join (notify ~room_id ~body)

let room =
  Cmdliner.Arg.(
    required
    & opt (some Matrix_cli.room_id_conv) None
    & info [ "room" ] ~docv:"ROOM_ID" ~doc:"The room to send $(b,--body) to.")

let body =
  Cmdliner.Arg.(
    required
    & opt (some string) None
    & info [ "body" ] ~docv:"TEXT" ~doc:"The message to send.")

let () =
  Main.run ~name:"notify" ~doc:"Sends one message to a room and exits"
    Cmdliner.Term.(const build $ room $ body)
```

<br>

`room` is `--room ROOM_ID`, parsed by
[`Matrix_cli.room_id_conv`](../../lib/matrix_cli/matrix_cli.mli), and `body`
is `--body TEXT`. `Cmdliner.Term.(const build $ room $ body)` builds the
`Bot.spec` from the two, and is what
[`Main.run`](../../lib/matrix_bot/main.mli) evaluates alongside its own
`--homeserver`, `--username`, `--password-file` and `--profile`.

[`Bot.on_join`](../../lib/matrix_bot/bot.mli) fires once for every room the
bot handles, including a room it was already a member of before this run, so
it fires as soon as the sync response after connecting lists the target
room. `notify` compares the room's id to `--room` and does nothing for any
other room the bot happens to be in.

[`Room.send_text`](../../lib/matrix_bot/room.mli) queues the message, and
[`Sent.await`](../../lib/matrix_bot/sent.mli) blocks until the homeserver
answers it. `Sent` on success prints the event id, and every other outcome,
`Failed`, `Cancelled` or `Timed_out`, prints the reason to standard error.
Either way [`Bot.stop`](../../lib/matrix_bot/bot.mli) is called next, which
is what makes `Main.run` return and the process exit.

The session and the encryption state live under
`$XDG_DATA_HOME/matrix/profiles/<profile>/`, as
[`7-profile`](../7-profile#folders-and-files) describes. A cron job supplies
no `MATRIX_PASSWORD`, so the profile must already hold a session from an
earlier run made with the password set, and the bot must already be a
member of `--room`, invited and auto-joined on that earlier run.

<br>

The profile named on `--profile` already holds a session for a bot that is a
member of the room.

<pre><code><b>$ dune exec -- example/h-notify/notify.exe --profile default --room '!NGiWQgBpCPsVaGWfLi:localhost' --body 'deploy finished'</b>
$zKzZHD_goh0ih3edddsoE30LBDfqx-u0jUkTzlWY5MY
<b>$ echo $?</b>
0
</code></pre>

<br>

**Next:** the tutorial ends here. [the index](../#readme) lists the `p-`,
`r-` and `u-` sections, which cover `matrix-chat.proto`, `matrix-chat.client` and
`matrix-chat.ui` on their own.

**See also:** [`g-membership`](../g-membership#folders-and-files) is a bot
that runs until stopped rather than exiting after one send.

<br>

[Up to the example index](../#readme)
