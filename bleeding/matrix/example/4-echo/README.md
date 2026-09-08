# `4-echo`

<br>

`echo` logs in to a homeserver with a password, then runs the `/sync` loop
and answers every text message it did not send itself with a notice
carrying the same body. It reads the homeserver, the user and the password
from the environment and takes no arguments. It builds on
[`3-sync`](../3-sync#folders-and-files) directly. `matrix-chat.bot`, a bot
framework built on the same loop, appears starting at
[`d-bot`](../d-bot#folders-and-files).

<br>

A Matrix bot is an ordinary client that logs in like any other account and
answers what it sees arrive over the sync loop, rather than a person typing
at a keyboard. Because a bot's own messages come back through that same
loop as everyone else's do, it has to recognize and ignore them, or it
would reply to its own reply forever. `echo` is the smallest program with
that shape, before `matrix-chat.bot` adds anything else around it.

<br>

The login and the call to `Matrix_eio.run_sync` repeat
[`3-sync`](../3-sync#folders-and-files), and the code that skips the first
response is described below. `echo` runs on every later response:

```ocaml
let echo client ~self (room_id, (room : S.Joined_room.t)) =
  match room.timeline with
  | None -> ()
  | Some timeline ->
      List.iter
        (fun (event : E.Raw_event.t) ->
          if
            (not (Matrix_proto.Id.User_id.equal event.sender self))
            && E.Event_type.equal event.type_ E.Event_type.Room_message
          then
            match
              Jsont.Json.decode E.Text_message_content.jsont event.content
            with
            | Ok content
              when E.Text_message_content.msgtype content = E.Msgtype.Text ->
                let room_id = Matrix_proto.Id.Room_id.of_string_exn room_id in
                let body = E.Text_message_content.body content in
                ignore (M.Messages.send_notice client ~room_id ~body)
            | _ -> ())
        timeline.events
```

<br>

The main function keeps a `first_response` flag and skips `echo` entirely
on the first response `on_sync` receives, since that response carries a
room's full recent history, as [`3-sync`](../3-sync#folders-and-files)
explains. Every later response carries only new events, and `echo` runs on
those.

<br>

`self`, read with
[`Matrix_eio.Client.user_id`](../../lib/matrix_eio/client.mli), is compared
against each event's sender, and `echo` answers only an event sent by
someone else. Without that comparison the bot would answer its own reply,
and then its own answer to that.

<br>

`echo` answers only when the decoded content's `msgtype` is
`Msgtype.Text`, so an emote, an image, a file and a notice all pass through
unanswered. [`Matrix_eio.Messages.send_notice`](../../lib/matrix_eio/messages.mli)
sends the reply as `m.notice` rather than `m.text`. Because a notice never
matches the `Msgtype.Text` guard, `echo` never answers one, including a
notice sent by another instance of itself, which is what keeps two such
bots in the same room from replying to each other forever.

<br>

The transcript runs the bot as `bob` in one terminal, sends `ping` as
`alice` with [`2-send`](../2-send#folders-and-files) from another, then
uses [`3-sync`](../3-sync#folders-and-files) as `alice` to show both
messages landing in the room.

<pre><code><b>$ export MATRIX_HOMESERVER=http://127.0.0.1:8008 MATRIX_USER=bob-a7f3 MATRIX_PASSWORD=pw12345</b>
<b>$ dune exec -- example/4-echo/echo.exe &</b>
<b>$ export MATRIX_USER=alice-a7f3</b>
<b>$ dune exec -- example/2-send/send.exe '!LtsHUNXRlllcmldbGs:localhost' 'ping'</b>
Sent $ucpdUCpMK7bDkRT5CG7SC8cAJfthc8dXl9VXL75jRi8
<b>$ dune exec -- example/3-sync/sync.exe</b>
...
[!LtsHUNXRlllcmldbGs:localhost] @alice-a7f3:localhost: ping
[!LtsHUNXRlllcmldbGs:localhost] @bob-a7f3:localhost: ping
</code></pre>

<br>

**Next:** [`5-rooms`](../5-rooms#folders-and-files) creates and manages the
rooms a bot like this one would live in.

**See also:** [`d-bot`](../d-bot#folders-and-files) rebuilds this bot on
`matrix-chat.bot`, which adds a profile, encryption, per-room ordering and
restart safety.

<br>

[Up to the example index](../#readme)
