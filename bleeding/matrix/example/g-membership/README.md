# `g-membership`

<br>

`membership` narrates who is in a room and what it is called. It greets a
user who joins, notes one who leaves or is kicked, reports a changed room
name or topic, and leaves a room once it is the only member left. It takes
`--homeserver`, `--username`, `--password-file` and `--profile`.

<br>

Who belongs to a room, and its name and topic, are not separate from the
room's timeline. They are `m.room.member`, `m.room.name` and `m.room.topic`
state events, sent and synced the same way a message is, each one replacing
whatever the room previously held for that piece of state. A client that
wants to show who is present, or a bot that wants to react when someone
arrives or leaves, watches these events rather than asking the server for a
snapshot on demand.

```ocaml
module Bot = Matrix_bot.Bot
module Room = Matrix_bot.Room
module Event = Matrix_bot.Event
module Main = Matrix_bot.Main
module Id = Matrix_proto.Id
module P = Matrix_ui.Presentation

let membership bot (m : Event.membership) =
  let room = m.envelope.room in
  let who = Id.User_id.to_string m.user in
  (if not (Id.User_id.equal m.user (Bot.user_id bot)) then
     match m.change with
     | P.Joined | P.Invitation_accepted | P.Knock_accepted ->
         ignore (Room.send_notice room (Printf.sprintf "Welcome, %s." who))
     | P.Left ->
         ignore (Room.send_notice room (Printf.sprintf "%s left the room." who))
     | P.Kicked | P.Kicked_and_banned ->
         ignore
           (Room.send_notice room
              (Printf.sprintf "%s was removed from the room." who))
     | _ -> ());
  match Room.members room with
  | [ only ] when Id.User_id.equal only (Bot.user_id bot) ->
      ignore (Room.leave room)
  | _ -> ()

let room_state _ (s : Event.room_state) =
  let room = s.envelope.room in
  match s.state with
  | P.Room_name (Some name) ->
      ignore
        (Room.send_notice room
           (Printf.sprintf "The room is now called %s." name))
  | P.Room_topic (Some topic) ->
      ignore
        (Room.send_notice room (Printf.sprintf "The topic is now %s." topic))
  | _ -> ()

let invite _ (i : Event.invitation) =
  Printf.printf "invited to %s by %s\n%!"
    (Id.Room_id.to_string i.room_id)
    (match i.inviter with
    | Some user -> Id.User_id.to_string user
    | None -> "an unknown user")

let spec =
  Bot.v ~name:"membership" ()
  |> Bot.on_membership membership
  |> Bot.on_room_state room_state
  |> Bot.on_invite invite

let () =
  Main.run ~name:"membership"
    ~doc:"Narrates who is in the room and what it is called"
    (Cmdliner.Term.const spec)
```

<br>

[`Bot.on_invite`](../../lib/matrix_bot/bot.mli) prints the room id and the
inviter of every pending [`Event.invitation`](../../lib/matrix_bot/event.mli)
on standard output, before `auto_join`, which is on by default, accepts it.
The inviter is printed as `an unknown user` when the invitation reached the
bot on its first sync, which carries no sender for it.

[`Bot.on_membership`](../../lib/matrix_bot/bot.mli) runs on every
[`Event.membership`](../../lib/matrix_bot/event.mli), the bot's own included,
so the handler skips its own join or it would greet itself. A join, an
accepted invitation or an accepted knock gets a `Welcome` notice, a departure
gets a note that the user left, and a kick or a kick with a ban gets a note
that the user was removed, all sent with
[`Room.send_notice`](../../lib/matrix_bot/room.mli). Every membership change
is then checked against [`Room.members`](../../lib/matrix_bot/room.mli), and
the bot calls [`Room.leave`](../../lib/matrix_bot/room.mli) once it is the
only member left.

[`Bot.on_room_state`](../../lib/matrix_bot/bot.mli) runs on every
[`Event.room_state`](../../lib/matrix_bot/event.mli). A changed room name or
topic is announced the same way, read from the `Room_name` and `Room_topic`
cases of
[`Matrix_ui.Presentation.other_state`](../../lib/matrix_ui/presentation.mli).
A name or topic cleared to `None` is not announced.

The session and the encryption state live under
`$XDG_DATA_HOME/matrix/profiles/<profile>/`, as
[`7-profile`](../7-profile#folders-and-files) describes.

<br>

A room already has a creator, `alice`, who invites the bot and then a second
user, `bob`, who joins. `alice` changes the topic with the client-server API
directly, and `bob` leaves.

<pre><code><b>$ export MATRIX_HOMESERVER=http://127.0.0.1:8008 MATRIX_USERNAME=membershipbot MATRIX_PASSWORD=pw12345</b>
<b>$ dune exec -- example/g-membership/membership.exe --profile default</b>
Logged in as @membershipbot:localhost, device OIVWOUYTEI; the session is saved under 'default'
invited to !NGiWQgBpCPsVaGWfLi:localhost by an unknown user
</code></pre>

`bob`'s view of the room after joining, the topic change and his own
departure.

```json
{"sender": "@membershipbot:localhost", "content": {"body": "Welcome, @bob:localhost.", "msgtype": "m.notice"}}
{"sender": "@membershipbot:localhost", "content": {"body": "The topic is now weekly sync notes.", "msgtype": "m.notice"}}
{"sender": "@membershipbot:localhost", "content": {"body": "@bob:localhost left the room.", "msgtype": "m.notice"}}
```

<br>

**Next:** [`h-notify`](../h-notify#folders-and-files) sends one message and
exits, for cron.

**See also:** the `matrix-bot` program in `bin/matrix-bot`'s
`Matrix_bots.Welcome` plugin is the same narration, composed with others.

<br>

[Up to the example index](../#readme)
