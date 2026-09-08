# `5-rooms`

<br>

`rooms` logs in to a homeserver with a password, creates a room, invites a
second user, sets the room's topic, lists who is in it, and leaves. It
reads the homeserver, the user and the password from the environment and
takes the invitee's user id as its one argument.

<br>

A Matrix room is defined by its state, a set of events each keyed by a
type, such as `m.room.name`, `m.room.topic` and one `m.room.member` per
participant, and by its timeline of messages. Creating a room, inviting a
user and setting its topic are all separate state changes, and a client
that wants to know who is in a room or what it is called reads that state
rather than guessing it from the timeline.

<br>

The login and the parsing of the invitee's user id are as in earlier
examples. The rest of the file is a sequence of calls:

```ocaml
let room_id =
  M.Rooms.create client ~name:"Tutorial room" ~topic:"Created by 5-rooms"
    ~preset:M.Rooms.Private_chat ()
in
Printf.printf "Created %s\n%!" (Matrix_proto.Id.Room_id.to_string room_id);
M.Rooms.invite client ~room_id ~user_id:invitee ();
Printf.printf "Invited %s\n%!" (Matrix_proto.Id.User_id.to_string invitee);
let topic_event =
  M.State.set_topic client ~room_id ~topic:"Set after creation"
in
Printf.printf "Topic set (%s)\n%!"
  (Matrix_proto.Id.Event_id.to_string topic_event);
Printf.printf "Joined rooms: %d\n%!"
  (List.length (M.Rooms.get_joined_rooms client));
List.iter
  (fun (user_id, (m : M.Rooms.joined_member)) ->
    Printf.printf "  joined: %s (%s)\n%!"
      (Matrix_proto.Id.User_id.to_string user_id)
      (Option.value m.display_name ~default:"no display name"))
  (M.Rooms.get_joined_members client ~room_id);
List.iter
  (fun (m : M.Rooms.member) ->
    Printf.printf "  member: %s is %s\n%!"
      (Matrix_proto.Id.User_id.to_string m.user_id)
      (E.Membership.to_string m.membership))
  (M.Rooms.get_members client ~room_id ());
M.Rooms.leave client ~room_id ()
```

<br>

[`Matrix_eio.Rooms.create`](../../lib/matrix_eio/rooms.mli) takes the same
fields as the
[`/createRoom`](https://spec.matrix.org/v1.11/client-server-api/#post_matrixclientv3createroom)
endpoint. `preset` sets who may join without an invite. `Private_chat` is
invite-only, with invitees joining at power level 0, `Public_chat` lets
anyone in, and `Trusted_private_chat` is invite-only and makes every
invitee an administrator. The room id `create` returns is typed, like the
one [`2-send`](../2-send#folders-and-files) parses from its argument.

<br>

[`Rooms.invite`](../../lib/matrix_eio/rooms.mli) sends an invitation and
does not wait for it to be accepted. The member listing below lists the
invitee still in the `invite` state.

<br>

[`State.set_topic`](../../lib/matrix_eio/state.mli) writes the room's
`m.room.topic` state event. A state event is keyed by its type, and the
most recent event under that key replaces the one before it, unlike a
message [`2-send`](../2-send#folders-and-files) sends, which is added to
the timeline rather than replacing anything.

<br>

[`Rooms.get_joined_rooms`](../../lib/matrix_eio/rooms.mli) lists every room
the account has actually joined, and
[`Rooms.get_joined_members`](../../lib/matrix_eio/rooms.mli) lists their
profiles, but neither reports a pending invite.
[`Rooms.get_members`](../../lib/matrix_eio/rooms.mli) does. Each
[`member`](../../lib/matrix_eio/rooms.mli) carries a
[`Matrix_proto.Event.Membership.t`](../../lib/matrix_proto/matrix_event_state.mli),
one of `Join`, `Invite`, `Leave`, `Ban` or `Knock`, read from that user's
`m.room.member` event.

<pre><code><b>$ export MATRIX_HOMESERVER=http://127.0.0.1:8008 MATRIX_USER=alice-b-88f9a4 MATRIX_PASSWORD=pw12345</b>
<b>$ dune exec -- example/5-rooms/rooms.exe '@bob-b-88f9a4:localhost'</b>
Created !SGeDXaLBJnWKbIjRrG:localhost
Invited @bob-b-88f9a4:localhost
Topic set ($rAELx1rCdsuqRHJv_25tNMex39ElNfBczGx3fjM364k)
Joined rooms: 1
  joined: @alice-b-88f9a4:localhost (alice-b-88f9a4)
  member: @alice-b-88f9a4:localhost is join
  member: @bob-b-88f9a4:localhost is invite
</code></pre>

<br>

**Next:** [`6-media`](../6-media#folders-and-files) uploads and sends a
file to a room.

<br>

[Up to the example index](../#readme)
