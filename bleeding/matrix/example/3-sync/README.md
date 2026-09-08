# `3-sync`

<br>

`sync` logs in to a homeserver with a password, then runs the `/sync` loop
and prints every text message that arrives in any joined room as it
arrives. It reads the homeserver, the user and the password from the
environment and takes no arguments.

<br>

A Matrix client learns about new messages, room state changes and invites
entirely by repeating a call to `/sync`, an endpoint that blocks until
something new has happened or a timeout passes. Each response carries a
token for the next call, so a client always resumes exactly where the
previous response left off, and this loop is what every long-running client
or bot in this tree is built on.

<br>

The login and the call to `Matrix_eio.run_sync` repeat
[`1-login`](../1-login#folders-and-files). The part specific to this
example is the callback that processes each response:

```ocaml
module M = Matrix_eio
module E = Matrix_proto.Event
module S = Matrix_proto.Sync

let print_messages (room_id, (room : S.Joined_room.t)) =
  match room.timeline with
  | None -> ()
  | Some timeline ->
      List.iter
        (fun (event : E.Raw_event.t) ->
          match event.type_ with
          | E.Event_type.Room_message -> (
              match
                Jsont.Json.decode E.Text_message_content.jsont event.content
              with
              | Ok content ->
                  Printf.printf "[%s] %s: %s\n%!" room_id
                    (Matrix_proto.Id.User_id.to_string event.sender)
                    (E.Text_message_content.body content)
              | Error _ -> ())
          | _ -> ())
        timeline.events

let on_sync (response : S.Response.t) =
  (match response.rooms with
  | None -> ()
  | Some rooms -> List.iter print_messages rooms.join);
  M.Sync.Continue
```

<br>

[`Matrix_eio.run_sync`](../../lib/matrix_eio/matrix_eio.mli) forks a fiber
that calls `GET /sync` in a loop, each request's `since` taken from the
previous response's `next_batch`. `on_sync` is called with each
[`Matrix_proto.Sync.Response.t`](../../lib/matrix_proto/matrix_sync.mli) and
returns a [`Matrix_eio.Sync.action`](../../lib/matrix_eio/sync.mli).
`Continue` asks for the next batch at once, and the loop keeps running until
a callback returns `Stop` or the switch closes. The first response, the one
with no `since`, is an initial sync and carries the full state and recent
timeline of every joined room. Every later response describes only what
changed.

<br>

A response's `rooms.join` is an association list of room id to
[`Matrix_proto.Sync.Joined_room.t`](../../lib/matrix_proto/matrix_sync.mli).
Each room's `timeline` holds its new events, oldest first, as
[`Matrix_proto.Event.Raw_event.t`](../../lib/matrix_proto/matrix_event.mli)
with the content still raw JSON. `print_messages` checks `type_` against
`Room_message`, then decodes the content with
[`Matrix_proto.Event.Text_message_content.jsont`](../../lib/matrix_proto/matrix_event_message.mli).
That codec requires only a `body` and a `msgtype` field, which every
`m.room.message` carries, so an image or a file message decodes as well,
and the program prints its filename as the message body.

<br>

The transcript logs in as `alice`, who already has one message of history
in the room, then a second user, `bob`, running
[`2-send`](../2-send#folders-and-files) in another terminal, sends a
message that arrives live over the same connection.

<pre><code><b>$ export MATRIX_HOMESERVER=http://127.0.0.1:8008 MATRIX_USER=alice-a7f3 MATRIX_PASSWORD=pw12345</b>
<b>$ dune exec -- example/3-sync/sync.exe</b>
[!LtsHUNXRlllcmldbGs:localhost] @alice-a7f3:localhost: Hello, room!
[!LtsHUNXRlllcmldbGs:localhost] @bob-a7f3:localhost: Hi Alice, this is Bob
</code></pre>

<br>

**Next:** [`4-echo`](../4-echo#folders-and-files) answers what it receives
instead of printing it.

<br>

[Up to the example index](../#readme)
