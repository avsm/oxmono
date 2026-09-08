# `2-send`

<br>

`send` logs in to a homeserver with a password, sends one text message to a
room, and prints the event id the server assigns it. It reads the
homeserver, the user and the password from the environment, and takes a room
id and a message body as its two arguments.

<br>

A Matrix room's history is an append-only timeline of events, and a text
message is the most common kind a client ever sends. Each message becomes
one event, identified by an event id the homeserver assigns when it accepts
it, and that id is what a later reaction, edit or reply refers back to.
Sending a message is the simplest event to send, since nothing about it
depends on room state or history the way creating a room or reading one does.

<br>

```ocaml
module M = Matrix_eio

let getenv name =
  match Sys.getenv_opt name with
  | Some v -> v
  | None ->
      Printf.eprintf "missing environment variable %s\n" name;
      exit 1

let () =
  let homeserver = Uriz.of_string_exn (getenv "MATRIX_HOMESERVER") in
  let user = getenv "MATRIX_USER" in
  let password = getenv "MATRIX_PASSWORD" in
  let room_id = Matrix_proto.Id.Room_id.of_string_exn Sys.argv.(1) in
  let body = Sys.argv.(2) in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = M.login_password ~sw ~env ~homeserver ~user ~password () in
  let event_id = M.Messages.send_text client ~room_id ~body () in
  Printf.printf "Sent %s\n%!" (Matrix_proto.Id.Event_id.to_string event_id)
```

<br>

[`Matrix_proto.Id.Room_id.of_string_exn`](../../lib/matrix_proto/matrix_id.mli)
parses the first argument and raises `Invalid_argument` if it is not a
well-formed room id. Every identifier in the library is parsed this way, so
a room id and a user id are distinct types, and the compiler rejects a
program that passes one where the other belongs.

<br>

[`Matrix_eio.Messages.send_text`](../../lib/matrix_eio/messages.mli) sends
the second argument as an `m.text` message, over
`PUT /rooms/{roomId}/send/m.room.message/{txnId}`. Each call generates its
own transaction id, so two calls with the same room and body create two
separate events, not one. The function returns the
[`Matrix_proto.Id.Event_id.t`](../../lib/matrix_proto/matrix_id.mli) the
homeserver assigned the new event, and the program prints it.

<br>

Nothing in `Matrix_eio.Messages` encrypts. In a room that requires
encryption, `send_text` still writes plaintext into the timeline, and
`Matrix_eio.Encryption` is the module that encrypts a room event before it
is sent.

<br>

A room id names one room and has the form `!opaque_id:server_name`. The
server name records where the room was created, not where it can be reached
now.

<br>

Running this example requires a room id. Any Matrix client displays one in
a room's settings, and [`5-rooms`](../5-rooms#folders-and-files) creates
one. The transcript below reuses the room from
[`1-login`](../1-login#folders-and-files)'s account, joined beforehand by a
second user, `bob`.

<pre><code><b>$ export MATRIX_HOMESERVER=http://127.0.0.1:8008 MATRIX_USER=alice-a7f3 MATRIX_PASSWORD=pw12345</b>
<b>$ dune exec -- example/2-send/send.exe '!LtsHUNXRlllcmldbGs:localhost' 'Hello, room!'</b>
Sent $yBE7ZjRM-TRjavE9VkI6Q7JSM4EihTIuFaRxetjqtrc
</code></pre>

<br>

**Next:** [`3-sync`](../3-sync#folders-and-files) receives messages instead
of sending them.

<br>

[Up to the example index](../#readme)
