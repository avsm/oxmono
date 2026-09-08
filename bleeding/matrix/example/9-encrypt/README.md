# `9-encrypt`

<br>

`encrypt` creates an end-to-end encrypted room, invites a recipient, sends
one message into it, and watches for a reply. It reads `--homeserver`,
`--username`, `--password-file`, `--profile`, `--to` and a message as in
`8-cli`, and prints the room id, the invited user and the sent event id
before it starts watching.

<br>

End-to-end encryption keeps a message's content readable only by the devices
in the room it was sent to, not by the homeserver that relays it or by
anyone who later obtains a copy of its database. It protects the content and
the room key that decrypts it, not who is talking to whom or when, since
sender, recipient and timing stay visible to the server as ordinary
metadata. A device earns the right to read a room's messages by holding the
room's current Megolm session, which the sender shares with each recipient
device over an encrypted channel of its own.

```ocaml
let run () homeserver username password profile recipient message =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    M.login_password ~sw ~env ~homeserver ~user:username ~password ()
  in
  let user_id = M.Client.user_id client
  and device_id = M.Client.device_id client in
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix" in
  let store = Matrix_client.Crypto_store.create ~xdg ~profile in
  let enc = M.Encryption.of_env env ~user_id ~device_id ~store () in
  M.Encryption.execute_requests enc client (M.Encryption.outgoing_requests enc);
  let room_id =
    M.Rooms.create client ~invite:[ recipient ] ~is_direct:true ~encrypted:true
      ()
  in
  let settings = M.Encryption.enable_room_encryption () in
  M.Encryption.set_room_encryption_settings enc room_id
    (M.Encryption.room_encryption_content settings);
  let members = List.map fst (M.Rooms.get_joined_members client ~room_id) in
  let event_id =
    M.Encryption.send_encrypted_text enc client room_id ~body:message ~members
  in
  M.Encryption.save enc;
  (* prints Room, Invited and Sent, then watches [max_rounds] sync responses
     for a reply, decrypting every m.room.encrypted event it sees *)
  M.run_sync ~sw ~env client ~on_sync ~encryption:enc ()
```

`print_incoming`, which decrypts and prints one event, and `on_sync`, which
scans each response for `m.room.encrypted` events and stops the loop once a
reply from someone else arrives or ten rounds have passed, are the rest of
the file.

<br>

A to-device message is a Matrix event addressed to one specific device
rather than delivered into a room timeline. Key-sharing traffic for Olm and
Megolm travels this way, addressed by user id and device id, and never
appears in a room's history.

<br>

[`Matrix_client.Crypto_store`](../../lib/matrix_client/crypto_store.mli) is
the profile's disk state for encryption, kept in the same directory
[`7-profile`](../7-profile#folders-and-files) keeps a session in.
[`Matrix_eio.Encryption.of_env`](../../lib/matrix_eio/encryption.mli)
restores the device's Olm account from it when one is stored, and generates a
fresh account otherwise. A store that holds an account but cannot be read
raises `Eio.Io` rather than the program overwriting it. The first
[`execute_requests`](../../lib/matrix_eio/encryption.mli) call publishes this
device's identity keys and a pool of one-time keys, without which no other
device can open a session with it.

<br>

[`Rooms.create`](../../lib/matrix_eio/rooms.mli) with `~encrypted:true` sends
an `m.room.encryption` state event as part of room creation.
[`set_room_encryption_settings`](../../lib/matrix_eio/encryption.mli) records
the same settings on the machine directly, since nothing has synced yet to
learn them from the server. `members` comes from the room's current joined
members, which at send time is only the sender, so this first message is
encrypted for nobody but the sender's own devices.
[`send_encrypted_text`](../../lib/matrix_eio/encryption.mli) encrypts the
message and sends it as `m.room.encrypted`, and
[`Encryption.save`](../../lib/matrix_eio/encryption.mli) writes the
machine's state back through the crypto store.

<br>

[`run_sync`](../../lib/matrix_eio/matrix_eio.mli) with `~encryption:enc`
processes to-device traffic and performs whatever the machine asks for
before `on_sync` runs. `on_sync` still receives the raw sync response,
ciphertext included, so it calls
[`decrypt_room_event`](../../lib/matrix_eio/encryption.mli) itself on every
`m.room.encrypted` event, by which point the machine already holds whatever
Megolm session that needs. The loop stops once it prints a reply from
someone else, or after ten rounds, so an unattended run does not hang.

<br>

Register `c9a-6827` and `c9b-6827` on the homeserver, then:

<pre><code><b>$ export XDG_DATA_HOME=$(mktemp -d) MATRIX_PASSWORD=pw12345</b>
<b>$ dune exec -- example/9-encrypt/encrypt.exe --homeserver http://127.0.0.1:8008 \
    --username c9a-6827 --profile alice --to @c9b-6827:localhost "hello from alice" -v</b>
encrypt.exe: [INFO] Created room !gEZViNUTYQugtMBReF:localhost
Room:    !gEZViNUTYQugtMBReF:localhost
Invited: @c9b-6827:localhost
Sent:    $qwUtR_pNKHEjfOEwZevVgHuEtB0trVaxwcGJF00O_-Y
* [!gEZViNUTYQugtMBReF:localhost] @c9a-6827:localhost: hello from alice
</code></pre>

While that is running, the second user accepts the invite and replies from
the [`omatrix`](../../bin/omatrix) client.

<pre><code><b>$ dune exec -- omatrix msg --profile bob -r '!gEZViNUTYQugtMBReF:localhost' \
    -e "hello from bob"</b>
Message sent (event ID: $r2ipes8gMoqJVkH-W0NBjRe0aonj6Yr2PTgnGA_GAzI)
</code></pre>

The first program picks that up on its next sync round and prints it before
exiting on its own.

<pre><code>* [!gEZViNUTYQugtMBReF:localhost] @c9b-6827:localhost: hello from bob
</code></pre>

<br>

**Next:** [`a-verify`](../a-verify#folders-and-files) checks that the device
on the other end of an encrypted room is who it claims to be.

**See also:** the [`omatrix`](../../bin/omatrix) client's `keys init`
publishes a device's keys as this example's first `execute_requests` call
does, and its `msg --encrypted` is the same send path.

<br>

[Up to the example index](../#readme)
