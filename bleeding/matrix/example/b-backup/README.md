# `b-backup`

<br>

`backup` creates or restores a server-side key backup, the mechanism that
lets a device with no history of its own recover every room key an account
has ever held. It has two subcommands, `enable`, which creates a backup and
uploads to it, and `restore`, which reads one back on a fresh device. Both
take `--homeserver`, `--username`, `--password-file` and `--profile`.

<br>

A device that was offline, or did not exist yet, when a room key was shared
holds no way to decrypt messages sent before it joined, since Megolm keys
travel only to devices present at the time. Key backup exists to break that
loss, an encrypted copy of every room key kept on the homeserver under a key
the server itself cannot read, so a new device can recover the whole history
once its owner supplies the matching recovery key.

The cmdliner wiring for the two subcommands is omitted, and so is the tail of
`enable_run` that prints the version, the count and the recovery key.
`restore_run` is omitted too, and mirrors this shape in reverse.

```ocaml
let enable_run () homeserver username password profile =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    M.login_password ~sw ~env ~homeserver ~user:username ~password ()
  in
  let enc = encryption env ~profile client in
  let room_id = M.Rooms.create client ~encrypted:true () in
  let members = List.map fst (M.Rooms.get_joined_members client ~room_id) in
  ignore
    (M.Encryption.send_encrypted_text enc client room_id ~body:"backup me"
       ~members);
  let random = Matrix_client.Client.random (M.Client.base client) in
  let key = Backup.Decryption_key.generate ~random in
  let auth_data =
    M.Encryption.sign enc
      (Backup.auth_data_to_json
         { public_key = Backup.Decryption_key.public key; signatures = [] })
  in
  let version =
    M.Room_keys.create_version client ~algorithm:Backup.backup_algorithm
      ~auth_data
  in
  M.Encryption.enable_backup enc ~version ~decryption_key:key
    (Backup.Decryption_key.public key);
  let uploaded = M.Encryption.backup_pending enc client in
  M.Encryption.save enc
```

<br>

`enable` first sends one message to itself in a fresh encrypted room, which
gives the device an outbound Megolm session, kept as an inbound one too, so
there is a room key on hand to back up. It then draws a fresh key with
[`Backup.Decryption_key.generate`](../../lib/matrix_eio/backup.mli), whose
public half is what every future room key gets encrypted to and what the
server is told about. [`Encryption.sign`](../../lib/matrix_eio/encryption.mli)
adds this device's Ed25519 signature to the auth data, so a later device can
tell which device made the backup before deciding to trust it. `enable` does
not verify that signature itself, it only makes one.

[`Room_keys.create_version`](../../lib/matrix_eio/room_keys.mli) is the only
call that reaches the server before the backup exists.
[`Encryption.enable_backup`](../../lib/matrix_eio/encryption.mli) then points
the local machine at that version, and
[`Encryption.backup_pending`](../../lib/matrix_eio/encryption.mli) uploads
every inbound Megolm session not yet sent, returning how many it sent. The
recovery key printed at the end,
[`Backup.Recovery_key.encode`](../../lib/matrix_eio/backup.mli), is the
private half in the form a person writes down, base58 in groups of four, and
is shown once and stored nowhere on the server.

`restore` takes the recovery key back with `Backup.Recovery_key.decode`,
reads [`Room_keys.get_current_version`](../../lib/matrix_eio/room_keys.mli)
to learn the current version string, and
[`Encryption.restore_from_backup`](../../lib/matrix_eio/encryption.mli)
downloads and decrypts every session in it in one call. Nothing here checks
that `restore`'s device is verified. The recovery key is the credential.

<br>

Register `bkp-7682` on the homeserver, then create the backup.

<pre><code><b>$ export XDG_DATA_HOME=$(mktemp -d) MATRIX_PASSWORD=pw12345</b>
<b>$ dune exec -- example/b-backup/backup.exe enable \
    --homeserver http://127.0.0.1:8008 --username bkp-7682 --profile bkp-a -v</b>
Backup version 1 created
1 room key uploaded
Recovery key: EsT2 tNkC dHAz VRVn 2aJd cyLh 8Qji PqFU u6qG 5Kqq 2bab 8USv
</code></pre>

A second device, under a different `--profile` and so with an empty crypto
store, restores from that recovery key.

<pre><code><b>$ dune exec -- example/b-backup/backup.exe restore \
    "EsT2 tNkC dHAz VRVn 2aJd cyLh 8Qji PqFU u6qG 5Kqq 2bab 8USv" \
    --homeserver http://127.0.0.1:8008 --username bkp-7682 --profile bkp-b -v</b>
Imported 1 room key from backup version 1
</code></pre>

<br>

**Next:** [`c-oauth`](../c-oauth#folders-and-files) logs in without a
password at all.

**See also:** the [`omatrix`](../../bin/omatrix) client has `backup enable`,
`status` and `restore` for the same three operations.

<br>

[Up to the example index](../#readme)
