# `u-room-list`

<br>

`room_list` logs in to a homeserver, builds a `matrix-chat.ui` runtime over the
session, and follows the room list the runtime publishes. It prints every
room already known when it subscribes, then a line for every insertion,
removal, replacement or move a later sync produces, until twenty batches of
diffs have arrived or the process is interrupted. It reads `--homeserver`,
`--username`, `--password-file` and `--profile`, and reuses or creates a
session under the profile exactly as `8-cli` does.

<br>

A client's account can belong to many rooms, and a screen that lists them
wants a stable order, a section for invites and favourites, and an unread
state, all current the moment a sync changes any of it. `Matrix_ui.Room_list`
computes that list from the synced state and the event cache and republishes
it after every sync, sorted and sectioned already, as a
`Matrix_ui.Observable.List.t`. A subscriber reads one consistent snapshot and
then applies the exact diffs a later change produced, rather than comparing
two copies of the whole list against each other on every sync.

```ocaml
let print_room (room : Ui.Room_list.room) =
  Printf.printf "  %-38s %-9s %-12s %s\n%!"
    (Matrix_proto.Id.Room_id.to_string room.id)
    (string_of_section room.section)
    (if Ui.Room_list.unread room then "unread" else "read")
    room.name

let run () homeserver username password profile =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    client_of_profile ~sw ~env ~profile ~homeserver ~username ~password
  in
  let sync = M.Sync_service.of_user ~user_id:(M.Client.user_id client) () in
  let rt =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client ~sync ()
  in
  Ui.Runtime.start rt;
  let list = Ui.Room_list.rooms (Ui.Runtime.room_list rt) in
  let snapshot, subscription = Ui.Observable.List.subscribe ~sw list in
  Printf.printf "%d room(s):\n%!" (Array.length snapshot);
  Array.iter print_room snapshot;
  let rec loop count =
    if count < max_batches then
      match Ui.Observable.List.next subscription with
      | None -> ()
      | Some diffs ->
          List.iter print_diff diffs;
          loop (count + 1)
  in
  loop 0
```

`client_of_profile`, which loads or creates the session exactly as `8-cli`
does, `string_of_section`, which names a `Room_list.section`, and
`print_diff`, which matches an `Observable.List.diff` and calls `print_room`
on the value an `Insert` or a `Set` carries, are the rest of the file.

<br>

[`Matrix_ui.Runtime.create`](../../lib/matrix_ui/runtime.mli) builds the
models over a client and a sync service, without persistence, and
[`Runtime.start`](../../lib/matrix_ui/runtime.mli) forks the sync loop that
drives them. [`Room_list.rooms`](../../lib/matrix_ui/room_list.mli) is
[`Runtime.room_list`](../../lib/matrix_ui/runtime.mli) filtered to
`Filter.Non_left` and sorted by `Activity`, its defaults, and is republished
after every sync the loop applies.

<br>

[`Observable.List.subscribe`](../../lib/matrix_ui/observable.mli) is a
snapshot and a subscription to what follows it, taken as one atomic step, so
no change can land between the print of the snapshot and the first call to
[`next`](../../lib/matrix_ui/observable.mli). Each room's unread state comes
from [`Room_list.unread`](../../lib/matrix_ui/room_list.mli), true when the
room has an unread count of its own or was marked unread by hand.

<br>

Each diff line starts with a mark, `+` for `Insert`, `-` for `Remove`, `=`
for `Set`, `~` for `Move`, and `truncate` or `reset` for the two diffs a
subscriber that fell too far behind receives instead of the ones it missed.
An index is into the list as the diffs before it in the same batch left it,
which is why the program applies each of a batch in order rather than
against the original snapshot. Twenty batches is a safety net for an
unattended run, and `Ctrl-C` stops it sooner.

<pre><code><b>$ export XDG_DATA_HOME=$(mktemp -d) MATRIX_PASSWORD=pw12345</b>
<b>$ dune exec -- example/u-room-list/room_list.exe --homeserver http://127.0.0.1:8008 \
    --username ua-aaf32efa --profile demo -v</b>
room_list.exe: [INFO] Logging in as ua-aaf32efa
room_list.exe: [INFO] Login successful: user_id=@ua-aaf32efa:localhost device_id=PYHQBCCEJK
0 room(s):
</code></pre>

While that keeps running, a second account, logged in to its own profile,
sends it a direct message from the `omatrix` client.

<pre><code><b>$ dune exec -- omatrix login --homeserver http://127.0.0.1:8008 \
    --username ub-aaf32efa --password-file pw.txt --profile ub</b>
Session saved to profile 'ub'
<b>$ dune exec -- omatrix msg --to @ua-aaf32efa:localhost --profile ub "hello from ub"</b>
Message sent (event ID: $giPHwDbf_-n_4DeYOLLadK1bATq00Ui_mBPwOvPOTBY)
</code></pre>

`room_list` picks up the new invitation on its next sync round and inserts
it.

<pre><code>+ [0]
  !snWeCNxmSOoQHslgMG:localhost          invites   read         Empty Room
</code></pre>

<br>

**Next:** [`u-timeline`](../u-timeline#folders-and-files) follows a room's
timeline over a persistent cache the same way.

**See also:** [`8-cli`](../8-cli#folders-and-files) is where the
login-or-save session this program reuses is explained. The
[`omatrix`](../../bin/omatrix) client is the second client in the transcript.

<br>

[Up to the example index](../#readme)
