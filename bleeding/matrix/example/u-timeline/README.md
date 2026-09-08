# `u-timeline`

<br>

`timeline` logs in to a homeserver, builds a `matrix-chat.ui` runtime with a
SQLite-backed event store under its profile, and follows one room's
timeline. It prints the items already cached when it subscribes, paginates
back once and prints what that returned, then prints every diff a later sync
produces until twenty batches have arrived or the process is interrupted. It
reads `--homeserver`, `--username`, `--password-file`, `--profile` and
`--room` as `8-cli` and `u-room-list` do.

<br>

A room's raw sync events are not what a timeline presents. An edit replaces the
message it targets rather than appearing beside it, a reaction attaches to
the event it reacts to, a redaction removes the event it names, and a
message a client is still sending appears as a local echo before the server
confirms it. `Matrix_ui.Room_timeline` folds all of that into one ordered
list of items, and `Matrix_ui.Event_store` persists what the cache holds as
chunks, contiguous runs of events with a token at each edge, so a hole the
server's sync window left behind stays a hole across a restart rather than
becoming indistinguishable from one the room itself has.

```ocaml
let event_store_of_profile ~env ~profile =
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix" in
  let dir = Eio.Path.(Xdge.data_dir xdg / "profiles" / profile) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  let path = Eio.Path.native_exn Eio.Path.(dir / "timeline.sqlite3") in
  match Matrix_ui_sqlite.create path with
  | Ok store -> store
  | Error e ->
      Logs.err (fun m ->
          m "Cannot open %s: %s" path (Ui.Event_store.Error.to_string e));
      exit Cli.exit_internal

let run () homeserver username password profile room_id =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    client_of_profile ~sw ~env ~profile ~homeserver ~username ~password
  in
  let event_store = event_store_of_profile ~env ~profile in
  let sync = M.Sync_service.of_user ~user_id:(M.Client.user_id client) () in
  let rt =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client ~sync
      ~event_store ()
  in
  Ui.Runtime.start rt;
  let timeline = Ui.Runtime.timeline rt room_id in
  let list = Ui.Room_timeline.items timeline in
  let snapshot, subscription = Ui.Observable.List.subscribe ~sw list in
  Printf.printf "%d item(s) in the cache:\n%!" (Array.length snapshot);
  Array.iter print_item snapshot;
  (match Ui.Room_timeline.paginate_back timeline () with
  | Ok pagination ->
      Printf.printf "paginate_back: %s\n%!" (string_of_pagination pagination)
  | Error error ->
      Format.printf "paginate_back failed: %a@." Matrix_client.Error.pp error);
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
does, `print_item`, which prints an event through `Presentation.preview` or
names the virtual item beside it, and `print_diff`, which matches an
`Observable.List.diff` as `u-room-list`'s does, are the rest of the file.

<br>

[`Matrix_ui_sqlite.create`](../../lib/matrix_ui_sqlite/matrix_ui_sqlite.mli)
opens or creates the file and hands back an
[`Event_store.t`](../../lib/matrix_ui/event_store.mli), so the timelines a
runtime opens over it survive a restart. Passed as `~event_store` to
[`Runtime.create`](../../lib/matrix_ui/runtime.mli), it is the backend the
event cache reads a room from the first time the room is touched and writes
to as sync responses and paginations change it.

<br>

[`Runtime.timeline`](../../lib/matrix_ui/runtime.mli) opens the room's
timeline on first call and keeps it alive until `Runtime.close_timeline`.
[`Room_timeline.items`](../../lib/matrix_ui/room_timeline.mli) is the room's
events oldest first, with gaps, date dividers, the read marker and the start
of the room mixed in as virtual items where they belong.
[`Presentation.preview`](../../lib/matrix_ui/presentation.mli) turns an
event into the short text a room list or a timeline row displays.

<br>

[`Room_timeline.paginate_back`](../../lib/matrix_ui/room_timeline.mli)
fetches one page from the room's oldest edge, fills the leading gap if there
is one, and answers `` `Reached_start `` once no token is left there,
`` `More `` while one remains, or `` `Nothing_to_do `` if nothing needed
fetching. `Observable.List.next` and the diff marks are as in `u-room-list`.

<pre><code><b>$ export XDG_DATA_HOME=$(mktemp -d) MATRIX_PASSWORD=pw12345</b>
<b>$ dune exec -- example/u-timeline/timeline.exe --homeserver http://127.0.0.1:8008 \
    --username uc-e8c83083 --profile tl -v --room '!IApSgaIZFsrhyKBCrT:localhost'</b>
timeline.exe: [INFO] Logging in as uc-e8c83083
timeline.exe: [INFO] Login successful: user_id=@uc-e8c83083:localhost device_id=VBHGJMJYGW
1 item(s) in the cache:
  [start of room]
paginate_back: nothing_to_do
+ [0]
  [gap]
+ [1]
  [-- 2026-09-02 --]
+ [2]
  @uc-e8c83083:localhost   @uc-e8c83083:localhost joined
...
+ [7]
  @ud-e8c83083:localhost   hello from ud
truncate to 8
</code></pre>

Nothing is cached yet, so the first snapshot holds only the placeholder that
means the room's beginning has not been reached, and `paginate_back` has no
token to fetch with. The first sync round then delivers the room's whole
known history at once, and the stale placeholder is dropped by the trailing
`truncate`.

A second run against the same profile loads the room from the SQLite cache
before any sync has completed, and `paginate_back` now has a token to act
on.

<pre><code><b>$ dune exec -- example/u-timeline/timeline.exe --homeserver http://127.0.0.1:8008 \
    --username uc-e8c83083 --profile tl -v --room '!IApSgaIZFsrhyKBCrT:localhost'</b>
8 item(s) in the cache:
  [gap]
  [-- 2026-09-02 --]
  @uc-e8c83083:localhost   @uc-e8c83083:localhost joined
...
  @ud-e8c83083:localhost   hello from ud
paginate_back: more
= [2]
  @uc-e8c83083:localhost   @uc-e8c83083:localhost joined
...
= [7]
  @ud-e8c83083:localhost   hello from ud
</code></pre>

<br>

**Next:** [`d-bot`](../d-bot#folders-and-files) builds a bot on the same
runtime and event cache this example subscribes to directly.

**See also:** [`u-room-list`](../u-room-list#folders-and-files) follows the
same runtime's room list instead of one room's timeline. The
[`omatrix`](../../bin/omatrix) client's `sync` command follows a room's
timeline over its own session rather than through a runtime built by hand.

<br>

[Up to the example index](../#readme)
