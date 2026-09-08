# `7-profile`

<br>

`profile` logs in to a homeserver and prints the user id, and on a later run
reuses the session it saved instead of logging in again. It reads the profile
name from the first command-line argument, defaulting to `default`, and reads
`MATRIX_HOMESERVER`, `MATRIX_USER` and `MATRIX_PASSWORD` from the environment
only when that profile holds no session yet.

<br>

A Matrix login produces an access token tied to one device, and losing that
token means logging in again, which the server sees as a new device with no
history of its own. Saving the session to disk lets a client reuse the same
device across runs instead, which matters once a later example starts
attaching encryption keys to the device id and expects them still to be
there next time.

<br>

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix" in
  let store = Profile_store.create ~xdg ~profile in
  let client =
    match Profile_store.load_session store with
    | Ok (Some session) ->
        Printf.printf "Reusing session for profile %S\n%!" profile;
        client_of_session session ~sw ~env
    | Ok None -> login_and_save ~sw ~env store
    | Error e ->
        Format.eprintf "session for profile %S is unreadable: %a\n%!" profile
          Matrix_client.Error.pp e;
        exit 1
  in
  let who = M.Auth.whoami client in
  Printf.printf "Logged in as %s\n%!" (Matrix_proto.Id.User_id.to_string who)
```

`login_and_save`, which performs the login and writes the session file, and
`client_of_session`, which rebuilds a client from one, are the rest of the
file.

<br>

[`Xdge.create`](https://tangled.sh/@anil.recoil.org/xdge) resolves the
[XDG Base Directory](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
paths for an application named `matrix`, honouring `$XDG_DATA_HOME` and
falling back to `~/.local/share`.
[`Profile_store.create`](../../lib/matrix_client/profile_store.mli) takes
that and a profile name and is the directory
`$XDG_DATA_HOME/matrix/profiles/<profile>/`, created mode `0700` if it does
not exist, so only the current user can read the access token it stores.

<br>

[`Profile_store.load_session`](../../lib/matrix_client/profile_store.mli) is
`Ok None` on a profile that has never logged in, `Ok (Some session)` once one
has, and `Error` when the profile's `session.json` exists but does not parse.
`login_and_save` builds a
[`Session.Session_file.t`](../../lib/matrix_client/session.mli) from what
[`M.login_password`](../../lib/matrix_eio/matrix_eio.mli) returns and writes
it with
[`Profile_store.save_session`](../../lib/matrix_client/profile_store.mli).
`client_of_session` builds a fresh
[`Matrix_eio.Client.t`](../../lib/matrix_eio/client.mli) for the stored
homeserver and attaches the stored access token with
[`Client.with_session`](../../lib/matrix_eio/client.mli).

<br>

Either way,
[`Matrix_eio.Auth.whoami`](../../lib/matrix_eio/auth.mli) asks the server
which user the access token belongs to, proving that a stored session is
still valid as well as a fresh one.

<br>

A missing environment variable, an unreadable `session.json`, or a save that
fails is reported on standard error and the program exits with status 1. A
request that fails raises `Eio.Io` uncaught.

<pre><code><b>$ export MATRIX_HOMESERVER=http://127.0.0.1:8008 MATRIX_USER=alice-b-88f9a4 MATRIX_PASSWORD=pw12345</b>
<b>$ export XDG_DATA_HOME=$(mktemp -d)</b>
<b>$ dune exec -- example/7-profile/profile.exe tutorial</b>
Logged in and saved session to profile "tutorial"
Logged in as @alice-b-88f9a4:localhost
<b>$ dune exec -- example/7-profile/profile.exe tutorial</b>
Reusing session for profile "tutorial"
Logged in as @alice-b-88f9a4:localhost
<b>$ ls -la $XDG_DATA_HOME/matrix/profiles/tutorial/</b>
-rw------- 1 alice alice 381 session.json
</code></pre>

<br>

**Next:** [`8-cli`](../8-cli#folders-and-files) turns the same program's
environment variables and positional argument into a proper command line.

<br>

[Up to the example index](../#readme)
