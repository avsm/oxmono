# `0-profiles`

A profile gives a name to a session URL and its credential. Save a login once,
then use it from every tutorial step, `jmap`, `jmapq` or `jmap-mosaic`.
This example lists profiles, saves connection settings and connects using
[`Profile.connect_name`](../../eio/profile.mli).

## Save and use a login

For a hosted service, put its bearer token on the first line of a private file,
then save the profile. Substitute your service URL and token-file path:

```sh
chmod 600 "$HOME/.config/jmap/token"
dune exec -- examples/0-profiles/profiles.exe save personal \
  --url https://api.example.com/.well-known/jmap \
  --auth bearer --api-key-file "$HOME/.config/jmap/token"
dune exec -- examples/0-profiles/profiles.exe list
dune exec -- examples/0-profiles/profiles.exe connect --profile personal
dune exec -- examples/3-inbox/inbox.exe --profile personal
```

`save` validates and stores the fields without contacting the server. Saving an
existing name replaces that profile atomically. It copies the secret from the
key file into the profile, so later changes to the key file require another
save. `connect` checks the saved login against the server with a 30-second
deadline. Listing profiles prints names and URLs, never credentials; running
the example with no subcommand also lists them.

`Profile.xdg_store` selects `$XDG_CONFIG_HOME/jmap/profiles`, falling back to
`$HOME/.config/jmap/profiles` when the XDG path is unset or relative. Profiles
contain plaintext credentials. `Profile.save` creates private directories and
mode-0600 files. Existing store directories with group or other permissions are
rejected without changing their mode. `Profile.load` checks the store and file
type, permissions and size.

For the local Cyrus fixture, the whole setup uses a disposable test key:

```sh
scripts/oracle-up.sh
(umask 077; printf '%s\n' 'user1:x' > /tmp/jmap-oracle-key)
dune exec -- examples/0-profiles/profiles.exe save oracle \
  --url http://localhost:18080/.well-known/jmap \
  --auth basic --api-key-file /tmp/jmap-oracle-key
dune exec -- examples/0-profiles/profiles.exe connect \
  --profile oracle --allow-insecure
dune exec -- examples/1-session/session.exe --profile oracle --allow-insecure
```

Profiles do not store the cleartext exception. `--allow-insecure` remains an
explicit choice on each command that connects to this HTTP fixture.

## Store a profile from OCaml

The executable uses [`Cli.config_term`](../../eio/cli.mli) for `save`, then
[`Cli.resolve`](../../eio/cli.mli) to apply the usual setting precedence.
It reads a named key file with [`Auth.read_secret_file`](../../eio/auth.mli)
because this command needs to persist the secret. An ordinary client can retain
the lazy `Auth.bearer_from_file` credential instead.

The storage operations are:

```ocaml
let ( let* ) = Result.bind in
let* profile =
  Profile.v ~name:"personal"
    ~session_url:"https://api.example.com/.well-known/jmap"
    (Profile.Bearer token)
in
let* store = Profile.xdg_store env in
Profile.save store profile
```

For Basic authentication use `Profile.Basic { user; password }`. A confined
application or test can select a different store with
`Profile.of_directory ~fs:(Eio.Stdenv.fs env) directory`; `Profile.load` and
`Profile.list` operate on the same store.

## Connect from a command of your own

[`Cli.profile_term`](../../eio/cli.mli) reads `--profile` or `JMAP_PROFILE`.
The example's `connect` command passes that name to:

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
match Profile.connect_name ~sw ~timeout:30. env name with
| Error error -> Fmt.epr "%a@." Profile.pp_error error
| Ok client ->
    let session = Client.session client in
    Fmt.pr "Connected as %s.@." (Cli.terminal_text session.username)
```

The client belongs to `sw`. A profile identifies the login; the application
still chooses an account from the fetched session. `connect` prints the primary
mail account. The other tutorial steps use `Cli.main`, which resolves
`--account` or `JMAP_ACCOUNT_ID` before falling back to that primary account.

For those steps, a flag wins over its environment setting, which wins over the
selected profile field. Unset old `JMAP_SESSION_URL`, `JMAP_API_KEY`,
`JMAP_API_KEY_FILE` and `JMAP_AUTH` settings when the profile should supply the
whole login. The direct `Profile.connect_name` call loads the named profile as
stored; it does not merge CLI overrides.

```sh
unset JMAP_SESSION_URL JMAP_API_KEY JMAP_API_KEY_FILE JMAP_AUTH
export JMAP_PROFILE=personal
dune exec -- examples/2-mailboxes/mailboxes.exe
dune exec -- examples/3-inbox/inbox.exe --account SHARED_ACCOUNT_ID
scripts/run-examples.sh --profile personal 1-session 2-mailboxes 3-inbox
```

For the oracle, use `--profile oracle --allow-insecure` on the runner. Select
read-only steps as above to browse an existing account. Running the whole suite
also runs examples that write mail.

Continue with [**`1-session`**](../1-session#readme) to read the session resource.
