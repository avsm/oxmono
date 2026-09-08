# `1-session`

<br>

A JMAP client begins by fetching the *session resource*, the document that
says where the API is, which accounts the credential reaches, and how much the
server will answer at a time. This step connects and prints what came back.

With a login saved by [**`0-profiles`**](../0-profiles#readme), run it as:

```sh
dune exec -- examples/1-session/session.exe --profile personal
```

No source change is needed to select a profile. For a local HTTP oracle profile,
also pass `--allow-insecure`.

```ocaml
module Client = Jmap_eio.Client
module Proto = Jmap.Proto

let doc = "Fetch the JMAP session resource and print what it says"

let () =
  Jmap_eio.Cli.main "session" ~doc @@ fun ctx ->
  let s = Client.session ctx.client in
  Fmt.pr "username               %s@." s.username;
  Fmt.pr "apiUrl                 %s@." (Client.api_url ctx.client);
  Fmt.pr "state                  %s@." s.state;
  let account_id =
    match Proto.Session.primary_account_for Proto.Capability.mail s with
    | Some id -> id
    | None -> Fmt.failwith "no primary account for %s" Proto.Capability.mail
  in
  let account =
    match Proto.Session.find_account account_id s with
    | Some a -> a
    | None -> Fmt.failwith "no account %a in the session" Proto.Id.pp account_id
  in
  Fmt.pr "primary mail account   %a (%s)%s@." Proto.Id.pp account_id
    account.name
    (if account.is_read_only then ", read only" else "");
  match Proto.Session.core_capability s with
  | None -> Fmt.failwith "the server does not offer %s" Proto.Capability.core
  | Some c ->
      Fmt.pr "maxObjectsInGet        %Ld@." c.max_objects_in_get;
      Fmt.pr "maxConcurrentRequests  %Ld@." c.max_concurrent_requests
```

<br>

## The frame

Most connected steps use [`Cli.main`](../../eio/cli.mli),
which takes a command name, a one line `doc` and the body to run, and does not
return. It builds a `Cmdliner` command of that name, so the program has a
`--help` page and takes the options below, then runs `Eio_main.run`, opens an
`Eio.Switch.run`, connects a client with [`Cli.connect`](../../eio/cli.mli)
and resolves the account named by `--account` or the primary mail account of
the session. The body receives all of it as a
[`Cli.context`](../../eio/cli.mli), whose fields are `env`, `sw`, `config`,
`client` and `account_id`. The switch owns what outlives a single request,
such as the session refresh the client makes on its own and the push fiber of
[**`f-push`**](../f-push#readme), and the client is valid only while the body
runs.

The body reports a failure by raising. `main` prints the message under the
command's name and exits 1 for a connection that fails, an account that cannot
be resolved, `Client.Jmap_client_error` from the `_exn` functions of
[`Client`](../../eio/client.mli), `Chain.Parse_error` from
[`Chain.parse_exn`](../../lib/core/chain.mli), `Sync.Sync_error` from the `_exn`
functions of [`Sync`](../../eio/sync.mli), and the `Failure` that
`Fmt.failwith` raises. A command line error exits with Cmdliner's usage status
instead. No step carries error plumbing of its own, and every step exits
non-zero on anything unexpected.

[`Cli.config_term`](../../eio/cli.mli) reads the options below, taking the
command line before the environment, then the selected profile and defaults.

| variable | flag | meaning |
| -------- | ---- | ------- |
| `JMAP_SESSION_URL` | `--url` | the session or `.well-known/jmap` URL |
| `JMAP_API_KEY` | `--api-key` | a bearer token, or `user:password` under basic auth |
| `JMAP_API_KEY_FILE` | `--api-key-file` | a file to read the secret from instead |
| `JMAP_AUTH` | `--auth` | `bearer`, the default, or `basic` |
| `JMAP_ACCOUNT_ID` | `--account` | the account to act on |
| `JMAP_PROFILE` | `--profile` | a saved login supplying missing URL, key and auth fields |

Profiles do not fix the account. `--account` selects a shared or secondary
account reached by the same login. Unset old connection environment variables
when switching entirely to a profile, since those values override its fields.

A step with an option of its own uses [`Cli.main'`](../../eio/cli.mli)
instead, which takes a `Cmdliner` term for that option and passes its value to
the body. [**`4-filter`**](../4-filter#readme) is the first to do so.

## Authentication

[`Profile`](../../eio/profile.mli) stores the URL and credential under a name.
The [profile example](../0-profiles#readme) covers saving, listing and connecting
with `Profile.connect_name`; this step lets `Cli.main` load the same store.

[RFC 8620 §8.2](https://www.rfc-editor.org/rfc/rfc8620#section-8.2) leaves
authentication to HTTP, so a client presents whichever credential the service
documents. [`Auth`](../../eio/auth.mli) names the two in use. `Auth.bearer` is
the `Authorization: Bearer` token of
[RFC 6750](https://www.rfc-editor.org/rfc/rfc6750#section-2.1) that a hosted
service such as Fastmail issues, and `Auth.basic` the `user:password` pair of
[RFC 7617](https://www.rfc-editor.org/rfc/rfc7617#section-2) that a
self-hosted server such as Cyrus expects, which `--auth basic` builds by
splitting `JMAP_API_KEY` on its first colon.

[`Client.connect`](../../eio/client.mli) takes the credential once and scopes
it to the origins the session names, those of `apiUrl`, `uploadUrl`,
`downloadUrl` and `eventSourceUrl`. A request to any other origin carries no
credential, so a `Location` header cannot walk a token off to another host. A
credential over cleartext `http://` is refused unless `--allow-insecure` is
given explicitly, which should only be done for a trusted local test server.

## The session resource

[RFC 8620 §2](https://www.rfc-editor.org/rfc/rfc8620#section-2) defines the
document [`Client.session`](../../eio/client.mli) hands back. `username` is
who the credential belongs to, and `state` changes whenever anything else in
the session does. `apiUrl` is where method calls are POSTed. Read it through
[`Client.api_url`](../../eio/client.mli) rather than from the record, because
the RFC allows a relative URL and Cyrus sends one, and `api_url` has resolved
it against the URL the session came from.

The session lists an account per capability URI.
[`Proto.Capability.core`](../../lib/proto/proto_capability.mli) and
`Proto.Capability.mail` are the URIs of RFC 8620 and RFC 8621, and
[`Proto.Session.primary_account_for`](../../lib/proto/proto_session.mli) finds
the account to send mail methods to. It returns the id the frame resolves as
`ctx.account_id`, which every later step passes to its calls.

[`Proto.Session.core_capability`](../../lib/proto/proto_session.mli) decodes
the settings object of the core capability into a record of limits.
`maxObjectsInGet` bounds a single `/get` call and `maxConcurrentRequests`
bounds how many requests may be in flight at once. The client waits for a slot
on its own, and [**`d-paging`**](../d-paging#readme) shows the loops the
limits force.

<pre><code><b>$ export JMAP_SESSION_URL=http://localhost:18080/.well-known/jmap</b>
<b>$ export JMAP_API_KEY=user1:x JMAP_AUTH=basic</b>
<b>$ dune exec -- examples/1-session/session.exe --allow-insecure</b>
username               user1
apiUrl                 http://localhost:18080/jmap/
state                  0
primary mail account   user1 (user1)
maxObjectsInGet        4096
maxConcurrentRequests  5
</code></pre>

Against Fastmail, set `JMAP_SESSION_URL=https://api.fastmail.com/.well-known/jmap`
and `JMAP_API_KEY` to an API token, and leave `JMAP_AUTH` at its `bearer`
default. Cyrus reports a session `state` of `0` and never changes it, so that
field cannot decide whether to refetch against this server.

On a server that changes its session, the client refreshes when a response's
`sessionState` differs. `Client.concurrency_limits` reports the current limits;
refreshing preserves active request accounting, and queued requests use the
new endpoint and limits when admitted.

<br>

**Next steps:**

- [**`2-mailboxes`**](../2-mailboxes#readme) makes the first method call, and
  prints the mailboxes of the account this step found.
- [**`9-errors`**](../9-errors#readme) goes under `Cli.main` to `Cli.connect`
  and shows what a wrong password looks like.

<br>

**See also:**

- [**`n-resync`**](../n-resync#readme) uses the session `state` and the
  per-type states to resync a local cache.

<br>

[Up to the tutorial index](../#readme)
