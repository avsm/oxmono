# ocaml-jmap — JMAP for OCaml

An implementation of the JSON Meta Application Protocol (JMAP) as specified in
[RFC 8620](https://www.rfc-editor.org/rfc/rfc8620) (core) and
[RFC 8621](https://www.rfc-editor.org/rfc/rfc8621) (mail), with a native client
built on [Eio](https://github.com/ocaml-multicore/eio).

## Libraries

The `jmap` package provides:

- **`jmap`** — protocol types, `jsont` codecs, and `Jmap.Chain`, a typed
  request builder that turns a chain of method calls into one JMAP request
  using RFC 8620 §3.7 result references, and reads the response back into a
  list of the same shape.
- **`jmap.eio`** — the client: `Jmap_eio.Auth` for the credential,
  `Jmap_eio.Transport` for the HTTP stack, `Jmap_eio.Client` for the session,
  requests and blobs, `Jmap_eio.Sync` for paging, change draining and
  batching, `Jmap_eio.Push` for the EventSource endpoint,
  `Jmap_eio.Profile` for shared named logins, and `Jmap_eio.Cli` for cmdliner
  terms and environment configuration.
- **`jmap.top`** — toplevel pretty-printers.

A second package, `jmap-mosaic`, builds on them: a terminal mail client that
reads, labels, files and replies to mail, written on the
[Mosaic](https://github.com/tmattio/mosaic) UI library. See `mosaic/README.md`.

The design is Eio-only. There is no second concurrency runtime and no
callback or promise layer to bridge: every client call is a direct-style
function, and concurrency is the caller's to arrange with fibers and
switches.

## The Eio client

The `jmap` core uses `httpz.uri` for URI templates and `httpz.media` for
bounded JSON decoding. `Httpz_media.Json` supplies the bounded readers and
structured JSON errors. Both libraries ship in the `httpz` opam package;
media codecs include the HTTP wire library as a dependency.

`Jmap_eio.Client` runs over [fetch](https://github.com/avsm/httpz) with the
`fetch-httpz` backend, so TLS, redirects and cookies come from `fetch`, and
credentials are scoped to an origin rather than attached to every request.
Three modules make a client: `Auth` (who), `Transport` (how the HTTP is done)
and `Client.connect` (the session).

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
let client =
  Jmap_eio.Client.connect_env ~sw ~timeout:30.
    ~auth:(Jmap_eio.Auth.bearer token) env
    "https://api.fastmail.com/.well-known/jmap"
  |> Result.get_ok
```

`connect_env` builds the default stack; `Client.connect ~sw ?auth transport url`
takes a `Jmap_eio.Transport.t` instead, which is how a program pins a
certificate, sets its own retry policy or paces requests per origin
(`Transport.v ?https ?retry ?max_concurrent ?min_interval env`), and how a test
puts a mock underneath (`Transport.of_fetch`).

Authentication is named by scheme, not by header:

- **Bearer** — `Jmap_eio.Auth.bearer token` (RFC 6750 §2.1), for services such
  as Fastmail that issue API tokens.
- **Basic** — `Jmap_eio.Auth.basic ~user ~password` (RFC 7617), for servers
  that predate bearer tokens, such as the Cyrus test server. Basic credentials
  over plain `http://` need `~allow_insecure:true`; otherwise the client
  refuses to send them.
- `Auth.bearer_from_file` / `Auth.basic_from_file` read the secret from the
  first line of a file when a request needs it, `Auth.refreshing ~refresh`
  calls a thunk per request (OAuth), and `Auth.of_env ()` reads
  `JMAP_API_KEY`, `JMAP_API_KEY_FILE` and `JMAP_AUTH`. `Auth.pp` redacts.

Credentials are attached only to the origins the session resource names, and
`refresh_session` re-scopes them when the session changes. A response whose
`sessionState` disagrees with the cached session (RFC 8620 §3.4) refetches it
once under a mutex; `Client.on_session_change` and `Client.session_changed`
report that, and `Client.concurrency_limits` reports the
`maxConcurrentRequests` / `maxConcurrentUpload` the client then obeys.
Before network access, it also rejects a method request larger than
`maxCallsInRequest` or `maxSizeRequest`, and rejects or stops an upload that
exceeds `maxSizeUpload`.

`~timeout` bounds the session fetch, a request, and an upload to their end with
the transport's clock. For a download it bounds the response head and then
each wait for more body bytes, so a large transfer can run longer while it
keeps making progress. Either kind of expiry gives `Client.Timeout`.
Everything else the network can do arrives as
`Client.Transport (Fetch.error, text)`, so a caller can tell a retryable
`Connection_failure` from a `Denied` that will never succeed.

### Shared connection profiles

Small JMAP clients can share named logins rather than each inventing a
credential file. `Profile.connect_name` loads a session URL and credential from
`$XDG_CONFIG_HOME/jmap/profiles/NAME` (falling back to
`~/.config/jmap/profiles/NAME`) and fetches the session in one call:

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
match Jmap_eio.Profile.connect_name ~sw ~timeout:30. env "personal" with
| Ok client ->
    let session = Jmap_eio.Client.session client in
    Fmt.pr "signed in as %s@." session.username
| Error error -> Fmt.epr "%a@." Jmap_eio.Profile.pp_error error
```

`Profile.load`, `list` and `save` expose the same store to a profile picker or
login command, and `Profile.of_directory` selects a confined or test store.
Profile files contain the secret in plain text, are bounded to 64 KiB, and are
accepted only when regular and inaccessible to group and other users. Saves
create mode-0700 directories and replace mode-0600 files atomically. Existing
profile directories with group or other permissions are rejected without
changing their mode. Profile fields reject terminal control characters. Secrets
remain immutable OCaml strings, so deployments must control core dumps and
process inspection when that matters.

`Jmap_eio.Cli.profile_term` supplies a common `--profile NAME` / `JMAP_PROFILE`
selector for task-specific commands. Account selection remains a property of
the task: resolve an explicit account or the capability's `primaryAccounts`
entry after connecting.

The runnable [profile example](examples/0-profiles/README.md) covers saving a
Bearer or Basic login, listing profiles and connecting by name. Every connected
tutorial step accepts it, for example:

```sh
dune exec -- examples/3-inbox/inbox.exe --profile personal
```

### Blobs without a copy in memory

`Client.upload`/`download` take and give a `string`; the streaming pair does
not (RFC 8620 §6):

```ocaml
Eio.Path.with_open_in path @@ fun file ->
let length = Optint.Int63.to_int64 (Eio.File.size file) in
Jmap_eio.Client.upload_flow client ~account_id
  ~content_type:"message/rfc822" ~length file
```

`Client.download_to client ~account_id ~blob_id sink` copies the response body
to any `Eio.Flow.sink` as it arrives and returns the media type the server
served it as; the client's body limit does not apply, because the bytes are the
caller's to place.

### Push

`Jmap_eio.Push.subscribe ~sw client ()` forks a daemon fiber that keeps the
event source of RFC 8620 §7.3 connected, reconnects with `Last-Event-ID` and
exponential backoff (`?backoff_initial`/`?backoff_max`), and delivers into a
bounded `Eio.Stream.t`:

```ocaml
let sub = Jmap_eio.Push.subscribe ~sw client ~types:[ "Email" ] ~ping:30 () in
match Jmap_eio.Push.next sub with
| `Event ev -> Fmt.pr "%a@." Jmap_eio.Push.pp_event ev
| `End -> ()
```

`~poll:seconds` closes and reopens the connection instead of holding it open,
which is what Cyrus needs: it holds a per-account lock for as long as an event
source is open, so while a stream is held that user's LMTP deliveries and JMAP
API calls both wait for it (another user is unaffected). Pair `~poll` with
`~last_event_id`, which is what makes each connection answer at once with what
changed while it was away (RFC 8620 §7.3). `Push.last_event_id` is a reconnect
cursor and may be ahead of events the application has processed. Persist the
object state after applying changes, reconcile from that state on restart, and
use push as a notification to fetch changes. An event's state is the new state,
so it must not replace the stored `sinceState` before reconciliation.
`Push.next` drains buffered events and then returns `End`, including when the
subscription ends while its queue is full. `Push.listen` is the raw connection
underneath.

### Sync: paging, changes and batching

`Jmap_eio.Sync` is the three loops the server's own limits force:

- `Sync.pages` walks a `/query` by `position`/`limit` (RFC 8620 §5.5), lazily,
  using the limit the server reports; `Sync.all_ids ~max` concatenates them.
  A changed query state or non-advancing page is an error, never a silently
  truncated success. Both use finite request fuel, configurable with `~fuel`,
  so even a server that keeps inventing new pages cannot run forever.
- `Sync.changes` (and `email_changes`, `mailbox_changes`, `thread_changes`)
  drains `hasMoreChanges` (§5.2) and reports `cannotCalculateChanges` as a
  value rather than an error, since it means "resync from scratch".
- `Sync.get_all` splits an id list over `maxObjectsInGet` (§5.1) and runs the
  batches in parallel, bounded by the session's `maxConcurrentRequests`.

`Jmap_eio.Cli` wires the same choices to a command line
(`--url`, `--api-key`, `--api-key-file`, `--account`, `--profile`,
`--allow-insecure`, `--debug`) and to the
environment (`JMAP_SESSION_URL`, `JMAP_API_KEY`, `JMAP_API_KEY_FILE`,
`JMAP_AUTH` = `bearer` | `basic`, `JMAP_ACCOUNT_ID`, `JMAP_PROFILE`). The
complete `Cli.config_term` accepts either direct URL/key settings or a profile;
`Cli.profile_term` is also available to programs with a different command
frame. `Cli.create_client ~sw` and `Cli.account_id` are the two calls every
example starts with, under an `Eio.Switch.run`.

### Chaining calls

`Jmap.Chain` builds several method calls into a single request, with each call
able to refer to an earlier one's response. A chain ends in the handles whose
responses are wanted, and `Client.run_exn` sends it and hands back their
decoded responses in the same order:

```ocaml
let (Results.[ query; got ]) =
  Jmap_eio.Client.run_exn client
    Jmap.Chain.(
      let* q =
        email_query ~account_id
          ~filter:(Proto.Email.filter ~in_mailbox:inbox ())
          ~sort:[ Proto.Email.sort ~ascending:false `Received_at ]
          ~limit:10L ()
      in
      let+ g =
        email_get ~account_id ~ids:(from_query q)
          ~properties:[ `Id; `Received_at; `From; `Subject ] ()
      in
      Handles.[ q; g ])
```

That list is typed, so the compiler knows how many responses come back and
what each one is. `query.ids` is a list of ids and `got.list` a list of
`Proto.Email.t`. A chain ending in a single handle goes to `Client.call_exn`,
which is that one response, and `Client.chain_exn` hands back the raw
`Proto.Response.t` for a caller that wants to print it. `Client.call` and
`Client.run` are the same two as results, reporting a server-side
`Method_error` (RFC 8620 §3.6.2) rather than raising; `Jmap.Chain.attempt`
turns one call's response into a `result` of its own, so a call that may
legitimately fail does not fail the read of the calls beside it.
`Client` rejects a chain larger than the session's `maxCallsInRequest` before
sending it; `Sync.calls_in_chain` and `Sync.chain_fits` let an application pick
a sequential fallback when result references are not essential.

## Command-line tools

`jmap` is a general-purpose inspection and maintenance client (mailboxes,
search, reading, flags, sync and deletion), and `jmapq` holds specialist
workflows. Sending is demonstrated by `jmap-mosaic` and the examples. Both
command-line tools take their configuration from `Jmap_eio.Cli`, so the flags
and environment variables above apply.

## Examples

[`examples/`](examples/README.md) begins with `0-profiles`, which saves and
uses a shared login. The protocol steps `1-session` to `g-raw` each introduce
one idea, followed by larger programs for sync, search, reading, threads,
sending, import, filing and streaming. `q-events` demonstrates `Push.next` and
subscription completion. Each directory has a walkthrough and runnable code.
Connected examples accept profiles and exit non-zero on unexpected responses:

```sh
dune exec -- examples/1-session/session.exe --profile personal
scripts/run-examples.sh --profile personal 1-session 2-mailboxes 3-inbox
```

## Testing against a real server

`test/` holds hermetic unit tests against JSON fixtures. `test/oracle/` holds
tests that run against a real [Cyrus IMAP](https://www.cyrusimap.org/) server:
where the RFCs are ambiguous, what a shipping server does is the tiebreaker.

Nothing in `test/oracle/` runs unless `JMAP_ORACLE_URL` is set, so a plain
`dune runtest` stays offline and hermetic — the oracle cases report as skipped.

```sh
scripts/oracle-up.sh                  # docker run, waits for /jmap to answer
eval "$(scripts/oracle-env.sh)"       # exports JMAP_ORACLE_URL and _LMTP
dune build @test/oracle/runtest --force
scripts/oracle-down.sh                # stop and remove the container
```

`--force` matters: without it dune caches the last successful run.

The oracle listens on `localhost:18080` (HTTP/JMAP), `18024` (LMTP, used to
inject test messages) and `18001` (Cyrus master). Users `user1` … `user5` exist
in `example.com` and any password is accepted, which is why the oracle tests
use Basic auth with `~allow_insecure:true`.

| variable                | default                            | meaning |
| ----------------------- | ---------------------------------- | ------- |
| `JMAP_ORACLE_URL`       | *unset* — tests skip               | session or well-known URL |
| `JMAP_ORACLE_USER`      | `user1`                            | login name |
| `JMAP_ORACLE_PASSWORD`  | `x`                                | password |
| `JMAP_ORACLE_DOMAIN`    | `example.com`                      | mail domain |
| `JMAP_ORACLE_LMTP`      | `localhost:18024`                  | `host:port` of the LMTP listener |
| `JMAP_ORACLE_HTTP_PORT` | `18080`                            | read by `oracle-up.sh` only |
| `JMAP_ORACLE_IMAGE`     | `ghcr.io/cyrusimap/cyrus-docker-test-server:bookworm` | image to run |

`scripts/run-examples.sh` points the examples at the same server. It defaults
to the oracle and Basic auth, takes `--seed N` to deliver N messages over LMTP
first so the read-only examples have something to show, and reports pass/fail
per example:

```sh
scripts/oracle-up.sh
scripts/run-examples.sh --seed 2
```

See [`test/oracle/README.md`](test/oracle/README.md) for the harness and the
recorded Cyrus behaviour, and [`examples/README.md`](examples/README.md) for
the examples themselves.

## Documentation

```sh
dune build @doc
```

The generated pages are `doc/index.mld` (the package index), `doc/tutorial.mld`
(a guided introduction whose examples are checked by `mdx`), and
`doc/coverage.mld` — a matrix of every RFC 8620 and RFC 8621 method against the
codecs, `Chain` builders, unit tests, oracle tests and examples that exercise
it, so gaps are visible rather than implied.

## Installation

This repository is preparing its first release. It currently needs development
versions of `httpz`, `fetch`, `fetch-httpz`, `json-pointer` and `mail-flag`;
the tested APIs do not yet have published minimum versions. Pin those
dependencies to compatible revisions before installing from this checkout:

```sh
opam install . --deps-only --with-test
dune build -p jmap
dune runtest
```

The optional `jmap-mosaic` package also needs compatible `mosaic` and
`matrix-eio` versions. See [the release review](RELEASE_REVIEW.md) for the
remaining publication prerequisites and verification results.

## License

ISC — see [LICENSE.md](LICENSE.md).
