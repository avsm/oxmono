# Tutorial

The numbered directories are a tutorial. Each step is one program with a
README that shows the program, walks through it and links the API and RFC
sections behind each idea. Start at [**`0-profiles`**](0-profiles#readme) to
save a reusable login, then [**`1-session`**](1-session#readme), or pick the
step whose idea you need. Existing sample server output is illustrative;
identifiers and advertised capabilities vary with the server.

Connection examples use the selected profile or `JMAP_SESSION_URL`; see
[Running the examples](#running-the-examples) below.

The setup step [**`0-profiles`**](0-profiles#readme) saves and lists profiles
offline, then demonstrates `Profile.connect_name` and `Cli.profile_term`.

Steps `1` to `g` each introduce one idea of JMAP or of this library.

- [**`1-session`**](1-session#readme) &nbsp;&mdash;&nbsp; connects, and reads the
  session resource that names everything else.
- [**`2-mailboxes`**](2-mailboxes#readme) &nbsp;&mdash;&nbsp; the first method call,
  `Mailbox/get`, and what a request looks like on the wire.
- [**`3-inbox`**](3-inbox#readme) &nbsp;&mdash;&nbsp; two calls in one request, the
  second taking its ids from the first by result reference.
- [**`4-filter`**](4-filter#readme) &nbsp;&mdash;&nbsp; filter conditions and the
  operators that combine them.
- [**`5-message`**](5-message#readme) &nbsp;&mdash;&nbsp; one message in full: headers
  in their parsed forms, body parts and body values.
- [**`6-threads`**](6-threads#readme) &nbsp;&mdash;&nbsp; the RFC's own four-call
  example, threads collapsed and expanded.
- [**`7-keywords`**](7-keywords#readme) &nbsp;&mdash;&nbsp; the first write: a
  PatchObject that sets and clears a keyword.
- [**`8-mailboxes-set`**](8-mailboxes-set#readme) &nbsp;&mdash;&nbsp; creating records
  and referring to them by creation id in the same request.
- [**`9-errors`**](9-errors#readme) &nbsp;&mdash;&nbsp; the three levels at which
  JMAP reports failure, and the transport underneath.
- [**`a-send`**](a-send#readme) &nbsp;&mdash;&nbsp; a draft and its submission in one
  request, filed to Sent on success.
- [**`b-blobs`**](b-blobs#readme) &nbsp;&mdash;&nbsp; binary data through the upload
  and download endpoints, in memory and streamed.
- [**`c-import`**](c-import#readme) &nbsp;&mdash;&nbsp; a raw RFC 5322 message
  imported into a mailbox, and one parsed without importing it.
- [**`d-paging`**](d-paging#readme) &nbsp;&mdash;&nbsp; the limits a server imposes,
  and the loops that respect them.
- [**`e-changes`**](e-changes#readme) &nbsp;&mdash;&nbsp; state strings, and fetching
  what changed since one.
- [**`f-push`**](f-push#readme) &nbsp;&mdash;&nbsp; the event source that tells you
  when to ask.
- [**`g-raw`**](g-raw#readme) &nbsp;&mdash;&nbsp; under the typed layer: raw
  invocations, unknown members, extension properties.

Steps `h` to `r` are whole tasks of a mail client, each built from the ideas
above and each exiting non-zero on any unexpected response, so together they
are the coverage check `scripts/run-examples.sh` runs.

- [**`h-search`**](h-search#readme) &nbsp;&mdash;&nbsp; the search box: a composed
  filter, sorts, `collapseThreads`, paging and `SearchSnippet/get` highlights.
- [**`i-reading`**](i-reading#readme) &nbsp;&mdash;&nbsp; the reading pane: header
  fields in their parsed forms, the body structure walked, the text body and an
  attachment downloaded.
- [**`j-conversation`**](j-conversation#readme) &nbsp;&mdash;&nbsp; a conversation
  fetched in one request, then the same page with `collapseThreads` on and off.
- [**`k-compose`**](k-compose#readme) &nbsp;&mdash;&nbsp; the compose window: an
  identity, an attachment streamed to `uploadUrl`, the submission and
  `onSuccessUpdateEmail`, then `undoStatus`.
- [**`l-parse`**](l-parse#readme) &nbsp;&mdash;&nbsp; raw messages in and out: a
  message file imported into a new mailbox in one request, and a forwarded
  message parsed out of it without importing.
- [**`m-organise`**](m-organise#readme) &nbsp;&mdash;&nbsp; filing and flagging in
  bulk: `Mailbox/set`, then `Email/set` patches over `mailboxIds` and
  `keywords`, guarded by `ifInState`.
- [**`n-resync`**](n-resync#readme) &nbsp;&mdash;&nbsp; a local cache brought up to
  date: `Email/changes` drained, `Mailbox/changes` feeding `updatedProperties`
  back into `Mailbox/get`, and `Email/queryChanges` with its fallback.
- [**`o-watch`**](o-watch#readme) &nbsp;&mdash;&nbsp; live updates: an event source
  under a switch, each `StateChange` turned into `Email/changes`.
- [**`p-stream`**](p-stream#readme) &nbsp;&mdash;&nbsp; a file streamed up and back a
  megabyte at a time, with sizes checked at both ends.
- [**`q-events`**](q-events#readme) &nbsp;&mdash;&nbsp; consuming `Push.next`,
  draining buffered events, checking the final result and closing on timeout.
- [**`r-contacts`**](r-contacts#readme) &nbsp;&mdash;&nbsp; the address book pane:
  RFC 9610 AddressBooks and ContactCards, a JSContact card built and patched,
  and what a `properties` argument leaves out.

That is the tutorial.

<br>

# Running the examples

### With a saved profile

[**`0-profiles`**](0-profiles#readme) shows how to save either a bearer token or
a Basic login from a private key file. Once `personal` is saved:

```sh
unset JMAP_SESSION_URL JMAP_API_KEY JMAP_API_KEY_FILE JMAP_AUTH
dune exec -- examples/1-session/session.exe --profile personal
dune exec -- examples/3-inbox/inbox.exe --profile personal
```

Or select it for all commands with `export JMAP_PROFILE=personal`. Profiles
also work with `jmap`, `jmapq` and `jmap-mosaic`. A profile chooses the login;
`--account` or `JMAP_ACCOUNT_ID` still selects an account within the session.

The runner accepts the same profile selection. These steps only read mail:

```sh
scripts/run-examples.sh --profile personal 1-session 2-mailboxes 3-inbox
```

Selecting a profile disables the runner's default oracle URL and credential.
Explicit connection environment variables continue to override profile fields.
For a saved local oracle login, add `--allow-insecure` to the runner.

### With connection settings

The connection examples use [`Jmap_eio.Cli`](../eio/cli.mli), so one set
of environment variables serves them all:

| variable            | meaning |
| ------------------- | ------- |
| `JMAP_SESSION_URL`  | the session or well-known URL |
| `JMAP_API_KEY`      | a bearer token, or `user:password` when `JMAP_AUTH=basic` |
| `JMAP_API_KEY_FILE` | a file to read the key from instead |
| `JMAP_AUTH`         | `bearer` (the default) or `basic` |
| `JMAP_ACCOUNT_ID`   | the account to use; defaults to the primary mail account |
| `JMAP_PROFILE`      | a shared profile supplying a missing URL, key and auth scheme |

The connection flags `--url`, `--api-key`, `--api-key-file`, `--auth` and
`--account` override their environment values, which override profile fields.
`--profile` selects a saved login, `--allow-insecure` permits cleartext
credentials, and `--debug` enables diagnostics. `--help` lists each example's
options. Profiles are read from
`$XDG_CONFIG_HOME/jmap/profiles`; an explicit URL, key or authentication scheme
overrides the corresponding profile field. Pass `--allow-insecure` explicitly
for each command run against the cleartext local oracle. The runner infers it
only from an explicitly known loopback HTTP URL, never from a saved profile.

### Against the Cyrus oracle

[`scripts/oracle-up.sh`](../scripts/oracle-up.sh) starts the Cyrus test
server the oracle tests use (see
[`test/oracle/README.md`](../test/oracle/README.md)). It speaks plain HTTP on
localhost, has users `user1` to `user5`, and accepts any password.

<pre><code><b>$ scripts/oracle-up.sh</b>
<b>$ export JMAP_SESSION_URL=http://localhost:18080/.well-known/jmap</b>
<b>$ export JMAP_API_KEY=user1:x JMAP_AUTH=basic</b>
<b>$ dune exec -- examples/1-session/session.exe --allow-insecure</b>
</code></pre>

To run everything, seeding a few messages over LMTP first so the read-only
steps have something to show:

<pre><code><b>$ scripts/run-examples.sh --seed 3</b>
</code></pre>

The steps run in order, and the script reports pass or fail for each.

### Against Fastmail

Create an API token with mail read and write scope, then:

<pre><code><b>$ export JMAP_SESSION_URL=https://api.fastmail.com/.well-known/jmap</b>
<b>$ export JMAP_API_KEY=&lt;your token&gt;</b>
<b>$ dune exec -- examples/3-inbox/inbox.exe</b>
</code></pre>

The steps that write (`7-keywords`, `8-mailboxes-set`, `a-send`, `c-import`,
`e-changes`, `k-compose`, `l-parse`, `m-organise`) use unique names, restore
what they touch and destroy what they create, but read the source before
pointing one at an account you care about. `f-push` and `o-watch` default to
polling, which Cyrus needs; on Fastmail pass `--poll 0` to hold the
connection open.

<br>

# Writing another one

- One directory per step, `examples/<n>-<word>/` with `<word>.ml`, a `dune`
  and a `README.md`. The `dune` stanza is an `executable` without a
  `public_name`, so `dune build` compiles it but `opam install` puts nothing
  on anyone's `PATH`.
- For a profile management command, start from [`0-profiles`](0-profiles#readme).
  For a connected mail command, start from [`1-session`](1-session#readme):
  [`Jmap_eio.Cli.main`](../eio/cli.mli) parses the command line and the
  environment, connects, resolves the account and hands your function a
  `Cli.context`; `Cli.main'` adds a Cmdliner term of your own. Errors raised
  by `Client.call_exn` and `Client.run_exn` are printed and become exit
  status 1, so a step needs no error plumbing of its own. `ctx.sw` owns
  anything that outlives a request, such as a `Jmap_eio.Push.subscribe`
  fiber.
- Build requests with [`Jmap.Chain`](../lib/core/chain.mli). A chain ending in
  one handle is sent and decoded by `Jmap_eio.Client.call_exn`; one ending in
  ``Handles.[ q; g ]`` goes to `Client.run_exn` and comes back as
  ``let Results.[ query; got ] = ...``, so a program aliases
  `module Results = Jmap.Chain.Results` beside its other aliases.
  `Client.chain_exn` is for a step that shows the `Proto.Response.t` itself,
  and `Chain.attempt` for a call whose method error the step expects. The
  `using` array is left to `Client.default_capabilities`, which names the four
  this library builds calls for that the session advertises; pass
  `?capabilities` only to name one outside them.
- Name properties, filters and sorts with the typed builders,
  ``~properties:[ `Id; `Subject ]``,
  `Proto.Email.filter ~in_mailbox:inbox ()` and
  ``Proto.Email.sort ~ascending:false `Received_at``. `~properties_raw` is for
  a property the library has no variant for, and `Chain.invocation` for a
  whole method it has no builder for. A later call takes its ids from an
  earlier one with `from_query` or `from_get_field heads Thread_id`, which
  refuses at build time a property the `/get` did not ask for, and a record
  created in the request is keyed by a creation token typed at the binding,
  `Proto.Email.creation "draft"`, and named by `Proto.Id.creation_ref` of the
  same token.
- Let [`Jmap_eio.Sync`](../eio/sync.mli) do the loops the server's limits
  force, and stream anything that comes from or goes to a file with
  `Jmap_eio.Client.upload_flow` and `download_to`.
- Consume a subscription with `Push.next` and check `Push.result` when it ends.
  Apply `/changes` from the previously stored object state before advancing it;
  continue a partial drain while `has_more` is true. `q-events` and `o-watch`
  show those two parts separately.
- Be idempotent: unique names for anything created, and clean up at the
  end. Exit non-zero on anything unexpected, and keep it runnable with no
  arguments so `scripts/run-examples.sh` can run it.
