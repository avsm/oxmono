# Spindle

A Tangled CI spindle using Proffer, HTTPz, JSONWT and Eio on OxCaml.
Trusted OCaml definitions and each repository's YAML workflows in
`.tangled/workflows` select workflows for an event. The spindle never runs a
workflow's commands itself. It hands each workflow to an executor, an Eio
callback that the operator supplies. The default executor logs what it was
given, with secret values replaced by placeholders, waits two seconds and
reports success. Tokens and authorization headers are excluded from job
metadata.

The spindle speaks the `org.tangled.*` protocol of Tangled core. A knot
reports each push to the spindle named in a repository's manifest by calling
`org.tangled.event.notifyEvent`. Users start manual runs through the same
method. Pull requests arrive as `org.tangled.review.patch` records on a
Jetstream source. See [the compatibility review](PARITY.md).

## Local Docker integration

From the oxmono root, with Tangled core checked out in `../core`:

```sh
python3 wip/spindle/testbed/stack.py test
```

This builds knot2 from the core checkout and runs it beside the [local ATP
stack](../atp/testbed/README.md), with a Jetstream on the knot's firehose and
this spindle behind a TLS gateway. The test drives the Tangled CLI through
SSH keys, repository creation, Git pushes over SSH, a repository workflow
using a secret, signed webhooks and redelivery, manual runs, retry,
cancellation, collaborator authority, a closed spindle's allow-list, invite
codes, quotas, bans, issues, pull requests that are closed and reopened,
approval of a contributor's revision, `skip-ci` and deletion.
`--core=/path/to/checkout` selects another checkout. Its revision is recorded
in `.state/core-revision`. `stack.py up`, `down` and `reset` manage the
services. Volumes persist across `down`.

| Service | Inside Compose | From the host |
| --- | --- | --- |
| Knot | `https://knot.tangled.test`, SSH `knot:2222` | `http://127.0.0.1:5555`, SSH `127.0.0.1:2222` |
| Spindle | `https://spindle.tangled.test` | none |
| Knot Jetstream | `wss://knot.tangled.test/subscribe` | none |

Image, package and crate downloads happen while building. Runtime PLC, PDS,
knot and spindle endpoints are all local, and the knot asks no public relay
to crawl it. The spindle and client images use Arch Linux so that they carry
a glibc at least as new as the build host's. Their containers allow io_uring,
which Docker's default seccomp profile refuses.

The smaller static-repository harness tests authentication, replay
protection, restart and cancellation without a knot:

```sh
python3 wip/spindle/testbed/run.py test
python3 wip/spindle/testbed/demo.py
```

For a persistent native service and HTTPS, see
[deployment instructions](DEPLOYMENT.md). The spindle creates its own SQLite
state and needs no PDS password or spindle signing key.

## OCaml workflows

Pass a list of trusted definitions in `Spindle.config.jobs`:

```ocaml
let open Spindle.Job in
let inspect = v "inspect" [step "list files" "ls -la"] in
let release =
  v "release" [step "build" "make release"]
    ~accepts:(fun event ->
      event.kind = Push &&
      Option.fold ~none:false ~some:(String.starts_with ~prefix:"refs/tags/")
        event.ref_)
in
[inspect; release]
```

The selection context includes repository, actor, commit, trigger kind, ref,
default-ref status and the full request. Knots do not report changed paths,
so `changed_files` is empty. Definitions can use
ordinary OCaml analysis to choose workflows. Explicit workflow selections
still respect each definition's predicate. Workflow names use 1–40 ASCII
letters, digits, underscores or hyphens. Each workflow has an independent
status, cancellation and log stream. Workflow discovery returns
`derived=false`, because definitions belong to the OCaml service.

## Executors

`Spindle.config.executor` runs each workflow. It receives a
`Spindle.Executor.request` holding the pipeline and workflow, the repository's
clone URL and commit, whether to check the commit out, the trigger metadata,
a repository workflow's definition and YAML text, the workflow's environment,
the CI variables, the secrets and the steps. Each step has a name, a command
for a shell and its own environment. `files` reads the repository at the
commit through its knot, with `list dir` and `read path`, without checking it
out. `labels ()` is the keys of the labels on the pull request that started
the pipeline. A pull request's author may label it, so only labels that the
repository's owner or a collaborator last added are given. The spindle reads
every version of the pull request from its knot, checks each one against its
content id and asks the knot who collaborates.

The executor reports through its context. `step ?kind ?command name f` runs
`f` as the next numbered step of the log, and the step fails if `f` raises.
`command` is shown with the step's name, and a `` `System `` step, such as a
checkout, is folded by viewers. `log stream text` adds output to the current
step, with secret values replaced by `***`, one line per log frame. The
executor returns when the workflow succeeds and raises to fail it.
Cancellation and `--workflow-timeout` cancel its fiber.

```ocaml
let executor (ctx : Spindle.Executor.context) (r : Spindle.Executor.request) =
  List.iter
    (fun (s : Spindle.Job.step) ->
      ctx.step ~command:s.command s.name (fun () ->
          ctx.log `Stdout "would run it\n"))
    r.steps
```

`spawn name steps` adds a workflow to the running pipeline, run by the same
executor. A planning executor uses it to start one workflow per build it
decides on, such as one per compiler a dependency solve allows. A retry of a
spawned workflow reruns the workflow that spawned it.

`await_spawned ()` waits until every workflow the current one spawned, and
everything those spawned, has finished, and returns how each ended with its
log as text. A planner calls it last to summarise its builds. While it
waits, the workflow gives up its worker slot and its place in the
repository's and owner's workflow limits, and the wait is not charged as CI
minutes, so waiting planners cannot starve their own builds. The workflow's
time limit still covers the wait.

`spawned ~since ()` is how far each of those workflows has got: pending,
running and in which step, or ended. Given the progress it last saw, it
waits for the next change, on the same terms as `await_spawned`. A planner
can use it to log its builds' progress as it happens.

`spindle --executor=solve` uses `Spindle.Executor.solve`, which simulates
this: its `solve` workflow pretends to solve for `ocaml-4.14`, `ocaml-5.5`
and `oxcaml-5.2minus39`, then spawns a workflow for each that logs made-up
build and test output after short random waits.

A custom spindle is a binary that calls `Spindle_cli.main` from the
`spindle.cli` library with its own options, OCaml workflows and executor. It
keeps the `spindle` command's options. `--concurrency` bounds the workflows
that run at once, 2 by default, and an executor that waits on remote builders
can allow many. [ocaml-ci-spindle](../ocaml-ci-spindle/README.md) is one.

`Spindle.Executor.print clock` is the default, used by the `spindle`
command. It logs a description of the request as system step 0 and each
step, with its command and environment, after it, also prints the description
to standard output, sleeps two seconds and succeeds.

## Repository workflows

The reference spindle's YAML workflows are read one per file in
`.tangled/workflows`, from the knot at the commit being run, and named by
their file name. `when` selects events as the reference
spindle does: `event` is `push`, `pull_request` or `manual`, `branch` and
`tag` are doublestar globs, and `types` chooses pull request actions, by
default `opened`, `reopened` and `synchronize`. A manual run runs every
workflow it names. The executor receives the file's text, so it can read
`engine`, `dependencies`, `cache` and `image` itself. The request carries the
workflow's `environment`, each step's `command` and `environment`, and
`clone: {skip: true}` as `clone = false`.

The CI variables follow the reference spindle: `CI`,
`TANGLED_PIPELINE_ID`, `TANGLED_PIPELINE_KIND`, `TANGLED_REPO_*`,
`TANGLED_REF`, `TANGLED_REF_NAME`, `TANGLED_REF_TYPE`, `TANGLED_SHA`, the
`TANGLED_PR_*` variables of a pull request and `TANGLED_INPUT_<KEY>` for each
manual input.

## Secrets, webhooks, quotas, bans and invites

These follow the reference spindle and its error codes.

- **Secrets** are set by a repository's owner and collaborators. Each is
  passed to the executor, which the reference spindle gives every step as an
  environment variable of the same name. The value, each of its lines and its
  base64 forms are replaced by `***` in what the executor logs. Only trusted pipelines receive secrets: not a fork's
  code, nor a pull request that needed approval, nor a retry of one.
  Values are kept in the state directory's SQLite database, as the reference
  spindle keeps them by default.
- **Webhooks** are POSTed for `push`, `repository:renamed` and
  `pull_request:created`, `resubmitted`, `closed`, `reopened` and `merged`,
  with `X-Tangled-*` headers and an `X-Tangled-Signature-256` HMAC-SHA256
  when the webhook has a secret. Each delivery is tried three times, and the
  last 50 are kept. Webhook URLs may not reach private addresses unless
  `--private-webhooks` is given.
- **Pull requests** run when opened and updated. Closing, reopening and
  completing their ticket dispatch `closed`, `reopened` and `merged`, which
  run only workflows that ask for them.
- **Quotas** limit a repository and an owner's repositories together. The
  enforced resources are `workflows` running at once, `webhooks` and
  `ci_minutes_monthly`. `--quota-repo` and `--quota-user` set defaults, and
  administrators override them per DID. Other resources are stored but
  mean nothing without virtual machines.
- **Bans** name a repository or an owner. A ban refuses the DID on every
  authenticated method, stops its pushes and pull requests, and deletes its
  pipelines, logs, secrets, webhooks and quota settings.
- **Invite codes** are made by administrators and admit whoever redeems
  them to the allow-list. `useCount` and `forAccount` are honoured, and a
  code expires after two days.

## API and authentication

All Tangled methods are under `/xrpc/`:

| Method | Behaviour |
| --- | --- |
| `org.tangled.spindle.describeSpindle` | Return the spindle DID and capabilities. |
| `org.tangled.event.notifyEvent` | Run a knot's push, or a user's manual trigger. |
| `org.tangled.ci.getPipeline` | Return a repository's pipeline and its workflow states. |
| `org.tangled.ci.queryPipelines` | Filter by commits and trigger NSIDs; paginate newest first. |
| `org.tangled.ci.listWorkflowDefinitions` | List the configured OCaml workflows. |
| `org.tangled.ci.getWorkflowDefinition` | Describe one configured workflow. |
| `org.tangled.ci.cancelPipeline` | Cancel all or selected live workflows. |
| `org.tangled.ci.approvePipeline` | Start the workflows of a blocked pull request. |
| `org.tangled.ci.retryPipeline` | Run a pipeline again as a new pipeline. |
| `org.tangled.ci.subscribePipelineLogs` | Stream selected workflows as Tangled CBOR WebSocket events. |
| `org.tangled.temp.spindle.allowList.*` | List, add and remove admitted owners. Admins only. |
| `org.tangled.secret.*` | List, add and remove a repository's secrets. Owner and collaborators. |
| `org.tangled.webhook.*` | Manage webhooks, list deliveries and redeliver. Owner and collaborators. |
| `org.tangled.temp.spindle.moderation.*` | Ban, unban, get and list bans. Admins only. |
| `org.tangled.temp.spindle.quota.*` | Set, unset, get and list limits, and report usage. Admins only. |
| `org.tangled.temp.server.createInviteCode` | Make an invite code. Admins only. |
| `org.tangled.temp.server.redeemInviteCode` | Join the allow-list with a code. |
| `_health` | Return liveness and observer, queue and maintenance diagnostics. |
| `_ready` | Return the same report, with HTTP 503 while degraded. |

`/.well-known/did.json` advertises the `#tangled_spindle` service at an HTTPS
endpoint, which is what a knot requires before it notifies a spindle.

A repository is served when the `org.tangled.repo.manifest` on its knot names
this spindle's `did:web`, the owner's `org.tangled.repo.declaration` names the
repository back, and the owner is admitted. `--admission=closed`, the
default, admits the owner, `--admin` DIDs and the allow-list. `--admission=open`
admits anyone. Acceptance is cached for `--reconcile-seconds`, and a manifest
change seen on Jetstream drops it at once. While a rename leaves the owner
with no declaration, a repository served before stays served under its old
name.

A push is accepted only from the knot that the repository DID's PLC document
names. Pushes outside `refs/heads/` and `refs/tags/`, deletions and pushes
with the `skip-ci` or `ci-skip` option run nothing. Manual runs, retries,
cancellation and approval need a fresh service JWT from the repository owner
or a collaborator. The knot's collaborator records are read on each such
request, so a revoked grant is not retained in a cache. A patch from an
author who cannot push creates a blocked pipeline until someone who can
approves it. Queries and logs are public. An unreachable knot, PDS or DID host
gives HTTP 503, which a knot retries.

Both ES256K and ES256 `Multikey` account keys are supported. Tokens require
`typ=JWT`, absent `kid` or `#atproto`, matching issuer/key/controller, scalar
audience, exact `lxm`, integral `iat` and `exp`, and a nonempty `jti` of at most
256 bytes. Accepted audiences are the spindle DID and that DID with the
specific `#tangled_spindle` fragment. Optional `nbf` is honored. Issuance may
be at most 30 seconds ahead; lifetime and remaining validity are bounded by
one hour. Normal PDS-issued tokens last approximately one minute.

After signature verification, SQLite atomically consumes `(issuer, jti)`
until expiration. Reuse fails across fibers and restarts; failed signatures
never reserve a nonce. The replay table fails closed at 65,536 live entries.
These checks implement the current
[ATProto service-auth profile](https://atproto.com/specs/xrpc#inter-service-authentication-jwt)
using the [reviewed JSONWT library](../jsonwt/spec/REVIEW.md).

## Persistence and limits

The state directory holds `spindle.db` and its WAL. One
process owns its directory lock. SQLite uses WAL with `synchronous=FULL`.
Jetstream events and the stream cursor commit together. Dispatch records and
their deduplication keys also commit together, so a knot's retried push or a
replayed patch starts no second pipeline. A repository's events are handled
in order, and other repositories' in parallel. Transient processing failures
leave events queued, backing off up to an hour, including after restart. A
full job queue delays an event without counting against it. An event that
fails sixteen times, about five hours, or is malformed or refused, is
recorded as rejected.
Patches and tickets of repositories the spindle has not accepted cost no
requests. Network-wide identity events, and account events other than an
accepted owner's deactivation, are not stored.

A deleted repository, as its knot reports it, and the repositories of an
owner whose account becomes inactive are wiped, as bans wipe them.

Pending and blocked workflows resume after restart. Each workflow is also a
job of a [Duraml](../duraml/README.md) store, `jobs.sqlite` in the state
directory, keyed by its pipeline and name and laned by its repository. A
workflow interrupted by a stop or a crash stays running, and runs again from
the start once its job's 60-second lease runs out, with a fresh log. What it
spawned before is not spawned again. After three interruptions it fails.
Re-running repeats the workflow's effects, so executors should make them
safe to repeat. An executor library built without a durable store keeps the
older behaviour, in which interrupted workflows fail. Executor output streams a line at a time. Log lines commit
before publication and survive interruption of the current step. Step
snapshots compact the journal atomically. Invalid UTF-8 becomes replacement
characters. Completed logs stay in SQLite and are loaded on demand.

Two workflows run at once unless `--concurrency` says otherwise. At most 32
pipelines may be outstanding, with up
to 50 configured workflows. Each workflow has a 1 MiB log budget and a
deadline set by `--workflow-timeout`, 60 seconds by default. Git is needed
only to read workflow files from a static `--source`.
Automatic maintenance retains completed history for 30 days, at most 1000
pipelines or 1024 MiB of stored payload. Completed event receipts last seven
days, capped at 100000 entries. The inbox admits 10000 events or 64 MiB before
applying backpressure without advancing its cursor. Pending work and live JWT
nonces are preserved. SQLite reuses freed pages. These are payload budgets,
not a limit on the database file's physical size.
History queries read summaries in batches. HTTP exchanges have a 15-second
total deadline. Event processing has a 30-second deadline. See [operating
settings](DEPLOYMENT.md#retention-and-readiness) for CLI flags and the
`Spindle.Operations` API.

A knot keeps undelivered push notifications in an outbox for three days, so
the spindle does not replay pushes itself. Jetstream reconnects resume from
the stored cursor within `--replay-hours`. An older cursor restarts from the
present, and patches published in between are not run.

The spindle starts no process for a workflow. Isolation is the executor's
responsibility.


## Web pages

With `--web`, the spindle serves pages at `/ui` for its users. A visitor
logs in with their atproto handle or DID. The spindle is a confidential
OAuth client, at `https://HOSTNAME/oauth/client-metadata.json`, that signs
its requests with a P-256 key kept in `web-client.key` in the state
directory and publishes its public half at `/oauth/jwks.json`. It verifies
the account once, revokes the tokens it was given and keeps none.

Only the spindle's users get a session: its owner and admins, allow-listed
accounts, and the owners and collaborators of the repositories it serves.
Admins see every repository. Others see the repositories they own or
collaborate on, as their knots record it, with their pipelines and logs.

Sessions and logins in progress are kept in the spindle's database, by the
hash of their IDs, so they survive a restart. The session cookie is
`__Host-spindle`, `Secure`, `HttpOnly` and `SameSite=Lax`, idle after two
hours and expired after twelve. A login is bound to the browser that started
it and can be finished once, within ten minutes. Every page is sent with a
Content Security Policy that allows nothing inline and with
`Cache-Control: no-store`. Logging out takes a CSRF token and is checked
against the site's origin. OAuth requests go only to public addresses over
HTTPS, since visitors choose where identity resolution leads, with no
cookie jar or retries. At most eight logins talk to other servers at once,
each for at most 30 seconds, and at most 500 may be in progress. Expired
sessions and logins are swept every ten minutes, and a new login ends the
browser's previous session. Visitors see only generic errors; the details
go to the spindle's log.

Since the pages' `form-action 'self'` policy forbids redirecting a form to
another site, the login form is answered with a page whose link Sitekit's
script follows to the account's authorization server. XRPC method names
never reach the web pages, and logging out takes a POST.

The pages need the spindle to be reached at `https://HOSTNAME`, which the
OAuth client's identity names.
## Verification

```sh
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check --force \
  wip/spindle/test wip/httpz/websocket_eio/test
```

Native tests cover signed service tokens, issuer-scoped replay prevention,
concurrent consumption, expiry, database reopening and atomic event receipts.
They cover repository acceptance against a mock knot, PDS and PLC directory:
manifest and declaration agreement, admission, caching, renames, deletions
and live collaborator checks. They cover event filtering, per-repository
ordering, workflow selection, approval, retry, cancellation,
retention budgets, queue backpressure and observer readiness. The WebSocket
client tests retain frames coalesced with the HTTP upgrade, report activity,
expose rejected upgrades, and reject bad accepts and HTTP versions. The Docker
harnesses check the service against a real PDS, PLC directory and knot2.
