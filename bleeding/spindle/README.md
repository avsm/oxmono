# Spindle

A Tangled CI spindle using Proffer, HTTPz, JSONWT and Eio on OxCaml.
Trusted OCaml definitions select and run workflows. The default `inspect`
workflow checks out the requested commit, prints request metadata and runs
`ls -la --`. Tokens and authorization headers are excluded from job metadata.

The spindle speaks the `org.tangled.*` protocol of Tangled core. A knot
reports each push to the spindle named in a repository's manifest by calling
`org.tangled.event.notifyEvent`. Users start manual runs through the same
method. Pull requests arrive as `org.tangled.review.patch` records on a
Jetstream source. See [the compatibility review](PARITY.md).

## Local Docker integration

From the oxmono root, with Tangled core checked out in `../core`:

```sh
python3 bleeding/spindle/testbed/stack.py test
```

This builds knot2 from the core checkout and runs it beside the [local ATP
stack](../atp/testbed/README.md), with a Jetstream on the knot's firehose and
this spindle behind a TLS gateway. The test drives the Tangled CLI through
SSH keys, repository creation, Git pushes over SSH, manual runs, retry,
cancellation, collaborator authority, a closed spindle's allow-list, issues,
pull requests, approval of a contributor's revision, `skip-ci` and deletion.
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
python3 bleeding/spindle/testbed/run.py test
python3 bleeding/spindle/testbed/demo.py
```

For a persistent native service and HTTPS, see
[deployment instructions](DEPLOYMENT.md). The spindle creates its own SQLite
state and needs no PDS password or spindle signing key.

## OCaml workflows

Pass a list of trusted definitions in `Spindle.config.jobs`:

```ocaml
let open Spindle.Job in
let inspect = v "inspect" [Metadata; Command ["ls"; "-la"; "--"]] in
let release =
  v "release" [Command ["make"; "release"]]
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
checkout, status, cancellation and log stream.

Commands receive argument vectors without a shell. Their environment contains
`TANGLED_REPO`, `TANGLED_COMMIT_SHA`, `TANGLED_PIPELINE_ID` and
`SPINDLE_REQUEST` JSON. Git hooks and ambient Git configuration are disabled.
The configured CA bundle is available to HTTPS Git clones. Workflow discovery
returns `derived=false`, because definitions belong to the OCaml service.

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
| `_health` | Return liveness and observer, queue and maintenance diagnostics. |
| `_ready` | Return the same report, with HTTP 503 while degraded. |

`/.well-known/did.json` advertises the `#tangled_spindle` service at an HTTPS
endpoint, which is what a knot requires before it notifies a spindle.

A repository is served when the `org.tangled.repo.manifest` on its knot names
this spindle's `did:web`, the owner's `org.tangled.repo.declaration` names the
repository back, and the owner is admitted. `--admission=closed`, the
default, admits the owner, `--admin` DIDs and the allow-list. `--admission=open`
admits anyone. Acceptance is cached for `--reconcile-seconds`, and a manifest
change seen on Jetstream drops it at once.

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

The state directory holds `spindle.db`, its WAL and temporary checkouts. One
process owns its directory lock. SQLite uses WAL with `synchronous=FULL`.
Jetstream events and the stream cursor commit together. Dispatch records and
their deduplication keys also commit together, so a knot's retried push or a
replayed patch starts no second pipeline. Transient processing failures leave
events queued. Malformed events have recorded rejections. Failed events back
off up to one minute, including after restart.

Pending and blocked workflows resume after restart. Interrupted workflows
become failed. Output streams without waiting for a newline. Log chunks
commit before publication and survive interruption of the current step. Step
snapshots compact the journal atomically. Invalid UTF-8 becomes replacement
characters. Completed logs stay in SQLite and are loaded on demand.

Two workflows run at once; at most 32 pipelines may be outstanding, with up
to 50 configured workflows. Each workflow has a 60-second execution deadline
and 1 MiB log budget. Git is a runtime dependency.
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

Commands execute as child processes with the service account's filesystem
access. The Docker harness supplies resource limits. Deploy custom commands
with the privileges and filesystem access intended for those jobs.
Normal completion, cancellation and timeout kill remaining children in the
command's process group. Jobs that deliberately detach need external isolation.

## Verification

```sh
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check --force \
  bleeding/spindle/test bleeding/httpz/websocket_eio/test
```

Native tests cover signed service tokens, issuer-scoped replay prevention,
concurrent consumption, expiry, database reopening and atomic event receipts.
They cover repository acceptance against a mock knot, PDS and PLC directory:
manifest and declaration agreement, admission, caching and live collaborator
checks. They cover workflow selection, approval, retry, cancellation,
retention budgets, queue backpressure and observer readiness. The WebSocket
client tests retain frames coalesced with the HTTP upgrade, report activity,
expose rejected upgrades, and reject bad accepts and HTTP versions. The Docker
harnesses check the service against a real PDS, PLC directory and knot2.
