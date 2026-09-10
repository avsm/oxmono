# Spindle

A Tangled CI spindle using Proffer, HTTPz, JSONWT and Eio on OxCaml.
Trusted OCaml definitions select and run workflows. The default `inspect`
workflow checks out the requested commit, prints request metadata and runs
`ls -la --`. Tokens and authorization headers are excluded from job metadata.

Automatic operation follows PDS repository and membership records through
Jetstream, verifies each repository against its DID and canonical knot, and
subscribes to that knot's events. Pushes, branch pull requests, manual retries
and explicitly requested fork checkouts use the same durable pipeline engine.
See [the compatibility review](PARITY.md) for the pinned Tack/Tangled baseline.

## Local Docker integration

From the oxmono root:

```sh
python3 bleeding/spindle/testbed/parity.py test
```

This builds the sibling `../tangled-core` knot and appview and starts them
beside the [local ATP stack](../atp/testbed/README.md). It tests real SSH pushes,
PDS records and tokens, multiple OCaml workflows, gzip pull-request blobs,
collaborator and member revocation, fork checkout, offline push recovery,
JWT replay rejection after restart, and Tangled's pipeline page. Fault tests
disconnect Jetstream, remove a push from the knot journal and age stored
history to verify readiness, current-ref recovery and automatic retention.

Image, package and Go module downloads happen during preparation. Runtime PLC,
PDS, Jetstream, knot and appview endpoints all refer to the local setup.
`--tangled-core=/path/to/checkout` selects another source checkout; its exact
revision is recorded in `.state/tangled-revision`. Local service state persists
across `parity.py down` and subsequent `up` commands.

| Service | Host URL |
| --- | --- |
| Spindle API | `http://127.0.0.1:9000` |
| Tangled appview | `http://127.0.0.1:3000` |
| Knot API | `http://127.0.0.1:5555` |
| Knot SSH | `127.0.0.1:2222` |

The smaller static-repository harness remains available:

```sh
python3 bleeding/spindle/testbed/run.py test
python3 bleeding/spindle/testbed/demo.py
```

For a persistent native service, HTTPS and Tangled registration, see
[deployment instructions](DEPLOYMENT.md). The spindle creates its own SQLite
state and needs no PDS password or spindle signing key.

## OCaml workflows

Pass a list of trusted definitions in `Spindle.config.jobs`:

```ocaml
let open Spindle.Job in
let inspect = v "inspect" [Metadata; Command ["ls"; "-la"; "--"]] in
let docs =
  v "docs" [Command ["make"; "docs"]]
    ~accepts:(fun event ->
      event.kind = Manual ||
      List.exists (String.starts_with ~prefix:"docs/") event.changed_files)
in
[inspect; docs]
```

The selection context includes repository, actor, commit, trigger kind, ref,
changed paths, default-ref status and the full request. Definitions can use
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
| `sh.tangled.owner` | Return the spindle owner DID for registration. |
| `sh.tangled.ci.triggerPipeline` | Queue authenticated manual or pull-request workflows. |
| `sh.tangled.ci.getPipeline` | Return all workflow states by pipeline TID. |
| `sh.tangled.ci.queryPipelines` | Filter repository, commits and kinds; paginate newest first. |
| `sh.tangled.ci.describeWorkflowDefinition` | List the configured OCaml workflows. |
| `sh.tangled.ci.cancelPipeline` | Cancel all or selected pending/running workflows. |
| `sh.tangled.ci.subscribePipelineLogs` | Stream selected workflows as Tangled CBOR WebSocket events. |
| `_health` | Return liveness and observer, queue, recovery and maintenance diagnostics. |
| `_ready` | Return the same report, with HTTP 503 while degraded. |

`/.well-known/did.json` advertises the `#tangled_spindle` service. Queries and
logs are public. Mutations require a fresh service JWT and write access to the
canonical repository. The knot's collaborator list is checked on each
mutation, so revoked grants are not retained in a permission cache.

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
Incoming events and their stream cursors commit together. Dispatch records
and their deduplication keys also commit together. Transient processing
failures leave events queued; malformed records have recorded rejections.
Failed tasks back off up to one minute, including after restart. Membership
and assignment events refresh current PDS state. Replayed grants cannot restore
revoked access, and mutations wait while relevant refreshes are pending.

Pending workflows resume after restart. Interrupted workflows become failed.
Output streams without waiting for a newline. Log chunks commit before
publication and survive interruption of the current step. Step snapshots
compact the journal atomically. Invalid UTF-8 becomes replacement characters.
Completed logs stay in SQLite and are loaded on demand. Existing TID-named
JSON checkpoints are imported once; keep the original static repository
mapping for that first migration. Original checkpoint files are retained.

Two workflows run at once; at most 32 pipelines may be outstanding, with up
to 50 configured workflows. Each workflow has a 60-second execution deadline
and 1 MiB log budget. Pull blobs have separate 16 MiB compressed and 64 MiB
expanded limits, plus CID verification. Git and `gzip` are runtime dependencies.
Automatic maintenance retains completed history for 30 days, at most 1000
pipelines or 1024 MiB of stored payload. Completed event receipts last seven
days, capped at 100000 entries. The inbox admits 10000 events or 64 MiB before
applying backpressure without advancing its cursor. Pending work and live JWT
nonces are preserved. SQLite reuses freed pages. These are payload budgets,
not a limit on the database file's physical size.
History queries read summaries in batches. HTTP exchanges have a 15-second
total deadline. Event processing and collection refreshes have 30-second
deadlines. See [operating settings](DEPLOYMENT.md#retention-and-readiness) for
CLI flags and the `Spindle.Operations` API.

Reconnects replay within a configured 24-hour window. Older cursors, expired
local receipts and upstream HTTP 410 responses create durable recovery tasks.
Startup, reconnects and five-minute scans reconcile PDS assignments, current
Git refs and current member-owned pull records. A missing push can dispatch
the current ref with `request.recovery.mode="current_refs"`. Its actor is the
spindle DID, with unknown committer and changed paths marked in the request.
Workflow predicates must account for that missing context when appropriate.
Pushes share a repository/ref/SHA deduplication key across stream delivery and
recovery, so repeated pushes of the same SHA share a pipeline while that key
is retained.

Upstream streams do not advertise their earliest retained cursor. Reconciliation
restores current-state coverage and records `historicalEventsComplete=false`.
It cannot reconstruct every deleted ref, intermediate commit or pull record
from an unknown author. `/xrpc/_health` reports these gaps even after recovery.

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
concurrent consumption, expiry, database reopening, atomic event receipts,
workflow selection and cancellation.
Native review regressions cover stale catalog grants, superseded snapshots,
persisted retries, history pagination and cleanup of shell descendants. They
also cover retention budgets, queue backpressure, replay floors, ref checkpoints
and observer readiness, including quiet streams kept alive by control frames.
The WebSocket client tests retain frames
coalesced with the HTTP upgrade, report activity and expose rejected HTTP
upgrades, and reject bad accepts and HTTP versions.
The Docker harnesses check service behaviour with actual PDS and Tangled code.
