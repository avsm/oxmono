# Tack and Tangled compatibility review

Reviewed 2026-09-10 against:

- Tack `8e3bd108617fdd83514b2955f47f045029e5577f` in `../tack`.
- Tangled core `338719d7d4f1e1e32becc4f4d3ef04f3c7daeb32` in
  `../tangled-core`.
- [ATProto service authentication](https://atproto.com/specs/xrpc#inter-service-authentication-jwt)
  and [DID profile](https://atproto.com/specs/did), retrieved 2026-09-09.

The compatibility target is Tack's Tangled CI behaviour with this service's
trusted OCaml workflow engine. Tack's Buildkite, Tekton and sourcehut provider
adapters and repository YAML definitions are replaced by OCaml definitions
and the local process runner.

## Feature mapping

| Tack/Tangled behaviour | OCaml spindle |
| --- | --- |
| Owner discovery and registration | `sh.tangled.owner`; owner PDS registration record |
| Spindle membership | Owner-issued grants, updates and revocations; PDS bootstrap |
| Repository discovery | Assignment records checked against DID service and knot `describeRepo` |
| Dynamic knot subscriptions | Verified WS/WSS origins; membership and assignment changes reconcile subscriptions |
| Automatic push CI | Branch/tag ref updates, skip-ci options, deletion suppression, changed paths and default-ref context |
| Automatic branch PR CI | Last-round gzip patch, verified blob CID, final commit and pull-URI/SHA deduplication |
| Manual retry and fork PR | Authenticated triggers; independently verified source repository |
| Repository collaborators | Current canonical knot ACL queried on every mutation |
| Workflow selection | Up to 50 named OCaml workflows and custom predicates; explicit selection |
| Pipeline queries | Repository, commit and kind filters; latest-per-commit and cursor pagination |
| Log subscriptions | Tangled binary XRPC/CBOR WebSocket events; selected workflow multiplexing |
| Tangled UI | Appview reads CI endpoints and renders pipeline/workflow pages |
| Restart and reconnect | Atomic inbox/cursors, dispatch deduplication, pending-work recovery |
| Expired upstream replay | Durable gap reports, current Git ref and member PDS reconciliation |
| Retention | Completed history and receipt budgets, inbox backpressure, live-work protection |
| Observer health | Connection/activity, queue age, discovery and recovery diagnostics, readiness status |
| Cancellation | All or selected pending/running workflows |
| Definition discovery | Configured OCaml workflow names with `derived=false` |
| Service JWT replay protection | Durable issuer/nonce consumption after signature verification |
| PDS proxy discovery | `/.well-known/did.json` with `#tangled_spindle` service |

Current Tangled uses `repoDid` in `sh.tangled.repo.describeRepo`; Tack's older
core dependency used `repo`. Collaborator changes now use the knot's
`addCollaborator`/`removeCollaborator` procedures. The spindle queries the
current canonical ACL instead of retaining authority from legacy PDS records.
The appview queries CI APIs directly; a separate spindle event stream is not
part of the required integration.

## Review details

A member cannot register another owner's repository by copying its DID into
a record. The publisher, record key, repository DID, knot origin and spindle
assignment must agree with the knot's authoritative response. Incoming push
events must originate from that canonical knot. Membership grants do not
confer write access to other members' repositories.

Event identity includes collection, record key and operation as well as PDS
publisher/timestamp, so multiple writes in one repository commit are distinct.
An event and its stream cursor are committed together, and completed event
receipts are checked inside that same SQLite transaction. Cursor advancement
is monotonic. Knot nanosecond timestamps overlap by one microsecond when
converted from JSON numbers; stable event IDs suppress that overlap.

Pipeline creation and its deduplication key commit together before execution.
Transient failures remain in the durable inbox. Invalid records are recorded
as rejected. Existing assignments are reloaded from PDSes after restart;
failed member collection refreshes retry independently. Discovery failures
must not terminate the HTTP service.

Membership and assignment events request authoritative PDS snapshots. Their
payloads never restore catalog state. A durable generation check discards a
snapshot if another notice arrives during its fetch. Pending refreshes block
affected mutations with `503 CatalogPending`. The catalog is checked again
after remote resolution and authorization. Non-owner membership records do
not schedule refreshes. These checks cover delayed grants after revocation,
bootstrap followed by replay, and in-flight snapshot races.

Event work runs in batches of 64 with four processing fibers. Catalog
refreshes run separately. Failed tasks retain exponential retry delays across
restart, capped at one minute. HTTP exchanges have a 15-second total deadline,
and event processing and collection refreshes have 30-second deadlines.
History queries and startup scan summaries in bounded batches. Query `total`
counts matches after the cursor, matching Tack and Tangled.

Commands run in separate process groups. Cancellation, timeout and normal
completion kill descendants that remain in the group. Log subscriptions send
only newly appended events, ping every 30 seconds and time out stalled reads
or writes. Invalid gzip patches become permanent event rejections.

The runner stores workflow definitions with accepted pipelines. Pending work
resumes using those accepted command vectors; running work becomes failed
after a restart. Completed logs are not retained in the active-work table.
Maintenance expires completed history and receipts by age and count, and
history by payload size. It preserves pending jobs, inbox entries and live JWT
nonces. An inbox count/size limit rejects new events without advancing their
cursors. Expired receipts leave durable per-source replay floors, so old
events cannot execute again after their individual receipts are removed.

Reconnects attempt replay within the configured 24-hour window. Older cursors,
local receipt floors and upstream HTTP 410 responses persist recovery tasks
before moving to the live stream. Current PDS assignments, member-owned pull
records and Git refs are reconciled on startup, reconnect and every five
minutes. Ref checkpoints are monotonic. Stream and recovered pushes share
repository/ref/SHA deduplication keys. Known skip-ci and deletion events also
update checkpoints. Recovered requests identify their unknown committer and
changed paths, and use the spindle DID as actor.

The pinned knot's `eventstream/store.go` queries events after a cursor without
advertising the earliest available position. The testbed Jetstream dependency
`e0274250f654` seeks into retained Pebble history and defaults to a 24-hour
event TTL. Neither proves that an old interval is complete. Recovery reports
keep `historicalEventsComplete=false` after current-state reconciliation.
An upstream HTTP 410 is handled explicitly, but is not required for recovery.

`/xrpc/_health` returns HTTP 200 with diagnostics. `/xrpc/_ready` and `/readyz`
return 503 for disconnected or inactive sources, pending discovery/recovery,
old inbox work, or maintenance failure. WebSocket control traffic keeps quiet
sources healthy. Event age remains separately visible.

The execution backend runs trusted commands as the service account, with a
bounded deadline/log budget and separate checkouts. It does not implement
Buildkite billing callbacks, Tekton cluster scheduling, sourcehut submission,
or a separate container per job. Those provider-specific capabilities are
outside the OCaml backend used here.

## Remaining operational limits

- Purged intermediate commits, deleted refs and records from unknown PR
  authors cannot always be reconstructed. Current-state reconciliation is
  explicit about this loss of historical coverage.
- Storage budgets count payloads. SQLite keeps freed pages for reuse and active
  work is exempt. History queries still scan stored summaries.
- Jobs share the service account. Process groups clean up ordinary descendants,
  but are not a security boundary against jobs that deliberately detach.
- Automatic PR events cover same-repository branches. Fork PRs use Tangled's
  authenticated trigger path, as in the pinned Tack baseline.

## Reproduction

```sh
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check --force \
  bleeding/spindle/test bleeding/httpz/websocket_eio/test
python3 bleeding/spindle/testbed/run.py test
python3 bleeding/spindle/testbed/parity.py test
```

The first Docker harness exercises static dispatch, malformed/auth failures,
state migration, cancellation and completed/interrupted recovery. The Tangled
harness runs the actual sibling knot and appview with the local PDS, PLC and
Jetstream. It exercises SSH pushes, two concurrent workflows, pull patches,
revocation, forks, restart catch-up and UI rendering. Runtime endpoints are
local; build preparation may download images, packages and Go modules.
The review regressions also cover replayed grants after revocation, superseded
PDS snapshots, persisted backoff, pagination and process-group cleanup.
The Tangled harness then stops Jetstream and checks degraded readiness, removes
an offline push from the knot's real SQLite journal, verifies current-ref
recovery without duplicate dispatch after restart, and exercises automatic
history expiry while preserving pending and recent pipelines.
