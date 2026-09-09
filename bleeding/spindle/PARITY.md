# Tack and Tangled compatibility review

Reviewed 2026-09-09 against:

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

The runner stores workflow definitions with accepted pipelines. Pending work
resumes using those accepted command vectors; running work becomes failed
after a restart. Completed logs are not retained in the active-work table.
History, receipts and rejected events have no automatic retention policy.

The execution backend runs trusted commands as the service account, with a
bounded deadline/log budget and separate checkouts. It does not implement
Buildkite billing callbacks, Tekton cluster scheduling, sourcehut submission,
or a separate container per job. Those provider-specific capabilities are
outside the OCaml backend used here.

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
