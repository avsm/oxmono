# Tangled compatibility review

Reviewed 2026-10-30 against Tangled core
`a6c5b305424aa51a8b6643c20023b57105f7769f` in `../core`: the Go spindle in
`spindle/`, knot2 in `knot2/` and the lexicons in `lexicons/org/tangled/`.

The target is the reference spindle's wire behaviour, with trusted OCaml
workflow definitions in place of repository YAML and the local process runner
in place of microVM and Nixery engines.

## Feature mapping

| Reference spindle | OCaml spindle |
| --- | --- |
| `org.tangled.event.notifyEvent` push from the repository's knot | Same. Issuer must be the knot that the repository DID's PLC document names |
| Manual run as a `ci.trigger.manual` event on `notifyEvent` | Same, for the owner and collaborators |
| Repository acceptance from manifest, declaration and admission | Same rules, read from the knot and the owner's PDS on demand and cached |
| Admission: open, or owner, admins and allow-list | `--admission`, `--admin` and `org.tangled.temp.spindle.allowList.*` |
| Collaborators from `org.tangled.graph.collaborator` | Read from the knot on every mutation |
| Pull requests from `track.ticket` and `review.patch` records | Patch records on Jetstream; patch and ticket re-read from the knot |
| Blocked pull requests and `approvePipeline` | Same, when the patch's editor cannot push |
| `queryPipelines`, `getPipeline`, `subscribePipelineLogs` | Same parameters, objects and CBOR frames |
| `cancelPipeline`, `retryPipeline` | Same |
| `listWorkflowDefinitions`, `getWorkflowDefinition` | The configured OCaml workflows, as external sources |
| `describeSpindle` and `#tangled_spindle` DID service | Same |
| `skip-ci` and `ci-skip` push options, `refs/heads` and `refs/tags` only | Same |
| Deduplication of `(repo, ref, oldSha, newSha)` | Same key, durable across restart |
| SHA-1 and SHA-256 object IDs | Both |

## Differences

- The reference spindle computes changed paths from its own clone. Knots do
  not send them and this spindle does not compute them, so workflow
  predicates see an empty `changed_files`.
- Repository tokens minted by a knot through `org.tangled.repo.mintToken`,
  which act on behalf of a user, are not accepted. Users sign their own
  service tokens.
- Secrets, webhooks, quotas, bans, invite codes, cron schedules, the mill and
  executor roles are not implemented. Their routes answer `MethodNotFound`.
- Discovery is on demand. The reference spindle backfills repositories with an
  embedded hydrant. This spindle checks a repository when a push, manual run
  or patch names it, and caches the answer for `--reconcile-seconds`.
- Workflow definitions are OCaml values, so they do not change with the
  repository's contents and carry no file source.

## Review details

A member cannot run CI for another owner's repository by naming its DID. The
manifest on the knot that the PLC document names must name this spindle, and
the owner's declaration must name the repository back. A push notification
must be signed by that knot's `did:web` key. Records on Jetstream only prompt
a re-read from the knot, so a forged event can start nothing the knot does not
hold.

Pipeline creation and its deduplication key commit together before execution.
Jetstream events and the stream cursor commit together. Transient failures
remain in the durable inbox. Invalid records are recorded as rejected.
Network failures while checking a repository answer 503, which a knot retries
from its outbox for three days.

## Reproduction

```sh
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check --force \
  bleeding/spindle/test bleeding/httpz/websocket_eio/test
python3 bleeding/spindle/testbed/run.py test
python3 bleeding/spindle/testbed/stack.py test
```

The static harness exercises dispatch, malformed and unauthorized requests,
replay rejection across restart, cancellation and interrupted work. The stack
harness runs knot2 built from the core checkout with the local PDS and PLC and
a Jetstream on the knot's firehose. It exercises SSH pushes, manual runs,
retry, collaborator authority, admission, issues, pull requests, approval,
`skip-ci` and deletion through the Tangled CLI. The repository owner signs in
with OAuth and the collaborator with a password.
