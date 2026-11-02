# Tangled compatibility review

Reviewed 2026-11-01 against Tangled core
`52549bde582beb2ce71cc88d6fa726c41cbc8fe7` in `../core`: the Go spindle in
`spindle/`, knot2 in `knot2/` and the lexicons in `lexicons/org/tangled/`.
Upstream's later commits to `c76eedefe` change the Go spindle's caches and
timing and add assignees to ticket views, none of which this spindle reads.

The target is the reference spindle's wire behaviour, with an executor
callback in place of the microVM and Nixery engines.

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
| Step end events with `outcome` | Same, for each step the executor reports, including a failed or cancelled one |
| YAML workflows in `.tangled/workflows` | Selected the same way and handed to the executor with the file's text |
| Secrets, masked in logs, for trusted pipelines only | Same |
| Webhooks, signed, retried and redeliverable | Same events, headers, payloads and limits |
| Pull request `closed`, `reopened` and `merged` | From ticket state changes, as the reference does |
| Quotas | `workflows`, `webhooks` and `ci_minutes_monthly` enforced |
| Bans and wiping | Same subjects and effects |
| Invite codes | Same, honouring `useCount` and `forAccount` |
| `cancelPipeline`, `retryPipeline` | Same |
| `listWorkflowDefinitions`, `getWorkflowDefinition` | The configured OCaml workflows as external sources, then the repository's YAML workflows |
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
- Workflows run wherever the executor puts them. The default executor runs
  nothing: it logs each workflow and succeeds. There is no checkout step.
  Scheduled workflows and the mill roles are not implemented.
- Quota resources that measure virtual machines (`vcpus`, `memory_mib`,
  `disk_mib`, the `workflow_*` caps and `cache_storage_bytes`) are stored but
  not enforced.
- A pull request's state is compared with the last state seen, not walked
  version by version, so a close and reopen between two events is missed.
- Invite codes honour `useCount` and `forAccount`, which the reference
  spindle ignores.
- Discovery is on demand. The reference spindle backfills repositories with an
  embedded hydrant. This spindle checks a repository when a push, manual run
  or patch names it, and caches the answer for `--reconcile-seconds`.
- OCaml workflow definitions do not change with the repository's contents
  and carry no file source.

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
