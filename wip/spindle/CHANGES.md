# Changes

- Clarify opam descriptions of package functionality and interfaces.

- Update CLI rendering to the current Console View and terminal APIs.

## Unreleased

- Hand every workflow to an executor callback instead of running commands on
  the host. The default executor logs the workflow, with secrets as
  placeholders, waits two seconds and succeeds. `--repository-workflows` is
  gone, since repository workflows are always read.

- Read repositories' own YAML workflows from `.tangled/workflows`, with the
  reference spindle's `when` rules and CI variables.
- Add secrets, given to trusted pipelines and hidden in their logs, and
  webhooks for pushes, renames and pull requests, signed and redeliverable.
- Dispatch pull requests that are closed, reopened or merged.
- Add bans, which wipe what is kept for a repository or owner, invite codes,
  and quotas on running workflows, webhooks and monthly CI minutes.
- `--workflow-timeout` sets how long a workflow may run.

- Log streams end every step with an `outcome`, even when it fails or is
  cancelled, and mark the checkout as a `system` step, as the reference
  spindle does. Tangled's log view uses them to mark the step that failed.
- The deployment guide uses the reorganised Tangled CLI commands.

- Speak Tangled core's `org.tangled.*` protocol. Run pushes that knots report
  through `notifyEvent`, manual runs, retries, cancellation and approval of
  blocked pull requests, and serve repositories by manifest, declaration and
  an admission allow-list.
- Accept SHA-256 commit IDs, which knots now create by default.
- Answer 503 when a repository's knot, PDS or DID host is unreachable, so that
  a knot retries the push.
- Test against knot2 built from a Tangled core checkout with
  `testbed/stack.py`. The spindle and client images use Arch Linux and allow
  io_uring.

- Wait for the client's WebSocket close acknowledgement after the last log
  event, preventing completed log reads from failing with a broken pipe.

- Recover refs despite unrelated catalog failures, tolerate notes and avoid
  duplicate annotated-tag jobs. Stream and durably journal partial log output.

- Bound retained history and event queues, report observer readiness, and
  reconcile current Git/PDS state when upstream replay is unavailable.

- Reconcile catalog changes from current PDS state, reject stale snapshots and
  persist retry delays. Bound history reads and clean up job descendants.

- Discover Tangled repositories and run push, pull and multi-workflow CI.
  Persist event recovery and JWT replay protection, with a full Docker testbed.

- Resolve PLC keys over verified HTTPS and test the local TLS gateway.
  Add persistent-service deployment instructions and a manual trigger client.

- Use JSONWT for ES256K verification and reject malformed JWT claims, encodings
  and key headers, with signed regressions and local PDS integration tests.

- Run OCaml inspection jobs from authenticated Tangled manual triggers, with
  commit checkout, request metadata, persistent status and WebSocket logs.
