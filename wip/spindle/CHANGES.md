# Changes

- Clarify opam descriptions of package functionality and interfaces.

- Update CLI rendering to the current Console View and terminal APIs.

## Unreleased

- Add `--web`, web pages at `/ui` where the spindle's users log in with
  atproto, as a confidential OAuth client, to see their repositories'
  pipelines and logs. Admins see every repository, and other users the
  repositories they own or collaborate on. Pages are built with Sitekit,
  under a strict Content Security Policy, with `proffer.session` sessions
  kept in the spindle's store. Logins are bounded in number, time and
  storage, and expired sessions are swept. Visitors may log in by naming
  their account's server, OAuth answers are read up to 1 MiB, and the
  client's signing key is replaced after 90 days. The client's metadata is
  at the conventional `/oauth-client-metadata.json`, so Bluesky's login
  page names it by host. Logs show their terminal colours, and admins have
  a jobs tab showing the Duraml queue's activity.
- Fix durable workflows cancelled while waiting to run again, keep the logs
  of a workflow that cannot run again, and never run one workflow twice at
  once after a lost lease. Reading a pipeline no longer deletes logs.

- Run each workflow as a job of a Duraml store in the state directory, so a
  workflow interrupted by a restart runs again from the start with a fresh
  log instead of failing. Spawning a workflow that the pipeline already has,
  with the same steps, is a no-op.
- Keep each workflow's parent across restarts, so `await_spawned` still
  finds what a restored workflow spawned.
- Give executors `finished`, how the pipeline's other workflows ended.

- Give executors a pull request's labels, counting only those that the
  repository's owner or a collaborator added, checked against every version
  of the pull request on its knot.

- Let an executor wait for the workflows it spawned, and read how each
  ended and its log, with `await_spawned`.

- Let an executor follow its spawned workflows' progress, step by step,
  with `spawned`.

- Let a custom spindle turn off repositories' own workflows with
  `Spindle_cli.main ~repository_workflows:false`.

- Give executors read access to the repository at the commit through the
  request's `files`, add `--concurrency`, and add the `spindle.cli` library
  so that custom spindles keep the `spindle` command's options.

- Let an executor add workflows to its running pipeline with `spawn`, and
  add `--executor=solve`, which simulates a dependency solve that spawns a
  build workflow for each of three compilers.

- Check a manual run's caller before asking the knot about its ref, answer
  503 when a workflow file cannot be fetched, and fetch any workflow by its
  file name.

- Count each patch of a pull request as a round, so resubmitting the same
  commit sends `pull_request:resubmitted`, and hold a patch without a valid
  editor for approval instead of rejecting it.

- Read any regular file in `.tangled/workflows` with a printable name, such
  as `my ci.yml`, as the reference spindle does.

- Refuse a log stream from a browser page on another site unless
  `--cors-origin` allows it, as the reference spindle does.

- Set CI variables as the reference spindle does: `TANGLED_REPO_NAME` is the
  lowercased name, any ref but a tag has `TANGLED_REF_TYPE=branch`, and the
  default branch and pull request branch variables are always set.

- Name a webhook's repository by its owner and lowercased name, as the
  reference spindle does, and limit deliveries to one host to two at once
  and 64 waiting.

- Serve `GET /_health` and the DID document as `application/did+json`, as
  the reference spindle does. `describeSpindle` sends `links`, and `contact`
  when `--contact-email` is given. `--privacy-policy` and
  `--terms-of-service` add its links.

- Show only a trigger's lexicon fields in pipeline views, so a push's
  options stay private, and `sourceRepo` only when it differs.

- Report the whole match count on every `queryPipelines` page, and every
  pipeline of a requested commit, as the reference spindle does.

- Answer with the reference spindle's error codes: 404 `RepoNotFound`,
  `RefNotFound`, `PipelineNotFound` and `WorkflowNotFound`, and 403
  `AccessControl` for a caller who cannot push. Authorize pipeline changes
  before reading the pipeline. Answer a push to an unserved repository with
  nothing to run. Bound refs, workflow names and streamed workflows.

- Refuse a manual run whose ref does not hold its commit within 100 commits,
  as the reference spindle checks it.

- Run a fork's pull request from the target repository at the pull
  request's commit, without the fork's branch, as the reference spindle does.

- Handle a repository's Jetstream events in order, retry a failed event for
  about five hours, and skip network requests for repositories the spindle
  has not accepted. Store only the account events that deactivate an
  accepted owner.
- Wipe a repository its knot reports deleted, and the repositories of an
  owner whose account becomes inactive.

- Keep serving a repository while a rename leaves its owner without a
  declaration, so pushes during the rename still run.

- Answer 503 when a DID host, PDS or knot returns a server error, rate
  limits the spindle or times out, so the knot retries the push.

- Refuse a manual run that matches no workflow with 400, as the reference
  spindle does, rather than report it started.

- Fire a push or pull request webhook once when the knot or Jetstream
  delivers its event again.

- Send executor output one line per log frame, so the web UI numbers lines
  correctly. Steps carry their command and may be marked as system steps,
  and the default executor's description step is one. Cancelled workflows
  report "User canceled the workflow", and timed-out ones no error.

- Add `--cors-origin`, which lets browser pages from an origin such as
  `https://tangled.org` call XRPC methods, and answer CORS preflights.

- Accept service tokens signed by a repository's own DID for that
  repository, as Tangled's settings pages send. Secrets, manual runs and
  retries record the user such a token names.

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
