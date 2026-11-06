# Changes

- Clarify opam descriptions of package functionality and interfaces.

- Update CLI rendering to the current Console View and terminal APIs.

## Unreleased

- Keep the inbox's size as events arrive instead of measuring it for each
  event, and drop log lines a workflow writes after its pipeline is wiped.

- Share webhook delivery slots by repository rather than by host, time
  deliveries with the monotonic clock, and read only a webhook's own
  deliveries.
- Prune old pipelines and receipts without rescanning those kept, and drop
  a user's cached repository list after 30 days unchecked.
- Index secret masking by first byte, and hide secrets inside longer base64
  text at any alignment.

- Limit anonymous lookups of repositories the spindle does not know, and
  reads of repository workflow files, to 10 a second with bursts of 50,
  answering 429 beyond that. Keep workflow definitions for a minute and DID
  documents for 60 seconds.
- Index pipeline views by repository, so a pipeline query reads only that
  repository's pipelines.
- Bound each observer pass to 10 seconds of new work, so one repository's
  slow events cannot hold back the others for long.
- Keep at most one knot answer in memory while reading a pull request's
  labels, and remember a pull request's source instead of listing every
  patch when its state changes.
- Reject events naming malformed URLs instead of retrying them.

- Keep durable workflows running through store errors, retrying the
  affected workflow later instead of stopping the worker pool.
- Refuse manual run inputs whose keys are not variable names, repeat a key,
  or whose values exceed 4 KiB or hold NUL, and at most 32 inputs.
- Do not charge CI minutes for time spent waiting for a runner slot.
- Keep workflow logs only in the journal, so persisting a pipeline no
  longer re-encodes every log event.

- Give each repository a share of 8 of the 32 pipelines that may have work
  to do at once, so that one repository's pushes cannot hold back the
  others, and refuse a blocked pipeline for a repository that already has
  16 waiting for approval.

- Change and delete webhooks under the hooks lock, so that an update cannot
  bring back a deleted webhook. Allow at most 50 webhooks and 100 secrets
  for a repository whatever its quotas, limit webhook URLs to 2 KiB and
  their secrets to 256 bytes, and require a boolean `active`.

- Decide whether an account may log in within the login's bounds, finding
  owners without asking a knot and stopping at the first repository it may
  see. Count pending logins without reading them, and refuse an
  authorization server whose addresses are over 2 KiB.
- Offer and accept "Move here" only for admitted owners, and drop a
  repository already moved here from the list at once.
- Keep a background lookup's failures inside its fiber, wait a minute before
  looking up again after one fails, and read a malformed stored list or
  handle as missing. Show a member's handle only once it is checked both
  ways, escape server text in the log, and time web requests by the
  monotonic clock.

- Give back a worker slot or quota place only while it is held, so that a
  cancelled or overlapping wait for spawned workflows cannot release it
  twice. Re-raise cancellation from a workflow's run, keep an ended
  workflow's outcome when a later step such as charging minutes fails, and
  mask secrets in that error. Refuse spawns from a workflow that has ended
  or been cancelled.
- Make step names one line of plain text and commands plain text, and set
  off output lines that look like step headings in the text given to
  executors, so that a log cannot forge its structure.

- Store only Jetstream patches and tickets of known repositories, and
  manifests of known repositories or naming this spindle, so that records
  anyone can publish cannot flood the inbox. Check a manifest's claimed owner
  against admission before looking the repository up, give a first run only
  when a repository is accepted for the first time, and keep a deactivated
  owner's settings, since moving PDS deactivates the old account.
- Refuse a collaborator check for an actor that is not a DID without asking
  the knot.

- Limit each token issuer to 1024 unexpired replay nonces, so that one DID
  minting tokens cannot lock every caller out, and keep the table's size
  without counting it on each request.
- Bound open log streams without letting upgrades queue for a slot, refuse
  unknown workflows before the upgrade, and accept only small frames.
- Bound Git's output for the static repository to 4 MiB and 30 seconds.
- Validate a push's old object ID and a commit hash from the knot, treat only
  40- and 64-digit refs as commit IDs, and build deduplication keys that two
  pushes cannot share. Answer 403 for a non-admin or a push from another
  knot, read the `Bearer` scheme case-insensitively, and accept a plain-http
  `Origin` only with `--allow-http`.

- Bound workflow branch and tag patterns: brace groups are counted before
  they are expanded, patterns and names have length limits, and glob matching
  takes time and memory proportional to the two lengths, so a workflow file
  in a pull request cannot exhaust the spindle's memory.

- Give a newly registered repository with no pipelines a first run, for the
  head of its default branch as a push of that branch by its owner.

- Show a user's other repositories at once from the store, instead of
  looking them up on their PDS and knots while the home page waits. They are
  looked up in the background when missing or over an hour old, or with a
  Refresh button, and a failed lookup keeps the previous list.

- Move a repository here from the web pages with a button, instead of a
  `tangled repo set-spindle` command to copy. The button asks the owner's
  PDS for permission to write the repository's manifest and its
  attestation, uses it once, and revokes it.

- Require `--hostname`, which defaulted to a test name. Report a bad port,
  workflow timeout, concurrency, retention limit, quota or `--repo` without
  `--source` as a usage error before starting. Quotas name one of the
  enforced resources and a positive limit. `--allow-http` now says that it
  also lets identity, knot and PDS requests reach private addresses.

- Refuse a fractional or out-of-range quota limit, invite use count or pull
  request round instead of truncating it, and accept only 40 or 64 zeros as
  a missing ref's ID.

- Refuse an answer over its size limit from any knot or PDS read instead of
  using it cut short, so a repository file over 1 MiB is unreadable rather
  than truncated.

- Export only the `admission` and `defaults` types of `Spindle.Catalog` and
  `Spindle.Settings`, whose functions took types the library does not
  export.

- Move the demonstration executors `solve` and `print` from
  `Spindle.Executor` to `Spindle.Demo`. `solve` no longer treats a
  repository workflow that sets `SPINDLE_COMPILER` as one of its builds.

- Verify handles in the web pages with `Xrpc.Identity.verified_handle`.

- Bound the repository acceptance cache, which any caller could grow, read
  a long pull request's label versions in batches and stop at an empty
  cursor, and keep a repository's new name only after its rename is handled.
- Keep a webhook delivery's response as valid UTF-8, so that one binary
  answer no longer breaks listing and retrying deliveries, and keep no
  delivery of a webhook deleted meanwhile.

- Refuse service-auth tokens whose ES256K or ES256 signature is in its
  high-S form, as atproto requires. The test fixtures are now generated with
  low-S signatures and include high-S copies.

- Run Git for a static repository without blocking the scheduler, refuse the
  internal health and DID names through `/xrpc/`, keep maintenance errors out
  of the public health report, list no repository workflows when they do not
  run, and allow at most 256 log streams of at most twelve hours each, which
  now follow workflows spawned after they open.

- Pass over a malformed Jetstream frame instead of reconnecting to it
  forever, back off reconnections up to a minute, and keep handling events
  after a store error. Wipe an inactive owner's repositories only for a
  lasting status that the account's PDS confirms, and forget a pull request
  only when its knot no longer holds the ticket.

- Spend an invite use, add CI minutes and add a secret atomically, so that
  requests at once cannot spend a code twice, lose minutes or overwrite a
  secret. An account already allowed spends no invite use.

- Match a repository workflow's branch and tag globs in time quadratic in
  their length, since a crafted pattern could stall the spindle, refuse
  patterns with more than 256 brace alternatives, and treat `**` as a whole
  segment, as doublestar does. Refuse workflow file names with C1 controls,
  line separators or bidirectional controls.

- Reach only public addresses when resolving identities and reading knots
  and PDSes, unless `--allow-http` is given for a development network, since
  the accounts and repositories asked about choose those hosts. Webhooks use
  the same address filter, which also refuses IPv4-compatible, NAT64 and
  6to4 forms of private addresses and tries each public address in turn.
- Serve pictures only of accounts the pages show, keep at most 32 MiB of
  them, and list at most 50 of a visitor's other repositories, those whose
  manifests name them, for at most ten seconds. Show the jobs tab only with a
  job store, and give the reason a member could not be added.

- Hide a secret whole when a shorter secret it begins with is complete first
  and the rest arrives in a later write, and hide secrets in step names,
  commands and errors too.
- Treat a workflow stopped while it waits in a builder's queue as
  interrupted, so that it does not run again on top of its old log. Fail a
  step whose last output no longer fits the log, record a workflow stopped
  from outside as failed and pass the stop on, keep a line of exactly 16 KiB
  whole, and balance worker slots and limits when an executor waits in
  several fibers at once.

- Keep pipelines waiting for approval when pruning history, and report a
  failed SQLite statement's own error.
- Let a workflow waiting for its repository's or owner's limit be cancelled,
  and wake it when its repository is wiped. Cancel the workflows a cancelled
  workflow spawned. Start a new pipeline's workflows after releasing the
  engine's lock, which could deadlock, and approve a pipeline once when two
  approvals race. Retry a spawned workflow through the planned workflow it
  descends from, with the original changed files and default-ref flag. Count
  only pipelines with work to do against the limit of 32, and refuse more
  than 50 workflows in a pipeline.
- Do not run a durable workflow again when an earlier run finished it while
  the new one waited.

- Show accounts in the web pages by their verified handles, kept across
  restarts, and their Bluesky profile pictures, served from the spindle.
- Send a browser asking for `/` to the web pages.
- List a visitor's repositories on other spindles, with the command that
  moves each here.

- Add a members tab to the web pages, where admins add the accounts whose
  repositories may run CI by handle or DID, and remove them.

- Give executors `queued`, which shows a workflow as pending while it waits
  in a remote builder's queue, starts it when it leaves, and does not charge
  the wait as minutes.

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
  log instead of failing. A re-run spawning a workflow it spawned before
  keeps the earlier one, even if it now plans it differently.
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
