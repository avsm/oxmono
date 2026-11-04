# Plans

Ideas not yet started. [README.md](README.md) describes current behavior.

## Estimated build times

The progress lines could say how long a build usually takes, such as
`building, usually 12m`, and the step for an ended build could say
`took 14m, usually 12m`.

The spindle already keeps what is needed. Each pipeline's view in the store
holds every workflow's name, status, `startedAt` and `finishedAt`, and the
full snapshot holds each step's start and end events. History is kept for
`--history-days` and `--history-limit`, 30 days and 1000 pipelines by
default.

- Add an executor call to the spindle, such as `previous ~workflow`, that
  folds over the repository's stored pipeline views, as `Engine.query` does,
  and returns recent successful runs of a workflow.
- Match runs by minor version, architecture and distribution, since a
  build's name changes with each compiler release.
- Estimate with the median of the last five successful runs.
- Separate queue time from build time using the `queue` step's events,
  because OCluster queue time varies by pool and dominates on Windows,
  riscv64 and ppc64. That needs the larger snapshots, not only the views.
- Tangled logs are append-only text, so an estimate appears when a build
  changes state. A countdown would need a log line every minute.

## Candidate fixes from diagnosis

A `diagnose` answer may contain a small patch. Maintainers could be offered
it as a branch from which to open a pull request. Every step that changes a
repository must be approved by someone with push access to it.

### Constraints found in Tangled

- A pull request is a `org.tangled.track.ticket` record written by its
  author, with an `org.tangled.review.submitPatch` call naming a source
  repository and branch. Both need the author's own OAuth session, so the
  spindle can never open a pull request itself. A maintainer opening it is
  the approval of its content.
- The web frontend's pull request form takes `source`, `sourceBranch`,
  `fork` and `targetBranch` from its URL, so a link can prefill it. Its fork
  list holds only the viewer's own forks, so a branch in a fork the spindle
  owns cannot be chosen. Patch-based pull requests are disabled ("Patch
  based PR is currently unsupported").
- A manual run, `org.tangled.ci.trigger.manual`, takes inputs, and
  `Engine.authorize` checks that its caller has push access. It is the
  authenticated approval channel Tangled already offers.

### Threat model

The answer is untrusted. The prompt holds build logs and repository files,
which anyone who can push or open a pull request controls, so the model can
be steered into proposing anything.

- Never apply or run a patch on the spindle's host. Applying it in a scratch
  clone is acceptable only with hooks, filters and submodules disabled.
- Refuse patches that touch `.tangled/`, opam `build` or `install` fields,
  `dune` `rule` actions, files outside the repository, binary files, symlinks
  or more than a few hundred lines, so that review stays possible.
- Build the patched commit on OCluster, as any build is, before offering
  it, and say which builds it passes.
- Offer fixes only for pipelines that ran with their secrets withheld or that
  were triggered by a push from someone with push access.

### Workflow

1. `diagnose` extracts the patch, checks it as above and keeps it in the
   spindle's store under an unguessable identifier, with the pipeline,
   commit and answer.
2. A `verify-fix` workflow, spawned by `diagnose`, builds the commit with
   the patch applied on the newest x86_64 variant.
3. The `diagnose` log shows the patch, the verification result and how to
   take it:
   - A maintainer applies it with `git am` from a `.patch` URL on the
     spindle, reviews it, pushes a branch and opens a pull request as usual.
     This needs no new credentials.
   - Or a maintainer approves it with a manual run of an `apply-fix`
     workflow with input `fix=ID`. Its caller's push access is checked. The
     spindle then pushes a branch such as `ci-fix/ID` to a fork it owns and
     logs a link to the pull request form.
4. The pushed branch carries a commit authored by the spindle's identity
   with the pipeline and model named in its message, so the pull request's
   history shows where it came from.

### Open questions

- The `apply-fix` route needs an atproto identity for the spindle, a fork on
  a knot and a credential to push with, and Tangled would have to accept
  pull requests from forks other than the viewer's. Until then, only the
  `git am` route works.
- Pushing straight to a branch of the repository itself would need the
  maintainer's credentials. The spindle should never hold those.
- Fixes kept in memory, as the `diagnose` hand-over is now, are lost on a
  restart, so the store is needed.
