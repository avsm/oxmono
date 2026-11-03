# ocaml-ci-spindle

A Tangled spindle that tests OCaml projects in the manner of
[OCaml-CI](https://github.com/ocurrent/ocaml-ci), without OCurrent. Builds
run on an [OCluster](https://github.com/ocurrent/ocluster) pool, so nothing
from a repository runs on the spindle's host.

```sh
ocaml-ci-spindle --ocluster-cap submission.cap --hostname ci.example.org \
  --owner did:plc:OWNER --plc https://plc.directory \
  --jetstream wss://index.tangled.network/subscribe \
  --cors-origin https://tangled.org \
  --concurrency 16 --workflow-timeout 10800 --oxcaml
```

It takes every option of the `spindle` command, and these of its own:

| Option | Meaning |
| --- | --- |
| `--ocluster-cap FILE` | The scheduler's submission capability. |
| `--pool POOL` | The pool to build on, `linux-x86_64` by default. |
| `--compilers MINORS` | OCaml minor versions, such as `4.14,5.4`. `auto`, the default, picks 4.14 and the two newest. |
| `--oxcaml` | Also build with OxCaml 5.2.0minus39, as oxmono does. |
| `--opam-cache DIR` | Where opam-repository is cloned to solve. |

## What it does

Every push, pull request or manual run starts an `ocaml-ci` workflow:

1. **analyse** lists the `*.opam` files at the repository's root through its
   knot. A repository without any is not an OCaml project, and the workflow
   ends there.
2. **solve** refreshes a shallow clone of opam-repository, at most every ten
   minutes, and solves the project's packages, with their test
   dependencies, for each compiler using opam-0install. The OxCaml build
   solves against the OxCaml repository layered over opam-repository.
3. A workflow is spawned for every compiler with a solution, such as
   `debian-12-ocaml-5.4`. Its one step carries the OBuilder spec as its
   command.

Each spawned workflow submits its spec to the pool with the repository's
clone URL and commit, which the worker checks out. The spec resets the
image's opam-repository to the solved commit, pins the project, installs the
solution and runs `dune build @install @check @runtest`, as OCaml-CI's Linux
builds do. The job's log streams into Tangled with each OBuilder operation
as a step of its own, and the workflow succeeds or fails with the job.
Cancelling the workflow cancels the job.

Builds wait in the pool's queue, so `--concurrency` should allow every
build, and `--workflow-timeout`, three hours by default here, the queue and
the build. The spindle needs
`git` to clone opam repositories.

## Checking a plan

`test/plan.exe OPAM_REPOSITORY PROJECT` prints the builds that would be
submitted for a local checkout, with their solutions and specs, without
submitting them.

## Differences from OCaml-CI

- Only Debian 12 on one pool is built. OCaml-CI also covers other
  distributions, architectures, macOS, FreeBSD, OpenBSD and Windows.
- There are no lint, lower-bound or opam-monorepo builds.
- The image's compiler is assumed to be the newest release of its minor
  version. OCaml-CI reads the version from the image.
- Only opam files at the repository's root are found.
