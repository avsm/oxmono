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
| `--compilers MINORS` | OCaml minor versions to build on Debian 13 x86_64, such as `4.14,5.4`. `auto`, the default, picks 4.14 and the two newest. |
| `--platforms LIST` | Platforms that build the newest compiler once its x86_64 build passes, from `arm64`, `ppc64le`, `riscv64` and `windows`. All four by default. |
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
   `OCaml 5.4.1 on Debian 13 (x86_64)`. Its first step carries the OBuilder
   spec as its command.

The newest compiler's build is also solved for each of `--platforms`, such
as `OCaml 5.5.1 on Debian 13 (riscv64)` or
`OCaml 5.5.1 on Windows, mingw-w64 (x86_64)`. Those builds are further steps
of its workflow, which spawns them once it passes, so a project that fails on
x86_64 does not occupy the smaller pools. Each platform builds on its own
pool: `linux-arm64`, `linux-ppc64`, `linux-riscv64` or `windows-x86_64`.

Each spawned workflow submits its spec to the pool with the repository's
clone URL and commit, which the worker checks out. The spec resets the
image's opam-repository to the solved commit, pins the project, installs the
solution and runs `dune build @install @check @runtest`, as OCaml-CI's
builds do. Windows builds use the `ocaml/opam` mingw-w64 images, with the
solve requiring `host-system-mingw`, and build from Cygwin's shell. The job's log streams into Tangled with each OBuilder operation
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

- Only Debian 13 and Windows are built. OCaml-CI also covers other
  distributions, s390x, macOS, FreeBSD and OpenBSD, and builds every
  platform at once rather than after x86_64.
- The opam variables of each platform are fixed in `Variant`. OCaml-CI
  reads them from the image.
- There are no lint, lower-bound or opam-monorepo builds.
- The image's compiler is assumed to be the newest release of its minor
  version. OCaml-CI reads the version from the image.
- Only opam files at the repository's root are found.
- `pin-depends` are not solved. OCaml-CI adds their targets as pins.
- Solves run one at a time and cannot be interrupted, so a pathological
  project delays other plans.
- Repositories' own `.tangled/workflows` are not run.
