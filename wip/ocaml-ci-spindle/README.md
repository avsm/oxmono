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
  --concurrency 32 --oxcaml --diagnose
```

It takes every option of the `spindle` command, and these of its own:

| Option | Meaning |
| --- | --- |
| `--ocluster-cap FILE` | The scheduler's submission capability. |
| `--compilers MINORS` | OCaml minor versions to build on Debian 13 x86_64, such as `4.14,5.4`. `auto`, the default, picks 4.14 and the two newest. |
| `--platforms LIST` | Platforms that build the newest compiler once its x86_64 build passes, from `arm64`, `ppc64le`, `riscv64` and `windows`. All four by default. |
| `--oxcaml` | Also build with OxCaml 5.2.0minus39, as oxmono does. |
| `--opam-cache DIR` | Where opam-repository is cloned to solve. |
| `--diagnose` | Ask a language model about failed builds. Off by default. |
| `--diagnose-url URL` | Its OpenAI-compatible API, `http://sequoia:8000/v1` by default, for testing. `https://openrouter.ai/api/v1` is OpenRouter's. |
| `--diagnose-model MODEL` | The model, `Qwen/Qwen3.8-27B-FP8` by default. |
| `--diagnose-key-file FILE` | The API key. `OPENROUTER_API_KEY` is used otherwise, and none is sent when neither is set. |

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
4. **summary** waits for every build, including those added later, and
   lists how each ended, with those never started because the newest
   compiler failed. It fails unless every build passed, so the `ocaml-ci`
   workflow shows the result of the whole pipeline.

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
solve requiring `host-system-mingw`, and build from Cygwin's shell. The
job's log streams into Tangled with each OBuilder operation as a step of its
own, and the workflow succeeds or fails with the job.
Cancelling the workflow cancels the job.

Builds wait in the pool's queue, so `--concurrency` should allow every
build, and `--workflow-timeout`, six hours by default here, the queue and
the build. Windows builds in particular can wait well over half an hour. The spindle needs
`git` to clone opam repositories.

## Diagnosing failures

With `--diagnose`, a pipeline with failed builds gets a `diagnose` workflow
once its summary is done. It makes one chat completions request to
`--diagnose-url`, with no tools, holding:

- the tail of each failed build's last step, at most 24 KiB each,
- the names of the builds that passed,
- the repository's root opam and dune files, and up to six files that the
  logs name as `File "path"`, read through the knot, at most 32 KiB each.

The model's answer, bullet points and a patch where the fix is small, is the
workflow's log. Any reasoning the model shows before `</think>` is dropped.
Without `--diagnose` nothing is sent to any model. Logs are masked for
secrets before they are kept, so none are sent.

## Checking a plan

`test/plan.exe OPAM_REPOSITORY PROJECT` prints the builds that would be
submitted for a local checkout, with their solutions and specs, without
submitting them.

`test/diagnose.exe URL MODEL PROJECT LOG...` asks the model at `URL` about
the failed build logs `LOG` of the checkout `PROJECT`, as the `diagnose`
workflow would, and prints its answer.

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
- The `ocaml-ci` workflow's time limit covers its wait for every build, so
  `--workflow-timeout` should allow the x86_64 build and then the other
  platforms' builds.
