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
  --concurrency 32 --oxcaml \
  --diagnose --diagnose-url https://openrouter.ai/api/v1 \
  --diagnose-key-file openrouter.key
```

It takes every option of the `spindle` command, and these of its own:

| Option | Meaning |
| --- | --- |
| `--ocluster-cap FILE` | The scheduler's submission capability. |
| `--compilers MINORS` | OCaml minor versions to build on Debian 13 x86_64, such as `4.14,5.4`. `auto`, the default, picks 4.14 and the two newest. |
| `--platforms LIST` | Platforms that build the newest compiler once its x86_64 build passes, from `arm64`, `ppc64le`, `riscv64` and `windows`. `arm64` by default. |
| `--full-platforms LIST` | Further such platforms, built only for a pull request labelled `ci:full`. `ppc64le`, `riscv64` and `windows` by default. |
| `--day10 ARCHES` | Debian architectures built with day10 on the `day10-linux-ARCH` pools instead of OBuilder, from `x86_64`, `arm64`, `ppc64le` and `riscv64`, or `none`. `x86_64,riscv64` by default. |
| `--oxcaml` | Also build with OxCaml 5.2.0minus39, as oxmono does. |
| `--opam-cache DIR` | Where opam-repository is cloned to solve. |
| `--remote-hosts HOSTS` | The only hosts a project's `x-opam-repositories` may name, since the build workers fetch them too. `github.com,gitlab.com,codeberg.org,bitbucket.org,git.sr.ht` by default. |
| `--diagnose` | Ask a language model about the failed builds of pull requests labelled `ci:diagnose`. Off by default. |
| `--diagnose-url URL` | Its OpenAI-compatible API, required with `--diagnose`. `https://openrouter.ai/api/v1` is OpenRouter's. A key is sent only over https or to a loopback address. |
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
   Opam repositories that the project's opam files name in
   `x-opam-repositories` are layered over both. The build uses the union of
   all its opam files' entries, in order of first appearance and at most
   four. Each is `[git+]https://HOST/PATH[#BRANCH]` with `HOST` one of
   `--remote-hosts`. Its branch, or default branch, is resolved to a commit
   when the solve fetches it, and the build adds it with `opam repository
   add` at that commit, with git following no redirects. Other entries are
   logged and ignored. A project with such repositories builds with
   OBuilder rather than day10, which takes one repository, and with a
   download cache of its own, since their packages' instructions run with
   it mounted. A solve gives up after two minutes, 10,000 packages or
   100,000 versions, or on an opam file over 256 KiB.
3. A workflow is spawned for every compiler with a solution, such as
   `5.4.1:x86_64:Debian-13`. Its first step carries the OBuilder
   spec as its command.
4. While builds run, a step called `waiting for N of M builds` logs each
   change as it happens, with the time since the builds began: a build
   waiting for a worker slot, queued on its pool, building a step, or ended,
   and the later platforms planned or skipped. Each build that ends then gets
   a step of its own, such as `5.5.1:x86_64:Debian-13 passed`, which fails if
   the build did not pass, before the next waiting step.
5. **summary** lists how each build ended, with those never started because
   the newest compiler failed. It fails unless every build passed, so the
   `ocaml-ci` workflow shows the result of the whole pipeline.

The newest compiler's build is also solved for each of `--platforms`, such
as `5.5.1:riscv64:Debian-13` or
`5.5.1:x86_64:Windows-mingw`. Those builds are further steps
of its workflow, which spawns them once it passes, so a project that fails on
x86_64 does not occupy the smaller pools. Each platform builds on its own
pool: `linux-arm64`, `linux-ppc64`, `linux-riscv64` or `windows-x86_64`.

Each spawned workflow submits its spec to the pool with the repository's
clone URL and commit, which the worker checks out. The workflow is pending
while the job waits in the pool's queue, and running from when a worker
takes it. The scheduler tells a submitter nothing else about the queue, such
as its place in it, which only its admin interface shows. The spec resets the
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

## Labels

A repository's owner turns on the slower builds and diagnosis for a pull
request with labels. They are defined once in the repository's label
settings, under exactly these names, which Tangled stores as the keys
`ci-full` and `ci-diagnose`:

- `ci:full` also builds the `--full-platforms`, which have smaller or slower
  pools.
- `ci:diagnose` asks the model about the pull request's failed builds.

A pull request's author may label it, so a label counts only when the owner
or a collaborator last added it. The spindle reads every version of the
pull request from its knot to see who did. Labels are read when the
`ocaml-ci` workflow starts and when its summary ends, and a label added
later takes effect on a retry. Pushes and manual runs have no labels, so
they build only `--platforms` and are never diagnosed.

## day10 builds

Debian builds on the `--day10` architectures are `day10 build` jobs, the
custom OCluster jobs of [Mark Elvers's day10
branch](https://github.com/mtelvers/ocluster/tree/day10), on the pool of the
same name with a `day10-` prefix, such as `day10-linux-x86_64`. Only workers
started with `--day10-cache` take them. The job names the project's source,
the opam-repository commit of the solve, the compiler, such as
`ocaml.5.4.1`, the project's packages, the platform's opam variables, and
`dune build @install @check @runtest` with tests. day10 solves and builds the
project itself in a cached sandbox. The solve here still decides which
builds a project has. The workflow's first step is `day10`, whose command is
the job as JSON.

day10 reads a single opam repository, so OxCaml builds, which layer the
OxCaml repository over opam-repository, stay OBuilder specs, as Windows
builds do. day10's output has no OBuilder operations, so its log is one
step.

## Diagnosing failures

With `--diagnose`, a pull request labelled `ci:diagnose` whose builds failed
gets a `diagnose` workflow once its summary is done. It makes one chat completions request to
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
