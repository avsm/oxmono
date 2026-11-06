# Changes

## Unreleased

- Build the OxCaml variant with OxCaml 5.4.0-ox9.
- Fetch `x-opam-repositories` remotes only at a public address, checked when
  each is fetched and pinned for git, with no redirects, no explicit port,
  no symbolic links in the checkout, none of the host's git configuration,
  and only the `packages` directory. A remote that cannot be fetched is
  logged and skipped, and is not tried again for ten minutes. Remotes are
  fetched before the shared repositories are locked, so a slow one delays
  only the plans that name it.
- Bound the diagnosis prompt to eight root files and 256 KiB.
- Use `x-opam-repositories` remotes only on `--remote-hosts`, public forges
  by default, since build workers fetch them too. Builds fetch them with no
  git redirects and use a download cache of their own.
- Give up a solve after two minutes, 10,000 packages or 100,000 versions, or
  on an opam file over 256 KiB.
- Require `--diagnose-url` with `--diagnose`, and send a key only over https
  or to a loopback address.
- Kill git's whole process group when a fetch times out or is cancelled.

- Layer the opam repositories named in a project's `x-opam-repositories`
  fields over opam-repository, the union across its opam files. Each branch
  is resolved to a commit for the solve, and builds add the repositories at
  those commits. Only https remotes on public hosts are fetched.

- Show the solve as a short summary: the repositories by short commit, each
  installable build with its package count and the versions chosen for the
  project's direct dependencies, and each other build with one reason, such
  as the project's own bound on the compiler, instead of the solver's
  traceback.

- Build Debian on `--day10` architectures, x86_64 and riscv64 by default,
  as `day10 build` jobs on the `day10-` OCluster pools, which are much
  faster than OBuilder ones. OxCaml and Windows builds stay OBuilder.

- Show a build as pending until an OCluster worker takes its job, rather
  than running from its submission. The build's first step is now `start`,
  the worker's setup before the first operation.

- Read the failed builds a `diagnose` workflow asks about from the
  spindle's store, so a diagnosis survives a restart.

- Build only arm64 after x86_64 by default. ppc64le, riscv64 and Windows,
  `--full-platforms`, are built for pull requests labelled `ci:full`.
- Diagnose only pull requests labelled `ci:diagnose`. Labels count only
  when the repository's owner or a collaborator added them.
- End the `ocaml-ci` workflow with a `summary` step that waits for every
  build and lists how each ended.
- Show the builds' progress in the `ocaml-ci` workflow as it happens: a
  waiting step logs each build being queued, starting each step and ending,
  and each build that ends gets a step that passes or fails with it.
- Add `--diagnose`, off by default, which asks a language model with no
  tools about failed builds in a `diagnose` workflow, through an
  OpenAI-compatible API such as OpenRouter's.
- Default `--workflow-timeout` to six hours, since the summary waits for
  every platform's build.
- Name workflows briefly as compiler, architecture and distribution, such
  as `5.4.1:x86_64:Debian-13`, or `5.2.0+ox:x86_64:Debian-13` for OxCaml.
  Tangled's workflow pages take the name as one path segment, so it holds
  no `/`.
- Build on Debian 13 with opam 2.6, as OCaml-CI does.
- Build the newest compiler on arm64, ppc64le, riscv64 and Windows once its
  x86_64 build passes, chosen with `--platforms`. `--pool` is removed.
- Ignore root opam files whose names are not opam package names.
- Refuse repositories' own workflows, which could otherwise submit any spec.
- Bound log line joining, partial lines and job cancellation, and time out
  stalled opam-repository fetches.
