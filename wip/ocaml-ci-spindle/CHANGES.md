# Changes

## Unreleased

- End the `ocaml-ci` workflow with a `summary` step that waits for every
  build and lists how each ended.
- Add `--diagnose`, off by default, which asks a language model with no
  tools about failed builds in a `diagnose` workflow, through an
  OpenAI-compatible API such as OpenRouter's.
- Default `--workflow-timeout` to six hours, since the summary waits for
  every platform's build.
- Name workflows after their compiler, distribution and architecture, such
  as `OCaml 5.4.1 on Debian 13 (x86_64)`.
- Build on Debian 13 with opam 2.6, as OCaml-CI does.
- Build the newest compiler on arm64, ppc64le, riscv64 and Windows once its
  x86_64 build passes, chosen with `--platforms`. `--pool` is removed.
- Ignore root opam files whose names are not opam package names.
- Refuse repositories' own workflows, which could otherwise submit any spec.
- Bound log line joining, partial lines and job cancellation, and time out
  stalled opam-repository fetches.
