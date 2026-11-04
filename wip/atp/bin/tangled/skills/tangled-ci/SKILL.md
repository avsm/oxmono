---
name: tangled-ci
description: Follow Tangled CI pipelines for the user's repositories and act on their results. Use after pushing to a Tangled repository, when asked to watch, check or fix CI on Tangled, or when a pipeline failed and needs its logs, a retry or an approval.
---

# Tangled CI

The `tangled` command reports and controls CI on Tangled. Its pipelines run
on the spindle a repository names. Check `tangled auth status` first. If
the user is not logged in, ask them to run `tangled auth login`.

## Watch

After a push, wait for that commit's pipeline and exit when it finishes:

```sh
tangled pipeline watch --commit HEAD --until-finished --json
```

Run it in the background, because a pipeline can take minutes or hours. The
command exits successfully only if the pipeline passed. To see what is
running across all the user's repositories now, run
`tangled pipeline watch --once --json`. Leave out `--once` to keep watching.

Each line is one JSON event:

- `event` is `started`, `workflow` or `finished`.
- `repo`, `pipeline`, `commit`, `trigger`, `ref` and `url` say which run.
- `status` is the pipeline's, and `workflow`, `workflow_status`,
  `previous` and `error` describe a workflow that changed.
- `passed`, `failed` and `workflows` count the workflows.
- `next` lists the commands worth running now. It is empty when nothing
  needs doing.

Without `--json`, the same events are one line each, and the commands follow
on lines starting with `  next: `.

## Act

- **A workflow failed.** Run its `next` command,
  `tangled pipeline logs -r REPO PIPELINE -w WORKFLOW`, and read the end of
  the log, where the error usually is. Fix the cause in the code, commit,
  push and watch again. Do not retry a failure the code causes.
- **A failure looks transient**, such as a network error, a lost worker or a
  timeout, with no change in between. Retry once with
  `tangled pipeline retry -r REPO PIPELINE`, then watch the new ID it
  prints.
- **A pipeline is blocked.** A pull request from someone without push access
  waits for approval. Approval runs their code on the spindle, so tell the
  user and approve with `tangled pipeline approve -r REPO PIPELINE` only when
  they ask.
- **Cancel** a run the user no longer wants with
  `tangled pipeline cancel -r REPO PIPELINE`.

For ocaml-ci spindles, the `ocaml-ci` workflow plans the builds and spawns one
workflow per compiler and platform, named like `5.5.1:x86_64:Debian-13`.
Builds on further platforms start only after the x86_64 build passes. Pull
requests that a maintainer labels `ci:full` build more platforms, and those
labelled `ci:diagnose` get a `diagnose` workflow with a model's reading of
the failures. Read that workflow's log before reading the build logs.

## Report

Tell the user the pipeline's result, its `url`, and for each failure the
workflow, the error and what you did about it.
