# ecosystems CLI Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans, task by task. Steps use checkbox syntax.

**Goal:** `oecosystems`, a command-line tool that queries packages.ecosyste.ms through the `ecosystems` client.

**Architecture:** Package `ecosystems-cli` in `bleeding/ecosystems/`. Library `ecosystems_cli` (`cli/`) holds the cmdliner commands and renderers and takes its output formatter as an argument. `bin/main.ml` is a thin `Eio_main.run`. Tests evaluate the commands against a loopback server that serves recorded fixtures.

**Tech Stack:** cmdliner, Eio, `ecosystems`, `ecosystems.client`, `openapi` (for JSON encoding).

**Spec:** `docs/superpowers/specs/2026-10-03-ecosystems-design.md` (CLI was out of scope there and is now requested). Design agreed in chat on 2026-10-03: human summary by default, `--json` opt-in.

## Global Constraints

- Executable `oecosystems`, opam package `ecosystems-cli`, library `ecosystems_cli`.
- Commands: `registries`, `package`, `versions`, `version`, `dependents`, `advisories`, `lookup`, `maintainer`, `keyword`.
- Every command takes `--json`, `--base-url`, `--user-agent`. List commands also take `--limit N`.
- Default output is a short human summary. `--json` prints the full typed value.
- A failing API call prints one line to stderr and exits 1. It never prints a backtrace.
- No test reaches the network. Fixtures are recorded API responses.
- Branch `ecosystems-cli`. Commits: one line, imperative, no trailers. A reformat is its own commit.
- Prose per CLAUDE.md. 80 columns. `eio_main` becomes `:with-test` in package `ecosystems`.
- No AI-disclosure tags.
- Build with `dune build @bleeding/ecosystems/all @bleeding/ecosystems/runtest --force`.

## Review Focus

- A server error (404, 500) prints a one-line message and exits 1.
- `--limit 1` over a multi-item listing prints exactly one item and requests no further page than needed.
- A `null` or absent optional field (description, licenses) renders as `-`, never as an exception.
- `--json` output decodes back through the same codec.
- A package or registry name with `/` or `@` (for example `@types/node`) is percent-encoded in the path.
- `lookup` treats an argument starting with `pkg:` as a purl and anything else as a repository URL.

---

## Task 1: Shared loopback server and the CLI skeleton

**Files:**
- Create: `test/loopback.ml`, `cli/{dune,ecosystems_cli.ml,ecosystems_cli.mli}`, `bin/{dune,main.ml}`, `test/test_cli.ml`
- Modify: `test/test_http.ml`, `test/dune`, `dune-project`

**Interfaces:**
- Produces: `Loopback.serve : sw:Eio.Switch.t -> _ -> (int -> string -> int * string * string) -> string * string list list ref`. The responder takes the request number and the request target, and returns status, extra header lines and body. The result is the base URL and the headers seen.
- Produces: `Ecosystems_cli.main : out:Format.formatter -> env -> int Cmdliner.Cmd.t`, where `env` has the capabilities `Ecosystems_client.create` needs.

- [ ] **Step 1: Move the server out of `test_http.ml` into `test/loopback.ml`.** Responders gain the request target as a second argument. `test_http.ml` uses `Loopback.serve`, and its `(modules ...)` field lists `loopback`.
- [ ] **Step 2: Run the existing suite.** Expected: PASS, the same assertions as before.
- [ ] **Step 3: Write `test_cli.ml` with one failing test**: `registries` against a server that returns `fixtures/registries.json` for `/registries` and `[]` for any request containing `page=2`. Assert exit 0 and that the output contains `npmjs.org`.
- [ ] **Step 4: Run it. Expected: FAIL**, library `ecosystems_cli` not found.
- [ ] **Step 5: Implement the library, the `registries` command, the executable, and the package stanza** in `dune-project` (`ecosystems-cli` depends on `ecosystems`, `cmdliner`, `eio_main`, `jsont`, `openapi`, and `fmt`). Mark `eio_main` `:with-test` in `ecosystems`.
- [ ] **Step 6: Run. Expected: PASS.** Commit `Add the oecosystems command-line tool with a registries command`.

## Task 2: Package, version and listing commands

**Files:** `cli/ecosystems_cli.ml`, `test/test_cli.ml`, new fixtures `maintainer.json`, `dependents.json`.

**Interfaces:** consumes Task 1. Adds `package`, `versions`, `version`, `dependents`, `advisories`.

- [ ] **Step 1: Record `dependents.json`** from `/registries/crates.io/packages/serde/dependent_packages?per_page=1`, and read it.
- [ ] **Step 2: Write failing tests**, one per command, asserting exit 0 and a distinctive substring of the output (`serde` for `package`, `1.0.229` for `versions`, `1.0.0` for `version`, `minimist` and an advisory title for `advisories`).
- [ ] **Step 3: Run. Expected: FAIL**, unknown command.
- [ ] **Step 4: Implement the five commands.** Lists use `Ecosystems_client.pages` with `--limit` through `Seq.take`. Path segments are percent-encoded by the generated client, and a test with `@types/node` in a recorded path proves it.
- [ ] **Step 5: Run. Expected: PASS.** Commit `Add package, version and advisory commands to oecosystems`.

## Task 3: Lookup, maintainer and keyword

**Files:** `cli/ecosystems_cli.ml`, `test/test_cli.ml`, fixture `maintainer.json`.

- [ ] **Step 1: Record `maintainer.json`** from `/registries/crates.io/maintainers/slaxxarn`.
- [ ] **Step 2: Failing tests**: `lookup pkg:npm/minimist` sends `purl=` and `lookup https://github.com/x/y` sends `repository_url=` (assert on the recorded request target), `maintainer crates.io slaxxarn`, `keyword rust`.
- [ ] **Step 3: Run. Expected: FAIL.**
- [ ] **Step 4: Implement.** Commit `Add lookup, maintainer and keyword commands to oecosystems`.

## Task 4: Errors, JSON output and limits

**Files:** `cli/ecosystems_cli.ml`, `test/test_cli.ml`.

- [ ] **Step 1: Failing tests**:
  - a 404 gives exit 1, one line on stderr containing `404`, and no `Raised at`;
  - `package --json` output decodes with `Ecosystems.Package.T.jsont`;
  - `registries --limit 1` over the 3-item fixture prints one registry;
  - a package with `null` description prints `-`.
- [ ] **Step 2: Run. Expected: FAIL.**
- [ ] **Step 3: Implement** error wrapping around `Openapi.Runtime.Api_error` and `Eio.Io`, and `-` for absent fields. Commit `Report oecosystems errors on one line and add JSON output`.

## Task 5: Docs and final verification

- [ ] **Step 1: README section** with the command table and one example per command, each run for real against the loopback fixtures or checked by a compiler probe. **Step 2: CHANGES entry.** **Step 3: Verify** the full build and tests with `--force`, check 80 columns, then commit `Document the oecosystems command-line tool`.
