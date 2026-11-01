# Working on the Tangled library and CLI

Notes for agents keeping this package in step with Tangled. Read the
repository's `CLAUDE.md` first. It sets the prose style, the build and test
commands, and the commit rules that apply here too.

## Layout

- `lib/tangled_api.ml` is the client: identity, records, repositories,
  tickets, Git reads, releases, CI and service administration. Every network
  call goes through it.
- `cli/` holds one `cmd_*.ml` per command group. `common.ml` has shared
  terms and repository resolution. `local.ml` reads the Git clone that a
  command runs in. `ui.ml` draws terminal views. `diff_view.ml` prints
  knot diffs.
- `test/test_cli.py` runs the built CLI against loopback fixtures of a PDS,
  PLC directory, knot and spindle.
- `web/tangled-oauth-client.json` is the published OAuth client metadata.
- `../../lexicons/tangled/json` is the vendored lexicon set, and
  `../../tools/audit_tangled.py` compares it with upstream.
- `../../../spindle/testbed/e2e.py` drives this CLI against a Docker stack
  of real knot2 and the OCaml spindle.

## Tracking upstream

Tangled lives at <https://tangled.org/tangled.org/core>. The monorepo
expects a checkout at `../core`, a sibling of the monorepo root. Pull it
first. Then, from the monorepo root:

```sh
python3 bleeding/atp/tools/audit_tangled.py ../core --check
python3 bleeding/atp/tools/audit_tangled.py ../core --sync --check \
  > bleeding/atp/tangled-lexicon-audit.json
dune build @bleeding/atp/lexicons/tangled/hermest-tangled
dune build @bleeding/atp/all
dune runtest --force bleeding/atp/hermest/test bleeding/atp/bin/tangled/test
```

The first command lists what changed. The second replaces the vendored JSON
and records the audit. The build regenerates the OCaml codecs in
`atp_lexicon_tangled.ml`. Record the new upstream revision and date in
`../../lexicons/tangled/README.md`. The test asserts the number of
documents, so update it if documents were added or removed. Commit the sync
on its own, before any change that uses it.

A structural change to a lexicon the CLI decodes shows up as a type error
in `tangled_api.ml` or a failed decode in the tests. Fix the reader, not
the vendored JSON, which is never patched.

## Finding what a method needs

A lexicon says what a method takes, not who serves it or what token it
needs. Answer both from `../core` before writing a call:

- **Who serves it.** knot2 registers its routes in
  `knot2/crates/knot-xrpc/src/lib.rs`, with the NSID constants in the
  modules beside it. The spindle registers its routes in
  `spindle/xrpc/xrpc.go`. Methods that neither serves, such as search,
  timelines and notifications, are the appview's, which this CLI does not
  call.
- **How to call it.** The web client in `web/src/lib/api/` is the reference
  implementation. Read the matching file, such as `cob.ts`, `comment.ts`,
  `artifact.ts`, `secrets.ts` or `webhooks.ts`. It shows the service token
  audience, the request body and any follow-up write.

Rules learned this way, which the client already keeps:

- A write into a repository account goes through the repository's knot
  with a token whose audience is the repository DID. Use `repo_client`.
  Knot-level calls, such as membership and invite codes, use the knot's
  `did:web`. Spindle calls use the spindle's `did:web`.
- After the knot writes a ticket, comment, artifact or edit, the author
  writes an `org.tangled.graph.attestation` naming it.
  `repo_create_record` and `repo_put_record` do this. Grants, which the
  knot answers with an empty object, are not attested.
- The knot stamps `x-tngl-*` fields on records it serves. Strip them with
  `strip_stamps` before writing a record back.
- A pull request from a fork has its head in the fork. Read its diff there.
- Methods under `org.tangled.temp` may change without notice. Say so in the
  docs of any command that uses one.

## Adding a feature

1. Add the call to `lib/tangled_api.ml` and document it in the `.mli`. Use
   the generated codecs under `Lex`. Decode a field that the generator
   leaves as `Jsont.json` with the codec of the definition it refers to.
2. If the call needs a service token, add its NSID to `service_methods`.
   The OAuth scope is derived from that list, so run `dune runtest` on the
   test directory, then `dune promote` to update
   `web/tangled-oauth-client.json`. Tell the user that the file must be
   uploaded again to its published URL, and that OAuth sessions need a new
   login to gain the scope.
3. Add the command to the right group, following the conventions below.
4. Add a fixture for each new method to `test/test_cli.py` and assert on
   what the CLI sends, not only that it exits 0.
5. If an existing command changed shape, update `e2e.py`.
6. Update `README.md` and add a line to `../../CHANGES.md`.

## Command conventions

- Group commands by what a user is doing: `repo`, `issue`, `pull`,
  `pipeline`, `knot`, `spindle`. Raw access stays under `api`.
- A command that acts in a repository takes it with `-r`/`--repo`, from
  `Common.repo`. A command about a repository takes it as an optional
  argument, from `Common.repo_arg`. Both default to the clone in the current
  directory, through `Common.resolve`. Never require a DID where a handle
  or name would do.
- Default from the clone where it is safe: the current branch, the
  checked-out commit, the latest pipeline for it. Warn on standard error
  when a default falls back.
- A positional argument with no value must never do something destructive.
  Give removal its own command or flag.
- Keep `--audience` on service calls but out of the manual, through
  `Common.audience`.
- Piped output is plain, stable text that scripts and the tests parse. Draw
  tables and panels only when `Ui.tty ()` holds. Offer `--json` wherever
  there is structured output. Confirm a change with `Ui.ok`, which prints
  nothing to a pipe.
- Wrap slow calls in `Common.with_api ~busy:"..."` or `Ui.busy`.

## Checking a change

Build and test in the dev profile, which turns warnings into errors:

```sh
dune build @bleeding/atp/all @bleeding/spindle/all
dune runtest --force bleeding/atp/bin/tangled/test bleeding/atp/xrpc/test \
  bleeding/atp/xrpc-auth/test
```

Read-only commands work against live Tangled without a login. Use them to
check a reader and its terminal view against real data:

```sh
tangled repo info tangled.org/core --color=always
tangled pull diff 4646 -r tangled.org/core --stat
```

Do not run commands that write against live services. The Docker stack is
for that:

```sh
python3 bleeding/spindle/testbed/stack.py test
```
