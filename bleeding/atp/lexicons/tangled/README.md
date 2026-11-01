# Tangled lexicons

The JSON documents are verbatim copies of `lexicons/` in
<https://tangled.org/tangled.org/core> at
`52549bde582beb2ce71cc88d6fa726c41cbc8fe7`, reviewed 2026-11-01.

The complete set contains 418 documents. Current records and methods are
under `org.tangled.*`, including temporary APIs under `org.tangled.temp.*`.
Legacy `sh.tangled.*` documents remain for data written before the move. The
set also carries the shared `com.atproto` documents Tangled references and
`com.bad-example.pocket`, a preferences store the Tangled web client uses.
`LICENSE.upstream` contains Tangled's MIT licence. No JSON schema is patched.
Current APIs are under `Atp_lexicon_tangled.Org.Tangled`, legacy ones under
`Atp_lexicon_tangled.Sh.Tangled`.

From the monorepo root:

```sh
python3 bleeding/atp/tools/audit_tangled.py ../core --check
python3 bleeding/atp/tools/audit_tangled.py ../core --sync --check \
  > bleeding/atp/tangled-lexicon-audit.json
opam exec --switch=5.2.0+ox -- dune build --profile release-check \
  @bleeding/atp/lexicons/tangled/hermest-tangled
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check --force \
  bleeding/atp/hermest/test bleeding/atp/bin/tangled/test
```

Review the diff before accepting a new upstream revision. The audit compares
parsed JSON by NSID, including temporary APIs and shared definitions.
`--check` fails on additions, removals or changes. The generator fails if any
document cannot be parsed. The CLI catalogue is generated from this same JSON
tree at build time.

Generated codecs preserve absent and explicit-null fields separately. A
reference to another definition in the same document decodes as raw JSON, to
be decoded with that definition's codec. They
provide OCaml wire types, but do not enforce every lexicon constraint. The CLI
adds local input checks. Servers remain responsible for authorization,
grapheme limits, blob constraints and derived state.
