# Tangled lexicons

The JSON documents are verbatim copies of `lexicons/` in
<https://tangled.org/tangled.org/core> at
`3185226a0cb1862c9e53595319d07072886d092b`, reviewed 2026-09-10.

The complete set contains 231 documents: 187 `sh.tangled.*`, 43
`org.tangled.temp.*` and the shared `com.atproto.repo.strongRef`.
`LICENSE.upstream` contains Tangled's MIT licence. No JSON schema is patched.
Temporary APIs are exposed under `Atp_lexicon_tangled.Org.Tangled.Temp`.

From the monorepo root:

```sh
python3 bleeding/atp/tools/audit_tangled.py ../tangled-core --check
python3 bleeding/atp/tools/audit_tangled.py ../tangled-core --sync --check \
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

Generated codecs preserve absent and explicit-null fields separately. They
provide OCaml wire types, but do not enforce every lexicon constraint. The CLI
adds local input checks. Servers remain responsible for authorization,
grapheme limits, blob constraints and derived state.
