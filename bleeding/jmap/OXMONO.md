# OxMono import

Imported `eb3201ccba6bbff7070fd759e952124715f3a2ff` from `../ocaml-jmap` on 2026-09-08.
The source checkout is unchanged. [oxmono/upstream.json](oxmono/upstream.json)
records the base for future merges.

## Local adaptations

The JMAP protocol, Eio client, command line, examples and tests retain their
upstream layout. OCaml's minimum is 5.2. Jsont defaults use factories and
recursive codecs use `Jsont.Portable_lazy`. Pure interfaces have checked
portable modes and immutable kinds. Portable maps and sets are constructed
fresh where needed. The patch module's shared empty map remains nonportable.
Local Eio buffers are globalized before capture. One `List.take` is expanded
for OCaml 5.2. HTTPz optional URI fields use `Null` and `This`.
The unused test dependency on Uri is removed.

`mosaic/` is retained as source but declared data-only. Its separate Mosaic
terminal UI dependencies are not in this workspace. Remove it from the root
`data_only_dirs` stanza after importing those dependencies to enable the UI.
`jmap-mosaic` is an empty package here. Its `matrix-eio` dependency is the
Mosaic UI backend, not the Matrix chat SDK.
Live mail-server oracle tests remain opt-in. No live mailbox was accessed.

The combined offline import run passed 957 Alcotest tests, plus JSON Pointer
cram checks and DAV protocol/client checks. Live mail-server oracles were skipped.
Jsonm matched its pristine decoder in 5013 token, error and position comparisons.

## Validation and refresh

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check @bleeding/jmap/all
opam exec --switch=5.2.0+ox -- dune runtest --force --profile release-check bleeding/jmap
```

Export `git diff --binary BASE..NEW` from the source checkout and apply with
`git apply --directory=bleeding/jmap`. Resolve against the recorded base,
retain the adaptations above, run the scoped checks and update the revision.
Upstream CI and documentation describe the standalone build.
