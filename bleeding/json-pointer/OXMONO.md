# OxMono import

Imported `c5008382648f9da8ba2b7eb9ecec945243cba08b` from `../ocaml-json-pointer` on 2026-09-08.
The source checkout is unchanged. [oxmono/upstream.json](oxmono/upstream.json)
records the base for future merges.

## Local adaptations

URI fragments use Uriz directly, retaining strict percent-escape validation.
Pure interfaces expose portable modes and immutable kinds. Equality and
comparison are eta-expanded. Jsont path operations that capture a supplied
value or absent default require that value's type to be portable and contended.
This is a compile-time constraint imposed by portable codec callbacks.

The combined offline import run passed 957 Alcotest tests, plus JSON Pointer
cram checks and DAV protocol/client checks. Live mail-server oracles were skipped.
Jsonm matched its pristine decoder in 5013 token, error and position comparisons.

## Validation and refresh

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check @bleeding/json-pointer/all
opam exec --switch=5.2.0+ox -- dune runtest --force --profile release-check bleeding/json-pointer
```

Export `git diff --binary BASE..NEW` from the source checkout and apply with
`git apply --directory=bleeding/json-pointer`. Resolve against the recorded base,
retain the adaptations above, run the scoped checks and update the revision.
Upstream CI and documentation describe the standalone build.
