# ocaml-codec TOML subset

Imported from `../ocaml-codec`, upstream `https://github.com/samoht/ocaml-codec`,
commit `fd9ecc5dd4e1296cc2555c74461495e002e9683f` on 2026-09-09.
The ISC license and original source notices are retained.

This import contains `lib/ascii`, `lib/utf8`, `lib/loc` and `lib/toml`.
Other codecs, TOML JSON adapters, command-line programs and upstream test
dependencies are omitted. Public names remain `codec.toml`, `codec.loc` and
`codec.utf8`. `mqttz.config` consumes the TOML codec.

## Local patches

`lib/toml/toml.ml` uses local `let* = Result.bind` and `let+` based on
`Result.map` instead of the two `Result.Syntax` opens. These equivalent
operators allow the source to compile on OxCaml's OCaml 5.2 base. Dune metadata
declares this partial package and removes upstream MDX stanzas. No parsing or
encoding algorithm is changed.

## Refresh and verify

Copy the four source directories from the recorded base or a reviewed newer
revision, excluding their subdirectories. Reapply the two local operator
bindings and the partial Dune layout. Update this file and `../upstreams.json`.

```sh
python3 vendor/ocaml-codec/check-port.py ../ocaml-codec
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check --force bleeding/mqttz
```

The differential builds pristine source with the `5.5.0` switch in a temporary
workspace and byte-compares 19 results with the OxCaml port. It exercises TOML
documents, syntax errors and valid/invalid offset and local datetimes. Switches
can be changed with `--upstream-switch` and `--port-switch`. Use the mqttz
consumer tests because recursive vendor test aliases skip vendored packages.
