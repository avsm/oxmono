# osdist

Generate Debian, RPM and static distribution packaging for OCaml projects.
Public interfaces are in `lib/*.mli`.

```sh
dune build --profile release-check @bleeding/osdist/all
dune runtest --profile release-check --force bleeding/osdist
```

Use the workspace's OxCaml build environment. Tests check generated packaging
without building containers or installing packages. See [OXMONO.md](OXMONO.md)
for import provenance and deployment limits.
