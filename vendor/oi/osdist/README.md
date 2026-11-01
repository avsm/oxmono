# osdist

Generate Debian, RPM and static distribution packaging for OCaml projects.
Start with the [Osdist API](lib/osdist.mli) for a usage example, the source
bundle contract and links to the packaging modules.

```sh
dune build --profile release-check @vendor/oi/osdist/all
dune runtest --profile release-check --force vendor/oi/osdist
```

Use the workspace's OxCaml build environment. Tests check generated packaging
without building containers or installing packages. See [OXMONO.md](OXMONO.md)
for import provenance and deployment limits.
