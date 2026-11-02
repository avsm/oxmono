# osrel

This unreleased library is maintained in OxMono under `wip/osrel`.
Its oi source provenance is recorded in [OXMONO.md](OXMONO.md).

Detect architecture, operating system and build parallelism through Eio.
The public interface is [lib/osrel.mli](lib/osrel.mli).

```sh
dune build --profile release-check @wip/osrel/all
dune runtest --profile release-check --force wip/osrel
```

Use the workspace's OxCaml build environment. See [OXMONO.md](OXMONO.md)
for import provenance and validation.
