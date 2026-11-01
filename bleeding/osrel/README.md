# osrel

Detect architecture, operating system and build parallelism through Eio.
The public interface is [lib/osrel.mli](lib/osrel.mli).

```sh
dune build --profile release-check @bleeding/osrel/all
dune runtest --profile release-check --force bleeding/osrel
```

Use the workspace's OxCaml build environment. See [OXMONO.md](OXMONO.md)
for import provenance and validation.
