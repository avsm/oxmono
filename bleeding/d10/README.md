# d10

Store binary layers, assemble installation prefixes, index files and lock
caches. The `d10` package provides two libraries:

| Library | Interface | Purpose |
| --- | --- | --- |
| `d10` | [lib/d10.mli](lib/d10.mli) | Local layers, prefixes, locks and system operations. |
| `d10.ir` | [ir/d10ir.mli](ir/d10ir.mli) | Build recipes, dependency graphs and install files. |

```sh
dune build --profile release-check @bleeding/d10/all
dune runtest --profile release-check --force bleeding/d10
```

Use the workspace's OxCaml build environment. HTTP tests require loopback
sockets but no external services. See [OXMONO.md](OXMONO.md) for adaptations
and cache constraints. The [ox runner](../../avsm/ox/README.md) uses this layer
store with permanent installation prefixes.
