# d10

Store binary layers, assemble installation prefixes, index files and lock
caches. The `d10` package provides two libraries:

| Library | Interface | Purpose |
| --- | --- | --- |
| `d10` | [lib/d10.mli](lib/d10.mli) | Local layers, prefixes, locks and system operations. |
| `d10.ir` | [ir/d10ir.mli](ir/d10ir.mli) | Build recipes, dependency graphs and install files. |

`D10ir.Direct.run` schedules a plan. `run_node` executes one recipe after its
dependencies are available, with optional preparation for generated metadata.
Both use the same execution and layer-capture implementation.

The default `Staging` policy captures layers from temporary prefixes. Consumers
must ensure the outputs can be relocated. `Permanent` builds at a stable
per-layer prefix and restores missing dependency prefixes on cache hits.
Producers must include the policy and permanent location in their cache keys.
Ox selects `Permanent` and disables host PATH augmentation.

`D10.Prefix.prepare` detaches restored hardlinks before installation.
`snapshot` and `diff` compare contents, permissions and symlink targets, and
reject deleted dependency files. Cached unions preserve layer order.

[D10ir.Makefile](ir/makefile.mli) exports plans as standalone Makefiles with
unpacked sources and shell helpers. It builds dependency layers at one shared
prefix and installs selected application binaries and shared data. Deferred
scalar configuration bindings are resolved from installed dependencies.

```sh
dune build --profile release-check @bleeding/d10/all
dune runtest --profile release-check --force bleeding/d10
```

Use the workspace's OxCaml build environment. HTTP tests require loopback
sockets but no external services. See [OXMONO.md](OXMONO.md) for adaptations
and cache constraints. The [ox runner](../../avsm/ox/README.md) uses this layer
store with permanent installation prefixes.
