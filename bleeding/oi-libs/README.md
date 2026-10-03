# Libraries from oi

These libraries are imported from oi 0.14.2 for use in OxMono.
The source revision, adaptations and validation are in [OXMONO.md](OXMONO.md).

| Library | Purpose |
| --- | --- |
| `osrel` | Detect architecture, operating system and build parallelism. |
| `d10` | Store binary layers, assemble prefixes, index files and lock caches. |
| `d10.ir` | Encode and validate build recipes and execute their dependency graph. |
| `osdist` | Generate Debian, RPM and static distribution packaging. |

Public interfaces are in `lib/*/*.mli`. The libraries build against OxMono's
Eio, Jsont, SQLite and Fetch. Their package names remain unchanged.
The `oi` implementation and command libraries are not included.

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check \
  @bleeding/oi-libs/all
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check \
  --force bleeding/oi-libs
```

HTTP tests bind a loopback socket. All tests run without external services.
[The ox plan](../../docs/ox-plan.md) describes the next stage after review.
