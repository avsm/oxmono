# zarrz - Zarr V3 for OxCaml

Reads and writes [Zarr V3](https://zarr-specs.readthedocs.io/en/latest/v3/core/)
chunked N-dimensional arrays. Decoded chunks are exposed through `Zarrz.Slab`,
whose per-type accessors return unboxed scalars (`float#`, `int64#`, …) and
allocate nothing.

The core carries every built-in codec: `bytes`, `transpose`, `gzip`, `zstd`,
`blosc`, `crc32c` and `sharding_indexed`. None is an optional dependency, so a
hierarchy written by another implementation reads without a build flag.

Stores are runtime values. An in-memory store ships with the core, `zarrz-eio`
adds an Eio filesystem store, and `zarrz-fetch` adds a read-only HTTP store over
the `fetch` client. Both of the latter answer byte ranges, so a subset of a
sharded array costs the inner chunks it touches rather than the whole shard.

The `zarrz.geoemb` sublibrary holds typed `jsont` codecs for the
[geo-embeddings convention][geoemb], mapping a group's `attributes` to a record
and back without losing the members of the other conventions the group
declares. It depends on `jsont` alone.

[geoemb]: https://github.com/geo-embeddings/embeddings-zarr-convention

See `DESIGN.md` for the architecture and the supported subset of the
specification. The Rust [zarrs](https://github.com/zarrs/zarrs) implementation
is the behavioural oracle, and `conformance/` builds a CLI compatible with its
`zarrs_conformance` harness.

## Build

    dune build @bleeding/zarrz/all @bleeding/zarrz/runtest
