# zarrz — Zarr V3 for OxCaml

Reads and writes [Zarr V3](https://zarr-specs.readthedocs.io/en/latest/v3/core/)
chunked N-dimensional arrays. Decoded chunks are exposed through `Zarrz.Slab`,
whose per-type accessors return unboxed scalars (`float#`, `int64#`, …) and
allocate nothing. Stores are runtime values: an in-memory store ships with the
core, `zarrz-eio` adds an Eio filesystem store, and `zarrz-fetch` adds a
read-only HTTP store over the `fetch` client with ranged shard reads.

See `DESIGN.md` for the architecture and the supported subset of the
specification. The Rust [zarrs](https://github.com/zarrs/zarrs) implementation
is the behavioural oracle, and `conformance/` builds a CLI compatible with its
`zarrs_conformance` harness.

## Build

    dune build @bleeding/zarrz/all @bleeding/zarrz/runtest
