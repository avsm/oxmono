## Unreleased

- Initial Zarr V3 core: metadata, codecs, stores, unboxed slab access.
- `Slab` holds a decoded chunk and reads and writes its elements through
  unboxed accessors, one module per data type, all zero-allocating.
  `Slab.to_genarray` is a zero-copy bigarray view of the same buffer.
- `Subset` walks a rectangular region as contiguous element runs, and
  gathers and scatters those runs between an array buffer and a dense
  block.
- `Codec` binds the built-in codecs: `bytes`, `transpose`, `gzip`,
  `zstd`, `crc32c` and `sharding_indexed`. `bytes` hands the store's
  buffer to the slab with no copy when the declared endianness is the
  host's, `crc32c` reports a mismatch as
  `Error.Checksum_mismatch`, and `sharding_indexed` decodes a subset of
  a shard with one ranged read of the index and one batched read of the
  inner chunks it needs. An inner chunk equal to the fill value
  everywhere is left out of the shard it is written to.
- `Store` is a record of closures, with `Store.memory` a copying in
  memory backend that supports every operation.
- `Arr` opens, creates, reads and writes arrays, `Group` does the same
  for groups, and `Node.open_` reads a `zarr.json` once and dispatches
  on its `node_type`. A read whose subset is one whole chunk goes
  straight to that chunk, a chunk is read through store byte ranges
  when the codec chain and the store both allow it, and a write covers
  whole chunks without reading them back.
