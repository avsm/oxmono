## Unreleased

- Initial Zarr V3 core: metadata, codecs, stores, unboxed slab access.
- `Slab` holds a decoded chunk and reads and writes its elements through
  unboxed accessors, one module per data type, all zero-allocating.
  `Slab.to_genarray` is a zero-copy bigarray view of the same buffer.
- `Subset` walks a rectangular region as contiguous element runs, and
  gathers and scatters those runs between an array buffer and a dense
  block.
