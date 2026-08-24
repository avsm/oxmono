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
- New package `zarrz-eio`. `Zarrz_eio.store` is a store over a
  directory, taking an `Eio.Path.t` rather than an environment, with the
  key to path mapping `zarrs_filesystem` uses. Reads land in a bigstring
  through `pread` with no intermediate string, a ranged read seeks
  inside the open file and a batch of ranges opens it once, `list` walks
  only the directories a matching key can lie in, and writing is off
  unless `~writable:true` is passed. A key that is absolute or carries a
  `..` component is refused rather than followed.
- New `zarrz_conformance` executable in `zarrz-eio`, the
  `zarrs_conformance` command line contract: `--array_path <dir>` reads
  the whole array rooted at that directory and prints one line per
  element in C order, each the element's fill value metadata as compact
  JSON.
- New package `zarrz-fetch`. `Zarrz_fetch.store` is a read only HTTP
  store over a `Fetch.t`, mapping a key to `base_url ^ "/" ^ key`. It
  reads bodies straight into a bigstring, sends one `Range` header per
  ranged read and slices locally when the origin ignores it, runs the
  ranges of one `get_ranges` as up to six concurrent fibers, and sizes
  an object with `HEAD`. Transport failures propagate as `Fetch` raises
  them and an unusable status raises `Error.Store`.
- New `bench/bench_zarrz.exe`, a private executable outside `runtest`,
  reporting chunk decode throughput per codec chain, `Slab.F64` element
  access against the equivalent `Bigarray.Array1` loop, and the store
  calls and bytes a sharded partial read moves against a whole shard
  fetch.
