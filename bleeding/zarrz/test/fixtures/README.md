# zarrz test fixtures

Golden data copied verbatim from the `zarrs` Rust workspace, the
behavioural oracle for this library.

- Source: <https://github.com/LDeakin/zarrs>, directory
  `zarrs/tests/data/`.
- Commit: `c17fe374b1fa7df8373b6c6f6eb3f1d33c3a3bd7`.
- Licence: MIT OR Apache-2.0, see `LICENCE-MIT` and `LICENCE-APACHE` in
  that repository.

Files:

- `array_metadata.json`, the array metadata example of
  `zarrs_metadata::v3::ArrayMetadataV3`.
- `group_metadata.json`, the group metadata example of
  `zarrs_metadata::v3::GroupMetadataV3`.

Codec fixtures, copied whole from the same tree. Each array directory is
verbatim, including the Zarr V2 `.zarray` and `.zattrs` the oracle writes
beside `zarr.json` under `v3/`.

- `v3/array_none.zarr`, `v3/array_gzip.zarr`, `v3/array_zstd.zarr`,
  `v3/array_none_transpose.zarr`. Written by the `zarrs` tests
  `array_v2_none_c`, `array_v2_gzip_c`, `array_v2_zstd_c` and
  `array_v2_none_f` in `zarrs/src/array.rs`. All four are 10 by 10
  `float32` in 5 by 5 chunks with fill value `0.0`, and every element is
  `a.(i).(j) = 10 * i + j`, asserted by `array_v2_to_v3` in that file.
  The chunk keys use the `v2` encoding with separator `.`.
- `v3_zarr_python/array_none.zarr`, `v3_zarr_python/array_gzip.zarr`,
  `v3_zarr_python/array_zstd.zarr`. Written by `tests/data/v3_generate.py`
  with `zarr` 3.0.8 and `numcodecs` 0.16.1, holding the same 10 by 10
  `float32` values, and read back by `array_v3_none`, `array_v3_gzip` and
  `array_v3_zstd`. The chunk keys use the `default` encoding.
- `v3/array_blosc.zarr`, `v3/array_blosc_transpose.zarr`. The same
  family, written by the `zarrs` tests `array_v2_blosc_c` and
  `array_v2_blosc_f` in `zarrs/src/array.rs`. Both are `blosc` with
  `cname` zstd, `clevel` 1, `bitshuffle` and `typesize` 4, the
  transposed one with a `transpose` codec of order `[1, 0]` ahead of
  `bytes`. Blosc frames are not reproducible across builds of the C
  library, so these are read and not re-encoded.
- `sharded_array_write_read.zarr`. Written by
  `zarrs/examples/sharded_array_write_read.rs`. An 8 by 8 `uint16` array
  in 4 by 8 shards of 4 by 4 inner chunks, gzip level 5 inside, index
  codecs `bytes` little endian then `crc32c`, index at the end. Element
  `(i, j)` is `8 * i + j`, from the example's write loop.

Hierarchy fixtures, for the store and node layers.

- `hierarchy.zarr`. The tree `zarrs/tests/hierarchy.rs` walks: a root group, a
  group `a` holding the arrays `a/foo` and `a/baz`, and a group `b` with
  the attribute `test_key`. Both arrays are 10000 by 1000 `float64` in
  1000 by 100 chunks, `bytes` little endian then `gzip` level 1, fill
  value `NaN`, dimension names `rows` and `columns`, and the attributes
  `foo`, `bar` and `baz`. No chunk is stored, so every element is the
  fill value.
- `array_write_read.zarr`. The final state of
  `zarrs/examples/array_write_read.rs`, whose write, overwrite and erase
  sequence the example lists in order. A root group, a group `group`
  with the attribute `foo`, and an 8 by 8 `float32` array `group/array`
  in 4 by 4 chunks, `bytes` little endian, fill value `NaN`, dimension
  names `y` and `x`. Chunk `[0, 0]` is erased at the end of the example
  and is absent, so the top left quarter reads as `NaN`. The other three
  chunks are the values `test_eio.ml` names, read out of the fixture
  bytes and cross checked against the example's write calls.

Re-vendoring: copy the file again from the same path in a newer `zarrs`
checkout and update the commit above. A test that fails afterwards means
the oracle changed, so change this library rather than the fixture.

## Real-world data

`tessera_band/` is not from the oracle. It is one array of the Tessera
geospatial embeddings, copied on 2026-08-24 from
<https://data.source.coop/tessera/tessera/zarr/v1/utm30/band>, which is
the `band` coordinate of the UTM zone 30 store. `zarr.json` and the
single chunk `c/0` are the store's own bytes, unaltered. The store
publishes no licence in its metadata, so it is named by its URL rather
than by a licence here; check the Source Cooperative repository page
before redistributing it further.

The array is 128 `int32` in one chunk, with the chain `bytes` little
endian then `blosc` with `cname` zstd, `clevel` 3, `shuffle` and
`typesize` 4. It is the chain the whole store uses, which is why one
chunk of it is worth keeping: it is the evidence that this library reads
a blosc store nobody here wrote. The 128 values are the band indices, 0
to 127.
