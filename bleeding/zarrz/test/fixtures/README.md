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
