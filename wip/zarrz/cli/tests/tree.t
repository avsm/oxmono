Printing a hierarchy through the filesystem store. The fixtures are the
golden hierarchies of the `zarrs` oracle, so the shapes and codec chains
below are that library's own output.

A root group with two child groups and two arrays under one of them.

  $ zarr tree ../../test/fixtures/hierarchy.zarr
  /  group
  ├── a  group
  │   ├── baz  array float64 10000x1000 chunks 1000x100 bytes(le) gzip(1)
  │   └── foo  array float64 10000x1000 chunks 1000x100 bytes(le) gzip(1)
  └── b  group
  4 nodes found by listing store keys.

Bounding the recursion.

  $ zarr tree ../../test/fixtures/hierarchy.zarr --depth 1
  /  group
  ├── a  group
  └── b  group
  4 nodes found by listing store keys, 2 shown at depth 1.

Starting below the root.

  $ zarr tree ../../test/fixtures/hierarchy.zarr /a
  /a  group
  ├── baz  array float64 10000x1000 chunks 1000x100 bytes(le) gzip(1)
  └── foo  array float64 10000x1000 chunks 1000x100 bytes(le) gzip(1)
  2 nodes found by listing store keys.

A hierarchy whose root carries no document of its own, holding one
sharded array. The shard spells its inner chunk shape, its inner chain,
then its index chain.

  $ zarr tree ../../test/fixtures/sharded_array_write_read.zarr
  /  group (no metadata document)
  └── group  group
      └── array  array uint16 8x8 chunks 4x8 sharding(4x4; bytes(le) gzip(5) | idx bytes(le) crc32c)
  2 nodes found by listing store keys.

A store that is one array.

  $ zarr tree ../../test/fixtures/v3/array_zstd.zarr
  /  array float32 10x10 chunks 5x5 bytes(le) zstd(5)
  0 nodes found by listing store keys.

  $ zarr tree ../../test/fixtures/tessera_band
  /  array int32 128 chunks 128 bytes(le) blosc(zstd)
  0 nodes found by listing store keys.

The same hierarchy as JSON.

  $ zarr tree ../../test/fixtures/hierarchy.zarr --depth 1 --json
  {
    "store": "../../test/fixtures/hierarchy.zarr",
    "path": "/",
    "discovery": "listing",
    "nodes": 4,
    "shown": 2,
    "tree": [
      {
        "path": "/",
        "name": "",
        "node_type": "group",
        "metadata_document": true
      },
      {
        "path": "/a",
        "name": "a",
        "node_type": "group",
        "metadata_document": true
      },
      {
        "path": "/b",
        "name": "b",
        "node_type": "group",
        "metadata_document": true
      }
    ]
  }
