One node in full.

An array with a two codec chain, a string fill value and attributes.

  $ zarr info ../../test/fixtures/hierarchy.zarr /a/foo
  store                 ../../test/fixtures/hierarchy.zarr
  path                  /a/foo
  node type             array
  data type             float64
  shape                 10000x1000
  elements              10000000
  nominal size          76.3 MiB
  dimension names       rows columns
  chunk grid            regular
  chunk shape           1000x100
  grid shape            10x10
  chunk count           100
  chunk key encoding    default separator '/'
  fill value            "NaN"
  codecs
    bytes
      endian            "little"
    gzip
      level             1
  storage transformers  none
  attributes
    foo                 42
    bar                 "apples"
    baz                 [1,2,3,4]
  extensions
    consolidated        absent

A group, whose children the filesystem store can count.

  $ zarr info ../../test/fixtures/hierarchy.zarr
  store                 ../../test/fixtures/hierarchy.zarr
  path                  /
  node type             group
  children              2 (2 groups, 0 arrays)
  attributes            none
  extensions
    consolidated        absent

A sharded array. The shard block is computed from the codec
configuration, and the index size comes from encoding the index chain
rather than from assuming its overhead.

  $ zarr info ../../test/fixtures/sharded_array_write_read.zarr /group/array
  store                 ../../test/fixtures/sharded_array_write_read.zarr
  path                  /group/array
  node type             array
  data type             uint16
  shape                 8x8
  elements              64
  nominal size          128 B
  dimension names       y x
  chunk grid            regular
  chunk shape           4x8
  grid shape            2x1
  chunk count           2
  chunk key encoding    default separator '/'
  fill value            0
  codecs
    sharding_indexed
      chunk_shape       [4,4]
      codecs
        bytes
          endian        "little"
        gzip
          level         5
      index_codecs
        bytes
          endian        "little"
        crc32c
      index_location    "end"
  shard geometry
    shard shape         4x8
    inner chunk shape   4x4
    inner chunks        1x2 (2)
    index location      end
    index size          36 bytes
  storage transformers  none
  attributes
    _zarrs              {"description":"This array was created with zarrs","repo ... (118 bytes)
  extensions
    consolidated        absent

An array of a real store, with a blosc chain and an extensions block
that no convention appears in.

  $ zarr info ../../test/fixtures/tessera_band
  store                 ../../test/fixtures/tessera_band
  path                  /
  node type             array
  data type             int32
  shape                 128
  elements              128
  nominal size          512 B
  dimension names       band
  chunk grid            regular
  chunk shape           128
  grid shape            1
  chunk count           1
  chunk key encoding    default separator '/'
  fill value            0
  codecs
    bytes
      endian            "little"
    blosc
      typesize          4
      cname             "zstd"
      clevel            3
      shuffle           "shuffle"
      blocksize         0
  storage transformers  none
  attributes            none
  extensions
    consolidated        absent

The same array as JSON, whose attributes are not cut short.

  $ zarr info ../../test/fixtures/v3/array_zstd.zarr --json
  {
    "store": "../../test/fixtures/v3/array_zstd.zarr",
    "path": "/",
    "node_type": "array",
    "data_type": "float32",
    "shape": [
      10,
      10
    ],
    "elements": 100,
    "nominal_bytes": 400,
    "dimension_names": null,
    "chunk_grid": "regular",
    "chunk_shape": [
      5,
      5
    ],
    "grid_shape": [
      2,
      2
    ],
    "chunk_key_encoding": "v2",
    "fill_value": 0,
    "codecs": [
      {
        "name": "bytes",
        "configuration": {
          "endian": "little"
        }
      },
      {
        "name": "zstd",
        "configuration": {
          "level": 5,
          "checksum": false
        }
      }
    ],
    "codec_summary": "bytes(le) zstd(5)",
    "shard": null,
    "storage_transformers": [],
    "attributes": {
      "key": "value"
    },
    "extensions": {
      "conventions": [],
      "unknown_members": [],
      "non_core": [],
      "consolidated_metadata": null,
      "geoemb": null
    }
  }
