What the arrays below a node cost. A filesystem store lists its keys, so
the stored sizes here are exact.

  $ zarr stats ../../test/fixtures/sharded_array_write_read.zarr
  node          elements  nominal  chunks  inner  objects  stored  ratio
  /group/array        64    128 B       2      2        2   280 B  2.19x
  total               64    128 B       2      -        2   280 B  2.19x
  Stored sizes are exact, from a walk of the store's keys.

The same array on its own.

  $ zarr stats ../../test/fixtures/sharded_array_write_read.zarr /group/array
  node          elements  nominal  chunks  inner  objects  stored  ratio
  /group/array        64    128 B       2      2        2   280 B  2.19x
  total               64    128 B       2      -        2   280 B  2.19x
  Stored sizes are exact, from a walk of the store's keys.

A hierarchy whose arrays have no chunk stored at all.

  $ zarr stats ../../test/fixtures/hierarchy.zarr
  node    elements    nominal  chunks  inner  objects  stored  ratio
  /a/baz  10000000   76.3 MiB     100      -        0     0 B  0.00x
  /a/foo  10000000   76.3 MiB     100      -        0     0 B  0.00x
  total   20000000  152.6 MiB     200      -        0     0 B  0.00x
  Stored sizes are exact, from a walk of the store's keys.

Sampling is for a store that cannot list its keys, so asking for it here
says so and measures exactly instead.

  $ zarr stats ../../test/fixtures/sharded_array_write_read.zarr --sample 4
  node          elements  nominal  chunks  inner  objects  stored  ratio
  /group/array        64    128 B       2      2        2   280 B  2.19x
  total               64    128 B       2      -        2   280 B  2.19x
  Stored sizes are exact, from a walk of the store's keys.
  The store lists its keys, so --sample was not used.

A group with no array below it.

  $ zarr stats ../../test/fixtures/hierarchy.zarr /b
  No array below this path.

  $ zarr stats ../../test/fixtures/sharded_array_write_read.zarr --json
  {
    "store": "../../test/fixtures/sharded_array_write_read.zarr",
    "path": "/",
    "discovery": "listing",
    "tier": "listing",
    "sample": 0,
    "arrays": [
      {
        "path": "/group/array",
        "elements": 64,
        "nominal_bytes": 128,
        "chunks": 2,
        "inner_chunks_per_shard": 2,
        "objects": 2,
        "stored_bytes": 280,
        "sampled": null,
        "found": null,
        "estimated_bytes": null
      }
    ],
    "total": {
      "path": "total",
      "elements": 64,
      "nominal_bytes": 128,
      "chunks": 2,
      "inner_chunks_per_shard": null,
      "objects": 2,
      "stored_bytes": 280,
      "sampled": null,
      "found": null,
      "estimated_bytes": null
    }
  }
