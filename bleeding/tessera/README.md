# tessera

OCaml client for [Tessera](https://geotessera.org) geospatial embeddings
over the Zarr V3 store, built on zarrz. The default dataset is
`v1.1-dclimate`, matching GeoTessera. See DESIGN.md. Projections use
the vendored geocaml/ocaml-proj over the system PROJ.

Queries take a WGS84 longitude and latitude in that order. Pixels come
back on the store's own UTM grid, never resampled, under the northern
EPSG code of the zone in both hemispheres, so a southern point has a
negative northing.

The `tessera` command reads the public store using fetch-httpz by default. Put
`--` before a negative coordinate, which the parser would otherwise take
for an option name.

    tessera info
    tessera probe --year 2024 -- -3.44 56.19
    tessera patch 0.0918 52.2109 --size 32 --year 2024 -o p.npy

    dune build @bleeding/tessera/all @bleeding/tessera/runtest

## Current Zarr stores

Sources are ordinary Zarr V3 stores over HTTP or local directories. Icechunk
repositories and legacy NPY tile sources are not supported. Existing NumPy
output for regions and patches is independent of the source format.

Published embedding prefixes are selected with `--depth N`, or by calling
`Tessera.of_store ~depth:N store`. `tessera info` lists available depths.
The reader opens the array named in `geoemb:depths`, so a small prefix reads
its own chunks instead of downloading and trimming a full vector. Stores
without this metadata expose their full embedding length only. Unavailable
prefixes fail with the available depths listed.

Groups marked `geotessera:mask_source = source_nodata`, including the
v1.1-dclimate publication, treat NaN scales as missing data. Probes can repair
them from a nearby finite pixel within `--search-px`. Older stores without
this marker retain the water interpretation and never repair water pixels.
Non-finite coordinates report `outside`.

## Persistent caching through the proxy

Run [zarr-perma-proxy](../../avsm/zarr-perma-proxy/README.md) in another
terminal:

    zarr-perma-proxy --cache-dir ./tessera-cache --port 9999
    tessera probe --store http://127.0.0.1:9999/tessera \
      --year 2024 -- 0.12 52.20

The proxy persists metadata, shard indexes, compressed inner chunks and
missing-object responses. Repeating a sampled workload with a fresh Tessera
process requires no upstream requests for those cached reads. Regions can
reuse the same fragments, and whole shards are assembled by fetching only
remaining gaps. Keep the cache between proxy restarts. The cache assumes
immutable URLs and has no automatic revalidation or eviction.

The read path follows [GeoTessera](https://github.com/ucam-eo/geotessera).
