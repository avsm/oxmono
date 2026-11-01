# zarr-perma-proxy

Permanent disk cache for public HTTP objects, including Zarr V3 shards.
Uses `fetch-httpz` upstream and `proffer-httpz` for serving. This refresh of
the former proxy uses the current HTTP libraries and persists partial
objects across requests and restarts. The cache and shard handling are
generic. Tessera appears only in the default CLI mapping and examples.

## Tessera

    dune build --profile release-check avsm/zarr-perma-proxy/bin/main.exe
    dune exec --profile release-check -- zarr-perma-proxy \
      --cache-dir ./tessera-cache --port 9999 --verbose

The default mapping is `/tessera` to the `v1.1-dclimate` Zarr store.
Keep this process running while reading through it:

    tessera info --store http://127.0.0.1:9999/tessera
    tessera probe --store http://127.0.0.1:9999/tessera \
      --year 2024 -- 0.12 52.20

Python readers use the same URL:

```python
from geotessera import GeoTesseraZarr

gt = GeoTesseraZarr("http://127.0.0.1:9999/tessera")
X = gt.sample_points([(0.12, 52.20), (-2.97, 53.44)], year=2024)
```

Repeat the queries with a fresh reader or after restarting the proxy. Cached
metadata, HEAD information, indexes, chunks and missing-object responses
require no upstream requests. No Icechunk or NPY source support is needed.
Use `--map /other=https://host/store` to replace the default mapping. Repeat
`--map` for several roots. The longest matching path prefix wins.

## Partial objects

Each URL has an atomic JSON manifest and a sparse data file. The manifest
records the object size, validators and sorted, merged coverage intervals.
`Perma_proxy.Manifest.jsont` defines the versioned on-disk format. It
validates exact nonnegative integer offsets, safe generation IDs, and sorted,
coalesced coverage within the object. Invalid manifests are rejected rather
than repaired. Manifests without a version are read as version 1.
File size alone does not imply completeness. Up to 1024 manifests are kept
in memory to avoid parsing JSON for every small repeated range. Downloads use identity content
coding and validate Content-Range, declared lengths and validators. Only
successfully received and synced bytes gain coverage. Interrupted transfers
leave their bytes uncovered and can be retried.

Explicit, open-ended and suffix ranges are supported. Only gaps in a requested
interval are fetched. HEAD uses the saved length without reading the body.
Concurrent requests for an object serialize, so overlapping misses do not
fetch the same bytes twice. Full GETs fill the remaining gaps with requests
of at most 8 MiB, then stream the assembled object from disk using 64 KiB
buffers. An origin that ignores Range may send the whole object, which is
cached and sliced correctly. Initial whole-object GETs stream to disk.

For arrays with the sole outer codec `sharding_indexed`, cached `zarr.json`
or root consolidated metadata describes the shard geometry. The proxy reads
the index at its declared start or end, decodes its configured codecs with
zarrz, checks checksums and offsets, and caches complete encoded inner chunks
overlapping a requested fragment. Adjacent intervals merge. The response
still contains exactly the requested bytes. Index decoding is reused for up
to 64 recently encountered shard generations. Other layouts and data queried
with URL parameters use ordinary range caching.

Data is never decoded or re-encoded by the proxy. Absent inner chunks remain
absent in the original index. The full shard becomes complete only when every
physical byte, including padding, is present. Whole reads fetch any padding
or other gaps instead of inventing bytes.

Responses expose `X-Cache: HIT|MISS` and `X-Cache-Complete: true|false`, with
CORS headers for browser range readers. Multi-range requests return 400.
The server binds loopback. Upstream mappings grant access to public read-only
objects. Client cookies and authorization headers are not forwarded.

## Lifetime

This is a permanent cache for immutable published URLs. Hits do not send
conditional requests or revalidate the origin. 404 and 410 responses are also
permanent, so repeated reads of missing shards stay local. Changing an object
in place or publishing new shards at the same URL requires a fresh cache.
Stop the server before clearing the cache directory. Do not share its cache
with another server process. The command takes a directory lock. Files, network requests, random
generation IDs and fiber synchronization use Eio. The lock uses a
nonblocking POSIX call on an Eio-owned descriptor because Eio has no
file-lock API.

On a miss, a changed entity tag, modification time, length or failed
precondition invalidates the old manifest and starts a new data generation.
Existing readers retain the old generation. Old generations and interrupted
transfers can leave unused files. There is no eviction or automatic cleanup.
Coverage manifests are synced and atomically replaced after data is synced.
Corrupt manifests and truncated data files are reported as errors.

## Verification

    dune runtest --force --profile release-check avsm/zarr-perma-proxy

Tests cover interval algebra, overlapping misses, restart reuse, permanent
negative entries, revision changes, failed downloads, concurrent requests,
shard index checksums, inner-chunk expansion and full assembly. A loopback
HTTP test reads a Zarr array through fetch-httpz and checks zero upstream
requests once its fragments are cached.

The shard layout follows the [Zarr sharding specification](https://zarr-specs.readthedocs.io/en/latest/v3/codecs/sharding-indexed/).
