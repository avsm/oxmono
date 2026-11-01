# zarr-perma-proxy

Permanent disk cache for immutable public HTTP objects, including Zarr V3
shards. A miss downloads the complete object with one upstream GET. Client
byte ranges are then served from disk. Concurrent misses for the same URL
share that download. Original bytes are never decoded or re-encoded.

## Run

    dune build --profile release-check u/avsm/zarr-perma-proxy/bin/main.exe
    dune exec --profile release-check -- zarr-perma-proxy serve --cache-dir ./tessera-cache --port 9999 --verbose

The default mapping is /tessera to
https://data.source.coop/tessera/tessera/zarr/v1.1-dclimate.
Use the proxy with existing readers:

    tessera info --store http://127.0.0.1:9999/tessera
    tessera probe --store http://127.0.0.1:9999/tessera --year 2024 -- 0.12 52.20

Python readers can use the same URL with GeoTesseraZarr. Repeat
`--map /prefix=https://host/store` for multiple stores. Explicit mappings
replace the default. The longest matching local prefix wins. Omitting
`serve` retains the same default invocation.

The server binds loopback. Put an authenticated reverse proxy in front if
remote access is needed. Client cookies and credentials are never forwarded.
Redirects must remain within a mapped upstream root, or a root explicitly
approved with `--allow-upstream URL`. Queries are rejected unless
`--allow-query` is set. That option creates a separate permanent entry for
every distinct query and stores the full URL in the private cache index.

## Responses and limits

GET supports a single explicit, open-ended or suffix byte range. Multi-range
requests return 400, and unsatisfiable ranges return 416. HEAD reports the
complete object size. **A cold HEAD or range request waits for the entire
object to download.** This trades initial latency and bandwidth for simpler
caching and local subsequent reads. No upstream HEAD or Range requests are
sent. Responses expose `X-Cache: HIT|MISS|SHARED` and
`X-Cache-Complete: true` for cached objects, with CORS headers.

| Option | Default | Meaning |
| --- | --- | --- |
| --max-object-bytes | 8589934592 | Maximum object size (8 GiB) |
| --reserve-bytes | 5368709120 | Free disk reserve (5 GiB) |
| --downloads | 4 | Maximum concurrent upstream downloads |
| --download-timeout | 1800 | Seconds per active download |
| --max-entries | 1000000 | Maximum URL entries, including missing objects |

The disk check reserves declared sizes against available space and other
downloads. A response without a declared length reserves the maximum object
size. Space limits return 503. The reserve is not a filesystem quota:
unrelated writers can still consume space. There is no automatic eviction.

URLs must be immutable. Hits never revalidate. 404 and 410 responses are also
cached permanently. To refresh a changed URL, stop the server and forget it:

    dune exec -- zarr-perma-proxy forget --cache-dir ./tessera-cache https://host/store/OBJECT
    dune exec -- zarr-perma-proxy gc --cache-dir ./tessera-cache

## Storage and recovery

Use a new, private directory for this version. Legacy sparse caches with JSON
manifests are rejected and are not migrated.

The directory contains:

- `index.sqlite3`: transactional URL mappings and blob lengths.
- `blobs/HH/HASH`: immutable SHA-256 objects shared by equal content.
- `tmp/`: unpublished downloads.
- `requests.sqlite3`: request and download telemetry.
- `.lock`: exclusive ownership across fibers and processes.

SQLite uses WAL and FULL synchronization. Downloads are size-checked,
hashed and synced before publication. The blob rename and its parent
directories are synced before the URL index transaction commits. A crash
can leave an orphan blob or temporary file, but cannot expose a partial
object through a committed URL. Cache hits check file type and length;
they do not rehash the entire blob on every read.

`gc` removes unreferenced blobs and recognized temporary files. It and
`forget` require exclusive ownership while the server is stopped. Reads and
downloads hold ownership for their entire lifetime, so GC cannot delete
files used by active readers. Restart the server after maintenance. GC
preserves telemetry and referenced blobs. A missing or truncated referenced
blob is an error: forget its URLs and collect it before retrying.

Keep storage on a local filesystem with reliable SQLite locking, atomic
rename and fsync support. Back up both databases and blobs while stopped,
or use a consistent filesystem snapshot.

## Measure effectiveness


Open the live terminal dashboard in another terminal:

    dune exec --profile release-check -- zarrtop --cache-dir ./tessera-cache

The dashboard uses Bonsai terminal components and polls every two seconds
(configurable with `--interval`, minimum 0.5 seconds). It shows open
connections, active and queued downloads, byte progress, hit rates,
request rates and traffic totals. Tab switches between recent requests,
recent downloads and active transfers. Use / to filter, Enter to finish
editing, Space to pause, j/k or arrow keys to scroll, and q or Ctrl-C to quit.

Filtering applies to the latest 200 completed events in the selected view.
Lifetime hit rates include negative cache hits. Rate counters use the time
between snapshots, while connection and transfer gauges update each second.
A stopped or unresponsive server is marked stale after five seconds and
its old gauges are hidden. Historical data remains available offline.

These commands can read a running proxy:

    dune exec -- zarr-perma-proxy stats --cache-dir ./tessera-cache
    dune exec -- zarr-perma-proxy export --cache-dir ./tessera-cache --kind requests --after 0 --limit 1000
    dune exec -- zarr-perma-proxy export --cache-dir ./tessera-cache --kind downloads --after 0 --limit 1000

`stats` emits JSON containing cache deduplication totals, lifetime telemetry
totals and hourly time series. Exports are JSON arrays ordered by row ID.
Continue with `--after` set to the last returned ID. Use these for later
plotting, or query SQLite directly. See [TELEMETRY.md](TELEMETRY.md) for the
schema, metric definitions, retention and example SQL.

## Verification

    dune build --profile release-check @u/avsm/zarr-perma-proxy/all
    dune build --profile release-check @u/avsm/zarr-perma-proxy/runtest --force

Tests cover range handling, cold HEAD, restart reuse, negative caching,
content deduplication, limits, concurrent misses, cancellation, redirect
restrictions, failed publication, offline GC, schema rejection and telemetry.
Loopback HTTP tests read real Zarr arrays with shard indexes at either end
and verify zero upstream requests after caching.
