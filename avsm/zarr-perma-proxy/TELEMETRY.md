# Proxy telemetry

Telemetry lives in requests.sqlite3, separate from the authoritative cache
index. Both use SQLite user_version=1. Unsupported versions fail explicitly.
Future schema changes should migrate inside a transaction before serving.
Read-only reporting opens the same WAL databases without taking the cache
ownership lock.

## Events

The requests table contains one row for each completed Proffer backend event:

| Column | Meaning |
| --- | --- |
| id, time | Increasing row ID and Unix wall-clock seconds |
| method, path | GET, HEAD, OPTIONS or OTHER; encoded path without query |
| object_key | SHA-256 of mapped upstream URL, or NULL without a mapping |
| range_request | Whether the client sent Range, including invalid ranges |
| status, cache | HTTP status and HIT, MISS, SHARED or BYPASS |
| served_bytes | Transmitted body bytes, zero for HEAD |
| duration_us | Time from parsed request head to last response byte |

The downloads table contains one row per initiating upstream attempt:

| Column | Meaning |
| --- | --- |
| id, time, object_key | Event identity, completion time and URL identity |
| status | Final upstream HTTP status, or NULL before receiving headers |
| fetched_bytes | Payload consumed by the cache, including failed transfers |
| stored_bytes | Newly published blob bytes; zero for deduplicated content |
| duration_us | Active fetch time including publication, excluding slot wait |
| outcome | success, missing, limit, cancelled or error |

These are payload counters, not network wire counters. Headers, redirect
bodies, negative-response bodies and transport read-ahead are not counted.
Failed publication can leave newly stored orphan bytes for GC. Download time
excludes telemetry writes. Wall-clock adjustments can move events between
hourly buckets; durations use a monotonic clock.

Cache outcomes mean:

- HIT: an existing URL entry supplied the response, including negative hits.
- MISS: this request initiated a download and received its result.
- SHARED: this request awaited another request's download.
- BYPASS: no cache outcome was attached, for example OPTIONS, invalid input,
  an unsatisfiable range or a failed download.

Request events follow the backend's reporting contract. A process crash,
aborted response or failure before event emission may leave a download
without a request row. These are separate event streams, not a one-to-one
join. Group activity by object_key; do not multiply request and download
rows in a join.

## Aggregates and retention

request_hours groups by UTC hour, method, range flag, status and cache
outcome. It sums requests, served bytes and duration. download_hours groups
by UTC hour and outcome and sums downloads, fetched bytes, newly stored
bytes and duration. Each raw row and aggregate update commit atomically.

Raw tables are pruned at startup and every 1000 writes to approximately
30 days and at most one million rows each at the last pruning pass.
Hourly aggregates are retained indefinitely. Their row count grows with
hours and observed categories. SQLite reuses freed pages; pruning does not
shrink the database file. Export before pruning for long-term raw history.

The logger performs SQLite work through sqlite3-eio, serializing each
transaction with an Eio mutex. Calls await completion, so slow storage can
reduce throughput. There is no unbounded in-memory event queue. Telemetry
errors are reported on stderr and do not invalidate cache publication or
completed responses. These are operational counters, not an audit log.

No client addresses, request headers, cookies or query strings are logged.
Paths are capped at 4096 bytes. Paths themselves can identify datasets:
keep the cache directory private. The authoritative index separately holds
the exact upstream URLs needed for cache lookup.

## Dashboard metrics

The stats command provides totals and hourly arrays. Its rates use
classified cache responses as the denominator, including negative entries:

- Hit rate: HIT / (HIT + MISS + SHARED).
- Avoided-fetch rate: (HIT + SHARED) / (HIT + MISS + SHARED).
- Fetch-to-serve ratio: upstream payload consumed / downstream body bytes.
  Values above one expose initial whole-object overfetch.
- Deduplication: compare cache logical bytes with indexed blob bytes.
  Indexed blob bytes can include reclaimable orphans until GC.

A zero denominator yields JSON null. Fetched and served totals need not
cover identical requests: failures and HEAD can download without serving
body bytes. Filters on status and method help explain this.

Example hourly request chart:

```sql
SELECT datetime(hour, 'unixepoch') AS utc_hour,
       cache, sum(requests) AS requests,
       sum(served_bytes) AS bytes,
       sum(duration_us) * 1.0 / sum(requests) / 1000 AS mean_ms
FROM request_hours
GROUP BY hour, cache
ORDER BY hour, cache;
```

Example recent hot paths and range usage:

```sql
SELECT path, count(*) AS requests, sum(range_request) AS ranges,
       sum(served_bytes) AS bytes
FROM requests
GROUP BY path ORDER BY requests DESC LIMIT 30;
```

Percentiles require raw events or a future histogram aggregate; the hourly
sum cannot recover them. Plot upstream and downstream bytes separately,
then overlay hit rate and range share to assess whole-object caching.


## Live monitoring

The additive runtime table stores one replaceable row: server session,
wall-clock heartbeat, running flag, accepted connection count, queued
downloads and active transfers. Transfers contain a URL hash, a query-free
upstream path, consumed bytes, optional total length and monotonic age.
They are encoded as a JSON array in the transfers TEXT column. No client
addresses are stored.

The HTTP backend reports connection open/close transitions, including
idle keep-alive sockets and cleanup after cancellation or TLS failure.
Sockets still in the listen backlog are excluded. Download queues count
distinct URLs waiting for a slot; followers sharing a download are excluded.
Gauges are sampled once a second, so very short transfers may never appear.

The server creates the runtime table when opening an existing version-1
telemetry database; historical tables and their schema version are unchanged.
No cache rebuild is needed. Old databases without this additive table still
support snapshot reads, with live gauges unavailable.

The snapshot command reads lifetime totals, gauges and the newest 200 events
per table in one SQLite read transaction. It includes a monotonic sample
timestamp for rate deltas. zarrtop resets rates across server session changes,
expires gauges after five seconds without a heartbeat, and never infers
active connections from completed-request logs.

The terminal frontend follows the polling and scroller patterns in
bonsai_term_examples, using bonsai_term_components borders and scrolling.
A bounded, non-overlapping helper query keeps Eio/SQLite work separate from
the Async terminal scheduler. Queries time out after five seconds. Exit
kills and joins any helper, including one still being created. Raw events
and hourly aggregates are unchanged by monitoring.
