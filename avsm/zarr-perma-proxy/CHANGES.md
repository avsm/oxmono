# Changes

- Add zarrtop, a Bonsai terminal dashboard with live connection/download gauges,
  traffic and hit rates, scrollable recent events, filters and pause controls.
- Publish expiring heartbeat snapshots and expose consistent read-only queries.

- Fetch complete upstream objects and serve client ranges from immutable
  SHA-256 blobs, with a transactional SQLite URL index and deduplication.
- Bound object size, disk reservations, concurrent downloads and URL entries.
  Share concurrent misses, restrict redirects and reject queries by default.
- Add offline URL invalidation and coordinated garbage collection.
- Record request and download telemetry in SQLite with retained hourly
  aggregates, hit rates, byte counters and JSON reporting/export commands.
- Require a fresh cache directory instead of legacy sparse JSON caches.
