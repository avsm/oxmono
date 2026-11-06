# Changes

- Add Eio resource queues with memory and SQLite backends, fenced claims,
  atomic successors, lifecycle events, worker pools and scripted mocks.
- Use abstract snapshots and validated completions, Ptime timestamps and spans,
  exact schema-v2 timestamp storage, and reference manuals for each interface.
- Add typed client queues, switch-owned workers, atomic staged successors,
  typed result handles and optional Jsont codecs.
- Open SQLite stores read-only for inspection, refusing every mutation.
- Count matching jobs without reading their records.
- Record the parent of every successor and filter listings by parent.
- Expose the running job and attempt to handler fibers for correlation.
- Read the newest events first with a descending tail and cursor.
- Dispatch oldest-first within fair-share lanes, so retries and expired
  claims keep their age and small submissions interleave with bulk ones.
  Schema 3 upgrades version 2 stores in place.
- Keep worker pools running through handler exceptions and storage failures
  when an `on_exn` completion is supplied.
