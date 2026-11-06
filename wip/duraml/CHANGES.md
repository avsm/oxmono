# Changes

- Read only a queue's jobs, or a queue and state's, when the memory backend
  lists them.
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
- Rank a lane that gains work at the back of its queue's rotation, forget
  emptied lanes, and claim with one indexed query in schema 4 SQLite
  stores, upgrading versions 2 and 3 in place.
- Answer idle polls without the SQLite write lock or fresh randomness, and
  wake memory waiters on renewal only when a lease shortens.
- Report damaged SQLite records and SQLite errors, with their text, as
  storage failures, and close a handle whose transaction cannot roll back.
- Raise settlement conflicts as `Conflict` errors, cancel a handler whose
  renewal outlasts the lease, release claims taken as a pool stops, and
  check schemas before the attempt budget so mismatched work is kept.
- Reject conflicting successors that share a key, oversized leases on idle
  queues and non-canonical job IDs. Add `Id.equal` and `Id.pp`.
- Store each payload once in schema 5 SQLite stores, keeping job and
  attempt records without it, so renewals no longer rewrite payloads, and
  upgrade versions 2 to 4 in place.
- End listing pages after 16 MiB of payloads and results.
- Refuse SQLite database paths that are symbolic links, and keep prepared
  statements per handle.
- Reject names made only of Unicode white space and an explicit empty lane,
  and count memory jobs from per-queue totals.
