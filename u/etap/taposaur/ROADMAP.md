# Taposaur roadmap

The agreed sequence is a library with a documented OCaml interface, a CLI,
then Proffer integration. Keep the existing archive format. Use Fetch for
HTTP and Eio for I/O. No Cap'n Proto layer is required.

See [TAPOSAUR.md](TAPOSAUR.md) for the 2026-11-03 agent handoff, operational
observations, uncommitted Duraml work and the next implementation sequence.

## 1. Read and inspect papers

Implemented. Core codecs, confined archive reads, index and event streams,
fixtures, inspection and sample validation are available.

## 2. Build a searchable catalogue

Implemented with SQLite FTS5, atomic shard replacement, resumable imports,
metadata enrichment, source checks and explicit retirement of missing shards.
Counters are maintained transactionally. Document errors are distinct from
complete metadata resolution. Cached projections survive failed refreshes.

The full-corpus search import is complete. Complete paper-detail caching is
running. Remaining: finish that backfill and measure storage and sustained
enrichment performance. `bench/` includes synthetic and production probes.

## 3. Deliver the read-only Proffer browser

Implemented. Overview, filtered search, paper metadata with per-field evidence,
PDF previews, constrained TEI rendering, artifact access and event history use
the shared libraries. Paper pages expose stale or unchecked search metadata.
Journal autocomplete preserves search filters. TEI and history panels retain
paper details, selected versions and reading positions.

## 4. Extract downloading and introduce controlled writes

The first CLI implementation provides:

- Explicitly configured publisher and metadata clients in `services/`.
- Identity lookup, publisher selection, OA and CORE fallback in `download/`.
- A dedicated staging writer in `store/`, with locks, bounded transfers,
  validation, SHA-256 deduplication, append-only events and recoverable
  manifest publication.
- `taposaur fetch IDENTIFIER --staging DIRECTORY`, or an explicit `--url`.

Cambridge routing, per-publisher pacing, busy retries, metadata deadlines and
verified CORE output fallback are implemented. Springer PDF entitlement is
opt-in. Remaining: live publisher acceptance tests, deployment-specific
egress routing, and acceptance/merge into the main archive.
The staging writer deliberately refuses existing papers created by other
writers. Its event projector handles its acquisition events, not arbitrary
legacy conversion or correction histories. Full legacy event replay belongs
with acceptance and migration.

## 5. Add independent conversion operations

Pub2TEI and GROBID HTTP clients are extracted, and conversion jobs run
independently through the job queue. Springer Nature's API envelope is
reduced to its JATS article before Pub2TEI by `Taposaur_document.Publisher_xml`.
Remaining: normalisation for other publisher envelopes as they appear.
Accept one input artifact and return validated output with provenance. Separate
permanent document refusals from transient service failures. Key reusable work
by input digest, route and converter/configuration version.

## 6. Compose ingestion and bulk processing

The generic [Duraml library](../../../wip/duraml/README.md) is implemented with
memory and SQLite backends, fenced claims, retries, cancellation, recovery,
atomic successors and a typed Eio client. Its 56 tests pass after the repository
rearrangement. The package remains uncommitted.

Taposaur integration is implemented in `jobs/`, `accept/` and `lock/`:
per-publisher acquisition queues, per-route conversion queues, catalogue
refresh successors, PID-bearing archive locks, no-clobber acceptance with
durable intents, an audit bridge, the `taposaur jobs` CLI and read-only
`/jobs` pages. One worker process owns each served queue on a host.
Remaining: a live pilot against configured publishers and converters, a
cross-host rate-limit permit if several hosts must serve one publisher,
retention of settled jobs and bulk submission of the soil-carbon review.

## 7. Build reviews and migrate

The first review implementation stores request identity, versioned query
definitions, recorded inputs, fixed run membership and immutable metadata
snapshots in a separate SQLite database. CLI imports and local queries produce
cached DOI, CSV and JSONL exports. The read-only browser exposes request
history, source evidence, snapshots and comparisons. See the review section
of `README.md` for the definition format and commands.

Remaining: external search execution, interactive editing, access controls,
persistent acquisition jobs, human screening decisions and complete legacy
migration. Preserve old site review metadata and unsuccessful attempts, which
cannot be reconstructed from archive files. Compare coverage and histories
alongside production before transferring archive writing operations.
