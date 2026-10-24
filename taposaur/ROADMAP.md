# Taposaur roadmap

The agreed sequence is a library with a documented OCaml interface, a CLI,
then Proffer integration. Keep the existing archive format. Use Fetch for
HTTP and Eio for I/O. No Cap'n Proto layer is required.

## 1. Read and inspect papers

Implemented. Core codecs, confined archive reads, index and event streams,
fixtures, inspection and sample validation are available.

## 2. Build a searchable catalogue

Implemented with SQLite FTS5, atomic shard replacement, resumable imports,
metadata enrichment, source checks and explicit retirement of missing shards.
Counters are maintained transactionally. Document errors are distinct from
complete metadata resolution. Cached projections survive failed refreshes.

Remaining: full-corpus import and measurement of disk size, import throughput,
search latency and enrichment cost. The benchmark in `bench/` exercises
synthetic rows. Production samples do not establish full-corpus performance.

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

Pub2TEI and GROBID HTTP clients are extracted. Remaining: publisher XML
normalisation and independent conversion jobs.
Accept one input artifact and return validated output with provenance. Separate
permanent document refusals from transient service failures. Key reusable work
by input digest, route and converter/configuration version.

## 6. Compose ingestion and bulk processing

Pending. Persist per-paper jobs, bounded Eio workers, attempts, retry deadlines,
cancellation and recovery. Compose identify, plan, fetch, convert, inspect,
accept and publish. Storage shards must not dictate scheduling. Publication
must leave durable pending catalogue updates. Expose jobs through the website
after the CLI works.

## 7. Build reviews and migrate

Pending. Keep review membership, holdings, acquisition attempts and human
decisions distinct. Store review and job state separately from the rebuildable
catalogue. Export existing site review metadata and unsuccessful attempts,
which cannot be reconstructed from archive files. Compare coverage and
histories alongside production before transferring writing operations.
