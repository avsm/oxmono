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

## 8. Export corpora from the website as Duraml jobs

Goal: an "Export corpus" action on a snapshot page submits a durable job,
a worker runs `Taposaur_corpus.export`, converts the rows to Parquet,
publishes the file immutably, and the snapshot page lists exports with
their progress and a download. Today this is a shell pipeline
(`taposaur review corpus` then `corpus-parquet.sh` with duckdb) started by
hand. The `/docs/corpus` page documents the row schema that must not
change.

Already in place and reusable as is: the export itself (rows, provenance,
venues, body text, bounded fibers on an executor pool, output budget) and
its JSON Lines encoding; Duraml stores, typed queues, worker pools with
lease renewal, keyed deduplication, lanes, successors and the audit
bridge; the `/jobs`, `/job` and `/activity` pages; the review store's
immutable snapshots, blob parts and manifest digests behind a typed write
handle; read access to the evidence store, journal links and live
catalogue; Proffer POST routes and forms; the ranged streaming response
used for PDFs.

Missing, largest first:

1. A Parquet writer in OCaml. The switch has none (only `decompress`).
   The schema needs Thrift compact metadata, PLAIN pages for
   BYTE_ARRAY, INT32, INT64 and BOOLEAN, RLE/bit-packed definition and
   repetition levels for nullable and nested columns (the authors LIST,
   the sources MAP, the provenance and acquisition STRUCTs), the
   TIMESTAMP(MILLIS, UTC) logical type, page headers and the footer.
   GZIP comes from `decompress`; ZSTD needs a small binding to the
   libzstd already on the host. Roughly 1,500 to 2,500 lines with tests
   that read the output back through duckdb. Until it exists, the job
   can call duckdb as an external converter the way GROBID and Pub2TEI
   are external services, behind one `Taposaur_parquet` interface with
   the converter version recorded on the export, so nothing above it
   changes when the native writer lands.
2. An export job family in `Taposaur_jobs`: request (snapshot, live,
   evidence, journal-links digest, format, revision, requested_by),
   result (path, SHA-256, bytes, summary), queue `review.corpus`, key
   `corpus:SNAPSHOT:OPTIONS:REVISION` so a repeat returns the existing
   export. The handler writes JSON Lines to local staging, converts,
   fsyncs, renames into an exports root (`--exports-root`), and records a
   descriptor in the review store (schema 4, `corpus_exports`: snapshot,
   key, path, SHA-256, bytes, options, summary, created, job). It never
   overwrites. One worker owns the queue per host (`own_queue`,
   concurrency 1, `--domains`, posix backend, a 15-minute lease renewed by
   `Duraml_worker`). Progress is visible through audit records every
   10,000 rows and the job's running state; cancellation uses the
   worker's stop promise. An optional successor copies the file to the
   CephFS review directory.
3. A write path on the website. The browser "submits and cancels
   nothing" and opens every database read-only. The smallest change that
   keeps that legible: `create` takes an optional `submit_export`
   capability, built by the executable from a second, writable Duraml
   handle only when `--submit-exports` is given, used by exactly one POST
   route, `/review-corpus`. The submission is audited through the
   existing Duraml-to-audit bridge; the actor comes from a form field
   because the site has no accounts and sits behind loopback or an
   authenticated proxy. The alternative, a CLI-only submission with the
   site showing progress, keeps the invariant intact but is not what was
   asked for.
4. Serving a 2 GB file. `Attachment` builds the body in memory; the
   download must use the PDF streaming path (ranges, HEAD, immutable
   cache headers, SHA-256 as ETag), reading only paths named by
   descriptors.
5. Snapshot page: an exports table (options, rows, texts, size, created,
   download, job link) and the submission form (live catalogue,
   evidence, journal links, actor), redirecting to `/job?id=`.
6. Policy: an output budget per export, a free-space check before
   starting, local-disk staging, and no retention rule for published
   snapshot files.
7. Tests: the handler on the fixture review with the memory Duraml
   backend; a web test posting the form and streaming the result; writer
   tests reading output back through duckdb when it is installed.

Order: A, the job family, worker, descriptor, streaming download and
exports table, with duckdb behind the converter interface and submission
from `taposaur review corpus --submit`; B, the website POST route and
form; C, the native Parquet writer, removing the duckdb dependency.

