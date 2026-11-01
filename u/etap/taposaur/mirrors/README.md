# Publisher evidence

`taposaur.mirrors` queries configured DOI services and retains their responses
in a separate SQLite database. It does not fetch papers or change archives.
The public interface is [taposaur_mirrors.mli](taposaur_mirrors.mli).

## Selection

`lookup client store doi` first reads accepted evidence. Otherwise it tries
configured providers in order. Each response must contain the exact canonical
DOI and a nonblank publisher. Failed requests and rejected responses are
recorded. They do not become permanent cached misses. Calls have a deadline
(default 15 seconds) and a 2 MiB response limit. Transport retries belong to
the supplied Fetch client.

Providers are Crossref Glutton (`/service/lookup`), the legacy DataCite service
(`/lookup`), and the legacy ClickHouse `datacite.dois` table. ClickHouse uses
parameterized exact DOI queries over its DOI sort key, checking lower- and
uppercase forms. Its responses receive the same DOI validation as Glutton.
The old `openalex.works` mirror contains source IDs but no publisher names.
It is not a publisher provider. An OpenAlex source catalogue would be needed
to add that provider.

`import_csv store ~path bytes` retains an original CSV with `doi` and
`publisher` columns. Quoted commas, quotes, newlines and CRLF are supported.
Conflicting publisher values for a DOI are excluded. Import is atomic and
limited to 32 MiB. `fallback store doi` selects a CSV record only when there
is no previously accepted evidence. Provider and CSV evidence use the same
DOI, publisher, source, retrieval time and SHA-256 record.

Accepted evidence is stable across reruns. A future explicit refresh policy
can supersede evidence, but this API does not silently replace it. All
providers and CSV imports fill gaps. They do not validate existing publishers.

## Storage and audit

Use a local durable path, independent of the rebuildable catalogue:

```sh
export TAPOSAUR_MIRRORS_DB=/local/taposaur-mirrors.db
export TAPOSAUR_AUDIT_DB=/local/taposaur-audit.db
```

The default evidence path is `taposaur-mirrors.db`. The indexer, web server
and `review snapshot` read it when present. An explicitly configured missing
database is an error. Readers make no network requests. Configure readers
before starting a recovery batch so later indexing preserves supplements.

The database contains deduplicated raw `records`, per-provider `attempts`,
accepted `publishers`, and imported `csv_candidates`. It uses WAL and FULL
synchronization. New files have mode 0600. `find store doi` and
`record store ~sha256` expose accepted evidence and original bytes. For
operational diagnosis:

```sh
sqlite3 -readonly "$TAPOSAUR_MIRRORS_DB" \
  'SELECT provider,decision,count(*) FROM attempts GROUP BY provider,decision;'
taposaur audit query --action mirrors.publisher.lookup
```

Pass an audit session to both `client` and `open_write`. The batch CLI does
this automatically with actor `system`, or `TAPOSAUR_ACTOR`. Audit events
record lookup outcomes, evidence retention, CSV imports, catalogue fills,
and batch/snapshot publication. Raw response bodies remain in the evidence
database rather than the audit log. Endpoint URLs must not contain embedded
credentials, query parameters or fragments.

## Review recovery CLI

```sh
taposaur-publishers \
  --db /local/catalog.db --reviews-db /local/reviews.db \
  --mirrors-db "$TAPOSAUR_MIRRORS_DB" --snapshot 4 \
  --glutton http://localhost:8180 \
  --datacite-clickhouse http://localhost:8124 \
  --fallback-csv /inputs/fulltext-request-articles.csv --jobs 4
```

The CLI examines missing publishers in the parent snapshot. It skips papers
whose catalogue publisher is already present. Provider URLs are explicit.
Use `--limit 10` for a pilot that retains evidence and fills catalogue gaps
without publishing a snapshot. Rerun without the limit to resume from stored
evidence and publish a child snapshot with identical membership.

Catalogue updates fill only missing publishers and atomically revise cached
paper details. They preserve cache times, freshness flags and other fields.
A revision fence rejects any in-flight indexer result based on older data.
Indexing subsequently applies the durable evidence after archive resolution.
An existing archive publisher takes precedence over a supplement.

Papers outside the catalogue also receive publisher evidence in the new
snapshot. No catalogue holdings are fabricated. Snapshots retain publisher
provenance and remain immutable. PDFs, TEIs and source JSONL shards are not
modified.
