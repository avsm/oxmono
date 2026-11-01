# Operational audit

Taposaur keeps its operational audit in a dedicated local SQLite database.
It is independent of the rebuildable catalogue, immutable review snapshots,
and each paper's authoritative `events.jsonl`. The web server only queries it.
An audit event describes an attempt or observation. It never replaces domain
history or proves that a document's metadata is correct.

## Schema and queries

Schema/application version 1 contains one append-only `events` table:

| Column | Meaning |
| --- | --- |
| `id` | SQLite-generated monotonic event ID and pagination cursor |
| `v` | Event envelope version, currently 1 |
| `time` | UTC Unix timestamp, stored as fractional seconds |
| `run` | Random 128-bit correlation ID for a CLI invocation/session |
| `operation` | Random 128-bit ID pairing the start and terminal event |
| `parent` | Enclosing operation, when present |
| `actor` | Caller identity, default `system` |
| `component` | Origin, such as `taposaur.review` or `taposaur.consolidate` |
| `action` | Stable dotted action name |
| `subject` | Paper UID, review slug, run/snapshot ID or resource path |
| `phase` | `started`, `succeeded`, `failed`, `cancelled`, or `notice` |
| `details` | Bounded JSON object of explicitly selected string fields |

Indexes cover `(subject,id)`, `(actor,id)`, `(action,id)`, `(run,id)` and
`(phase,id)`. Operation/phase uniqueness and a partial unique terminal index
prevent duplicate terminal events. Triggers reject updates and deletes.
Queries bind all values, use exact filters and return newest IDs first.
The `before` cursor is exclusive. Limits are 1–500; the web page uses 100.
The incomplete filter finds starts without a terminal event. It includes
currently running operations, not just crashed processes.

`Taposaur_audit` exposes separate read/write handles, immutable sessions,
`with_operation`, `record`, `query` and JSONL encoding. Sessions carry actor,
component, run and parent context explicitly; no mutable global logger or
HTTP dependency is involved. A parent operation's child context is passed
into nested work. Publishers and staging may be sibling operations in a
run because the staging capability is opened before acquisition begins.

## Mutation and failure semantics

`with_operation` commits the start before calling the mutation. If this
write fails, the mutation does not start. It commits the terminal event
before reporting success. Expected result-valued failures use a success
predicate, so HTTP errors are not recorded as successful transfers.
Result details can retain generated revision, run or snapshot IDs.

SQLite uses WAL, `synchronous=FULL`, a five-second busy timeout and a mutex
per handle. Statements run through sqlite3-eio. Separate processes share
SQLite's writer lock. Writes are synchronous rather than queued in memory.
Cancellation is masked only while writing an event. New files are created
with mode 0600; SQLite creates its adjacent WAL/SHM files as needed.

The audit database and a paper filesystem or review database cannot commit
atomically together. A crash after the domain mutation but before the audit
terminal event leaves a durable start with an unknown outcome. Compare the
recorded UID, hashes, generated IDs and recovery paths with domain state.
Do not automatically mark such events failed or replay their mutations.
A terminal-write failure after mutation raises `Completion_unrecorded`.
When the mutation itself raises, a failure or cancellation event is attempted
without replacing the original exception. A failed audit write emits an
operation-ID-only diagnostic to stderr and leaves the start incomplete.
A `failed` event means the operation failed to return normally, not that
all earlier changes were rolled back.

Catalogue enrichment records one outcome per attempted paper after its
rebuildable cache publication, within a batch operation. Import records
changed/failed shards within a batch; unchanged shards are omitted. These
high-volume notices do not have separate per-row start events. A crash can
leave a cache update without its notice, while the batch remains incomplete.
No retention or automatic deletion is implemented. Plan disk space for
per-paper enrichment events; this is a durable log, not a ring buffer.

Append-only triggers prevent application accidents, not tampering by someone
who can replace the SQLite file or change its schema. Login is not yet
implemented. Actor names are caller assertions. Future authenticated web
mutations must construct their session from verified identity rather than
accepting an actor string from a form.

## Instrumentation

| Action family | Recorded operations |
| --- | --- |
| `cli.fetch`, `fetch.*` | Fetch attempt, metadata lookup, each transfer/retry, HTTP status or bounded failure category |
| `staging.*` | Per-paper locking/recovery scope, identity changes, artifact publication, file/hash/size, recorded source failure |
| `conversion.grobid` | Converter attempt, source hash, header consolidation mode, validation outcome |
| `archive.tei.replace` | Validated replacement intent and outcome, archive/recovery paths and new TEI hash |
| `consolidation.*` | Batch lifecycle, per-paper result and final counts |
| `cli.review.*`, `review.*` | Command attempt, schema opening, retained input hash, definition revision, run, snapshot, derived statistics, file export |
| `catalog.*` | Import/enrichment lifecycle, changed shards, per-paper metadata outcomes, freshness checks, shard retirement |
| `audit.initialize` | Explicit audit database initialization/validation |

Existing per-paper logs, originals and pending publication recovery are kept.
The logger does not introduce filesystem deletion or replacement APIs.
Library hooks are optional so embedding programs can supply their own audit
policy; the mutation-capable Taposaur CLIs install sessions by default.
Read-only search, rendering and audit queries do not generate audit noise.

No raw command lines, credential values, HTTP headers, URL query strings,
document bodies or exception messages are passed by the instrumentation.
Details are limited to 32 unique keys and 16 KiB. Credential-like field names
are refused as a second guard. Callers remain responsible for selecting safe
values. Failures retain a category; domain diagnostics remain in their
existing logs. This deliberately avoids copying potentially sensitive remote
responses into the cross-cutting audit database.

## Operating the log

Mutation commands use `TAPOSAUR_AUDIT_DB`, default `taposaur-audit.db` in the
working directory. Set an absolute path for all jobs to share one database.
`TAPOSAUR_ACTOR` defaults to `system`. Review `--by` defaults to `system` and
is retained as the actor of review publications when explicitly supplied.

```sh
export TAPOSAUR_AUDIT_DB=/absolute/local/taposaur-audit.db
# Schema creation is also automatic before the first audited mutation.
taposaur audit init

taposaur audit query --subject doi:10.1002/agj2.21016
taposaur audit query --actor system --phase failed
taposaur audit query --run RUN_ID --limit 100
taposaur audit query --incomplete
taposaur audit query --before 12345
```

Audit commands also accept `--audit-db FILE`. JSONL includes the complete
envelope and details. Queries do not create missing databases.

Start `taposaur-web --audit-db FILE` to expose `/audit` and
`/audit?format=jsonl`. The debug panel reports the database path and links
to the audit query; on paper pages it filters by paper UID. No web mutation
or audit-delete endpoints are installed. The log begins at deployment;
older binaries and external programs are not retroactively instrumented.

Use SQLite's backup API or a coordinated checkpoint/backup for a live log.
Copying the database file alone can omit committed WAL transactions.
