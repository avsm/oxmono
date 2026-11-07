# Taposaur agent handoff

Updated 2026-11-03. This is the continuation plan for the publisher acquisition
queue work. Read [AGENTS.md](../../../AGENTS.md), [ROADMAP.md](ROADMAP.md),
[ARCHITECTURE.md](ARCHITECTURE.md) and the relevant public interfaces before
editing. Distinguish implemented library support from deployed workflows.

## Resume here

The Duraml integration is implemented and tested but has not been run against
live publishers, converters or the production archive. Next: a pilot on one
or two selected soil-carbon DOIs with `taposaur jobs submit --doi`, inspection
of the resulting archive files, catalogue rows and audit records, then bulk
submission of the snapshot. Do not implement a second job queue in Taposaur.

The user approved a generic Eio queue first, followed by Taposaur integration.
Both exist now. New libraries are `taposaur.lock`, `taposaur.accept` and
`taposaur.jobs`, with the `taposaur jobs` CLI group and read-only `/jobs`
pages. See the README section "Acquire and convert review papers with
durable jobs" and ARCHITECTURE.md for the boundaries and invariants.

The initial integration should support the soil organic carbon review,
including the acquisition request associated with Robert Powell. Use actual
review membership and configured publisher capabilities when selecting work.
Do not treat an implemented strategy as proof of credentials, entitlement or
availability for a particular DOI. `taposaur jobs plan` reports held,
unsupported, unconfigured and invalid members separately for this reason.

## Repository and working tree

- Branch: `minus39`.
- Fetched and fast-forwarded to `origin/minus39` at `11a772b54` on 2026-11-03.
  No local merge commit was needed. Nothing was pushed.
- Shared libraries moved from `bleeding/` to `wip/`. Duraml is now
  [wip/duraml](../../../wip/duraml/README.md).
- Taposaur moved from `taposaur/` to `u/etap/taposaur/`.
- Duraml remains entirely uncommitted. The working tree also contains its root
  README link and a root `.gitignore` addition for `/taposaur/.local/`.
  Preserve this work. Do not assume an untracked package is disposable.
- The temporary merge stash was removed after checking that all 33 package
  files were restored. A backup remains at
  `/tmp/duraml-before-minus39-20261103.tar.gz`, with the old source paths.
  The current working tree is authoritative, not that backup.
- The merge did not move operational databases or stop/restart services.
  The old runtime directory is still ignored. Do not stage its contents.

## User requirements and decisions

1. Keep application logic in OCaml, with Eio I/O, Fetch HTTP and explicit
   capabilities. No new Python orchestration layer.
2. Batch papers by publisher and process them through existing strategies,
   respecting publisher rate limits and logging attempts and outcomes.
3. Validate downloaded PDF/XML before publication. Convert valid inputs to
   TEI concurrently with other downloads, with separate conversion capacity.
4. Publish reliably into the central paper filesystem. Never overwrite
   existing artifacts. Permit replacement only for explicitly defined mutable
   metadata, with recoverable publication and retained provenance.
5. Hold a filesystem lock for each paper directory during updates. The user
   requested PID-bearing lock information. Queue leases are not file locks.
6. Preserve immutable review definitions, runs and snapshots. Refreshing
   metadata produces new snapshots rather than rewriting delivered history.
7. Keep the web UI read-only initially. Agents and CLIs can submit/import work.
   Authentication and interactive mutations are later work. Audit actor
   defaults to `system`.
8. Keep API representations abstract, use library time types, and document
   architecture, invariants and usage in `.mli` files in reference-manual style.

## Duraml: implemented

See [the package guide](../../../wip/duraml/README.md). Libraries are:

| Library | Responsibility |
| --- | --- |
| `duraml` | Eio resource protocol and shared transition engine |
| `duraml.memory` | Transactional ephemeral backend |
| `duraml.sqlite` | Local durable backend with process-safe claims |
| `duraml.worker` | Explicit-claim worker pool and lease renewal |
| `duraml.client` | Typed queue capabilities and structured workers |
| `duraml.jsont` | Optional application JSON codecs |
| `duraml.mock` | Typed scripted providers and request fault injection |

The core supports keyed deduplication, delayed work, fenced claims, renewal,
explicit retries, cancellation, atomic successors and transactional lifecycle
events. Settlements retain receipts for idempotent response reconciliation.
Workers support bounded concurrency, attempt limits and graceful drain.
Delivery is at least once. External effects still need idempotent publication.

Specifications, completions, job snapshots, queries, events and wait tokens
have abstract representations. Times are `Ptime.t`, durations `Ptime.Span.t`.
Eio wall clocks are explicit backend inputs, while worker intervals use an
explicit monotonic clock. SQLite schema 2 preserves timestamp precision in
records and its eligibility index. Experimental schema 1 databases are refused
without migration. Use a local disk for the queue database, not Ceph or NFS.

The review fixed two regressions: a shortened memory-backend lease now wakes
consumers, and invalid final-attempt retries are no longer silently replaced
by an attempt-limit result. Core completions now validate at construction.
Clock-boundary conversion rounds deadlines upward to avoid immediate-wakeup
loops when a timestamp is finer than the clock's floating-point resolution.

### Preferred application API

Read [duraml_client.mli](../../../wip/duraml/client/duraml_client.mli).
Use `Duraml_client`, not the core provider protocol, for Taposaur handlers.

- `Codec.v` supplies deterministic encode/decode functions without JSON coupling.
  `Duraml_jsont.codec` adapts a Jsont codec when needed.
- `Queue.v` binds store, name, kind, version and request/result codecs once.
- `Queue.submit` returns `Added job`, `Existing job` or `Conflict id`.
- `Job.id`, `inspect`, `snapshot` and `cancel` provide typed job access.
  `Queue.find` restores a typed handle after reopening the store.
- `Queue.run` blocks in the caller. `Queue.start ~sw` forks into an explicit
  switch. Handlers receive decoded requests and their own child switch.
- `Completion.succeed`, `retry` and `fail` express handler outcomes.
- `Queue.stage` prepares encoded successor work without publishing it.
  Attach stages through `~followups` to success or permanent failure for
  atomic publication in the same store. Ordinary `Queue.submit` inside a
  handler is a separate transaction, not an atomic successor.

One queue name must have one kind/version among active workers. Binding is
local, not a durable schema registry. An incompatible stored specification
raises `Queue.Schema_mismatch` and leaves the claim recoverable by expiry.
A matching-schema decode error records `invalid_payload` without running the
handler or storing potentially sensitive decoder diagnostics. Codec exceptions
stop the pool. Use new queue names when incompatible versions coexist.

Duraml does not provide publisher rate-limit permits, distributed concurrency
quotas, artifact validation, file publication, automatic retention or a durable
cross-database audit transaction. Worker concurrency is per runner invocation.
`on_settled` is transient notification, not reliable audit delivery. Consume
Duraml events using store identity plus sequence for deduplication.

## Existing Taposaur capabilities to reuse

- `catalog`, `indexing`, `paper` and `reader`: resumable shard import, complete
  resolved-detail caching, evidence selection, stale-source detection and
  guarded cache publication. Directory-ordered enrichment reduces Ceph
  metadata access costs. Do not return to full archive resolution per page.
- `review`, `review-store`, `review-run`: versioned definitions, retained input
  evidence, fixed membership, immutable snapshots and cached DOI/CSV/JSONL
  exports. Snapshot statistics and publisher/presence facets support indexed
  paper browsing. Existing UI has sortable tables, coverage bars, linked
  counts and clickable paper titles, with irrelevant zero bars suppressed.
- `ui`, `web`, `document`: shared compact widgets, journal autocomplete,
  retained paper details alongside TEI/history panels, formatted references
  and DOI links. OpenAlex journal links use supplied cached mappings.
- Debug UI reports configured storage paths and publisher capabilities.
  Configured keys must remain secret and must not be copied into the plan,
  jobs, audit details, logs or commits.
- `services`: publisher, metadata, GROBID and Pub2TEI clients. Mirror
  configuration is explicit. Inspect current service interfaces rather than
  duplicating the legacy stack's clients.
- [download interface](download/taposaur_download.mli): single-paper identity
  lookup, publisher routing, OA/CORE fallbacks, bounded transfers and PDF/XML
  validation. Empty `artifacts` means failure even if a function returns
  normally. Returned `attempts` retain expected remote failures.
- [store interface](store/taposaur_store.mli): dedicated local staging tree,
  per-paper advisory locks, hash deduplication, retained events and durable
  publication intents. It refuses papers owned by other writers and does not
  accept staging into the main archive. Current locking uses `.lock` and
  `flock`; PID-bearing owner metadata is not provided by this interface.
- [audit](audit/README.md): separate SQLite audit with explicit sessions,
  correlation, default actor `system`, CLI instrumentation and read-only
  querying. Durable starts and terminal records bracket mutations. An audit
  failure after a domain mutation can leave an unknown outcome to reconcile.
- [mirrors](mirrors/README.md): reusable exact-DOI publisher lookup, retained
  responses and audit. Crossref Glutton and DataCite/ClickHouse can fill gaps.
  The old OpenAlex works table alone does not supply publisher names.
- `taposaur-consolidate`: existing soil-review GROBID header regeneration,
  strict DOI/publisher/provenance validation, retained old files, archive
  publication and catalogue refresh. Its current operation requires other
  archive writers to be stopped. It is not yet a generic concurrent converter
  worker and must not silently bypass the new publication locking protocol.

## Operational state and locations

These are observations and historical configuration, not promises that a
service is currently healthy. Recheck before restarting or scheduling work.

| Item | Location or observation |
| --- | --- |
| Source | `u/etap/taposaur/` |
| Existing runtime state | `/home/avsm2/src/git/avsm/oxmono/taposaur/.local/` |
| Catalogue | Runtime directory's `catalog.db` |
| Reviews | Runtime directory's `reviews.db` |
| Index status | Runtime directory's `full-index.status.json` |
| Consolidation recovery | Runtime directory's `soil-header-consolidation/` |
| Archive default / legacy setup | `/data/papers/taposaur` |
| Legacy Docker stack | `/data/papers/taposaur/stack` |
| Legacy ClickHouse location supplied by user | `/eeg/taposaur/site` |
| Review inputs supplied by user | Relevant subdirectories under `/eeg/avsm2/` |
| Requested local browser | `http://localhost:8766` |

At status timestamp **2026-11-03 09:32:07 UTC**, `full-index.status.json`
reported `running`, PID 801031, 32 workers, directory order, 1,038,500 papers
examined in that run and **3,088,679 details cached**. The two counters are
not the same measure. Read the status file again for current progress.
Systemd service status could not be queried through the sandbox's bus access.
The website and mirror endpoints were not health-checked during this handoff.

The soil-header status file was last modified **2026-10-31 20:18:19 UTC**.
It reports total/examined 16,743, updated 2,144, skipped 14,535, failed 63 and
resumed 1. Preserve these original counters rather than assuming that resumed
work makes them a disjoint partition. Inspect retained per-paper results
before retrying. Do not delete conversion errors or invalid TEI responses:
those are regeneration evidence.

Earlier work reported 54 unresolved shard/path-identifier failures. That count
was not re-queried in this handoff. Inspect current import diagnostics and DOI
normalization before proposing repairs. Do not silently normalize away
ambiguous identifiers or mark the entire detail backfill finished.

Audit and mirror database paths can be explicit environment/CLI settings or
relative defaults. Check actual process/service configuration and the debug
pane, without printing secrets. Repository path rewrites are not host data
migrations. In particular, legacy `/data/papers/taposaur` paths do not become
`/data/papers/u/etap/taposaur` because source code moved.

## Implemented on 2026-11-03

### Jobs library and CLI

`jobs/taposaur_jobs.mli` defines versioned Jsont payloads for acquisition,
conversion and catalogue refresh, keyed `acquire:UID:STRATEGY:vREVISION`,
`convert:UID:ROUTE:INPUT_SHA256:CONVERTER` and `refresh:UID:MANIFEST_SHA256`.
Queues are `acquire.<strategy>` per `Taposaur_download.strategies` entry,
`convert.grobid`, `convert.pub2tei` and `catalogue.refresh`. Payloads carry
review attribution, requester and publisher evidence, never credentials.

`Plan.review` reads a frozen snapshot and current archive manifests and
returns one decision per member. `Submit.entries` submits with semantic keys.
`taposaur jobs plan|submit|inspect|show|cancel|audit-sync|work` compose the
library with environment-configured clients from `bin/clients.ml`, which the
`fetch` command now shares.

### Rate limiting and conversion

`taposaur jobs work` takes a lock file beside the queue database for each
served queue, so one process owns a publisher queue per host. Pacing is the
existing per-client middleware. Workers build the downloader with
`busy_retries = 0`, and `Taposaur_download.attempt` now carries
`retry_after`, so busy responses become Duraml retries at the server's delay.
Conversions run in separate pools with their own leases, validate TEI
well-formedness and header DOI, record permanent refusals in the manifest
through `Taposaur_accept.record_failure` and retry only service failures.

### Archive acceptance and recovery

`accept/taposaur_accept.mli` specifies the only writer beneath an archive
root: the paper directory `.lock` held through `Taposaur_lock` with PID,
host and component metadata, no overwriting of existing files, allowlisted
metadata fills, append-before-manifest ordering and a durable intent that
reopening completes. The staging writer and `taposaur-consolidate` take the
same lock. `refresh_index` rewrites the paper's index row under a shard lock,
and the refresh job then imports the shard and resolves metadata. Review
snapshots remain immutable; take a new snapshot after a run.

### Audit and website

`Audit_bridge` copies Duraml events into the audit as `queue.event` notices
with a cursor file and deduplication by store identity and sequence, also
consulting the newest audit records after a lost cursor. `Duraml_sqlite.open_`
gained `~read_only:true`, which `taposaur-web --jobs-db` uses for `/jobs`.

### Review findings addressed

Two reviews of the implementation led to these changes: conversion and
refresh payloads equal their keys, so successors from different reviews
reconcile instead of stopping the pool; handlers classify archive, staging,
I/O and size failures into job outcomes while audit failures still stop the
pool; replayed acquisitions and conversions reuse held artifacts; retry
delays are clamped; converter 429, 502 and 504 retry; acceptance refuses
reserved names, links final files into place, checks the log ends in a
newline, uses 0755/0644 modes and runs its lock calls off the scheduler;
the lock refuses symbolic links; consolidation refuses a pending acceptance
intent; queue counts use SQL; the website and CLI share one job description;
and `taposaur jobs work` records success only after draining.

### Soil-carbon plan on 2026-11-03

`taposaur jobs init --queue-db taposaur/.local/jobs.db` created the queue
(identity `68357235eacbfa1f12b841e98c638054`) and the website now serves it
read-only. `taposaur jobs plan 5 --db taposaur/.local/catalog.db` over the
16,743 members of snapshot 5 took 3.5 seconds using catalogue holdings:

| Decision | Members |
| --- | --- |
| Held (PDF or XML already archived) | 7,938 |
| Unconfigured credentials: Elsevier 2,909, Springer 831, Wiley 490 | 4,230 |
| Acquire without credentials: Taylor & Francis 358, Cambridge 270, PLOS 4 | 632 |
| Open-access strategy once `GLUTTON_SERVER_URL` is set (MDPI, Frontiers, CSIRO and others) | 3,934 |
| Unsupported (blank publisher) | 9 |

No publisher, Glutton, CORE or converter variables were set in the shell
that produced this, so nothing was submitted. Planning without `--db` reads
every manifest from CephFS and takes longer than ten minutes.

### Credentials

The user's Sekrit keychain was initialised on 2026-11-03 at the default
paths (`~/.local/share/sekrit/vault.sekrit`, key `~/.config/sekrit/master.key`).
It holds no Taposaur credentials yet. `Taposaur_secrets` (`secrets/`) resolves
`taposaur/elsevier/api-key`, `taposaur/elsevier/inst-token`,
`taposaur/springer/api-key`, `taposaur/wiley/tokens` and
`taposaur/core/token` from it, with the old environment variables as
fallbacks. `taposaur jobs credentials` reports presence without values. Add
keys with `sekrit put NAME --file FILE` from a private file, never from a
shell argument. Service addresses stay as flags or environment variables.

### Pilot on 2026-11-03

With the keychain populated from the legacy stack's private
`secrets/publishers.env` (never printed) and the stack's services at
Glutton `http://localhost:8180`, GROBID `http://localhost:8070` and Pub2TEI
`http://localhost:8060`, three Elsevier members of snapshot 5 were submitted:
`10.1016/j.agee.2018.02.021`, `10.1016/j.agee.2018.07.005` and
`10.1016/j.agee.2018.07.009`. One `taposaur jobs work` process served
`acquire.elsevier`, both conversion queues and `catalogue.refresh` with
`--sync`. All three acquisitions, three Pub2TEI conversions and the refresh
successors succeeded within three minutes. Two papers already held an
S2ORC-derived `tei.xml`, so their Pub2TEI output was accepted as `tei-2.xml`
and the index row now prefers it. One conversion retried once with
`archive_busy` while a refresh held the paper lock, as designed. Audit,
queue events and the `/activity` page recorded every step. The worker
process was left running and owns those queues; stop it with an interrupt
before starting another on this host.

A further batch of ten Elsevier members from book series and other journals
(jobs 29 to 38) completed later the same day in 320 seconds: ten
acquisitions, ten Pub2TEI conversions and twenty refreshes, all on their
first attempt. The queue has processed 17 acquisitions, 17 conversions and
34 refreshes with no failures. Throughput with one Elsevier handler is about
30 seconds per paper, dominated by the paced requests and the conversion.

### Batch of 100 across publishers

Submitted on 2026-11-03 with every credential in the keychain and the
`oa` strategy enabled, served by two worker processes (one owning
`acquire.elsevier` with both converters, refresh and `--sync`, the other
owning the remaining publisher queues with `--publisher-pdf`):

| Queue | Done | Failed | Failure codes |
| --- | --- | --- | --- |
| acquire.elsevier | 25 | 0 | |
| acquire.springer | 6 | 14 | 13 `http_404` (mostly Glutton lookup misses for DOIs the snapshot attributed to Springer), 1 `http_403` |
| acquire.wiley | 14 | 1 | `http_403` |
| acquire.cup | 10 | 0 | |
| acquire.plos | 4 | 0 | |
| acquire.tandf | 0 | 10 | 9 `http_403` (no entitlement from this host), 1 `attempt_limit` |
| acquire.oa | 1 | 15 | 11 `http_404` (no open-access link), 4 `http_403` |
| convert.grobid | 36 | 0 | |
| convert.pub2tei | 46 | 6 | `converter_error` HTTP 400 on Springer envelopes |

Two defects found and fixed afterwards: Pub2TEI rejects Springer's API
envelope, so `Taposaur_document.Publisher_xml.unwrap` now reduces it to
the article (verified against the live service), and a transient error on
a fallback source kept retrying a job whose publisher had already refused,
so retry classification now follows the strategy's own sources. The six
envelope failures are recorded as permanent `pub2tei-1` failures in their
manifests; those papers hold GROBID TEI from their PDFs, so planning
treats them as held. Permanent acquisition refusals recorded from now on
appear in `acquisitions_failed`; these 40 predate that marker. Taylor &
Francis needs entitled network egress before its queue is worth serving.

### Full submission and the Taylor & Francis route

All 8,361 acquirable members of snapshot 5 outside Taylor & Francis were
submitted in 11 seconds and are served by four worker processes: Elsevier
at concurrency 2, the other publishers at 1, Pub2TEI alone, and GROBID with
refresh and `--sync` at 3. Taylor & Francis admits only `128.232.98.30`, the
address of `svr-avsm2-eeg-ce.cl.cam.ac.uk`, which `avsm2` can reach by SSH
without a password. The tunnel `ssh -N -D 127.0.0.1:1080 avsm2@128.232.98.30`
runs on this host, `TANDF_PROXY=socks5h://127.0.0.1:1080` routes the
Taylor & Francis client through it (verified: 200 through the tunnel, 403
direct), and the 358 Taylor & Francis members were then submitted. The
tunnel is a plain background process, not a service, so restart it after a
reboot before serving `acquire.tandf`. Pub2TEI output of old Elsevier
articles carries no header DOI; such output is now accepted for that route
and the eleven affected conversions were re-staged at converter revision 2.

### Incident: catalogue contention with the enrichment indexer

While the full submission ran, the refresh handler re-imported a whole
index shard for every published paper, each a multi-second write
transaction on `catalog.db`. The long-running enrichment process (PID
801031, 32 workers, `full-index.py`) failed at 14:59:44 UTC on 2026-11-03
with an SQLite error, recorded in the audit as `catalog.enrich` failed with
`error_kind=sqlite`; its status file shows `failed` at 1,086,984 papers
examined and 3,137,163 details cached. Its supervisor does not restart it.
The refresh pool was stopped at once. The handler now publishes the one
changed row with `Taposaur_catalog.Shard.upsert`, a short transaction that
also keeps the shard stamp current, and falls back to a whole-shard import
only for a shard the catalogue has never seen. Refreshes were restarted at
concurrency 6. The indexer has not been restarted; its run is resumable and
`full-index.py` must be started again by the operator when the catalogue
is quiet enough. Shard file rewrites on CephFS still cost about 12 seconds
each and dominate refresh time.

### Worker stop under lock contention

With six refresh handlers beside the conversion pools, a refresh reading a
paper's manifest while a conversion held its lock raised `Accept.Busy`
outside the handler's retry scope and stopped the GROBID and refresh pool;
its claims expired and were retaken after the fix. The whole refresh body
now retries on `Busy`, catalogue errors and I/O, and fails permanently on
refusals and invalid manifests. Pub2TEI refuses some Springer JATS
articles even once unwrapped (HTTP 400 "format not recognized"); those
are recorded as permanent `converter_error` failures and the papers keep
their GROBID TEI from the PDF route.

### Throughput observed at 16:03 UTC

With the four processes above, over 27 minutes: Elsevier 8 papers a
minute at concurrency 2, Springer 3.3, Wiley 2.9, Taylor & Francis 3.7
through the tunnel, Cambridge 0.7 on its 30-second pacing, GROBID 11
conversions a minute at concurrency 6, Pub2TEI 3.3 with one handler, and
catalogue refresh 20 a minute at concurrency 6. Median durations: acquire
16.5 s, convert 28.5 s, refresh 19.3 s of which the shard file rewrite on
CephFS is 13.3 s. Backlogs were growing for Pub2TEI (662 queued; its
refine step shares the GROBID service with the GROBID pool) and refresh
(1,608 queued, two per paper). The open-access strategy yielded 16
successes against 389 refusals, almost all `http_404`, so it should be
cancelled or planned only with `--skip-refused` once markers exist.
Claims stranded by the earlier worker stop sat behind the queued backlog
until the queue reached their expired deadline, since eligibility ordered
by time; the fair dispatch below removes that. Activity snapshot latency
stayed at 0.25 s with 1,600 queued jobs and a 24 MB queue database.

### Fair dispatch in Duraml (2026-11-03, 17:30 UTC)

The throughput run exposed two scheduling faults in time-ordered
dispatch: a retry or an expired claim was re-queued at its new
availability time, so it waited behind every job queued before that time
(a 30-second busy retry became a four-hour wait on the Elsevier backlog,
and the six claims stranded by the worker stop sat behind 1,600 refreshes),
and one bulk submission monopolised a queue, so a single paper requested
during a sweep would have waited for the whole sweep.

Duraml now dispatches oldest-first within fair-share lanes. A
specification carries an optional `lane` name (`Duraml.Spec.v ?lane`,
`Duraml_client.Queue.submit ?lane`); the default is the unnamed lane. A
claim takes the lane of the queue served least recently (never-served
lanes first, ties by name), then that lane's eligible job with the
earliest creation time, then the lowest ID. Consequences: a retry or a
recovered claim keeps its age and precedes every younger job; a lane with
one job gets its turn after at most one job of every other lane; a
successor without a lane inherits its parent's; and lanes schedule but do
not identify work, so a keyed repeat in another lane reconciles with the
stored job. The SQLite backend is schema 3: `lane` and `created` columns
on `jobs`, a `lanes(queue, lane, served)` table, an `eligible` index on
`(queue, lane, created, id)` that is probed once per lane in service
order, and a `pending` index for the next-deadline wait. Schema 2 stores
upgrade in place on the first writable open; read-only opens require
version 3. The memory backend sorts the same way. Tests cover age order,
expiry, lane interleaving, inheritance, reconciliation and the upgrade of
a hand-built schema-2 store (60 Duraml tests, 96 Taposaur tests pass).

Taposaur submissions set the lane: `snapshot:N` for a sweep and `doi` for
`--doi` requests, overridable with `--lane`. Workers' successors inherit
it. `taposaur jobs show` and the job page print the lane. `taposaur jobs
cancel --queue NAME` revokes every queued job of a queue in pages of
1,000, leaving running attempts to settle.

Operational status: the open-access backlog (3,219 queued jobs) was
cancelled through the still-running schema-2 binary (`/proc/<worker>/exe`,
one `jobs cancel` per job, ~33 ms each). At 17:39 UTC the four workers
were interrupted and drained in 18 s, `jobs init` upgraded `jobs.db` to
schema 3 in place (identity unchanged, `.schema2-backup` kept beside it,
36 MB), and the workers and website restarted on the new build. The first
claim of every queue after the restart was its oldest queued job. Two
restart faults were found and fixed in `taposaur/.local/restart-workers.sh`:
workers started together contend for the Sekrit keychain lock and one
dies with `Secret store is busy` (start them two seconds apart; a retry
in `Clients.secrets` would remove the race), and the website ignores
SIGINT so it must be terminated with SIGTERM before the port is free.
All jobs submitted before the upgrade are in the unnamed lane; lanes
persist once seen, so name them per batch, not per job.

### Abstracts missing from full-text papers (2026-11-03, evening)

In review snapshot 3 (butterflies), 53,412 of 342,937 papers with a TEI
had no abstract. A 100-paper sample split three ways: 53 Pub2TEI
conversions of Elsevier XML that itself carries no abstract (median
cover date 1992, OCR raw text or metadata-only stubs; 22 of their TEIs
have empty bodies), 21 S2ORC conversions (S2ORC JSON has no abstract
field), and 26 GROBID conversions, of which 14 hold the abstract as the
unheaded first body paragraphs, 3 have empty bodies and 9 none at all.
The example `doi:10.1002/15-0783` is a GROBID 0.9.1 header-model miss on
the ESA layout: `<abstract/>` empty and a body section headed
"Abstract."; the live GROBID reproduces it.

Changes: `Document.Tei.inspect` scans the first four body sections for a
section headed Abstract/Summary or the unheaded leading block (whole when
200 to 3000 characters, else its first paragraph of 200 to 6000, since
GROBID sometimes runs abstract and introduction together), reports `text` (prose outside figures, notes, tables and
formulae) and `derived_abstract`; `metadata` derives from it. The paper
resolver records `text` per document and for the paper, its detail cache
is version 2 (version 1 still decodes), and `Taposaur_paper.version` is
2 so every catalogue row re-resolves. Catalogue schema 7 adds a nullable
`text` column, published with the metadata and surfaced as
`Summary.holdings.text`; `Summary.full_text` counts a PDF, S2ORC, or a
converted document with prose, falling back to any XML or TEI while the
body is unknown, and the review "Full text" statistic uses it. The
conversion handler refuses a TEI without body text with the permanent
code `empty_body`. Snapshot observations gained an optional `text`
holding; older snapshots decode unchanged. Tests: 97 Taposaur.

Deployed 2026-11-04 between 07:47 and 07:51 UTC: the four workers were
interrupted and drained in three seconds (every queue was already empty),
`restart-workers.sh` started them and the website on the new build, and
the refresh worker's first writable open migrated `catalog.db` to schema
7. The new `taposaur` binary replaced `taposaur/.local/taposaur-indexer`
(the 2026-10-24 copy is kept as `taposaur-indexer.oct24`) and the
enrichment was started directly, not through `full-index.py`, as
`taposaur-indexer enrich --jobs 32 --order directory --progress-file
full-index.status.json` logging to `enrichment-0003.log`, PID in
`full-index.pid`. The catalogue holds 25.8 M rows of which 2.84 M were at
resolver version 1 and 22.98 M were never resolved, so the run resolves
everything; it examined about 5 papers a second in its first three
minutes, the same rate as the earlier run, which projects to roughly two
months for the whole catalogue. Take a new review snapshot after the
review's members have been re-resolved. The restart script now backs up
`jobs.db` only when it is still at Duraml schema 2. External abstracts
remain an option: OpenAlex requires a free
API key since 2026-02-13 (100,000 credits a day; single-work lookups by
DOI are free of credits), Semantic Scholar answers without a key from a
shared pool (5,000 requests per 5 minutes, 1 request a second with a free
key), Europe PMC needs none but is biomedical, and Crossref needs none
but rarely has Wiley abstracts.

### Butterflies abstracts and full text push (2026-11-04, from 11:30 UTC)

The butterflies review is slug `ce-butterflies`, run 3, snapshot 3
(610,180 members, frozen 2026-10-30). The live catalogue showed 344,187
members held, 342,937 with TEI, 289,638 with an abstract, 53,299 holding
TEI without one, and 265,993 members absent from the archive. No
acquisition batch had ever been submitted for it; every earlier submission
was snapshot 5 (soil carbon). A dry-run plan routed all 277,616 unheld
members to "no publisher", because an unindexed member's frozen
observation names none. Of the absent members the largest DOI prefixes are
JSTOR 10.2307 (69,486), Cambridge 10.1017 (23,519), Canadian Science
10.1139 (21,635), Chicago 10.1086 (19,673), Magnolia 10.11646 (13,315),
OUP 10.1093 (12,508), CSIRO 10.1071 and Inter-Research 10.3354 (11,500
each); most have no implemented strategy.

Three tracks were started:

1. Body-derived abstracts: `taposaur enrich --uid-file` (new) resolves a
   listed selection in order. `butterflies-enrich-uids.txt` lists the
   53,237 members holding TEI without an abstract first, then the other
   289,192 held members, grouped by shard; the run (PID in
   `butterflies-enrich.pid`, status `butterflies-enrich.status.json`, log
   `enrichment-butterflies.log`, 32 jobs) examines about 4.5 papers a
   second. The full catalogue sweep (`enrichment-0003.log`) was stopped at
   140,700 examined to make room and is resumable.
2. Publisher acquisition by DOI prefix: planning now routes a member with
   no publisher evidence by its Crossref prefix. Submission for snapshot 3
   added 36,721 jobs in lane `snapshot:3`: Cambridge 23,517, Wiley 8,563,
   Taylor & Francis 3,324, Elsevier 1,170, Springer 127. The publishers
   worker was split into one process per paced publisher (Wiley, Taylor &
   Francis, Cambridge; Springer/PLOS/OA together). Early results are poor:
   the first Wiley jobs are 1996 SICI DOIs that the TDM API answers 404,
   and a 2010 Ibis DOI also 404s; 10.1016 members include journals that
   moved to OUP (ICES JMS) and aliased DOIs that doi.org redirects to
   another DOI, which the Elsevier API refuses; many 10.1080 members are
   DOIs that do not resolve at all. Cambridge succeeds but at its 30-second
   pacing (about 2 a minute, 23,500 queued). Following doi.org aliases
   before the publisher request would recover some Elsevier members.
3. OpenAlex abstracts: new `taposaur.abstracts` library and `taposaur
   abstracts fetch|fill`, evidence store `taposaur/.local/abstracts.db`
   (`TAPOSAUR_ABSTRACTS_DB` is set for workers and website by
   `restart-workers.sh`). A 50-DOI probe found all 50 works and 38
   abstracts. The fetch over `butterflies-abstract-dois.txt` (265,993
   absent members first, then 53,789 held members without an abstract; PID
   in `abstracts-fetch.pid`, log `abstracts-fetch.log`) ran keyless:
   batches of up to 100 at 10 requests a second until the $0.10 daily list
   budget (1,000 batches) is spent, then single-work reads, which are
   free, at 10 a second. Acceptance ran at about 65 percent of DOIs
   examined. A free OpenAlex key (https://openalex.org/settings/api,
   email verification) would allow 30 requests a second and $1 a day.

`butterflies-followup.sh` was superseded at 20:57 UTC by
`butterflies-pipeline.sh` (PID in `butterflies-pipeline.pid`, log
`butterflies-pipeline.log`) so that a snapshot and a corpus export follow
each stage rather than waiting for the slow enrichment: when the OpenAlex
fetch finishes it runs `abstracts fill`, publishes a snapshot of run 3 with
parent 3 ("OpenAlex abstracts; enrichment and publisher acquisitions in
progress"), exports that snapshot's corpus; when the review-scoped
enrichment finishes it fills again, publishes another snapshot with the
first as parent, and exports again. Each snapshot uses the 700,000-member,
900 MiB, one-hour budget. Workers and website were restarted at 12:09 UTC
on the build with `abstract_lookup` in refresh workers, so a refresh keeps
a filled abstract. Tests: 103 Taposaur (new `test_abstracts` suite of 6)
and 60 Duraml.

### Corpus export for the butterflies review (2026-11-04, from 20:40 UTC)

The user asked for the review's titles, abstracts and TEI full texts in
Parquet. Nothing in Taposaur produced body text as a string: the TEI
reader only reported whether a body held prose, and no Parquet writer
exists in the tree. Added `Taposaur_document.Tei.body_text` and the
`taposaur.corpus` library with `taposaur review corpus` (see the README
section "Corpus export with body text"); Parquet is produced from the JSON
Lines file by `duckdb` 2.0.0 (`~/.local/bin/duckdb`) through
`taposaur/.local/corpus-parquet.sh`, which declares every column so that
rows whose optional values are all null never change the schema, writes
with ZSTD in 10,000-row groups and prints row, text, abstract and title
counts from the finished file. A 2,000-row sample converted in 0.4 s.
Tests: 24 in `test_taposaur` (new "body text extraction") and 7 in
`test_reviews` (new "corpus export with body text", which also checks that
a domain pool gives the same rows).

Throughput was the problem. CephFS metadata latency under the day's load
was 0.3 to 3 seconds per operation (a sequential `cat` of eight TEI files
took 25 s; 64 in parallel took 4 s), and Eio reads regular files with a
blocking syscall on the calling domain, so fibers alone gave about 3 rows
a second. The export therefore runs archive lookups on an
`Eio.Executor_pool`; 32 domains gave about 8 held rows a second. 128
domains failed twice: under io_uring every domain allocates a ring and the
8 MiB locked-memory limit returned ENOMEM, and OCaml 5 allows at most 128
domains in a process ("failed to allocate domain"). `EIO_BACKEND=posix`
with 96 domains gives about 50 held rows a second, so a 344,000-paper
snapshot exports in roughly two hours; the CLI caps `--domains` at 100.
The export reads only members the snapshot found in the catalogue, so a
paper acquired after the snapshot was taken appears without text until the
next snapshot.

Running at 21:05 UTC: a corpus export of snapshot 3 (PID in
`taposaur/.local/corpus-3.pid`, log `corpus-3.log`, output
`taposaur/.local/exports/ce-butterflies-snapshot-3.jsonl`) as the first
deliverable with the older frozen abstracts, and the pipeline above, which
writes `ce-butterflies-snapshot-N.jsonl`, `.parquet` and `.summary.json`
under `taposaur/.local/exports/` and copies the Parquet file and summary to
`/data/papers/taposaur/reviews/ce-butterflies/`; `corpus-3-finish.sh`
(PID in `corpus-3-finish.pid`) converts and copies the snapshot 3 file the
same way when its export finishes. A native Parquet
writer in OCaml would remove the duckdb step and is the obvious follow-up;
reading paper manifests from the catalogue instead of the archive would
halve the metadata operations per paper.

### Overnight results and the snapshot budget (2026-11-05, 07:45 UTC)

The snapshot 3 corpus finished at 23:25 UTC after 2 h 20 min: 610,180
rows, 344,185 held, 274,322 with body text, 1,903 text errors (empty or
non-TEI files that report "expected root element"), 5.70 G characters;
`ce-butterflies-snapshot-3.jsonl` is 6.5 GB and the Parquet file 1.93 GB,
copied to `/data/papers/taposaur/reviews/ce-butterflies/`. The OpenAlex
fetch ended at 21:23 UTC with 207,053 member abstracts in the store, and
the review-scoped enrichment ended at 04:34 UTC with all 342,427 listed
papers examined (68,204 partial). The catalogue now holds 345,263 members,
331,005 with a title and 322,665 with an abstract (289,505 at capture).

Both pipeline snapshots failed with "Snapshot exceeds its byte budget":
snapshot 3 already encoded to 746 MB of observations (1,223 bytes a row),
and about 200,000 added abstracts push the new capture past the 900 MiB
cap shared by the CLI, `Taposaur_review_run.snapshot` and
`publish_snapshot`. Raising the cap alone is not enough: the cached JSONL
export is one SQLite blob (747 MB for snapshot 3) and SQLite refuses
blobs over 1,000,000,000 bytes, so a larger snapshot needs chunked or
compressed cached exports in the review store. That is the next piece of
work; until then the review has no new snapshot.

To deliver the metadata anyway, `review corpus` gained `--live CATALOG`
and `--abstracts`, which replay the snapshot policy against the live
catalogue and the evidence store without touching the snapshot. The
refreshed export `ce-butterflies-snapshot-3-live-20261105` (PID in
`corpus-live.pid`, log `corpus-live.log`, finisher `corpus-live-finish.sh`)
started at 07:50 UTC and should take about 2.5 hours; expect roughly
510,000 abstracts of 610,180 and body text for a few thousand more papers
than the first file.

### Snapshot 6 and review schema 3 (2026-11-05, 07:55 to 08:05 UTC)

The review store now stores a cached export longer than 64 MiB as ordered
parts behind a descriptor (`export_parts`, review schema 3, migrated in
place on open; `reviews.db` was backed up first to
`reviews.db.pre-schema3.bak`), and the snapshot observation budget allows
4 GiB. Snapshot 6 of run 3 (parent 3, label "Body-derived and OpenAlex
abstracts after review-scoped enrichment; publisher acquisitions in
progress") published at 08:01 UTC after a five-minute capture: 610,180
members, 331,012 titles, 508,630 abstracts (289,505 in snapshot 3),
345,270 held. Its JSONL export is 1.15 GB in 18 parts and its CSV 755 MB
in 12 parts; `review export` and the website reassemble and verify them.
The website was restarted on the schema 3 binary by
`corpus-new-finish.sh`, which then waits for the refreshed snapshot 3
export and exports snapshot 6 as `ce-butterflies-snapshot-6` (JSON Lines,
Parquet and summary, copied to `/data/papers/taposaur/reviews/ce-butterflies/`).
Snapshot 6 is the canonical source for the corpus; the refreshed snapshot 3
export carries the same metadata policy applied at export time.

### Titles for members outside the catalogue (2026-11-06, from 09:00 UTC)

The user asked why snapshot 6's corpus had more abstracts (508,630) than
titles (331,012). Titles came only from the catalogue, and the 264,910
members the catalogue does not hold had none, while OpenAlex abstracts
reached 185,958 of them. The retained OpenAlex responses could not supply
titles: the fetch had requested `select=id,doi,abstract_inverted_index`
only. Snapshot 6 gaps by field: outside the catalogue every row lacked
title, authors, journal and year; inside it 14,258 lacked a title, 20,826
authors, 115,416 a journal and 106,388 a year.

The fix keeps the bibliography of every matched work. `Taposaur.Work_evidence`
is `{doi; bibliography; provider; endpoint; sha256; retrieved}` with the
abstract left to `Abstract_evidence`. The evidence store is at schema 2
(`works(doi, evidence)`, migrated on a writable open; a read-only version 1
store answers `find_work` with `None`, so running workers and the website
kept working). The fetch now sends `select=id,doi,title,display_name,
authorships,primary_location,publication_year,publication_date,biblio,ids,
abstract_inverted_index`; authors split "Family, Given" or at the last
space, pages join with an en dash as the catalogue does, and PubMed
identifiers are the last URL segment with bare PMC numbers prefixed (a
live probe showed OpenAlex gives `pmc/articles/5193348`). A DOI is `known`
once its work is stored, so abstracts accepted at version 1 are queried
again. `Taposaur_review.fill` applies work evidence to every blank field
of an observation with the label as source; `review snapshot` collects
works for all members, `review corpus --evidence` (alias `--abstracts`)
applies abstracts then works, and rows gain a `sources` map (Parquet
`MAP(VARCHAR, VARCHAR)`) naming every field's source. Tests: abstracts 8
(work decoding, schema migration), reviews 7 (works overlay, sources).

Running: `taposaur/.local/butterflies-works.sh` (PID in
`butterflies-works.pid`, log `butterflies-works.log`, fetch progress in
`works-fetch.log`) over `butterflies-work-dois.txt`, the 400,725 snapshot 6
members lacking a title, abstract, authors, journal or year, titles first.
Keyless: about 100 batches a minute until the shared $0.10 list budget
(1,000 batches, about 100,000 DOIs) is spent, then single-work reads at
10 a second, so roughly 8 to 9 hours in all; a free OpenAlex key would
finish the list queries in minutes. The script then runs `abstracts fill`,
publishes snapshot 7 (parent 6, 4 GiB budget) with the work evidence, and
exports `ce-butterflies-snapshot-7.{jsonl,parquet,summary.json}` to
`taposaur/.local/exports/` and `/data/papers/taposaur/reviews/ce-butterflies/`.
A copy of the store before the migration is `abstracts.db.pre-schema2.bak`.
The early batches, SICI DOIs from 10.1002, matched about half the DOIs.
The list budget lasted past 1,500 batches this time; single reads began
near 150,000 DOIs at about 10 a second.

### Provenance trail in the corpus (2026-11-06, from 10:30 UTC)

The user then asked for the Parquet to carry where the title, abstract and
full text came from, the journal as OpenAlex id and name, and a type that
names sources such as OpenAlex or GROBID; asked to clarify, they chose one
struct per field, the live catalogue's cached resolution as the source of
artifact-level detail (as of export time, like the text), OpenAlex venues
for every row best effort, the full-text acquisition trail and provenance
for authors, year and identifiers as well; not the review-inclusion
origins. The pipeline wrapper was stopped after the fetch started so the
export could wait for this work; the fetch itself kept running (PID in
`works-fetch.pid`).

Rows now have `<field>_provenance` for title, abstract, authors, journal,
publisher, year, pmid, pmcid and text: `{kind; provider; artifact;
converter; version; digest; retrieved}` (Parquet STRUCT, RFC 3339 time as
TIMESTAMP). The kind is the resolver source class in reader's terms:
grobid, pub2tei, s2orc, publisher_xml, index, manifest, correction,
crossref, datacite, openalex, review_input, or catalogue when the frozen
label cannot be refined. A frozen "catalogue" label is refined through
`Taposaur_catalog.Metadata.detail` and `Taposaur_paper.of_cache_string`
when the live resolution still selects the frozen value; "openalex · sha"
labels gain the retrieval time from the store when digests agree;
"input:KEY:LOCATION" labels become review_input with the key and location.
`text_acquisition` is the manifest entry the TEI was converted from (its
`from`, else the first artifact of the format the converter reads): file,
format, source, strategy, origin, URL, status, version, confidence, digest
and time. `journal_openalex_id`/`_name`/`_match` come from the member's work
venue, else a venue keyed by ISSN (first ISSN-shaped run, since GROBID
often leaves "0030-1299 OIKOS 96:1 (2002)" in the field), else by folded
journal name, from every work's primary source, the `sources` table and
`--journal-links`. Work evidence gained `venue`; the store is schema 3
with `sources(key, venue, sha256)`; `taposaur abstracts redecode` rebuilds
works from retained bodies (the fetch under way stores works without
venues) and `taposaur abstracts sources KEYS` reads sources singly by
`issn:` or S id. Snapshot 6 holds 603 distinct ISSN strings (396 parse)
and 421 journal names; `butterflies-source-keys.txt` lists 601 keys.
`butterflies-works-finish.sh` (PID in `butterflies-works-finish.pid`, log
`butterflies-works.log`) waits for the fetch, then runs redecode, sources,
fill, snapshot 7 (parent 6) and the export with `--live --evidence
--journal-links`, converting to Parquet with the struct columns declared in
`corpus-parquet.sh`.

### Snapshots 7 and 8 and the provenance export (2026-11-06 22:55 to 2026-11-07 02:08 UTC)

The fetch ended at 22:55 UTC after 400,725 DOIs: 283,195 abstracts
accepted and 386,267 work bibliographies retained, 43 DOIs left on
OpenAlex 504s. The SIGTERM sent to `butterflies-works.sh` at 11:57 UTC
had not stopped it: a non-interactive bash defers the signal while a
foreground child runs, so the wrapper woke when the fetch exited and ran
its own fill, snapshot and export alongside the finisher. Snapshot 7
(23:09 UTC) is therefore the wrapper's: same label and parent as snapshot
8, exported without `--live --evidence --journal-links`, so its Parquet
has coarse provenance (kind catalogue or openalex, no venues). Snapshot 8
(23:16 UTC, finisher) is the intended export. Both files are in
`/data/papers/taposaur/reviews/ce-butterflies/`; snapshots are immutable,
so snapshot 7 stays as a plain export. Check the process is gone, not
just signalled, before starting a replacement (`kill -0`).

Finisher steps: redecode rewrote 386,245 of 386,267 works with venues
(5 min); `sources` found 588 of 604 keys (16 ISSNs unknown to OpenAlex);
fill added 17 catalogue abstracts; the export with 96 domains took 2 h 50
min for 610,180 rows (346,846 held, 276,751 texts, 1,903 TEI parse errors:
1,881 empty or truncated files, 22 non-TEI). Snapshot 8 Parquet (2.18 GB):
597,676 titles (was 331,012 in snapshot 6), 510,229 abstracts, 595,769
journals; OpenAlex venue on 593,423 rows (386,245 from the member's work,
148,584 by ISSN, 58,594 by name; 16,757 unmatched). Title provenance:
openalex 265,086, grobid 123,991, publisher_xml 115,069, pub2tei 64,209,
manifest 17,881, s2orc 11,440. Text provenance: grobid 143,101, pub2tei
120,422, s2orc 13,228; every text row carries `text_acquisition`.
Retrieval times are present on 579,795 title provenances (artifact times
for TEI, response times for OpenAlex); digests only where the manifest
or store records one (265,234).

### Remaining before a bulk run

Run the pilot. Confirm live publisher entitlements and converter health.
The archive root is on CephFS, where `flock` is coherent across hosts, but
the queue database and staging tree must stay on local disk. Shared
services such as Glutton, CORE and open-access hosts are paced per worker
process, and `taposaur fetch` is not excluded by the queue locks, so run
one acquisition process per host while publishers are being drained. Add
retention for settled jobs once volumes justify it.

## Verification already completed

After the `origin/minus39` merge, these commands succeeded:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check \
  wip/duraml @wip/duraml/install
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check \
  --force wip/duraml
```

All **56 tests** passed: 20 backend conformance tests, 10 worker tests,
8 real SQLite process/recovery tests and 18 typed-client tests across both
backends. Typed tests cover deduplication, reopening handles, atomic successors,
retry/failure/cancellation, decode errors, schema mismatch, cross-store refusal,
result validation, switch cleanup and the Jsont adapter.

All seven interface examples type-checked. The odoc parser accepted 225
comments without warnings. Build/install and whitespace checks passed.
`@wip/duraml/fmt` could not run because `ocamlformat` is absent from the switch.
No Taposaur integration code changed, so its application tests were not rerun
for the generic-library work. Run affected Taposaur tests when integrating.

Useful local verification records, which are not durable project assets:
`/tmp/duraml-postmerge-build.log`, `/tmp/duraml-postmerge-tests.log`,
`/tmp/duraml-postmerge-fmt.log` and `/tmp/duraml_examples.ml`.

Leave new implementation reviewable and preserve unrelated working changes.
The latest work was intentionally left uncommitted. Do not push without a
new instruction from the user.
