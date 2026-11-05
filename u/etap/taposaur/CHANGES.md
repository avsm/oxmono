# Changes

- Export a review snapshot as a corpus: `taposaur review corpus` writes
  flat JSON Lines rows with the frozen bibliography and provenance, the
  holdings, and the body prose of the preferred TEI read from the archive
  on a pool of domains, ready for Parquet conversion, optionally refreshed
  from the live catalogue and the abstract evidence store; the TEI reader gains
  `body_text`, which returns headings, paragraphs and list items without
  figures, notes, tables, formulae or back matter.
- Treat an empty `jobs submit --lane` as the unnamed Duraml lane.
- Build the website on Sitekit: its page shell and generic widgets, with
  Taposaur's stylesheet and scripts served at content-hashed paths, so the
  Content Security Policy no longer allows inline styles. Coverage bars are
  SVG images rather than styled spans, and pages send
  `Referrer-Policy: same-origin` and `Cross-Origin-Opener-Policy:
  same-origin`.
- Recover missing abstracts from OpenAlex: `taposaur abstracts fetch` batches
  exact-DOI lookups, retains every response, and falls back to free
  single-work reads when the daily budget refuses a batch; the resolver
  applies the evidence only when the archive has no abstract, enrichment,
  refresh workers and the website consult the store, `abstracts fill`
  updates catalogue rows in place, and review snapshots carry the evidence
  to members the archive does not hold.
- Plan acquisitions for members with no publisher evidence by the owner of
  their DOI prefix, so unindexed Wiley, Elsevier, Springer, PLOS, Taylor &
  Francis and Cambridge members become acquirable; the strategy still
  resolves the DOI itself.
- `taposaur enrich --uid-file` resolves a listed selection in order, for a
  review-scoped pass ahead of a full catalogue sweep.
- Add durable acquisition, conversion and catalogue refresh jobs on Duraml
  with planning from review snapshots, per-publisher worker ownership,
  Retry-After scheduling, no-clobber archive acceptance with PID-bearing
  locks and recoverable intents, an audit bridge and read-only job pages.
  Shared durable file helpers back both the staging and archive writers.
  Publisher credentials come from the Sekrit keychain, with environment
  variables as fallbacks and a `credentials` report that prints no values.
  A live activity page follows website requests, audit records and jobs as
  they happen, with jobs nested beneath the job that staged them, state
  icons, and a job page that traces each attempt to its audited operations.
  Submissions name a Duraml fair-share lane per snapshot or ad hoc request,
  and `jobs cancel --queue` revokes a queue's queued work in bulk.
- Recover abstracts that GROBID leaves in the TEI body, record whether a
  converted document holds any prose, refuse empty conversions, and count
  only documents with prose as full text once a paper is resolved. The
  resolver version rises to 2 so every catalogue row is resolved again.
  Taylor & Francis requests can travel through a SOCKS proxy to a
  whitelisted address, and Pub2TEI output without a header DOI is accepted.
  Catalogue refreshes publish one row per paper instead of reloading its
  shard, keeping catalogue transactions short beside other writers.
  The activity page seeds from the event log's tail and pages older events
  on demand, so it stays fast as the queue grows.
  Permanent acquisition refusals are recorded in archive manifests so later
  sweeps can skip them, Springer envelopes are unwrapped before Pub2TEI, and
  fallback hiccups no longer retry a publisher that already refused.

- Show publisher download strategies, access requirements and existing full
  text in snapshot paper lists using the acquisition router.

- Recover missing publishers from configured DOI mirrors and retained CSV
  inputs, with audited OCaml queries and immutable review snapshots.

- Add a durable SQLite audit trail for acquisition, archive changes, review
  publication and indexing, with CLI queries and a read-only audit page.

- Add a collapsed debug panel showing configured storage paths, current-paper
  artifact locations and the implemented publisher acquisition capabilities.

- Unify the website with compact Htmlit widgets, sortable review and file
  tables, format badges, coverage summaries and an abstract visibility toggle.

- Add resumable GROBID header consolidation for review PDFs, preserving
  prior TEIs and validating DOI and publisher before archive publication.

- Add sortable snapshot coverage tables with percentage bars and count links
  to indexed lists of matching papers, preserving frozen snapshot metadata.

- Expand the TEI reader with structured citations, DOI and cached OpenAlex
  source links, backlinks, tables, notes, figures and mathematical typesetting.

- Show cached snapshot coverage, missing metadata and full text, format
  counts and publisher breakdowns in place of individual review titles.

- Separate TEI bibliography entries, label identifiers and link citations
  within the reader sidebar without closing it.

- Add versioned review definitions, recorded DOI batches, frozen metadata
  snapshots and cached exports through an OCaml CLI and read-only review UI.

- Enrich papers directory by directory to reuse Ceph metadata caches,
  with resumable indexed scans and serialized progress updates.

- Record oversized manifests as per-paper errors so enrichment can continue.

- Port Cambridge acquisition and GROBID/Pub2TEI clients onto shared Fetch.
  Add publisher pacing and busy retries, separate metadata deadlines,
  verified CORE output fallback and wrapped Springer XML validation.

- Persist complete paper details during enrichment and serve cached pages
  without archive reads. Backfill older rows and report cache progress.

- Show saved paper metadata immediately and load archive details in the
  background, keeping cold file reads off the initial page response.

- Add an OCaml TEI audit that records conversion failures, source-PDF checks
  and regeneration evidence without changing archive files.

- Check artifact freshness without opening artifact files, retaining existing
  stamps for regular files and refusing to follow symlinks.

- Add journal autocomplete and TEI/history side panels that retain paper
  details and reading positions.

- Add single-paper fetching into recoverable staging with publisher, OA and
  CORE clients, content validation, source events and idempotent retries.

- Add source freshness checks, partial metadata diagnostics, explicit shard
  retirement and maintained catalogue counters. Hide unreadable TEI links.

- Separate pure paper models, format readers, catalogue storage and indexing
  workflows. Add typed catalogue access and preserve caches during migration.

- Resolve paper metadata with per-field provenance across all conversions.
  Add resumable catalogue enrichment and metadata availability filters.

- Fill missing paper metadata from publisher XML and TEI headers, including
  authors, journal, publication date, volume, issue, and pages.

- Simplify the browser layout and add inline PDF previews, fuller paper
  metadata, and expandable file provenance.

- Add read-only archive and TEI libraries, transactional SQLite search,
  inspection/import commands, and a Proffer corpus browser.
