# Changes

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
