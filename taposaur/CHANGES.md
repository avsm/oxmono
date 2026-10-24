# Changes

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
