# Taposaur

Read, search and browse an existing Taposaur paper archive. This first
milestone exposes read-only archive operations, a rebuildable SQLite
catalogue, a CLI and a Proffer website. The archive format stays unchanged. [Roadmap](ROADMAP.md) records the
remaining acquisition, conversion and review work.

## Libraries

| Dune library | OCaml interface | Responsibility |
| --- | --- | --- |
| `taposaur` | `Taposaur` | Pure corpus codecs, bibliography and summaries |
| `taposaur.paper` | [Taposaur_paper](paper/taposaur_paper.mli) | Pure evidence selection and resolved records |
| `taposaur.archive` | [Taposaur_archive](archive/taposaur_archive.mli) | Confined Eio archive reads |
| `taposaur.document` | [Taposaur_document](document/taposaur_document.mli) | Bounded format readers and TEI rendering |
| `taposaur.reader` | [Taposaur_reader](reader/taposaur_reader.mli) | Load and resolve one archive paper |
| `taposaur.catalog` | [Taposaur_catalog](catalog/taposaur_catalog.mli) | Typed SQLite handles, queries and atomic publications |
| `taposaur.indexing` | [Taposaur_indexing](indexing/taposaur_indexing.mli) | Import, enrichment and audit workflows |
| `taposaur.services` | [Taposaur_services](services/taposaur_services.mli) | Explicitly configured Fetch clients |
| `taposaur.store` | [Taposaur_store](store/taposaur_store.mli) | Recoverable staging publications |
| `taposaur.download` | [Taposaur_download](download/taposaur_download.mli) | Single-paper acquisition |
| `taposaur.review` | [Taposaur_review](review/taposaur_review.mli) | Query definitions, set selection and frozen values |
| `taposaur.review-store` | [Taposaur_review_store](review_store/taposaur_review_store.mli) | Durable revisions, runs, snapshots and exports |
| `taposaur.review-run` | [Taposaur_review_run](review_run/taposaur_review_run.mli) | Local query execution and metadata capture |
| `taposaur.web` | [Taposaur_web](web/taposaur_web.mli) | Proffer routes and views |

[Architecture](ARCHITECTURE.md) describes the dependency boundaries,
public data models and publication contracts.

The core modules have separate interfaces for [DOIs](core/doi.mli),
[UIDs](core/uid.mli), [paths](core/layout.mli), [manifests](core/manifest.mli),
[index entries](core/index.mli), [authors](core/author.mli) and
[events](core/event.mli). They perform no filesystem or network I/O.

The CLI calls the archive, indexing and catalogue libraries. The website
uses the reader and read-only catalogue APIs. Neither shell commands nor a running CLI are required
by a library. HTTP listeners and Eio switches belong to the executables.

## Build

Run from the OxMono root with its OxCaml switch:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check taposaur
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check --force taposaur
```

The executables are `_build/default/u/etap/taposaur/bin/main.exe` and
`_build/default/u/etap/taposaur/bin/server.exe`. Installed names are `taposaur`
and `taposaur-web`. The examples below use the installed names.

## Inspect the archive

The default root is `/data/papers/taposaur`. Override it with `--root`.

```sh
taposaur inspect doi:10.1016/j.ijer.2018.10.003
taposaur events pmc:PMC4460142
taposaur validate --shard doi/10.1016/06.jsonl --limit 100 --artifacts
```

`inspect` prints a decoded manifest. `events` streams JSONL in file order.
`validate` checks sampled index entries against their paper directories and
identities. `--artifacts` also verifies that each manifest-listed artifact
opens and has the recorded size. Validation does not repair anything.

Archive lookup implements the separate DOI and PMC sharding rules.
Staging lookup tries the sharded layout before the legacy flat layout.
An explicit recursive audit finds `paper.json` markers at any depth.

JSONL errors include the source and line number. Buffers have configurable
limits. Unknown outcome and summary payloads remain generic JSON. The
reader does not replay the event log into a new manifest. Events remain
the source of truth when a stored projection disagrees with them.

## Create a catalogue

Put the database on local storage. It is derived from the archive index
and document metadata and can be rebuilt. It owns no review decisions or
user state.

```sh
taposaur import --db /local/path/catalog.db --shard doi/10.1016/06.jsonl
taposaur import --db /local/path/catalog.db --shard pmc/446/3e.jsonl
taposaur stats --db /local/path/catalog.db
taposaur search --db /local/path/catalog.db 'OpenClinica'
```

Load the full index or advance through a few changed shards at a time:

```sh
taposaur import --db /local/path/catalog.db
taposaur import --db /local/path/catalog.db --limit 10
taposaur import --db /local/path/catalog.db --prefix doi/10.1016/ --limit 10
```

A shard loads through a temporary table in bounded batches. Publication
replaces its rows, FTS entries and counters in one transaction. Bad JSON,
duplicate UIDs, misplaced UIDs and files changing during a read leave the
previous shard intact. Errors remain visible until a successful retry.
Cancellation interrupts loading. Final publication defers cancellation
until the transaction completes.

Size, modification time and loader version identify unchanged files. `--force` reloads
them, including after changes to parsing or projection code. Limits count
changed shards, so repeating a limited import advances through the index.
Missing source shards retain their existing rows. Explicitly retire one with
`taposaur retire-shard --db catalog.db doi/10.1016/06.jsonl` after confirming
it is no longer wanted. The command refuses an existing source shard.

Normal imports list all index shards to record discovery coverage. That
listing can be substantial on the production filesystem. `--shard` reads
one named file directly and does not claim complete coverage. A partial
catalogue must not be interpreted as a complete inventory of holdings.

Search uses exact canonical identifiers or literal words across titles,
authors, journals, publishers and abstracts. It uses UID ordering and a
UID cursor, with pages capped at 100 records. SQL and FTS operators in
user text have no special meaning. Publisher and journal filters are exact
matches. Whole-document text search is outside this milestone.

The journal selector suggests matching journals and their catalogue counts.
Use the arrow keys and Enter to select a suggestion, or clear it to search all
journals. Selecting a journal preserves the other search filters.

Paper pages open Read TEI and History in a side panel. Switching tabs retains
the selected TEI version and scroll position. Close the panel with Escape or
use Open full page for a separate view. Without JavaScript these links open
standalone pages.

Paper pages use persisted complete details when available, including
provenance, file links and reading controls. These requests need no archive
reads. Details show when they were saved, with warnings for known stale data
or failed refreshes. Source changes are checked by the freshness workflow.
Until details have been cached, indexed pages show catalogue metadata while
the full details load in the background. If that request fails, the saved metadata stays visible.
The Load complete paper details link also works without JavaScript.
`/paper?uid=IDENTIFIER&view=full` serves the complete page directly.
Use `view=live` to explicitly inspect current archive metadata without
updating the cache. Reading TEI, PDFs and history still accesses archive files.

## Resolve metadata

Some corpus index rows contain only holdings. Resolve their bibliography
from the actual documents after importing the index:

```sh
taposaur describe 'doi:10.1016/0001-4575(69)90059-1'
taposaur enrich --db /local/path/catalog.db --limit 1000 --jobs 8
taposaur enrich --db /local/path/catalog.db
taposaur enrich --db /local/path/catalog.db --force --uid 'doi:10.1016/0001-4575(69)90059-1'
```

`describe` exposes selected values and alternative evidence without a
database. `enrich` stores a search projection using the same resolver.
Each paper publishes its metadata and FTS changes atomically. Document
reads happen outside SQL transactions. The original imported index JSON
is retained separately and remains the resolver's fallback input.

Limited runs resume at the next unresolved paper. Completed attempts are
versioned, including unavailable manifests. `--force` refreshes them after
archive changes or retries failures. Reimporting a shard invalidates its
cached metadata. Failed refreshes retain the previous search projection.
Up to eight papers are read concurrently by default, configurable with
`--jobs`. Cancellation leaves completed papers available. Catalogues using
schema versions 1 through 6 migrate when opened for writing,
preserving existing cached metadata. `taposaur enrich --limit 0` upgrades
a catalogue without scheduling any metadata reads.

Enrichment defaults to `--order directory`: complete one publisher/hash
shard before moving to the next. This improves metadata-cache locality on
Ceph. Selection uses the existing SQLite shard index and row-ID cursors,
with no new index or schema migration. `--order uid` retains identifier
ordering for comparisons. Both modes skip saved work on restart. Within
a directory, up to 512 candidates feed the bounded worker pool.

Enrichment also stores manifests and per-document metadata observations.
The pure resolver reconstructs complete paper details, preserving alternatives,
corrections and diagnostics. Existing enriched rows without this cache are
automatically backfilled. Failed refreshes preserve the previous details and
their saved timestamp. Reimporting a shard invalidates its details.

Use `--uid-file FILE` to resolve only the identifiers listed one per line, in
that order, skipping any that are already current; list them grouped by
archive directory so filesystem metadata is reused, as a review-scoped pass
ahead of a full sweep.

Use `--progress-file /local/path/progress.json` to write atomic progress updates
from the OCaml enrichment process, including scan order and the last shard.
Concurrent progress writes are serialized. `stats` and the homepage report the total
number of papers with saved details. The cache contains no PDF/TEI bodies or
event logs.

Search returns `Taposaur.Summary.t`, containing a UID, bibliography and
holdings flags. The CLI serializes this summary as JSONL. The search page
renders titles, authors, journal, publisher and abstract snippets. Its metadata filter selects papers with or without a title,
abstract or publisher. `stats` and the homepage show availability counts
and enrichment progress. These counts describe the loaded catalogue.

### Freshness and diagnostics

Check a bounded page of sources, then refresh the changed papers:

```sh
taposaur check-freshness --db catalog.db --limit 1000
taposaur check-freshness --db catalog.db --after 'doi:10.1000/last' --limit 1000
taposaur enrich --db catalog.db
taposaur enrich --db catalog.db --retry-errors
```

Use the emitted cursor for the next page. Source stamps include the decoded
manifest and artifact sizes, mtimes and ctimes. The check reads no artifact
bodies and uses directory-entry metadata without opening artifact files.
It marks missing or changed sources stale while preserving cached
values. There is no background watcher. Paper pages compare their current
source stamp with the cached stamp and identify stale or unchecked metadata.

Enrichment checks the source before and after reading. A changed source is
not published. Individual document errors produce partial, searchable metadata
with retained diagnostics. `--retry-errors` retries partial resolutions and
unavailable manifests. The homepage and `stats` distinguish complete, partial,
stale and failed resolutions. Metadata counts use transactional counters.

### Audit failed TEI conversions

Validate the complete TEI documents for a work list of DOI or PMC identifiers:

```sh
taposaur audit-tei --work-list papers.txt --jobs 8 > tei-audit.jsonl
```

This read-only OCaml command emits one JSON object per TEI artifact. It
recognises GROBID error text, records the artifact's SHA-256 and source stamp,
and checks the selected source PDF's signature and size. PDF checks do not
establish that its pages can be parsed. Complete XML validation is bounded to
16 MiB and 256 nesting levels. It checks well-formedness, not a TEI schema.

`grobid_error_NO_BLOCKS` requires PDF inspection for OCR or replacement.
`grobid_error_GENERAL` with an available PDF is a regeneration candidate.
Malformed XML, metadata-header limits, missing PDFs and changing sources
remain separate findings. No conversion is started and no archive file is
changed. Recheck the saved source stamp and failed-artifact digest before
using a report to regenerate a file. Work-list coverage is explicit, so the
report does not claim to validate the entire archive.

### Resolved paper interface

`Taposaur_reader.load ?index paper` returns one record containing the
manifest, original index entry, resolved metadata, all document outcomes
and the preferred readable TEI. `Taposaur_paper.resolve` is the pure
entry point for callers supplying already-parsed documents. Neither function writes.

Each metadata field has a selected `{ value; source }` and a list of
alternatives. A source identifies a correction, manifest, document or
index. Failed document parses remain explicit outcomes. Agreement from
multiple sources is retained too. `Field.value` supplies the selected
value for a renderer, and `presence` reports title, abstract and publisher
availability. `bibliography` returns the selected `Taposaur.Bibliography.t`
for catalogue publication. Corpus index rows remain unchanged.

Selection is independent for every field. The priority is the newest
explicit correction, manifest value, publisher XML, Pub2TEI, GROBID,
S2ORC, other documents, then index fallback. Within each document class,
published versions precede accepted versions, preprints and unknown
versions. Ties retain manifest order. Blank fields contribute no value.
Author lists are selected intact rather than merged across documents.
The manifest UID always identifies the paper.

Multiple TEIs occur in this corpus. For example,
`doi:10.1016/0166-4328(95)90032-2` has both Pub2TEI and S2ORC artifacts.
All TEI headers contribute evidence, so a secondary conversion can supply
an abstract absent from the preferred conversion. The reading link uses
the highest-ranked TEI with a successfully parsed header. Every version
remains accessible in the file table.

Current parsers cover TEI headers and front matter, Elsevier retrieval
XML's `coredata`, and S2ORC JSON's bibliographic fields. When a TEI header
carries no abstract, the reader scans the first four body sections for one
GROBID left there, headed "Abstract" or "Summary", or as the unheaded
block before the first headed section: the whole block when it is 200 to
3000 characters, otherwise its first paragraph of 200 to 6000 characters,
since GROBID sometimes runs the abstract into the introduction; a headed
introduction is never promoted. The same bounded scan reports whether the
body holds any prose outside figures, notes, tables and formulae, which
the catalogue records as the paper's `text` holding. XML headers are
bounded at 1 MiB and JSON at 16 MiB. Unsupported XML dialects and malformed
documents remain visible as metadata errors. PDFs are previewed, not
parsed for metadata. The format readers are `Document.Tei`,
`Document.Elsevier` and `Document.S2orc`. All return `Bibliography.t`;
`Document.Tei.inspect` adds the body observations. Changing a reader
raises `Taposaur_paper.version`, which marks every catalogue row for
re-resolution so search, exports and later snapshots follow the readers;
existing snapshots stay frozen until a new one is taken. Raw events remain accessible through history and
are not replayed by the metadata resolver.

## Check coverage

Compare a work list with the loaded catalogue:

```sh
taposaur coverage --db /local/path/catalog.db journals/example.doi
```

Results distinguish `tei`, `held`, `no-artifacts` and `not-indexed`.
`not-indexed` does not assert that the archive lacks the paper. Comment
and blank lines are skipped. This command does not create a review or
schedule acquisition.

## Review requests, queries and snapshots

Review state lives in a separate local SQLite database. The website opens it
read-only. Agents and the CLI append definitions, runs and snapshots. A
catalogue rebuild cannot remove review membership or previous deliveries.

The public OCaml interfaces are `Taposaur_review` (pure definitions, set
selection and export codecs), `Taposaur_review_store` (durable state), and
`Taposaur_review_run` (catalogue execution and capture). The schema is in
`review_store/schema.sql`. No review command reads Ceph or calls a provider.

Import an existing list and capture available catalogue metadata:

```sh
taposaur review import education-eef education-eef.doi \
  --reviews-db reviews.db --db catalog.db \
  --title 'Education EEF included studies' --requester 'Research group' \
  --by avsm2 --expected 0 --reason 'Initial supplied list' \
  --label 'Initial delivery'
```

Use `--input-format metadata_jsonl` to import one flat bibliography object
per line. Title and abstract are optional:

```json
{"doi":"10.1000/example","title":"Title","abstract":"Abstract"}
```

DOI URLs and prefixed DOIs are normalized using the shared identifier library.
Duplicates remain in the source evidence but count once in membership.
Invalid lines abort unless explicitly marked `--partial`. Partial status and
line diagnostics follow the run into its snapshots and manifests.

For an externally executed query, add `--provider NAME --query 'EXACT QUERY'`.
Optional `--parameters FILE.json` retains native filters and other parameters.
This records supplied results. It does not execute the query or establish
that the provider returned an exhaustive result set. The actor supplies that
completion declaration. Original result bytes and query text are retained.

### Definition version 1

A definition contains named inputs and a bounded set expression:

1. Union the inputs named in `include_any`.
2. Intersect that set with each input named in `require_all`.
3. Subtract the union of inputs named in `exclude_any`.

Every declared input must be referenced. Unknown keys, unsupported versions,
duplicate keys, invalid source shapes and contradictory metadata predicates
are rejected. Query definitions contain no pagination or worker settings.
Local searches select DOI records only. Version 1 uses the catalogue's
literal words joined by AND, exact journal and publisher filters, inclusive
year bounds, and AND-combined metadata predicates. Native Boolean syntax is
preserved only for recorded external queries. It is not translated into FTS.

```json
{
  "version": 1,
  "inputs": [
    {
      "key": "soil-carbon",
      "label": "Local soil carbon literature",
      "source": {
        "kind": "catalogue",
        "local": {
          "text": "soil carbon",
          "year_from": 2000,
          "holdings": "any",
          "metadata": [
            {"field": "title", "availability": "present"}
          ]
        }
      }
    }
  ],
  "include_any": ["soil-carbon"],
  "require_all": [],
  "exclude_any": []
}
```

The three source kinds are:

| Kind | Required fields | Execution |
| --- | --- | --- |
| `catalogue` | `local` | Consistent local DOI search. |
| `list` | `blob`, optional `format` | Fixed SHA-256 input reference. |
| `recorded_search` | `provider`, `query`, optional `parameters`, `format` | Supplied result file for each run. |

Formats are `doi_lines` (default) and `metadata_jsonl`. A list's blob is
created with `taposaur review blob FILE --reviews-db reviews.db`. Replace
that reference in a new revision to change a fixed list. A recorded query
can receive new results without changing its definition revision.

```sh
taposaur review define soil-carbon definition.json \
  --reviews-db reviews.db --title 'Soil carbon' --requester 'Research group' \
  --by avsm2 --expected 0 --reason 'Initial search definition'
taposaur review run soil-carbon --revision 1 --by avsm2 \
  --reviews-db reviews.db --db catalog.db
taposaur review snapshot 1 --label 'Initial delivery' --by avsm2 \
  --reviews-db reviews.db --db catalog.db
```

For recorded inputs, add `--input KEY=FILE` to `review run`. Use
`--partial-input KEY` for a known incomplete result. Bindings for fixed lists
must match the retained bytes exactly. `review show SLUG` prints a definition,
and `review history SLUG` lists the recorded runs and snapshots.

`--expected` is mandatory when creating a revision. Zero creates a review.
An existing review requires its current revision number. A stale revision
fails atomically. Requester, owner and title are fixed for this initial
interface. Actor and reason are recorded per revision. These CLI attributions
are caller supplied and are not an authentication or access-control system.

### Metadata refresh and exports

Run IDs and snapshot IDs printed by the commands are database-wide identifiers.
Refresh metadata for an existing run without executing its queries again:

```sh
taposaur review snapshot RUN --parent PREVIOUS_SNAPSHOT \
  --label 'Metadata refreshed' --by avsm2 \
  --reviews-db reviews.db --db catalog.db
taposaur review compare BEFORE AFTER --reviews-db reviews.db
taposaur review export SNAPSHOT --format csv --reviews-db reviews.db
taposaur review export SNAPSHOT --format manifest --reviews-db reviews.db
```

Snapshots freeze selected bibliographic values, holdings, source references,
catalogue revisions, diagnostics and request context. The "Full text"
statistic counts a PDF or S2ORC document, or a converted document whose
body the catalogue has seen to hold prose; a TEI or publisher XML without
body text does not count once the paper has been resolved, and counts
until then. Nonblank catalogue
values precede supplied metadata. Supplied metadata follows input declaration
order and original row order. Missing catalogue records remain members.
The snapshot never calls the paper resolver when opened or exported.

DOI text, DOI/title/abstract CSV and provenance JSONL are generated and cached
when the snapshot is published. All include missing metadata. CSV uses exact
field values, quoted cells and CRLF record separators. Spreadsheet software
may interpret leading formula characters. The manifest includes the query
definition, completion status, counts, input digests and export checksums.
Repeated exports return the same bytes. `--output FILE` creates a new file
and refuses to overwrite an existing one.

### Corpus export with body text

`taposaur review corpus SNAPSHOT --output FILE.jsonl` writes one JSON object
per member in DOI order: the frozen title, abstract, authors, journal,
publisher, year and identifiers with the source of the title, abstract and
publisher, the holdings recorded at capture, and the body prose of the
paper's preferred TEI document read from the archive when the export runs
(Pub2TEI before GROBID, published versions first, the first with prose
wins). Rows are flat, every key is present and absent values are `null`, so
the file converts directly to Parquet:

```sh
EIO_BACKEND=posix taposaur review corpus 7 --reviews-db reviews.db \
  --root /data/papers/taposaur --output snapshot-7.jsonl --domains 96
duckdb -c "COPY (SELECT * FROM read_json('snapshot-7.jsonl',
  format='newline_delimited', columns={snapshot:'INTEGER', doi:'VARCHAR',
  uid:'VARCHAR', title:'VARCHAR', abstract:'VARCHAR', authors:'VARCHAR[]',
  journal:'VARCHAR', publisher:'VARCHAR', year:'INTEGER', published:'VARCHAR',
  volume:'VARCHAR', issue:'VARCHAR', pages:'VARCHAR', issn:'VARCHAR',
  pmid:'VARCHAR', pmcid:'VARCHAR', title_source:'VARCHAR',
  abstract_source:'VARCHAR', publisher_source:'VARCHAR',
  in_catalogue:'BOOLEAN', pdf:'BOOLEAN', xml:'BOOLEAN', tei:'BOOLEAN',
  s2orc:'BOOLEAN', tei_file:'VARCHAR', tei_converter:'VARCHAR',
  tei_sha256:'VARCHAR', tei_bytes:'BIGINT', text:'VARCHAR',
  text_error:'VARCHAR'})) TO 'snapshot-7.parquet'
  (FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 10000)"
```

Members the snapshot did not find in the catalogue are written without an
archive lookup. Each row names the TEI file, its converter, SHA-256 and
size, so the text can be traced to an archive artifact; `text_error`
records a TEI that could not be read or parsed. Body text is the TEI
body's headings, paragraphs and list items with figures, notes, tables,
formulae and the back matter left out; a block is a line and blocks are
separated by blank lines. A TEI larger than 16 MiB or a body text over
8 MiB is an error for that row. The command prints a summary with row,
held, text and error counts. Regular-file reads block an Eio domain, so
`--domains` (1..100, default 16) is what overlaps archive latency; the
io_uring backend needs locked memory for every domain, so large counts
run under `EIO_BACKEND=posix`. The output file must not exist.

Inputs are bounded to 32 MiB each and 64 MiB combined. The default budget is
100,000 source occurrences or snapshot members. `run --max-members` and
`snapshot --max-members` allow up to 2,000,000. Snapshots default to 256 MiB
of encoded observations and a 60-second capture deadline. Explicit
`snapshot --max-bytes` and `--capture-seconds` allow up to 900 MiB and
3,600 seconds. Large captures need proportionally more memory and retain a
catalogue WAL read transaction during capture. `blob --max-bytes` permits
retaining larger original evidence files. Exceeding a budget fails rather
than truncating a result.
Capture runs in a background CLI process. A failed or interrupted capture
publishes no partial snapshot and can be retried from the same run.

The SQLite store deduplicates inputs, observations and exports by SHA-256.
Publication is one transaction. Use SQLite's online backup API or `sqlite3
reviews.db '.backup BACKUP.db'` for a consistent backup while writers run.
Do not copy only the main database file while WAL writes are active.

Enable the read-only website with `taposaur-web --reviews-db reviews.db`
alongside the existing archive and catalogue options. `/reviews` lists
requests. Review pages expose revision definitions, run evidence, snapshots,
comparisons and cached exports. There are no review mutation routes.
Snapshot pages show cached coverage and missing-data statistics, format
counts and publisher breakdowns. Click a column header to sort. Compact
bars show present and missing percentages. Publisher gaps can be sorted
by either count or percentage. Counts and publisher names link to filtered
paper lists at `/review-papers`, with DOI cursor pagination and frozen titles
and abstracts. These lists use the snapshot's membership and metadata,
so their contents match the displayed counts even after catalogue updates.
Full-text coverage means a recorded PDF, XML, TEI or S2ORC holding, not
verified conversion quality. Format counts overlap. Statistics use frozen
observations and never scan the catalogue when a page opens. For older
snapshots, run `review cache-statistics SNAPSHOT --reviews-db reviews.db`.
This adds statistics and browsing indexes without changing the original
manifest or exports. Review schema 2 adds derived coverage flags and indexed
publisher filters. Write commands upgrade schema 1 automatically. Older
snapshots require the cache command before their paper filters can be used.

## Browse

```sh
taposaur-web --root /data/papers/taposaur \
  --db /local/path/catalog.db --port 8765
```

Open `http://localhost:8765`. The server binds loopback and opens SQLite
read-only. It serves overview, search, paper, history, PDF and TEI pages.
The homepage displays loaded coverage and import errors. A running import
can update the catalogue while the browser reads it through a separate
connection.

Paper pages render the resolved paper record. These bounded reads do not
change the catalogue. Pages show an inline PDF preview when available,
link to the preferred TEI reading view, and expose metadata alternatives
and file provenance in expandable details. Artifact access uses the UID and the
manifest, never the absolute index paths. Only listed filenames can be
opened, inside a confined paper subtree. File symlinks are refused.

PDF responses stream with single-range and HEAD support. The TEI reader
renders section hierarchy, contents navigation, abstracts, emphasis, lists,
glossaries, tables with headers and spans, notes, captions and citations with
return links. Both citation directions preserve the sidebar. Bibliographies
separate authors, titles and publication details. DOI links open separately.
Raw references retain their original evidence in an expandable section.
Catalogue metadata may replace raw references only after DOI verification.

MathML is rendered through a fixed vocabulary. TeX and LaTeX formulae use
the locally bundled KaTeX distribution to produce native MathML. No CDN,
remote fonts or trusted TeX commands are used. Invalid formulae retain their
source. Remote figure images load on user activation. Missing local figure
assets are labelled unavailable. The renderer cannot recover missing source
images, mathematical structure or incorrect citation targets.

`--journal-links FILE.jsonl` loads cached OpenAlex source links. Each object
contains `source_id` and either `journal`, `doi`, or both. For example:

```json
{"journal":"Example Journal","source_id":"S123"}
{"doi":"10.1000/example","source_id":"S123"}
```

Journal names match ignoring ASCII case and repeated whitespace. Ambiguous
names or DOI/source assignments remain unlinked. DOI entries can confirm DOI
boundaries in concatenated raw references even when the cited paper is absent
from the catalogue. Cache files are supplied by an agent or metadata import.
No ClickHouse scan or external metadata request runs while rendering a page.
Only loaded mappings are used. Restart the server after replacing the file.

Input, output, node count and nesting are bounded. Unsafe URLs, executable
elements and source CSS are omitted. Unknown TEI elements retain readable
text. Source repair remains separate from display formatting.

The standalone browser has no account system. Keep its loopback listener
or place an authenticated reverse proxy in front of it when sharing it.

## Use a library

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
let archive = Taposaur_archive.open_root ~sw
    Eio.Path.(env#fs / "/data/papers/taposaur") in
let uid = Result.get_ok
    (Taposaur.Uid.of_string "pmc:PMC4460142") in
match Taposaur_archive.find archive uid with
| None -> ()
| Some paper ->
    let manifest = Taposaur_archive.manifest paper in
    List.iter
      (fun (a : Taposaur.Manifest.Artifact.t) -> print_endline a.file)
      manifest.artifacts
```

Link `taposaur`, `taposaur.archive` and `eio_main` for this example.
Database and HTTP libraries are not linked by the archive reader.

For resolved values, link `taposaur.reader` and call
`Taposaur_reader.load paper`. For queries, open a
`Taposaur_catalog.open_read` handle and use `Catalog.Query.make` with
`Catalog.search`. `Catalog.find_index` supplies the original row to a
resolver. `Catalog.find` returns the public summary instead.

## Dependencies and provenance

An opam search of XML, SQLite and TEI packages informed the choices:
Jsont for declarative codecs, Xmlm for pull parsing, Htmlit for escaped
HTML, Cmdliner for commands, and sqlite3-eio for database work that does
not block Eio fibers. All are already available in this workspace.
Uriz supplies the workspace's portable URI percent codec. No additional
vendored dependency or package installation is needed.

DOI handling, UID handling, manifest vocabulary/codecs, authors, index
codecs and TEI header extraction were adapted from
`~/ocaml/taposaur`, revision
`4230fdcba57a5a5bebb624557417a1e7d4651b31`, under ISC. The imported files
were clean at extraction. The archive contract is
`/data/papers/u/etap/taposaur/FORMAT.md`, checked against production samples.

Local changes separate codecs from I/O and writers, adapt Jsont callbacks
to OxCaml, validate versions and confined filenames, use 64-bit artifact
sizes, require the format's mandatory index members, check UID/scheme
agreement, and bound TEI flow parsing. The archive, catalogue and Proffer
libraries are new. Production service processes and source files are
unchanged.

## Fetch into staging

Fetch one paper using metadata discovery, or supply an explicit URL:

```sh
taposaur fetch '10.1000/example' --staging /local/staging/batch-1
taposaur fetch '10.1000/example' --staging /local/staging/batch-1 \
  --url https://example.org/article.pdf --format pdf
```

Automatic discovery uses `GLUTTON_SERVER_URL`, with `DATACITE_SERVER_URL`
optional for missing DOI records. Supported publisher routes are Elsevier,
Springer, Wiley, Taylor & Francis, PLOS and Cambridge University Press.
Credentials live in the user's encrypted [Sekrit](../../../wip/sekrit/README.md)
keychain under these names, written with `sekrit put NAME --file FILE`:

| Credential | Keychain name | Environment fallback |
| --- | --- | --- |
| Elsevier API key | `taposaur/elsevier/api-key` | `ELSEVIER_API_KEY` |
| Elsevier institution token | `taposaur/elsevier/inst-token` | `ELSEVIER_INST_TOKEN` |
| Springer API key | `taposaur/springer/api-key` | `SPRINGER_API_KEY` |
| Wiley tokens, comma-separated | `taposaur/wiley/tokens` | `WILEY_TOKENS` |
| CORE API token | `taposaur/core/token` | `CORE_API_TOKEN` |

`taposaur jobs credentials` reports which are present and from where,
without printing values. Taylor & Francis admits only a whitelisted
address: set `TANDF_PROXY` to a proxy URL such as
`socks5h://127.0.0.1:1080`, with an SSH tunnel (`ssh -N -D 127.0.0.1:1080
HOST`) to that address, and its requests travel through it. The keychain overrides the environment. `sekrit
init` creates the vault and master key once per user. Library callers
supply configured service capabilities through `Taposaur_secrets` or
directly. No credentials are read from dotenv files implicitly.

Publisher clients space requests two seconds apart, or thirty seconds for
Cambridge, independently of metadata lookups. Busy publisher responses
(429, 500, 502, 503, 504) receive up to three retries, with five-minute waits
outside the transfer deadline. Library callers can override the retry policy
and supply their own pacing middleware. `--timeout` bounds each transfer,
including its body (120 seconds by default); metadata lookups use at most
15 seconds. `--max-bytes` bounds each artifact (128 MiB by default).

Springer defaults to XML; `--publisher-pdf` also tries its PDF route using
network entitlement. PLOS attempts both formats. Cambridge resolves the DOI
to an article page before requesting its PDF. CORE prefers download URLs;
its output-ID fallback first requires metadata with the requested DOI.
Springer XML response wrappers are retained and must contain exactly one
article. Live entitlement checks remain outstanding.

`Taposaur_services.Grobid` and `Pub2tei` provide explicitly configured clients
for PDF extraction and publisher XML conversion, using the shared Fetch HTTP
libraries. They return HTTP responses for callers to validate. Conversion
jobs, output publication and regeneration remain separate operations.

Files appear beneath `STAGING/doi/PREFIX/SUB/SUFFIX`, with `paper.json` and
`events.jsonl`. A PMCID resolved to a DOI is staged under that DOI. The writer
accepts a dedicated local staging tree and refuses existing papers created
by other writers. It does not merge into `papers/` or update archive indexes.

Transfers stream to a temporary file. Publication requires a PDF signature
and end marker, or well-formed publisher XML with a recognised root. These
checks reject common login/error pages but do not establish bibliographic
identity or complete PDF structural validity. Each artifact has a SHA-256
digest. Identical retries reuse the original artifact; changed bytes receive
another filename. Acquisition failures remain events with typed CLI outcomes.

A per-paper lock excludes concurrent writers. Publication syncs a pending
intent, finishes the artifact, appends the event and atomically replaces the
manifest. Reopening recovers a partially appended event without duplicating it.
The writer's projector handles acquisition events emitted by this package.
Acceptance, merging, legacy event replay and conversion are future modules.

Service clients were adapted from production under ISC. See
[service provenance](services/PROVENANCE.md) and [roadmap](ROADMAP.md).

## Consolidate review PDF headers

`taposaur-consolidate --snapshot 2 --reviews-db reviews.db --db catalog.db
--work /local/soil-headers --root /data/papers/taposaur` regenerates archived
PDFs through GROBID with full header consolidation and reference consolidation
disabled. The default server is `http://localhost:8070`, with two requests in
flight. `--doi DOI` runs one member as a pilot. Reuse the same work directory
to resume. Papers without PDFs are reported and left unchanged.

Publication requires well-formed TEI, an exact header DOI, a nonempty publisher
and explicit Glutton header provenance. HTTP responses and source PDF hashes
remain in the work directory. Successful conversions preserve the previous TEI,
manifest and events there before replacing the GROBID artifact. Publication
appends conversion and artifact events, updates the manifest and refreshes
the catalogue. Retain this directory for recovery and audit. Other archive
writers must be stopped during the batch. Read-only servers and indexing can
continue. Index JSONL shards and existing review snapshots remain unchanged.
Create a new review snapshot after the run to capture the refreshed metadata.

## Acquire and convert review papers with durable jobs

`taposaur jobs` plans work from a frozen review snapshot, records it in a
local [Duraml](../../../wip/duraml/README.md) SQLite queue and serves that
queue with explicit worker processes. The library is `Taposaur_jobs`, the
archive writer is `Taposaur_accept` and the shared lock is `Taposaur_lock`.
No review membership or snapshot changes. The web UI shows jobs read-only.

```sh
taposaur jobs init --queue-db /local/taposaur-jobs.db
taposaur jobs plan 2 --reviews-db reviews.db --root /data/papers/taposaur
taposaur jobs submit 2 --reviews-db reviews.db --root /data/papers/taposaur \
  --queue-db /local/taposaur-jobs.db --by avsm2 --doi 10.1000/pilot
taposaur jobs work --queue-db /local/taposaur-jobs.db \
  --staging /local/taposaur-staging --root /data/papers/taposaur \
  --db catalog.db --acquire elsevier --convert pub2tei --convert grobid \
  --refresh --sync
taposaur jobs inspect --queue-db /local/taposaur-jobs.db --counts
```

Planning reads every snapshot row in DOI order and decides each member from
the catalogue's cached holdings, or the frozen snapshot for unindexed
members, reading archive manifests only without a catalogue or for TEI
input digests: already held, acquire through an implemented
publisher strategy, convert a held PDF or XML, unsupported publisher,
unconfigured credentials or invalid identifier. A member whose observation
names no publisher, typically one the archive does not hold, is routed by
the owner of its DOI prefix (10.1016 Elsevier, 10.1007 and 10.1023
Springer, 10.1371 PLOS, 10.1002, 10.1111 and 10.1046 Wiley, 10.1080
Taylor & Francis, 10.1017 Cambridge); the strategy still resolves the DOI
itself, so a prefix whose journal has moved publisher fails safely as a
404. Pass `--db` so planning
avoids reading thousands of manifests from the archive filesystem.
`--strategy NAME`, repeatable, restricts a submission to some strategies.
Every submission names a Duraml dispatch lane, `snapshot:N` for a sweep
and `doi` for `--doi` requests unless `--lane` says otherwise; successors
inherit it. Duraml dispatches oldest-first within a lane and takes turns
across lanes, so a paper requested during a sweep runs after at most one
job of the sweep rather than behind all of it, and a retry keeps its age.
`taposaur jobs cancel --queue NAME` revokes every queued job of one queue,
leaving running attempts to finish. The counts are printed and
each decision is one JSONL line. Publishers without a
strategy are queued on the `oa` strategy, which uses the Glutton
open-access link and CORE fallbacks, when `GLUTTON_SERVER_URL` is set.
`--target tei` plans conversions instead of downloads. Credentials come from the same keychain as
`taposaur fetch`. `GROBID_SERVER_URL` and `PUB2TEI_SERVER_URL`, or `--grobid`
and `--pub2tei`, decide which conversion routes are configured.

Keys are semantic: `acquire:UID:STRATEGY:vREVISION`,
`convert:UID:ROUTE:INPUT_SHA256:CONVERTER` and
`refresh:UID:MANIFEST_SHA256`. Submitting the same work again returns the
existing job. The same acquisition requested by another review or actor is
reported as a conflict and the existing job is reused. Change `--revision`
to repeat acquisitions deliberately and `--converter-revision` on the worker
to repeat conversions. Acquisition payloads name the paper, strategy,
revision, publisher evidence, requester and review snapshot. Conversion and
refresh payloads are exactly their keys, because that work is shared between
reviews. Credentials and service clients are worker inputs only.

`work` serves one queue per `--acquire STRATEGY`, `--convert ROUTE` and
`--refresh`, with `--concurrency` handlers each. It holds a lock file beside
the queue database for every served queue, so a second process for the same
publisher is refused rather than doubling the request rate. Publisher
pacing is the same as `taposaur fetch`, which the queue lock does not
exclude, and shared services such as Glutton, CORE and open-access hosts are
paced per process. Busy responses return the job to the queue with the
server's `Retry-After`, clamped between thirty seconds and one day, instead
of sleeping on a leased worker. Downloads go through the staging writer, are
validated, accepted into the archive and followed atomically by conversion
and catalogue refresh jobs. A replayed acquisition whose paper already holds
a PDF or XML succeeds without fetching, and a replayed conversion reuses the
recorded TEI, because publisher and converter bytes are not stable across
runs. Conversions reduce a Springer Nature API envelope to its JATS article
before Pub2TEI, validate well-formed TEI with the paper's DOI before
acceptance, retry HTTP 429, 502, 503 and 504, timeouts and transport
failures, and record other refusals in the manifest as permanent. Once a
publisher's own source has refused a paper, a transient failure on the
open-access or CORE fallbacks no longer retries the whole acquisition. A
permanent acquisition failure, other than missing credentials, is likewise
recorded in the paper's archive manifest as an `acquisitions_failed` entry
with the strategy, HTTP status and reason, creating an artifact-free paper
directory when the paper is new. The paper page lists these refusals, and
`taposaur jobs plan --skip-refused` reads manifests to leave such members
out of the next batch. A later success through that strategy clears it. Refreshes
rewrite the paper's index row, import its shard and resolve its metadata.
The first interrupt drains active handlers, the second aborts.

Acceptance holds `papers/.../.lock` with `flock` and writes PID, host and
component metadata into it for diagnosis. The metadata is never the
authority, and a symbolic link at the lock path is refused. The lock is
coherent across hosts that mount the same CephFS archive. Existing
artifacts are never overwritten: identical content is reported as existing,
different content receives a numbered filename, and final names are linked
into place so even a writer outside this protocol cannot be replaced. Only
missing title, publisher, year, PMID and PMCID values are filled. Every
publication writes a durable intent first, so reopening the paper completes
an interrupted copy, append or manifest replacement without duplicates; a
publication that never made anything visible is abandoned and retried.
`taposaur-consolidate` and the staging writer take the same lock, and
consolidation refuses a paper with a pending acceptance intent.

`--sync` or `taposaur jobs audit-sync` copies durable queue events into the
audit database as `queue.event` notices, keyed by store identity and
sequence, with a locked cursor file beside the queue. Repeating a delivery
into the same audit database adds no duplicates; the cursor is not bound to
an audit database, so keep one audit database per queue. `taposaur-web --jobs-db FILE` opens the queue read-only for
`/jobs` and `/job?id=N`, which link jobs to papers and review snapshots.
`/activity` is a live tab. It opens on a snapshot of the jobs still
running and the most recently touched ones, read from the tail of the
queue's event log, then polls every two seconds for new events only, so
its cost does not grow with the size of the queue. A button loads older
pages back through the log. The status strip shows queued and running
totals. Each top-level job is a pipeline card titled by its paper and
review snapshot.
Jobs staged by another nest beneath it, using the parent that Duraml
records on every successor, and each job row shows its queue, state and
one chip per attempt with its outcome and duration, updating in place.
Website requests sit in a collapsed section; the audit trail has its own
page at `/audit`. `/job?id=N` lists what staged a job and what it staged, its
queue timeline from the bridged events, and every audited operation its
handlers ran, nested as it happened: handler records carry the job and
attempt plus a note with the retry reason or failure code, and transfer
and conversion results carry HTTP statuses, so a retry can be traced to
its cause. Job state icons mark the activity rows and job tables. Website requests are held in memory by
the running server, so they start empty on restart. Submission and
cancellation remain CLI operations with audit records.

Create a new review snapshot after a run to capture refreshed holdings.
Use a local disk for the queue and staging, not Ceph or NFS.

## Catalogue measurement

The synthetic benchmark requires a new database filename:

```sh
opam exec --switch=5.2.0+ox -- dune exec --profile release-check \
  u/etap/taposaur/bench/catalogue.exe -- /tmp/catalogue-benchmark.db 250000
```

It reports import time and warm p50/p95 latency for statistics, browsing and
an FTS term shared by every paper. See [benchmark results](bench/README.md).
Run `python3 u/etap/taposaur/test/http_smoke.py` from the repository root to test
the built CLI against synthetic loopback HTTP responses.
A full production import and corpus-wide enrichment remain to be measured.

## Shared website widgets

The `taposaur.ui` library supplies Htmlit tables, badges, metrics, coverage
indicators and native form controls with shared styles and browser sorting.
See [the component interface](ui/taposaur_ui.mli) and
[usage notes](ui/README.md). Sorting applies to the displayed page. Search
results have a persistent “Show abstracts” preference.

## Operational audit

Mutation-capable CLIs now write a separate SQLite audit log, default
`taposaur-audit.db`, with actor `system`. Set `TAPOSAUR_AUDIT_DB` to share an
absolute path across jobs. See [the audit design and operations guide](audit/README.md)
and [OCaml interface](audit/taposaur_audit.mli). Use `taposaur audit query`
for JSONL or start the website with `--audit-db FILE` for `/audit`.

## Recover missing publishers

`taposaur-publishers` fills missing review publishers through exact DOI
queries to configured Crossref and DataCite mirrors, with an original CSV
fallback. It retains raw evidence, logs operations and publishes a new
immutable snapshot. See [configuration, CLI and OCaml API](mirrors/README.md).

## Recover missing abstracts

`taposaur abstracts fetch DOIS` looks up abstracts in OpenAlex for the listed
DOIs, batching up to 100 per request and falling back to free single-work
reads when the daily budget refuses a batch, and retains every response in
`taposaur-abstracts.db` (or `TAPOSAUR_ABSTRACTS_DB`). The resolver applies
that evidence only when no archive document or index row has an abstract;
enrichment, refresh workers and the website consult the store, `taposaur
abstracts fill` updates catalogue rows that lack one without re-reading the
archive, and `taposaur review snapshot` carries it to members the archive does
not hold. See [costs, keys and the OCaml API](abstracts/README.md).
