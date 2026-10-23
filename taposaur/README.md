# Taposaur

Read, search and browse an existing Taposaur paper archive. This first
milestone exposes read-only archive operations, a rebuildable SQLite
catalogue, a CLI and a Proffer website. The archive format stays unchanged.

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

The executables are `_build/default/taposaur/bin/main.exe` and
`_build/default/taposaur/bin/server.exe`. Installed names are `taposaur`
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

Size and modification time identify unchanged files. `--force` reloads
them, including after changes to parsing or projection code. Limits count
changed shards, so repeating a limited import advances through the index.
Missing source shards retain their existing rows. There is no implicit
retirement operation.

Normal imports list all index shards to record discovery coverage. That
listing can be substantial on the production filesystem. `--shard` reads
one named file directly and does not claim complete coverage. A partial
catalogue must not be interpreted as a complete inventory of holdings.

Search uses exact canonical identifiers or literal words across titles,
authors, journals, publishers and abstracts. It uses UID ordering and a
UID cursor, with pages capped at 100 records. SQL and FTS operators in
user text have no special meaning. Publisher and journal filters are exact
matches. Whole-document text search is outside this milestone.

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
`--jobs`. Cancellation leaves completed papers available. Catalogues using schema versions 1 or 2 migrate when opened for writing,
preserving existing cached metadata. `taposaur enrich --limit 0` upgrades
a catalogue without scheduling any metadata reads.

Search returns `Taposaur.Summary.t`, containing a UID, bibliography and
holdings flags. The CLI serializes this summary as JSONL. The search page
renders titles, authors, journal, publisher and abstract snippets. Its metadata filter selects papers with or without a title,
abstract or publisher. `stats` and the homepage show availability counts
and enrichment progress. These counts describe the loaded catalogue.

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
XML's `coredata`, and S2ORC JSON's bibliographic fields. XML headers are
bounded at 1 MiB and JSON at 16 MiB. Unsupported XML dialects and malformed
documents remain visible as metadata errors. PDFs are previewed, not
parsed for metadata. The format readers are `Document.Tei`,
`Document.Elsevier` and `Document.S2orc`. All return `Bibliography.t`. Raw events remain accessible through history and
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

PDF responses stream with single-range and HEAD support. TEI text is
rendered through a fixed HTML vocabulary with escaped text. Source URLs,
attributes and executable elements are not copied. Input, output, nesting
and history display sizes are bounded. The renderer is a reading view,
not a facsimile of the publisher's layout.

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
`/data/papers/taposaur/FORMAT.md`, checked against production samples.

Local changes separate codecs from I/O and writers, adapt Jsont callbacks
to OxCaml, validate versions and confined filenames, use 64-bit artifact
sizes, require the format's mandatory index members, check UID/scheme
agreement, and bound TEI flow parsing. The archive, catalogue and Proffer
libraries are new. Production service processes and source files are
unchanged.

## Next modules

The next milestone extracts the production publisher and metadata clients
into `services/`, using a caller-supplied Fetch capability, then builds
single-paper downloading into staging. `store/` will own event append,
projection, locking and atomic publication. Conversion, review state and
persistent jobs follow as separate libraries with CLI entry points before
web integration. The current package has no archive writer, downloader,
conversion service client or Cap'n Proto dependency.
