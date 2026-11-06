# Abstract and work evidence

`taposaur.abstracts` looks up exact DOIs in OpenAlex and retains the
responses in a separate SQLite database: an abstract when the work has one,
and the work's bibliography in every case. It does not fetch papers or
change archives. The public interface is
[taposaur_abstracts.mli](taposaur_abstracts.mli).

## Selection

An OpenAlex work carries its abstract as an inverted index (word to
positions); `reconstruct` rebuilds the text in position order and refuses an
empty or absurdly long result. `decode` accepts one work only when its DOI
matches the requested DOI exactly and the abstract reconstructs. `decode_work`
reads the same work's bibliography whenever the DOI matches: `title` (else
`display_name`), authors from each authorship's display name (split as
"Family, Given" or at the last space), the primary location's source as
journal, its `host_organization_name` as publisher and `issn_l` as ISSN,
`publication_date` and `publication_year`, volume, issue and first–last
pages, and PubMed identifiers taken from the last segment of their URLs
with a bare PMC number given its `PMC` prefix; the primary location's source
also becomes the work's `venue` (OpenAlex source id, name, ISSNs, type and
host). Both kinds of evidence record
the provider, endpoint, retrieval time and the SHA-256 of the retained
response body, which `record` rereads. The request `select` names exactly
the fields read.

`lookup_many client store dois` sends up to 100 DOIs as one filtered list
query. The body is retained once and every DOI gets an attempt row: `accepted`,
`not_found` (absent from the results), `missing_abstract`, or a failure name;
work evidence is stored for every DOI the results matched, with or without
an abstract, and a DOI is `known` (skipped by later lookups) once it has
work evidence. The store is at schema version 3: version 1 held abstracts
only, version 2 added the `works` table and version 3 the `sources` table;
a writable open migrates, a read-only version 1 store answers `find_work`
and `find_source` with `None`, and abstracts accepted at version 1 are
queried again so that their bibliography is retained too. `redecode`
rebuilds every work from its retained body, so a field the decoder learnt
later (the venue, say) reaches works fetched before it without a request.
`lookup_source client store key` reads one source by `issn:NNNN-NNNN` or
OpenAlex id, free of the credit budget, and retains it under that key.
`batches` splits a longer list by count and by encoded length, because
OpenAlex refuses request URLs over 8 KB and a percent-encoded SICI DOI runs
to 120 bytes. `lookup client store doi` is a single-work read, which 404s for
an unknown DOI (recorded as `not_found`). Rate limits (429) and budget
refusals (402, 403) store nothing, so those DOIs can be retried.

## Costs and keys

OpenAlex charges list queries at one credit each, about $0.10 per thousand,
and single-work reads nothing. Without a key the daily budget is $0.10,
shared by every caller on the network's address, at 10 requests a second; a
free key has $1 a day at 30 requests a second. `taposaur abstracts fetch`
therefore batches first and, when a batch is refused, continues with
single-work reads. Pass `--api-key-env NAME` to read a key from the
environment; the value is never printed or audited, and the audit records
only whether a key was used.

## Applying evidence

Evidence never replaces an abstract the archive supplies. The paper resolver
applies it through `Taposaur_paper.with_abstract` when no document or index
row has one, labelling the field's source with the provider and digest, and
caches it with the resolution. Enrichment, catalogue refresh workers and the
website apply the store named by `TAPOSAUR_ABSTRACTS_DB` (or
`taposaur-abstracts.db` in the working directory) in that way, so a refreshed
row keeps its filled abstract. `taposaur abstracts fill` updates catalogue
rows that lack an abstract without re-reading the archive, through
`Taposaur_catalog.Metadata.fill_abstract`, which also updates the search
index and the cached resolution in one transaction. Review snapshots take
`~abstracts` evidence for members whose frozen observation has none, catalogued
or not, and record the evidence label as the field's source, so a snapshot
can carry abstracts for members the archive does not hold, and `~works`
evidence, which `Taposaur_review.fill` applies to every blank bibliographic
field of a member's observation with the evidence label as that field's
source. `taposaur review corpus --evidence` applies both when exporting.

## CLI

```
taposaur abstracts fetch DOIS --store taposaur-abstracts.db [--retry] [--limit N] [--interval 0.1] [--api-key-env NAME]
taposaur abstracts redecode --store taposaur-abstracts.db
taposaur abstracts sources KEYS --store taposaur-abstracts.db [--interval 0.1] [--api-key-env NAME]
taposaur abstracts fill --store taposaur-abstracts.db --db catalog.db [--dois FILE]
```

`DOIS` lists one DOI per line (a `doi:` prefix, blank lines and `#` comments
are ignored). `fetch` skips DOIs with work evidence and, without `--retry`,
DOIs already attempted; a DOI with an abstract but no work evidence is
queried again. Progress goes to standard error every
thousand DOIs; the summary names how many were examined, accepted and held.
