# Corpus fixtures

Manifest, event and index snapshots read on 2026-10-23 from
`/data/papers/taposaur` for these papers:

- `doi:10.1016/j.ijer.2018.10.003`
- `pmc:PMC4460142`

The snapshots exercise fetched and migrated artifacts, converter metadata,
missing optional fields, and differing event outcome shapes. Tests never
open the paths embedded in them. Full texts and PDFs used in the tests
are synthetic strings created in the test's temporary directory.

`elsevier-coredata.xml` retains only the retrieval wrapper and bibliographic
`coredata` from the publisher XML for
`doi:10.1016/0001-4575(69)90059-1`. It reproduces missing index metadata
without including the article body.

`catalog-v2.sql` preserves the SQLite schema before the architecture review.
Migration tests populate it with synthetic raw and enriched metadata, then
check that both remain usable after upgrading.
