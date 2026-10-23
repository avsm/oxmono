# Unreleased

- Share document bodies and facets across retrieved passages. Encode ordinary
  vectors directly as float32 SQLite BLOBs.

- Reduce allocation when chunking long documents and indexing large batches.
- Normalize embedding vectors with fewer traversals and no intermediate float
  allocations.
- Require OxCaml on OCaml 5.2 or later.

- Terminate passage chunking on malformed UTF-8 without discarding bytes, and
  normalize tiny nonzero embedding vectors without underflow.
- Scan only enough documents to fill the indexing budget and reuse each
  inference input's hash when writing its vector.
- Version the corrected normalization as v2. Existing embedding indexes are
  refreshed as indexing runs within its configured batch budgets.
