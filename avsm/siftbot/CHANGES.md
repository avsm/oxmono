Siftbot now shares embedding inference, vector storage primitives and hybrid ranking with the retrieval library, preserving existing index identities.

# Changes

- Page reconciliation IDs and check thread membership directly in SQLite.
  Bind ordinary embedding queries as compact float32 vectors.

- Add a local JMAP corpus, resumable import, vector retrieval, bounded model
  suggestions and Bonsai review with revision checks, explanations and undo.
- Add synthetic fixtures, frozen baseline evaluation, source-linked overviews
  and explicit Dooit capture with durable deduplication.

- Add a terminal profile picker and setup form for JMAP URLs, masked bearer
  tokens, account selection and chat models, available with `review --setup`.
