Siftbot now shares embedding inference, vector storage primitives and hybrid ranking with the retrieval library, preserving existing index identities.

# Changes

- Use the vendored Console 0.9 views and monotonic progress displays.

- Retrieve relevant examples from all manual corrections, with optional cached
  semantic matching, visible provenance and selective `--refresh-feedback`.

- Correct attention and delivery with `l`, for one message, sender or List-ID.
  Saved rules apply to existing and future mail without deleting model results.

- Show coloured labels in a dedicated message-list column. Toggle thread
  grouping with `t`, preserving selection and matching keyboard navigation.

- Give review a full-height message list and wider reading pane with coloured
  labels. Move topics behind `g` and show classifier explanations with `e`.

- Load review labels only for displayed messages. Add an animated loading bar,
  bordered panes, highlighted selections and a larger message list.

- Bound encoded classification input, including headers and recipients, while
  preserving useful mail headers and leaving room for validation retries.

- Classify four emails concurrently by default, configurable with `--jobs`.
  Save each result and its caches atomically, with resumable cancellation.

- Add incremental `classify --all` with live labels and subject previews.
  Scan pending source IDs and load bodies individually for full-mailbox runs.

- Constrain classification output with JSON Schema. Validate model replies,
  retry malformed output once and report specific failures.

- Show classification batch progress, elapsed time and remaining work, with
  plain output and clean cancellation that preserves completed results.

- Support custom OpenRouter-compatible chat endpoints such as Sequoia without
  an API key. Keep hosted credentials isolated and include endpoints in caches.

- Sync to completion with phase progress and resumable cancellation. Keep
  large imported mail visible in review by loading selected bodies on demand.

- Allow syncing when the JMAP session advertises a separate HTTPS API host
  or an API URL with a query, retaining read-only request restrictions.

- Repair terminal exits and stalled setup requests. Add `setup`, cursor editing,
  backwards navigation and scrolling that keeps the selected field visible.

- Support calendar lookbacks such as `learn --since 6m` and explicit dates.
  Resolve shared Sekrit bearer references and preserve them during setup edits.

- Page reconciliation IDs and check thread membership directly in SQLite.
  Bind ordinary embedding queries as compact float32 vectors.

- Add a local JMAP corpus, resumable import, vector retrieval, bounded model
  suggestions and Bonsai review with revision checks, explanations and undo.
- Add synthetic fixtures, frozen baseline evaluation, source-linked overviews
  and explicit Dooit capture with durable deduplication.

- Add a terminal profile picker and setup form for JMAP URLs, masked bearer
  tokens, account selection and chat models, available with `review --setup`.
