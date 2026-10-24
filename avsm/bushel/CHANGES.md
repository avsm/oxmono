# Changes

## Unreleased

- Share lazy provider initialization safely across fibers and move synchronous
  source reads off the scheduler. Use the supplied Eio clock for latest posts.

- Reject malformed date filters and invalid CLI responses explicitly.

- Serialize embedding configuration updates so text and image settings cannot
  overwrite each other during concurrent writes.

- Add dedicated agent tools for searching Bushel site content and listing
  published posts by original publication date without feed synchronization.

- Bound the MLX buffer cache during text and image indexing to prevent memory
  growth across batches. Preserve completed passages after a forced termination.

- Avoid reading MLX model files for keyword search, source reads and contact
  lookups. Load the provider only for semantic or hybrid search.

- Skip image outputs narrower or shorter than 32 pixels and report their count.
  Keep completed batches when indexing is interrupted and resume on rerun.

- Index shared image outputs once when JPG and PNG sources generate the same
  WebP filename, retaining source metadata and cached vectors across reordering.

- Show image model catalogues, indexing coverage and search metadata through
  separate `knowledge images` commands, with readable output and `--json`.

- Print readable knowledge results by default, with `--json` for scripts.
  Index all remaining passages unless `--max-batches` sets a limit.

- Show indexing coverage and remaining passages on terminal stderr, with
  `--no-progress` to disable the display.

- List, select and download pinned MLX embedding models from `bushel knowledge`,
  reusing the Hugging Face cache and defaulting to Qwen3-Embedding-4B.

- Select local MLX embeddings with shared model paths and query instructions,
  alongside existing HTTP embedding configuration.

- Configure, index and search shared Bushel/Sortal embeddings with `bushel
  knowledge`. Applications reuse one corpus index and provider configuration.

- Index knowledge graph adjacency, contact identities and refresh URL lookups.

- Add allocation-free nullable lookup to immutable string maps.

- Registering a release again keeps its summary and its registries.

- Show registered releases as one line each in the arod notes view, inside the
  month they were made, with links to the release page and to ecosyste.ms.

- Load `releases.yml` into the arod context.

- Add `bushel release list|discover|add|refresh`. A release is registered from
  its GitHub or tangled release, with the registries that carry it found
  through ecosyste.ms.

- Attach package registries to a release through ecosyste.ms.

- Parse GitHub releases and Tangled artifacts into release candidates.

- Add a `[releases]` section to the configuration.

- Replace `Bushel.Release` with a forge release that carries a one-line summary
  and the registries it reached.
