# Bushel

Personal knowledge base and research entry management for OCaml.

Bushel is a library for managing structured research entries including notes,
papers, projects, ideas, videos, and contacts. It provides typed access to
markdown files with YAML frontmatter and supports link graphs, markdown
processing with custom extensions, and search integration.

## Features

- **Entry Types**: Papers, notes, projects, ideas, videos, and contacts
- **Frontmatter Parsing**: YAML metadata extraction using `frontmatter`
- **Markdown Extensions**: Custom `:slug`, `@handle`, and `##tag` link syntax
- **Link Graph**: Bidirectional link tracking between entries
- **Eio-based I/O**: Async directory loading with Eio

## Subpackages

- `bushel`: Core library with entry types and utilities
- `bushel.eio`: Eio-based directory loading
- `bushel.config`: XDG-compliant TOML configuration
- `bushel.sync`: Sync pipeline for images and thumbnails
- `bushel.knowledge.store`: Shared local retrieval store and provider settings
- `bushel.knowledge.service`: Typed search, source and contact queries
- `bushel.agent`: Bounded knowledge and contact tools

## Installation

```bash
opam install bushel
```

## Usage

```ocaml
(* Load entries using Eio *)
Eio_main.run @@ fun env ->
let fs = Eio.Stdenv.fs env in
let entries = Bushel_loader.load fs "/path/to/data" in

(* Look up entries by slug *)
match Bushel.Entry.lookup entries "my-note" with
| Some (`Note n) -> Printf.printf "Title: %s\n" (Bushel.Note.title n)
| _ -> ()

(* Get backlinks *)
let backlinks = Bushel.Link_graph.get_backlinks_for_slug "my-note" in
List.iter print_endline backlinks
```

## CLI

The `bushel` binary provides commands for:

- `bushel list` - List all entries
- `bushel show <slug>` - Show entry details
- `bushel stats` - Show knowledge base statistics
- `bushel sync` - Sync images and thumbnails
- `bushel paper <doi>` - Add paper from DOI
- `bushel config` - Show configuration
- `bushel init` - Initialize configuration
- `bushel knowledge` - Configure, index and query Bushel and Sortal knowledge

## Releases

`bushel release` records code releases made on GitHub or tangled, with a
one-line summary, in `releases.yml` beside `links.yml`. arod shows each as a
line in the notes view. The date comes from the forge. ecosyste.ms says which
package registries, such as PyPI and opam, carry the version.

- `bushel release discover [--repo REPO] [--since DATE]` lists releases you
  published that are not registered.
- `bushel release add REPO TAG [--summary TEXT] [--project SLUG] [--force]`
  registers one. `TAG` is the git tag on GitHub and the version on tangled.
- `bushel release refresh [--days N]` attaches registries that have gained a
  registered version since.
- `bushel release list` shows what is registered.

```
$ bushel release add ucam-eo/geotessera v0.10.2
Registered ucam-eo/geotessera 0.10.2 (2026-09-04): Python library interface to the Tessera geofoundation model embeddings
  pypi.org: https://pypi.org/project/geotessera/0.10.2
```

Configure it in `config.toml`. `github_user` is the login whose GitHub releases
count as yours. Set `GITHUB_TOKEN` to raise the GitHub rate limit.

```toml
[releases]
github_user = "avsm"
github = ["mirage/ocaml-cohttp"]
tangled = ["anil.recoil.org/dune-rpc-eio"]
registries = ["pypi.org", "opam.ocaml.org"]

[releases.projects]
"ucam-eo/geotessera" = "tessera"
```

See `RELEASE-SPEC.md` for the design.

## License

ISC License. See [LICENSE.md](LICENSE.md).

## Knowledge retrieval and agent tools

`bushel.knowledge` indexes a local Bushel/Sortal snapshot using the shared
`retrieval` library. It reads entries, saved links, contacts and cached feeds,
with typed identities for Bushel slugs, vCard UIDs, feed URL/item-ID pairs and
external URLs. Contact handles remain lookup aliases. Recorded graph edges
carry a relationship and evidence; semantic similarity does not create edges.
Use `Bushel.Link_graph.all_internal_links` when target kinds matter. A contact
and an entry with identical names are separate targets.

`bushel.agent` supplies seven typed `Agentkit.Agent.Tool` definitions and a
`bind` function for direct use. Its `invoke` function returns bounded JSON for
hosts such as Crowthebot that own dispatch and auditing. Supply an authorization
and cancellation `check` and serialize calls sharing a corpus. Embedding settings
belong to Bushel and are separate from Crowthebot's chat model.

`bushel_search` searches the owner's website, defaulting to notes/blog posts.
Its `kind` can select papers, projects, ideas or videos. It excludes contacts
and syndicated feeds before ranking. `bushel_latest_posts` lists up to 100
published notes by original publication date, excluding drafts, future dates
and index pages. Read full sources with `knowledge_get`. Neither tool requires
feed synchronization. `knowledge_search` remains the combined corpus search.

Applications use `bushel.knowledge.store` to read a published snapshot.
Readers keep one consistent SQLite generation while indexing proceeds in
other processes. `knowledge refresh` publishes changed local sources without
embedding them. `knowledge index` refreshes sources before indexing. Source
manifests avoid reparsing unchanged files. A missing index is initialized once.

`bushel.knowledge.service` supplies typed requests and results for native
callers. `bushel.agent` adds Agentkit tool schemas and JSON dispatch. Keep
invocations inside their store callback:

```ocaml
let runtime = Retrieval_mlx.Runtime.create ~sw in
Bushel_knowledge_store.with_store ~runtime ~fs ~clock ~fetch ~check (fun store ->
    let service = Bushel_knowledge_service.of_store store ~max_bytes:65536 in
    Bushel_knowledge_service.call service ~check
      (Bushel_knowledge_service.Latest_posts 0))
```

The runtime retains text and vision models until its application switch closes.
`Retrieval_mlx.Runtime.release runtime` explicitly releases idle weights.
Omitting `runtime` creates a session-scoped runtime that releases weights when
the callback returns. Search reads existing vectors and reports coverage. It
never starts document indexing.

The `retrieval` package contains a corpus-independent embedding backend,
HTTP provider, vector validation, incremental batching and reciprocal-rank
fusion extracted from Siftbot. Siftbot retains its authoritative email store
and existing vector-index identities through a backend adapter. The generic
`Retrieval.Index` supplies a separate SQLite FTS5/chunk/vector store for Bushel.
It preserves full source text in UTF-8 passages, rejects stale vector writes,
and filters before ranking. `replace` reconciles an entire snapshot atomically;
its input is not an incremental batch. Model, dimensions, normalization and
input hashes control embedding reuse.

### Shared knowledge commands

The CLI and Crowthebot use the same Bushel configuration and retrieval database.
Knowledge commands show readable summaries, model tables and search results.
Errors and progress go to stderr. Add `--json` to any command for
machine-readable JSON on stdout, including errors. Failures return a nonzero
exit status.
The store commands and `configure` accept `--config FILE` to select another
Bushel TOML configuration.

```sh
bushel knowledge configure \
  --url https://embeddings.example/v1/embeddings \
  --model your-embedding-model --dimensions 768 \
  --api-key-file ~/.config/bushel/embedding-key
bushel knowledge status
bushel knowledge index
bushel knowledge search 'research notes' --mode lexical
bushel knowledge search 'distributed storage' --mode hybrid --kind note
bushel knowledge search 'research' --tag systems --since 2025-01-01
bushel knowledge contacts 'Alice'
bushel knowledge get SOURCE_ID --offset 0
```

`configure` updates `[embeddings]` while retaining other Bushel settings. The
configuration contains a key-file path, never the API key. Store the key in that
file with mode `0600`. Relative key paths supplied to `configure` are saved as
absolute paths. Hand-written relative paths resolve beside the config file.
HTTPS is required when a key file is configured. Omitting
`--api-key-file` configures a provider without credentials.

```toml
[embeddings]
url = "https://embeddings.example/v1/embeddings"
model = "your-embedding-model"
dimensions = 768
api_key_file = "~/.config/bushel/embedding-key"
```

Local MLX inference requires an Apple Silicon Mac running macOS 26 or later.
Building the native backend uses Swift 6.3 and the macOS SDK. Python and Ollama
are not required at runtime. See the
[native package](../../bleeding/apple-mlx-embeddings/README.md) for build setup.

List the pinned model catalogue, sizes and local cache status, then configure
the default Qwen3-Embedding-4B model at its full 2560-dimensional width:

```sh
bushel knowledge models
bushel knowledge configure --backend mlx
```

`configure` reuses the Hugging Face snapshot cache or downloads the selected
revision with `hf`, falling back to `uvx hf`. Install either downloader before
configuring an uncached model. Download progress goes to stderr. Invalid
settings are rejected before downloading, and a failed download leaves the
configuration unchanged. Inference loads the saved local directory without
network requests, Python or Ollama.

Select `qwen3-0.6b` (1024 dimensions), `qwen3-4b` (2560, the default), or
`qwen3-8b` (4096) with `--model`. The catalogue's full repository identifiers
are also accepted. `--dimensions` can request a smaller output width:

```sh
bushel knowledge configure --backend mlx --model qwen3-0.6b
bushel knowledge configure --backend mlx --model qwen3-4b --dimensions 1024
```

`models` shows each model's dimensions, download size and cache status, with the
default marked. `models --json` also includes the repository, pinned revision
and snapshot directory. Listing models does not download anything. The cache
follows `HF_HUB_CACHE`, then
`HUGGINGFACE_HUB_CACHE`, then `HF_HOME/hub`, otherwise the Hugging Face cache
under `XDG_CACHE_HOME` or `~/.cache`.

To use an existing local model without any download, provide `--model-dir` and
its dimensions. This option is mutually exclusive with `--model`:

```sh
bushel knowledge configure --backend mlx \
  --model-dir ~/models/Qwen3-Embedding-4B --dimensions 2560
```

```toml
[embeddings]
backend = "mlx"
model_dir = "~/models/Qwen3-Embedding-4B"
dimensions = 2560
max_tokens = 8192
```

MLX dimensions must be between 32 and the model's output width, at most 4096.
`--max-tokens` accepts 32 through 32768 and defaults to 8192.
`--query-instruction` overrides the instruction used for search queries. The
default is "Given a search query, retrieve relevant
passages from personal notes, papers, feeds, and contacts." Documents are
embedded without that instruction. Instructions must be at most 8192 bytes and
contain no NUL. An empty instruction disables the query prefix. Relative model
directories follow the same rules as key paths: `configure` stores an absolute
path, while hand-written paths resolve beside the config file. `configure`
validates and saves settings without loading the model into GPU memory.

Omitting `backend`, or setting it to `"http"`, retains the HTTP configuration
above. Switching backend removes settings belonging to the previous backend.

Settings are read on each invocation, including each Crowthebot knowledge tool
call. The corpus combines the configured Bushel data directory with the native
Sortal store, contacts, and cached feeds. `refresh` and `index` ingest local
files. Search, get and status read the published snapshot. Crow checks source
manifests before each invocation to include newly added or edited posts.
These commands do not synchronize either data source or fetch new feeds. Native
Sortal configuration and its environment overrides still select the Sortal paths.

The shared database lives at
`$XDG_DATA_HOME/bushel/knowledge/<corpus-id>/knowledge.sqlite3`, with
`~/.local/share` as the default XDG data home. The corpus ID identifies the
canonical Bushel and Sortal roots, so clients using the same sources share one
index. `status` reports the actual database path, generation, document and
passage counts, provider model and dimensions, and current embedding coverage.
There is no separate CLI database or data-directory override.

Lexical search works without embeddings. Semantic and hybrid search require a
configured provider. `index` processes all remaining passages by default. Use
`--max-batches N` to cap an invocation at 1 through 10000 batches. Its summary
shows new passages, elapsed time and corpus coverage, with a resume command if
work remains. `index --json` retains the machine-readable `newly_indexed` count
and current coverage. CLI HTTP provider requests have a 60-second timeout.
Applications supply their own Fetch client for HTTP providers.

Completed passages are saved incrementally. Rerun `knowledge index` after an
interruption to resume missing passages. Preserve the database and its
`-wal` file after a forced termination so SQLite can recover committed work.
Local MLX inference limits reusable buffers to 256 MiB. Model weights and
active batches use additional memory.

`index` shows progress on terminal stderr, starting while sources and model
files are checked. The bar counts all indexed passages, including cached ones,
and advances after each stored batch. Reaching `--max-batches` reports the
remaining passages. A fully cached index reports that it is up to date.
`--no-progress` disables the display. Redirected stderr and `TERM=dumb` are
quiet. The final summary remains visible. `--json` preserves JSON results on
stdout.

Search accepts `--kind`, `--tag`, `--contact`, `--project`, `--since`, `--until`,
and `--offset`. The contact filter takes a Sortal handle. Dates use
`YYYY-MM-DD`. Search results show titles, excerpts, URLs and copyable `get`
commands. `get` prints source text. Contacts retain ambiguous matches for you
to choose. Follow the displayed pagination hint with the same command and
filters to continue. Machine-readable pages are bounded to 65536 bytes and
include `next_offset` for continuation.

Existing per-profile Crow knowledge indexes remain on disk but are no longer
used. Run `bushel knowledge index` to populate the shared index. Source files and
legacy index files are not removed.

### Image retrieval

Image embeddings use a separate Qwen3-VL model and index. The source is the
Srcsetter `index.json` and its referenced local pixel files in the existing
`[images].images_output_dir`. Point that setting at your generated image output:

```toml
[images]
images_output_dir = "~/bushel/images-web"
```

The example expects `~/bushel/images-web/index.json`. Generate that index with
your Srcsetter image pipeline before indexing. Configure and index images
independently of the text embedding provider:

```sh
bushel knowledge images models
bushel knowledge images configure
bushel knowledge images index
bushel knowledge images status
bushel knowledge images search 'a rocky coastline'
bushel knowledge images get IMAGE_ID
```

`images configure` selects `qwen3-vl-2b` by default and downloads its pinned
checkpoint with `hf` or `uvx hf` when it is absent from the Hugging Face cache.
The download is about 1.8 GB and separate from the text model. Output dimensions
default to 2048, with 64 through 2048 supported for this model.

Settings are saved in `[image_embeddings]`, independently of `[embeddings]`.
To use an existing local vision model without downloading, pass `--model-dir`
and `--dimensions`, or configure it directly:

```toml
[image_embeddings]
backend = "mlx"
model_dir = "~/models/Qwen3-VL-Embedding-2B"
dimensions = 2048
max_tokens = 8192
```

The CLI accepts plain text queries and searches indexed still-image pixels.
It does not provide video search. Inference uses local model and tokenizer
files. Image vectors remain separate from text-only Qwen3 embeddings, even
when the configured dimensions are equal.

Image results show titles, source URLs and local pixel paths, with a copyable
`images get` command for metadata. Status and indexing summaries count images.
`--json` selects machine-readable output for these commands too. Image indexing
processes all remaining images unless `--max-batches N` limits the invocation.
Outputs narrower or shorter than 32 pixels are skipped and counted in the
summary. Rerunning indexes only new or changed images, preserving completed
batches after interruption. Removing an image from the manifest removes it
from search on the next refresh.
