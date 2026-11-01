# apple-mlx-embeddings

Native Qwen3 text and Qwen3-VL image embeddings for OCaml through MLX Swift.
Inference runs on the Apple GPU in the calling process. It needs no Python
runtime, Ollama or HTTP server. Setup can download a named, pinned checkpoint into the Hugging
Face cache. Inference reads model and tokenizer files from that local snapshot.

## Build

Use an Apple silicon Mac running macOS 26 or later, Swift 6.3 or later, and
the macOS SDK from Xcode or the command-line developer tools.

```sh
dune build @bleeding/apple-mlx-embeddings/all
dune runtest bleeding/apple-mlx-embeddings --force
```

The first build fetches the Swift dependencies pinned in
`lib/swift/Package.resolved` and a checksummed MLX shader archive. The
shader is embedded in the native library, so executables can move after
installation. A Metal shader compiler is not required. The archive adds
about 182 MB of shader data to linked executables. See
[SHADERS.md](lib/SHADERS.md) for its version and provenance.

Swift build artifacts normally live under Dune's build directory. Set
`APPLE_MLX_SWIFT_BUILD_DIR` to an absolute directory to share this cache
between Dune profiles. Runtime shaders are cached under
`$XDG_CACHE_HOME/apple-mlx-embeddings`, or the user's macOS cache directory
when that variable is absent. The first inference creates this cache.

Other platforms build an unavailable backend. They can still use the
retrieval HTTP provider.

## Use from OCaml

```ocaml
Eio_main.run @@ fun env ->
let module Model = Apple_mlx_embeddings.Model in
let selected = Model.default in
let model_dir =
  Model.ensure ~proc:(Eio.Stdenv.process_mgr env)
    ~stderr:(Eio.Stdenv.stderr env) selected
in
let model =
  Apple_mlx_embeddings.load ~model_dir ~max_tokens:8192
in
Fun.protect ~finally:(fun () -> Apple_mlx_embeddings.close model) (fun () ->
  let vectors =
    Apple_mlx_embeddings.embed model ~dimensions:selected.dimensions
      [| "A paper about compiler locality." |]
  in
  Printf.printf "%g\n" vectors.{0, 0})
```

The model directory needs Qwen3 embedding weights in safetensors format,
`config.json`, `tokenizer.json` and `tokenizer_config.json`. Both ordinary
and MLX-quantized checkpoints are supported by the underlying loader.
`load` only reads local files. `Model.ensure` handles explicit downloads.
Neither operation runs code from model repositories.

Outputs are normalized float32 Bigarrays. Batches contain up to 16 texts.
Inputs use right padding and the final non-padding token. Dimensionality
reduction takes the vector prefix and normalizes it again. Calls on a
model are serialized and run outside the Eio domain. Cancellation waits
for an active GPU batch to finish before it is raised. Keep the model open
to reuse its weights across requests.

## Text and image embeddings

`Apple_mlx_embeddings.Vision` embeds text queries and local images into the
same Qwen3-VL vector space. It uses separate weights from the text-only model.
Keep vision vectors in a separate index, even when their dimensions match
text-only vectors.

`Model.vision_all` lists the vision catalogue. `Model.vision_default` selects
`qwen3-vl-2b`, the pinned `mlx-community/Qwen3-VL-Embedding-2B-4bit` checkpoint.
It is a separate download of about 1.8 GB and has 2048 output dimensions.
`Model.find_vision` accepts its short name or full repository name.
`Model.ensure` uses the same Hugging Face cache and download mechanism as text
models. `Vision.load` and both embedding operations only read local files.

```ocaml
Eio_main.run @@ fun env ->
let module Model = Apple_mlx_embeddings.Model in
let module Vision = Apple_mlx_embeddings.Vision in
let selected = Model.vision_default in
let model_dir =
  Model.ensure ~proc:(Eio.Stdenv.process_mgr env)
    ~stderr:(Eio.Stdenv.stderr env) selected
in
let model = Vision.load ~model_dir ~max_tokens:8192 in
Fun.protect ~finally:(fun () -> Vision.close model) (fun () ->
  let instruction = "Represent the user's input." in
  let queries =
    Vision.embed_text model ~instruction ~dimensions:selected.dimensions
      [| "a rocky coastline" |]
  in
  let images =
    Vision.embed_images model ~instruction ~dimensions:selected.dimensions
      [| "/path/to/coast.jpg" |]
  in
  let similarity = ref 0. in
  for column = 0 to selected.dimensions - 1 do
    similarity := !similarity +. queries.{0, column} *. images.{0, column}
  done;
  Printf.printf "Cosine similarity: %g\n" !similarity)
```

Vision batches contain one or two inputs and return normalized float32
Bigarrays. The default model supports 64 through 2048 dimensions. Reduced
vectors use the leading dimensions and are normalized again. Instructions
are at most 8192 bytes. An empty instruction selects
`Represent the user's input.`. Text inputs are truncated to the token limit.

Image inputs are regular local files of at most 64 MiB, with at most 100
million decoded pixels and neither edge above 16384 pixels. Preprocessing
resizes within 4096 to 1843200 pixels. An image that exceeds the configured
token budget fails without truncating patch tokens. Calls on a handle are
serialized, and cancellation waits for an active GPU batch to finish.

## Model setup and cache

The catalog contains pinned MLX-quantized Qwen3 embedding checkpoints:

| Name | Full vector dimensions | Approximate download |
| --- | ---: | ---: |
| `qwen3-0.6b` | 1024 | 351 MB |
| `qwen3-4b` (default) | 2560 | 2.28 GB |
| `qwen3-8b` | 4096 | 4.27 GB |

`Model.all` lists the catalog, `Model.default` selects the 4B checkpoint, and
`Model.find` accepts a catalog name or its full repository name. Arbitrary
remote repositories are not accepted. Each entry exposes its name, repository,
pinned revision, dimensions and required files with their sizes.

`Model.cache_dir ()` resolves the Hugging Face cache in this order:

1. `HF_HUB_CACHE`
2. `HUGGINGFACE_HUB_CACHE`
3. `$HF_HOME/hub`
4. `$XDG_CACHE_HOME/huggingface/hub`
5. `$HOME/.cache/huggingface/hub`

`Model.dir selected` returns the pinned snapshot path. `Model.present selected`
checks whether it is complete. `Model.ensure ~proc ~stderr selected` returns
that path immediately when the snapshot is complete, without starting a
command or using the network. These three functions accept `?cache_dir` to
override the cache root. Snapshots use the native Hugging Face layout and can
be shared with other tools.

When files are missing, setup uses an installed `hf` command, or falls back to
`uvx hf`. Install either `hf` or `uv` before the first download. The fallback
obtains the CLI in an ephemeral environment. This setup step uses the Python
Hugging Face CLI. Native inference needs neither that CLI nor a Python runtime.

## Shared Bushel configuration

List the model catalog, then configure the default 4B checkpoint:

```sh
dune exec avsm/bushel/bin/main.exe -- knowledge models

dune exec avsm/bushel/bin/main.exe -- knowledge configure \
  --backend mlx
dune exec avsm/bushel/bin/main.exe -- knowledge index
dune exec avsm/bushel/bin/main.exe -- knowledge status
dune exec avsm/bushel/bin/main.exe -- knowledge search \
  "papers about compiler locality" --mode hybrid
```

Knowledge commands print readable results by default. Add `--json` for scripts,
for example `knowledge models --json`. Indexing processes all remaining
passages.
Use `knowledge index --max-batches N` to limit one invocation.

Configuration downloads the selected checkpoint when necessary and derives
its dimensions from the catalog. To choose the smaller checkpoint, use
`knowledge configure --backend mlx --model qwen3-0.6b`. An existing complete
pinned snapshot is reused. To use a manually prepared local model instead:

```sh
dune exec avsm/bushel/bin/main.exe -- knowledge configure \
  --backend mlx --model-dir /path/to/qwen3-embedding --dimensions 2560
```

`retrieval.mlx` formats Qwen3 queries with an instruction and leaves
documents unchanged. The shared Bushel configuration supplies that
instruction, the token limit and output dimensions. Crow and other
Bushel consumers use the same configuration and database.

Bushel keeps vision settings in a separate `[image_embeddings]` section.
Its image commands read the Srcsetter `index.json` and local pixel files under
`[images].images_output_dir`:

```sh
bushel knowledge images models
bushel knowledge images configure
bushel knowledge images index
bushel knowledge images search "a rocky coastline"
```

Image configuration selects and downloads `qwen3-vl-2b` by default. These
commands leave the text embedding provider unchanged. Image search accepts a
plain text query and ranks indexed still images by their pixel embeddings.
There is no video search command.

The provider fingerprints model and tokenizer contents and includes
inference settings in the index identity. A model or setting change needs
reindexing. Long-running applications retain one model and reuse its
fingerprint. Separate CLI invocations each hash and load their own model.

The native runtime caps MLX's reusable free-buffer cache at 256 MiB, preserving
any smaller existing limit. Text and vision models share this process-wide
policy. This bounds retained scratch buffers across varied input lengths. It
does not cap active tensors or total process memory. Model weights and long
batches still require their own memory. `Apple_mlx_embeddings.memory_stats ()`
reports active, cached and peak active bytes plus the cache limit. These
counters exclude non-MLX allocations.

## GPU validation

Normal tests require no downloaded model. The optional inference test uses
a local Qwen3-Embedding-0.6B checkpoint with its full 1024 dimensions:

```sh
APPLE_MLX_TEST_MODEL=/path/to/qwen3-embedding-0.6b \
  dune build @bleeding/apple-mlx-embeddings/test/runtest-model --force
```

It checks batch padding against individual inference, Unicode, normalized
and reduced vectors, truncation, concurrent callers, cancellation and
closed handles. Set `APPLE_MLX_TEST_CACHE_RECOVERY=1` and `XDG_CACHE_HOME`
to an empty temporary directory to also test retry after cache creation
fails. To compare against the separate Python reference package:

```sh
dune exec bleeding/apple-mlx-embeddings/test/test_inference.exe -- \
  /path/to/qwen3-embedding-0.6b /tmp/native-vectors.json
HF_HUB_OFFLINE=1 TRANSFORMERS_OFFLINE=1 python3 \
  bleeding/apple-mlx-embeddings/test/compare_reference.py \
  /path/to/qwen3-embedding-0.6b /tmp/native-vectors.json
```

The comparison script needs `mlx-embeddings` and NumPy only for validation.
It never downloads a model. Quantized kernels can produce small numerical
differences across batch shapes and MLX versions.

The optional memory regression runs 64 four-row batches with varying token
lengths and verifies bounded active and cached memory plus stable vectors:

```sh
APPLE_MLX_TEST_MODEL=/path/to/qwen3-embedding-0.6b \
  dune build @bleeding/apple-mlx-embeddings/test/runtest-memory --force
```

Set `APPLE_MLX_MEMORY_TEST_BATCHES` between 64 and 10000 for a longer run.

The vision inference test uses a local Qwen3-VL-Embedding-2B checkpoint:

```sh
APPLE_MLX_VISION_TEST_MODEL=/path/to/qwen3-vl-embedding-2b \
  dune build @bleeding/apple-mlx-embeddings/test/runtest-vision --force
```

It checks text and image batches, image resizing, normalized and reduced
vectors, input validation, concurrent callers, cancellation and closed handles.
The fixture images are included in the repository. The test does not download
a model.

The wrapper is ISC licensed. Linked dependencies and the shader retain
their upstream licences in [THIRD_PARTY_LICENSES.md](THIRD_PARTY_LICENSES.md).
