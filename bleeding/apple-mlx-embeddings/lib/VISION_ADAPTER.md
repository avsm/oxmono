# Qwen3-VL embedding adapter

The native bridge uses MLX Swift LM 3.32.3, commit
`3b339ad6e3b3f44c8121ecff5131c7fd55e075e6`, from
<https://github.com/ml-explore/mlx-swift-lm>. Its MIT license is reproduced in
`../THIRD_PARTY_LICENSES.md`.

`build_swift.sh` appends `Qwen3VLEmbeddings.swift.inc` to the build checkout's
`Libraries/MLXVLM/Models/Qwen3VL.swift`. The original file must have SHA256
`406caff3f084e23a36f01872a62b7a43ba5dfd8c9d7193c61845911d70704056`.
The extension shares the upstream vision tower, patch projection, deepstack
features, M-RoPE positions and language transformer. It exposes the normalized
final hidden states before vocabulary projection. The embedding checkpoint
has no language-model head. The unused head is removed before strict weight
loading, which still verifies every inference parameter.

The adapter follows the Qwen3-VL embedding model's system/user chat template
and last-token pooling described in
<https://github.com/QwenLM/Qwen3-VL-Embedding/blob/main/src/models/qwen3_vl_embedding.py>.
Swift Transformers chat templating skips tokenizer postprocessors. The bridge
therefore appends the checkpoint's `<|endoftext|>` token once, after the full
prompt. Image-placeholder fragments are encoded without postprocessing.
Text truncation preserves this terminal pooling token. Images that exceed
the configured token budget are rejected without truncating patch tokens.

Rows are processed separately with no padding or cross-row position state.
Image bounds are 4096 to 1843200 pixels before patchification. CoreImage
decodes and resizes images. Its bicubic kernel differs from the Python
reference's Pillow kernel, so resized images have a separate parity threshold.
Outputs take the requested leading dimensions and are normalized in float32.
Image and text calls share a vision model's vector space. They do not share
the text-only Qwen3 embedding provider's space.

## Validation

The public checkpoint is `mlx-community/Qwen3-VL-Embedding-2B-4bit`, revision
`c713a965783416b9467b419f2d14ca6445847455`. Run `test/test_vision.exe` with
the local model directory, `test/vision-fixtures`, and an output JSON path.
Then run `test/compare_vision_reference.py MODEL_DIR OUTPUT_JSON` in an
environment with MLX embeddings and its Qwen3-VL processor dependencies.
The reference runs offline and asserts the template token IDs, final pooling
token, truncated length, vector shape and normalization. Python is used only
for this optional validation.

The fixture thresholds are cosine greater than 0.999 for text and images
without resizing, and greater than 0.995 for a gradient image resized from
101 by 73 to 96 by 64 pixels. Native tests also cover JPEG and lossless WebP,
dimension truncation, malformed inputs, serialization, cancellation and close.

When updating dependencies, refresh the lockfile, review the original-file
hash and extension against upstream, and rerun native tests and the offline
reference. Check that text truncation still preserves the pooling token and
that all model parameters are loaded without permitting missing weights.
