# Changes

- Limit reusable MLX buffers to 256 MiB for sustained text and image inference.
  Expose allocator statistics and test memory use across repeated batches.

- Add local Qwen3-VL text and image embeddings with a separate pinned 2B
  model catalogue, native image preprocessing and GPU regression tests.

- Add native Qwen3 embedding inference through MLX Swift, with local model
  loading, normalized float32 vectors and Eio integration.
