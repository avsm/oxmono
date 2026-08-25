# zstdz - Zstandard for OxCaml

Bindings to the system [libzstd](https://facebook.github.io/zstd/) (1.5 or
later). The API is one shot rather than streaming: the caller sizes the
destination and `Zstdz.compress` and `Zstdz.decompress` write into it, so a
codec pipeline can hand its own buffers straight to the compressor.

- Buffers are `Base_bigstring.t`, so the data is off heap and the stubs read
  it without copying.
- The runtime lock is released around calls that move more than 64 KiB, which
  lets other domains run during a large compression.
- `Zstdz.frame_info` and `Zstdz.error_name` return values allocated in the
  caller's stack region, so probing a frame or naming an error does not touch
  the minor heap. `Zstdz.content_size` is checked `[@zero_alloc]`.

## Build

    dune build @bleeding/zstdz/all @bleeding/zstdz/runtest
