## Unreleased

- Find the C library through pkg-config, falling back to the Homebrew
  prefixes, so the build works on macOS out of the box.
- Initial libzstd bindings: one-shot compression and decompression into
  caller-supplied bigstrings, frame probing and error names returned as
  stack-allocated values.
