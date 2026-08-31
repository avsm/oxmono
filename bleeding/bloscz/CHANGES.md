## Unreleased

- Find the C library through pkg-config, falling back to the Homebrew
  prefixes, so the build works on macOS out of the box.
- Initial C-Blosc1 bindings: the context interface only, compressing and
  decompressing into caller-supplied bigstrings, with frame inspection
  and the compressor list of the installed library.
