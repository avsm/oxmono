# Changes

- Share recipe execution across staging and cached permanent prefixes, with
  safe writable assembly, content-based layer capture and source replay.

- Give d10 and d10.ir their own project under `bleeding/d10`, preserving library names.

- Expose the standalone opam `.install` file handler through `D10ir.Install_file`.

## OxMono import

Import d10 and d10.ir with OxCaml adaptations and Fetch downloads.
Rebase cached prefix metadata and isolate cached prefixes by platform.
