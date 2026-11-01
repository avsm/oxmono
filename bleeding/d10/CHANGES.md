# Changes

- Expose uncached installation into a caller-prepared prefix through `run_node`,
  allowing frontends to rerun tests without capturing a successful layer.

- Document public cache and executor contracts and declare direct dependencies.

- Export resolved plans as standalone Makefile builds through `D10ir.Makefile`,
  adapted from oi with build-path rebasing and static-build environment support.

- Share recipe execution across staging and cached permanent prefixes, with
  safe writable assembly, content-based layer capture and source replay.

- Give d10 and d10.ir their own project under `bleeding/d10`, preserving library names.

- Expose the standalone opam `.install` file handler through `D10ir.Install_file`.

## OxMono import

Import d10 and d10.ir with OxCaml adaptations and Fetch downloads.
Rebase cached prefix metadata and isolate cached prefixes by platform.
