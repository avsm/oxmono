# Changes

- Accept `ox run --from=URL#REV -- BINARY ARG...`, with cached branch, tag or
  commit selection and `--refresh` for branch updates.

- Use one recipe build path for compilers and packages, removing `--compiler-prefix`.
  Restore package prefixes from day10 metadata and keep smaller run receipts.

- Build the OxCaml toolchain and package dependencies as reusable day10 layers,
  with in-process solving, offline restoration and no opam CLI or switches.

- Add `ox stamp` snapshot versions and `ox run` with Git dependency fetching,
  OxCaml compiler isolation, local opam overlays and a per-user binary cache.
