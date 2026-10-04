# Changes

- Use d10 platform keys to separate local caches by distribution and version.
  Existing cache entries are retained but builds use new keys.

- Add `ox dist pkg` to export standalone source bundles and osdist packaging
  for Debian, Ubuntu, Fedora and Alpine, with optional Docker Compose builds.

- Delegate recipe execution, prefix restoration and installed-file capture to
  d10, retaining permanent local prefixes and existing package cache keys.

- Accept `ox run --from=URL#REV -- BINARY ARG...`, with cached branch, tag or
  commit selection and `--refresh` for branch updates.

- Use one recipe build path for compilers and packages, removing `--compiler-prefix`.
  Restore package prefixes from day10 metadata and keep smaller run receipts.

- Build the OxCaml toolchain and package dependencies as reusable day10 layers,
  with in-process solving, offline restoration and no opam CLI or switches.

- Add `ox stamp` snapshot versions and `ox run` with Git dependency fetching,
  OxCaml compiler isolation, local opam overlays and a per-user binary cache.
