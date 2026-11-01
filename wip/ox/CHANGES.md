# Changes

- Reuse previous package versions and URLs when rebuilt distfile checksums
  match. Plan incremental exports with `--previous`, propagate dependency
  changes and skip exports with no changed packages.

- Version source releases as `BASE+ox-YYYYMMDD-EPOCH` using an existing Git
  tag. Add `ox release tag` and reject tags moved after planning.

- Remove Git subpaths from extracted source archives and fill missing port
  maintainers from the declared release metadata.

- Preserve Dune quoted-line strings when freezing source release metadata.

- Read committed project release bases from `release-versions.json`.

- Validate release plans and previous releases before export, and reject source
  links before rewriting metadata. Keep distinct project archive paths unique.

- Add isolated owner workspaces, owner templates, install bundles and Linux
  image commands using pinned minus39 repositories.
- Export deterministic committed source releases and cumulative opam overlays
  with explicit compiler, snapshot and upstream guard constraints.

- Add package and editable-workspace builds, tests, environments and source
  preparation, combining local opam definitions with OxCaml repository dependencies.

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
