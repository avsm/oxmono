# Changes

- Build the OxCaml toolchain and package dependencies as reusable day10 layers,
  with in-process solving, offline restoration and no opam CLI or switches.

- Add `ox stamp` snapshot versions and `ox run` with Git dependency fetching,
  OxCaml compiler isolation, local opam overlays and a per-user binary cache.
