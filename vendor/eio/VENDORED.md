# Vendored Eio provenance

This directory is based on upstream `main` after Eio 1.5, with local OxCaml
patches. The exact revision below was the latest upstream main when fetched.
This record covers `eio`, `eio_main`, `eio_linux`, `eio_posix` and `eio_windows`.

| Field | Value |
| --- | --- |
| Upstream repository | <https://github.com/ocaml-multicore/eio> |
| Upstream branch | `main` |
| Upstream revision | [`0ee73e48b566e7cd09cd3c1fc08ef1da199558b0`](https://github.com/ocaml-multicore/eio/commit/0ee73e48b566e7cd09cd3c1fc08ef1da199558b0) |
| Upstream version (`git describe --tags --abbrev=7`) | `v1.5-5-g0ee73e4` |
| Upstream commit date | 2026-09-05 |
| Refresh date | 2026-09-07 |
| Provenance last verified | 2026-09-07 |

## Import history

- 2026-08-06: [original import](https://github.com/avsm/oxmono/commit/ec798b1517442f3dd2378c26f1a92184bd21c2e8)
  of `af471dfb5ed007279e3bc86eaaad4690fe6659d2` (`v1.4-17-gaf471df`).
  All 345 imported files matched that upstream snapshot, including file modes.
- 2026-09-07: refresh to `0ee73e48b566e7cd09cd3c1fc08ef1da199558b0`
  (`v1.5-5-g0ee73e4`), incorporating all upstream changes since the original
  base while retaining the local patches below.

The current base includes the complete [v1.5 release](https://github.com/ocaml-multicore/eio/releases/tag/v1.5),
including `Eio.Net.connect ?bind_to ?options`, plus the subsequent
`Eio.Process.Env` API and documentation index. The upstream [changelog](CHANGES.md)
still starts at v1.5; use the exact revision above to identify this development
snapshot. Dependency constraints in the opam files do not identify the vendored
source version.

## Local patch set

- `ebcc086d07ffdc272c25fa5a201ee3e7390ba90d`: OxCaml portability annotations
  for the fiber core and public interfaces, effect and shared-state adaptations,
  and a portable callback requirement for `Domain_manager.run`, with an explicit
  `unsafe_run` escape hatch.
- `a26b70c6ecc1285ba635103353e5c97350ca1d5a`: remove an obsolete repro reference
  from a Resource implementation comment.
- `425513abac8639a57cf9c809a4a7e0ed06ce0286`: adapt Flow, Resource and backend
  buffer handling to the shared Cstruct implementation, with local-flow tests.
- [Clock implementation](lib_eio/time.ml): use the vendored portable Mtime
  interface directly; the earlier compatibility casts have been removed.
- [Network interface](lib_eio/net.mli): retain portable `connect` with the new
  optional arguments and the existing portable error-handling adaptation.
- [Test dependencies](tests/dune): explicitly include the vendored package
  closure for MDX, avoiding incompatible installed interfaces.
- [Refresh regression](tests/test_upstream_refresh.ml): check connection
  options, source binding and `Process.Env` through Linux and POSIX backends.
- [Mainloop dependencies](lib_main/dune): declare `eio.unix` and `fmt`
  directly, with matching package dependencies in `dune-project`. Upstream
  obtains them only through optional backends. When none is available,
  `eio_main.mli` otherwise fails with `Unbound module Eio_unix`. Verified with
  all backend selections disabled in an isolated build, unchanged POSIX
  smoke-test output before and after the patch, and Crowthebot's tests.

These patches do not advance the upstream base. The complete local history is
available with `git log -- vendor/eio`; include working-tree changes when
comparing against upstream.

## Updating this vendor

On each upstream refresh, update the exact upstream revision, its tag-based
description, the refresh date, import history and verification date above. Review
and retain the required OxCaml patches, and update this patch summary. Record
individual backports separately without advancing the base revision unless
the complete upstream snapshot has been incorporated. Update the corresponding
entry in [../upstreams.json](../upstreams.json) too. Keep this file when replacing
the upstream tree. The shared checking workflow and validation limitations are
in [../README.md](../README.md).

On macOS and other POSIX systems, the current backend requires `iomux >= 0.2`.
The vendored sources do not install their external dependencies. An older
installed `eio_posix` may not have brought in `iomux`. Install it in the build
switch before compiling applications:

```sh
opam install --switch=5.2.0+ox 'iomux>=0.2'
```
