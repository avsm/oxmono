# Vendored Eio provenance

This directory is based on the upstream Eio 1.6 release, with local OxCaml
patches.
This record covers `eio`, `eio_main`, `eio_linux`, `eio_posix` and `eio_windows`.

| Field | Value |
| --- | --- |
| Upstream repository | <https://github.com/ocaml-multicore/eio> |
| Upstream tag | `v1.6` |
| Upstream revision | [`1fc0efa41ccfb3818b09f54feec90ec29b47f1b6`](https://github.com/ocaml-multicore/eio/commit/1fc0efa41ccfb3818b09f54feec90ec29b47f1b6) |
| Upstream version (`git describe --tags --abbrev=7`) | `v1.6` |
| Upstream commit date | 2026-09-21 |
| Refresh date | 2026-10-03 |
| Provenance last verified | 2026-10-03 |

## Import history

- 2026-08-06: [original import](https://github.com/avsm/oxmono/commit/ec798b1517442f3dd2378c26f1a92184bd21c2e8)
  of `af471dfb5ed007279e3bc86eaaad4690fe6659d2` (`v1.4-17-gaf471df`).
  All 345 imported files matched that upstream snapshot, including file modes.
- 2026-09-07: refresh to `0ee73e48b566e7cd09cd3c1fc08ef1da199558b0`
  (`v1.5-5-g0ee73e4`), incorporating all upstream changes since the original
  base while retaining the local patches below.
- 2026-09-28: local portability patch
  `b7063e1750018e47b39a6ef038d0927ba2ad9ad1`, described below. The upstream
  base is unchanged.
- 2026-10-03: refresh to `1fc0efa41ccfb3818b09f54feec90ec29b47f1b6`
  (`v1.6`), retaining the local patches below. Upstream's removal of the
  unused `Switch.run_in` also removes its local effect-call adaptation.

The current base includes the complete [v1.6 release](https://github.com/ocaml-multicore/eio/releases/tag/v1.6).
See the upstream [changelog](CHANGES.md) for its environment, file descriptor,
symlink and Windows changes. Dependency constraints in the opam files do not
identify the vendored source version.

## Local patch set

- `ebcc086d07ffdc272c25fa5a201ee3e7390ba90d`: OxCaml portability annotations
  for the fiber core and public interfaces, effect and shared-state adaptations,
  and a portable callback requirement for `Domain_manager.run`, with an explicit
  `unsafe_run` escape hatch.
- `b7063e1750018e47b39a6ef038d0927ba2ad9ad1`: portability for fiber keys, `Io`
  tests, system threads, timeouts and native paths. No signature is
  tightened, since every closure argument keeps its legacy mode. The guard
  test is `bleeding/imap/test/vendor_modes/`, and each claim below was
  confirmed to fail it, or the Eio build, when removed.
  - [Fiber keys](lib_eio/core/fiber.ml): `'a key` is an unboxed record whose
    `Hmap.key` field carries `@@ portable contended`, and
    [the interface](lib_eio/core/eio__core.mli) declares it
    `value mod portable contended`. An Hmap key is an immutable identifier,
    so the identity coercions at creation and lookup are sound. The
    representation is unchanged.
  - [Io test](lib_eio/core/exn.ml): new `Exn.is_io : exn -> bool @@ portable`.
    A portable function can match an exception constructor only if its
    arguments cross portability, and this compiler rejects a kind on the
    extensible `err` with `The kind of type "err" is value non_float because
    it's an extensible variant type`. Portable code calls `is_io` instead.
    The patch adds a function and changes no existing behaviour.
  - [Paths](lib_eio/path.ml): `pp` and `native_exn` call `Format.fprintf` and
    `Format.asprintf` in place of `Fmt.pf` and `Fmt.str`, which Fmt defines
    as exactly those functions, and [the interface](lib_eio/path.mli)
    declares both `@@ portable`. Output is unchanged.
  - [Timeouts](lib_eio/time.mli): `with_timeout_exn` is declared
    `@@ portable`. Annotation only.
  - [System threads](lib_eio/unix/thread_pool.ml): `run_in_systhread`
    performs its effect through a portable `%perform` external, the same
    primitive as `Effect.perform`. The idle-thread timer registration moves
    unchanged into `schedule_drop`, asserted portable because `Zzz` and
    `Psq` are unannotated and it touches only the pool that the calling
    domain's scheduler returns. [Eio_unix](lib_eio/unix/eio_unix.mli) and
    [Thread_pool](lib_eio/unix/thread_pool.mli) declare it `@@ portable`.
    The closure it runs is not required to be portable, because a system
    thread shares the domain and its runtime lock.
  - [Sleep](lib_eio/unix/eio_unix.ml): `Eio_unix.sleep` performs through the
    same external and is declared `@@ portable`.
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
  options, source binding, `Process.Env`, environment snapshots, symlink
  rejection and imported file descriptor ownership through Linux and POSIX
  backends.
- [Mainloop dependencies](lib_main/dune): declare `eio.unix` and `fmt`
  directly, with matching package dependencies in `dune-project`. Upstream
  obtains them only through optional backends. When none is available,
  `eio_main.mli` otherwise fails with `Unbound module Eio_unix`. Verified with
  all backend selections disabled in an isolated build, unchanged POSIX
  smoke-test output before and after the patch, and Crowthebot's tests.

These patches do not advance the upstream base. The complete local history is
available with `git log -- vendor/eio`; include working-tree changes when
comparing against upstream.

## 1.6 validation

With the `5.2.0+ox` switch and `release-check` profile, the HTTPz, Fetch,
Proffer, IMAP, Maildir, SQLite, examples, Arod and Bushel builds pass.
Forced HTTPz, Fetch, Proffer, IMAP, Maildir and SQLite Eio tests pass,
including the IMAP portability guard. The HTTPz cookie test's filesystem
wrapper forwards the new required `follow` argument.

Run `tests/test_upstream_refresh.exe` and `tests/test_local_flow.exe` explicitly
through `dune exec`. Both pass. The refresh regression also passes against
pristine `v1.6` with OCaml 5.5.0 after removing its OxCaml-only portable
annotation. Its output matches the patched build byte for byte.

The workspace-wide `@all` is blocked by unavailable Apple Speech and Bonsai
dependencies on this Linux host. HTTPz's `@fmt` encounters existing Dune
formatting differences and an ocamlformat that cannot parse OxCaml syntax.
Windows and the optional MDX suites were not run. The upstream Windows test
file retains its CRLF endings, so use `git -c core.whitespace=cr-at-eol diff
--check` when checking this import.

## Updating this vendor

On each upstream refresh, update the exact upstream revision, its tag-based
description, the refresh date, import history and verification date above. Review
and retain the required OxCaml patches, and update this patch summary. Record
individual backports separately without advancing the base revision unless
the complete upstream snapshot has been incorporated. Update the corresponding
entry in [../upstreams.json](../upstreams.json) too. Keep this file when replacing
the upstream tree. The shared checking workflow and validation limitations are
in [../README.md](../README.md).

Verify the local patches through consumer aliases, since the vendored tests
are inert. `dune build @bleeding/imap/test/vendor_modes/runtest --force`
guards the portability claims of `b7063e17`, and the IMAP, Maildir and
sqlite3 `runtest` aliases exercise fiber keys, system threads and paths.

On macOS and other POSIX systems, the current backend requires `iomux >= 0.2`.
The vendored sources do not install their external dependencies. An older
installed `eio_posix` may not have brought in `iomux`. Install it in the build
switch before compiling applications:

```sh
opam install --switch=5.2.0+ox 'iomux>=0.2'
```
