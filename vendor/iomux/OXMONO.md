# OxMono import

Imported iomux.0.4 at `c2c89ea0198e8119f75766359211d5a3d961535a`.
`OXMONO.json` records the release archive URL and checksums, which match
opam-repository's `iomux.0.4`. The directory is the release archive,
unchanged. It differs from the `v0.4` tag only in the version stamps that
`dune subst` adds to `dune-project` and `iomux.opam`.

## Scope and adaptations

Upstream poll and ppoll bindings, unchanged. Eio's POSIX backend needs them.
Without this library, Dune's `select` in `../eio/lib_main` silently builds
`posix_backend.disabled.ml`, and `EIO_BACKEND=posix` then fails with "The
POSIX backend was disabled at compile-time".

## Refresh

1. Fetch the recorded release and the proposed replacement. Compare their
   library sources and dependency declarations.
2. Verify the downloaded archive checksum against opam-repository and record
   its commit in `OXMONO.json` and `../upstreams.json`.
3. Build with `release-check` in the `5.2.0+ox` switch and confirm that
   `_build/default/vendor/eio/lib_main/posix_backend.ml` comes from
   `posix_backend.enabled.ml`.
4. Run `dune build --profile release-check --force
   @@vendor/iomux/test/runtest`, then
   `_build/default/vendor/eio/tests/test_upstream_refresh.exe`, which
   exercises both Linux backends. Recursive vendor aliases skip tests.
