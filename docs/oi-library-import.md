# OxMono import and review

Imported oi 0.14.2 at `ca8c59ff26bd7350e909324dc804982f0e4cee5b` from
`~/src/git/avsm/oi`. The source checkout was clean and was not modified.
Each project records its import scope in `oxmono/upstream.json`:

| Project | Libraries | Upstream scope |
| --- | --- | --- |
| [osrel](../bleeding/osrel/README.md) | `osrel` | [manifest](../bleeding/osrel/oxmono/upstream.json) |
| [d10](../bleeding/d10/README.md) | `d10`, `d10.ir` | [manifest](../bleeding/d10/oxmono/upstream.json) |
| [osdist](../bleeding/osdist/README.md) | `osdist` | [manifest](../bleeding/osdist/oxmono/upstream.json) |

## Scope

The import contains `osrel`, `d10`, `d10.ir` and `osdist`. It excludes
`lib/oi`, `lib/cmd`, `lib/d10ir/cmd`, executables, registry configuration,
release scripts and tool integration tests. Fifteen independent osdist tests
were extracted from the tool suite. Its four CLI signing-key tests are omitted.
Generic remote-read and archive APIs remain in d10 and d10.ir for compatibility.
No default remote registry or S3 publisher is configured by these libraries.

## Adaptations

- Replace Requests and Nox RNG dependencies with workspace Fetch/Curl.
  Downloads stream, follow redirects, require a 2xx response, report byte
  progress and retain the timeout controls. Sessions reuse libcurl connections.
  `Accept-Encoding: identity` preserves archive bytes and content length.
  Caller cancellation propagates. An interrupted download can leave a partial
  destination, which a caller must not publish as a completed cache entry.
- Supply Git's non-interactive environment to Sysops child processes instead
  of calling `Unix.putenv` during library initialization.
- Adapt Jsont absent values to factories, use its portable string map, and
  declare only the layer-hash conversions needed by portable codecs portable.
  These interfaces are compiler checked. Other APIs keep their original modes.
- Assemble cached prefixes through `Layer.restore` so the upstream
  `dune-package` relocation pass also runs during prefix assembly. Include
  `os_key` in the prefix path to prevent reuse across platforms.
- Expose the existing `.install` file handler as `D10ir.Install_file` for
  callers building into permanent prefixes. Its implementation is unchanged.
- Retry interrupted `waitpid` calls in the upstream lock test harness.
- Keep upstream `OI_*` controls and layer metadata names. Ox defines its own
  configuration boundary.
- Add permanent-prefix and single-node execution to the shared IR executor.
  Detach writable prefixes, compare file content and modes, reject dependency
  deletions, preserve overlay order and record recipes for local replay.

## Dependencies

New vendors are opam-core/opam-format 2.5.2, opam-file-format 2.2.0,
ocamlgraph 2.2.0, sha 1.15.4, swhid_core 0.1, patch 3.1.2 and
Dockerfile/Dockerfile-opam 8.3.9. Seven source trees supply these nine packages.
Each has `OXMONO.json`, `OXMONO.md`, a license and an upstream manifest entry.
Upstream whitespace in source, licenses and golden fixtures is preserved.
The staged whitespace check reports 73 unchanged vendor lines.
Release archives were verified against opam checksums, retained sources were
compared against those archives, and release commit IDs were resolved upstream.

Opam has explicit callback wrappers for OxCaml's local argument modes.
The other new library implementations match their upstream release sources.
Optional GUI tools and packaging CLIs are omitted. SHA's optional OUnit2
tests are excluded from workspace traversal. Existing workspace libraries supply the remaining dependencies.
The ox frontend vendors the generic opam-0install and 0install-solver libraries.
Opam state/repository libraries are not required.

## Review findings for ox

The initial review identified the following constraints. The shared executor
now provides permanent prefixes and safe layer capture for ox.

1. `Layer.store` and `Prefix.assemble_cached` rely on caller serialization.
   Upstream supplies this in the omitted oi harness. Ox must own a cache lock
   and serialize fibers as well as processes before using these writes.
2. Ordinary layer files are hardlinked. `Prefix.prepare` detaches writable
   build inputs. Callers must keep completed
   store entries immutable by convention. A cache cleanup must not delete a
   prefix in use by a running command.
3. The default IR policy uses staging directories. Its permanent-prefix policy
   retains stable final paths for compilers and packages that embed them.
   Rewriting `dune-package` alone does not relocate binaries, META files,
   stubs, scripts or runtime data paths.
4. Upstream layer hashes cover effective opam metadata and supplied dependency
   closures. Ox must additionally account for compiler build/configuration,
   source and local overlay contents, platform, build flags and absolute paths.
5. `Prefix.diff` now compares contents, modes and symlink targets, including
   replacements that preserve timestamps. Deleted dependency files are rejected.
6. Registry memo tables and Curl sessions are not advertised as portable.
   Use one Eio domain with concurrent fibers. Do not share these objects across
   domains or add blanket portable annotations.
7. Osdist retains oi's distribution targets, including its stock-OCaml Alpine
   builder image. Packaging generation is tested, not an OxCaml deployment path.

The ox runner addresses these cache and prefix constraints as described in
[ox-plan.md](ox-plan.md).

## Validation

The dev and `release-check` library builds and 34 Alcotest cases cover platform
normalization/detection, JSON defaults and round trips, cross-process locks,
layer storage/restoration, cached-prefix rebasing and platform separation,
HTTP behavior/cancellation, child environments and osdist generators.
An additional differential fixture compares opam parsing, effective metadata,
MD5/SHA256/SHA512 and child environment behavior with stock OCaml and
opam-format 2.5.2. Its baseline is
[`bleeding/d10/test/compat/opam.expected`](../bleeding/d10/test/compat/opam.expected).

Build and test the projects in the workspace's OxCaml environment:

```sh
dune build --profile release-check \
  @bleeding/osrel/all @bleeding/d10/all @bleeding/osdist/all
dune runtest --profile release-check --force \
  bleeding/osrel bleeding/d10 bleeding/osdist
```

The explicit vendor suites pass for the opam parser (64 cases), patch,
swhid_core, ocamlgraph, Dockerfile and Dockerfile-opam:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check --force \
  @@vendor/opam-file-format/tests/runtest \
  @@vendor/patch/test/runtest @@vendor/swhid_core/test/runtest \
  @@vendor/ocamlgraph/tests/runtest \
  @@vendor/ocaml-dockerfile/test/runtest \
  @@vendor/ocaml-dockerfile/test-opam/runtest
```

The scoped `@fmt` gate checks Dune formatting. OCaml formatting is disabled
without a project configuration, so the adapted codecs and new HTTP code were formatted explicitly
with the switch's OxCaml-aware ocamlformat.

Executor tests cover permanent and staging builds, parallel scheduling,
restoration, relocation of a simple shell program, archive replay and failed
capture. Validation was on macOS arm64. Linux execution, TLS against an external
server and native package installation remain untested.

## Refresh

Compare the recorded oi revision with the replacement before copying the
scoped directories. Preserve the patches above and local tests. Re-extract
only the osdist library tests from the tool suite. Rebuild with `release-check`,
force the library tests and explicit vendor suites, and update the recorded
revision only after the complete scoped change has been incorporated.
