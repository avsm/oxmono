# Run workflow review

The review covers executable selection, working-tree and owner commands,
repository precedence, request receipts, prefix restoration and the release
export path used by the recent repository-layout changes.

## Changes

- Binary discovery caches ownership by ordered repository paths and content
  digests. Metadata changes invalidate the index. Provider ambiguity and
  package-version shadowing retain their existing behavior.
- Warm preparation restores the combined dependency closure once. Missing
  permanent prefixes are still reconstructed even when the final run prefix
  already exists. This matters for absolute library and bytecode paths.
- `ox run --local BINARY` discovers the owning package and uses Dune's
  incremental build against editable files. An explicit `--with PACKAGE`
  handles ambiguous or generated binary declarations. Editable commands honor
  the checkout's compiler lock.
- Owner runs prepare the selected binary's root instead of every owner root.
  Owner runs and `ox exec --user` preserve the caller's directory. Local
  repository and overlay paths are resolved before entering staged sources.

## Checks

The regression suites cover metadata changes, ambiguity, repository order,
corrupt indexes, missing prefixes, dirty local source, arguments, working
directories, native and bytecode execution, source refresh, signals and
concurrent builds. Owner fixtures include an unrelated package with an
unsatisfiable dependency to detect accidental whole-owner resolution.

Release tests exercise deterministic exports, compiler guards, independent
package installation, unchanged archive reuse and immutable package versions.
The description cleanup does not change dependency formulas. The oi libraries
retain their source provenance after moving to `wip/`.

The optional lookup benchmark is:

```sh
python3 wip/ox/test/run_cache.py _build/default/wip/ox/bin/main.exe --benchmark
```

On this Mac, five warm-receipt samples over a synthetic 5,000-package local
repository gave medians of 0.376 seconds with the ownership index and 0.710
seconds when rebuilding it. These measurements include local metadata hashing
and process startup. They do not measure compiler bootstrap or real package
builds.

## Remaining costs

A cold run still builds its compiler and dependency closure. Local repository
contents are hashed, and cached Git repositories are checked for edits before
using receipts. Skipping those checks would weaken invalidation guarantees.

Metadata and cache locks still serialize preparation with concurrent builds.
Allowing warm execution during an unrelated build needs a separate publication
and lifetime design. Owner commands still stage a fresh isolated source tree.
Use `--local` for repeated development runs in the original checkout.

Source release versions and runtime snapshots remain separate workflows.
Changing opam descriptions changes source archive bytes, so the next tagged
export must follow normal versioning rather than overwrite existing archives.
