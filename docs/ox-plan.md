# ox: runner and monorepo plan

## Implemented foundation

The reviewed oi constituent libraries live in `bleeding/oi-libs`. The
[`ox` runner](../avsm/ox/README.md) uses opam metadata, an in-process
opam-0install solver and day10 package layers. The generic solver libraries
are vendored with provenance and unchanged implementation sources. Ox
supplies the repository context adapted from oi.

`ox run` builds the default `oxcaml` toolchain and application dependencies
from source. Opam commands are interpreted through opam-format APIs and
executed directly. There is no opam CLI invocation, switch creation, reporepo
registry, S3 backend or remote binary cache in the runner.

```sh
ox run utop -- -version
ox run --from https://github.com/OWNER/oxmono.git --ref COMMIT yamlcat
ox run --from . --with yamlrw yamlcat -- --help
ox run -n --from . yamlcat
```

The default repository order is a source snapshot, explicit overlays, the
user's local opam overlay, `oxcaml/opam-repository`, then
`ocaml/opam-repository`. The final repository supplies packages absent from
the OxCaml overlay. OxCaml patch guards remain enforced for external packages.
Stamped local packages supply their own patched definitions.

## Monorepo versions and forks

`.opam` files own dependency constraints and build/install commands. There is
no second package manifest. `ox stamp` exports committed packages as ordinary
opam repository entries. Versions are `BASE+ox.COUNT.COMMIT`, with a `0.0.0`
base when neither the opam file nor its Dune project declares one. Git URLs
pin full commits and record each Dune project's source subdirectory.

Same-snapshot dependencies select exact versions. Generated constraints keep
external dependencies from replacing local packages with upstream releases.
A fork retains package names and gets a distinct source identity. Uncommitted
edits are excluded. Only the requested closure is built.

Share an exported overlay using `ox stamp --source GIT-URL`, or invoke
`ox run --from GIT-URL --ref COMMIT` on the other machine. Local file URLs are
for local use. Ordinary edits require commits but no tags or manual version
bumps. Complete Git history is required for revision counts.

## Build and cache model

Each package builds at its permanent, hash-derived prefix in the per-user
cache. Dependency layers are restored at that prefix and detached before
building. Full content and mode manifests capture installed changes.
Deleting dependency files fails the build. Generated `.install` files use
the imported day10 installer. Generated `.config` values feed later recipes.

The package key includes sources, effective recipe metadata, dependency
layers, platform, compiler inputs, common environment flags and absolute
cache root. Different requested tools share matching dependency layers.
The compiler is itself built and cached through this path. A local overlay
can provide a custom compiler recipe.

A completed request records layers and runtime environment. Warm runs avoid
network and solving. Missing prefixes can be reconstructed. Prefixes stay
at their original locations because bytecode interpreters, native stubs and
other artifacts can contain absolute paths. Cache movement rebuilds locally.
No cross-machine binary relocation is required.

Source downloads use temporary files and declared checksum verification.
Mutable references refresh explicitly. Build failures publish no completed
request. Coarse process locks serialize cache mutations. Build logs and
resolved day10 recipe records remain available for inspection.

## Next increments

1. Add `ox plan` and a portable source lock containing repository revisions,
   selected package metadata and pinned sources. This would make a solve
   repeatable across machines even after repository updates.
2. Expose the same resolver and layer store through `ox sync`, `ox env` and
   `ox build`. Keep selected roots in workspace settings and dependencies in
   opam files. Test the actual monorepo and a fork.
3. Add dirty-worktree and incremental Dune builds. Keep mutable build trees
   per checkout and reuse immutable dependency layers. Distinguish source
   edits from dependency edits when invalidating the workspace environment.
4. Add oi-style script dependency declarations and script-content cache keys.
5. Add cache inspection and cleanup with leases for running programs, then
   finer build parallelism and Linux coverage.

## Acceptance evidence

The forced integration suite exercises opam-free execution, source snapshots,
fork precedence, dependency reuse, native/bytecode binaries and C stubs,
concurrent invocations, offline prefix reconstruction, source refresh,
checksums, failed-build retries, arguments, cwd, exit status and signals.
A clean cache built OxCaml 5.2.0minus39 and ran the monorepo's `yamlcat`.
The merged `minus39` snapshot reused those external layers. Removing source
caches and run prefixes still allowed offline restoration and execution.
These checks ran on macOS arm64. Linux remains unverified.
