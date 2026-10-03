# ox: staged import and design

## Implemented stages

The oi library import and compatibility review are in `bleeding/oi-libs`.
The first runner is in [`avsm/ox`](../avsm/ox/README.md): `ox stamp`, installed
binary `ox run`, exact snapshot roots, local binary caching and explicit Git
refresh. Package versions are generated from Git snapshots and metadata
selection uses local opam overlays.

The runner delegates resolution, fetching and package actions to an isolated
opam CLI root. This keeps normal opam recipe semantics without importing oi's
registry or toolchain machinery. Opam-format handles metadata and D10 supplies
process locks. An existing OxCaml switch supplies the compiler. There is no
opam-0install dependency in this implementation.

The sections below describe the broader workspace roadmap. Script execution,
`plan`, `sync`, `env`, dirty builds, incremental workspaces and compiler
bootstrapping remain future increments. See the runner README for the current
interface and its tested limits.

## Product goal

Ox manages this monorepo and forks through their `.opam` files. Package names,
dependency constraints, compiler requirements, build/install commands and source
pins stay in opam metadata. There is no second dependency manifest to keep in
sync. The runner, workspace environment and builder share one resolver, source
model and cache.

## Workflow

Keep the useful oi runner interface:

```sh
ox run utop
ox run --with=some-package some-binary -- --help
ox run --with=fmt --with=cmdliner ./script.ml
ox run ./script.ml                 # dependencies in [@@@opam ...]
ox run -n utop                     # solve and show source/cache work
ox plan utop                      # detailed reproducible plan
```

The first invocation resolves, builds locally, records the result and runs it.
An unchanged request reuses its recorded solve and completed prefix. Forward
arguments after `--` unchanged, preserve the program's exit status and signals,
and keep build diagnostics on stderr. A missing or ambiguous binary must name
candidate packages instead of choosing silently. Without a remote binary index,
`--with` is the reliable package-to-binary mapping on a cold cache.

## Repository model

Use a local opam overlay, as agreed, rather than reporepo's handle registry.
Package metadata precedence, highest first:

1. Explicit project pins and local package definitions.
2. The user's local opam overlay, stored under the ox data directory.
3. A pinned checkout of `https://github.com/oxcaml/opam-repository.git`.
4. A pinned ordinary opam repository for packages absent from the OxCaml tree.

The final fallback is needed because the OxCaml repository is itself an overlay.
Its checked-out README uses the `ox,default` repository order. Selection of
repository metadata and enforcement of the compiler constraint are separate:
a fallback package may be selected only if it builds with the selected OxCaml
compiler. Never silently select a stock compiler to satisfy a solve.

`ox init` creates the local overlay and records the initial upstream revisions.
`ox repo update` explicitly refreshes upstream snapshots. A warm `ox run`
uses recorded revisions and does not need to contact a registry. Local overlay
edits are detected by content, including dirty files and package `files/`, and
invalidate affected solves. A lock record captures repository revisions,
selected metadata digests, pinned source revisions and local source digests.
`--with-repo` can later add an ordinary opam repository with documented precedence.
No overlay handles, transitive handle graph, registry baking or publication UI.

## Monorepos and forks

Index workspace `.opam` files by package name and source directory. Exclude
build outputs and nested VCS administrative data. Vendored packages are available
to satisfy dependencies but are not automatically all solver roots. Select roots
explicitly at first, for example `--package sortal`, and solve their closure.
Workspace selection settings may record root packages and paths, but must not
repeat dependency constraints from the opam files.

Track package identity separately from source identity. One checkout can provide
many packages and one Dune workspace can build several package projects. Retain
the build root and the relative path to each package's opam file. Do not assume
that copying the directory beside an opam file produces a complete build tree.
Run declared build commands in the appropriate source context, with the selected
package set and installed dependency environment. A first implementation spike
must exercise the actual layout here, including shared vendors and multiple
packages per project, before this source model is considered complete.

Proposed workspace commands use the same machinery as the runner:

```sh
ox sync --package sortal          # resolve dependencies and prepare the environment
ox run -- dune build avsm/sortal  # run a command in the prepared workspace environment
ox build --package sortal        # build through the package's opam recipe
ox env                           # print shell activation for the selected environment
```

The `ox run -- COMMAND ...` form explicitly chooses an environment command.
`ox run PACKAGE ...` retains package-runner lookup. Both keep the caller's working
directory. Changes to opam files require a new solve. Changes only to application
sources can reuse the dependency environment and Dune's incremental build state.
A script or tool built from those changed sources still gets a new binary key.

Cloning a fork and selecting its local packages must override the corresponding
upstream package definitions without renaming packages. External development
checkouts use ordinary opam pins, and shareable source pins can use `pin-depends`.
The generated lock records repository URL, commit, package metadata and relevant
source contents, including dirty and untracked build inputs. A remote URL alone
is insufficient to identify a fork build. Machine-local absolute checkout paths
belong in local state, not shared dependency declarations.

Keep generated activation files and workspace state under `_ox/`, ignored by
Git. Keep the reproducibility lock suitable for committing alongside the opam
files. A fresh fork should reproduce the locked toolchain and source selections,
then rebuild any artifacts whose absolute install prefix differs. Distinct
checkouts must not share mutable build trees or accidentally consume one
another's local package builds. Immutable downloaded sources and compatible
dependency environments can be reused in the per-user cache.

## Compiler and cache

Start with an explicitly selected existing OxCaml switch, including this
workspace's `5.2.0+ox`, to validate the runner. Fingerprint the compiler binaries,
`ocamlc -config`, installed compiler package metadata and its absolute prefix.
A version string such as `5.2.0+ox` is not enough to distinguish compiler builds.
The next increment can bootstrap a private compiler from a pinned OxCaml
repository snapshot into a stable per-user directory.

Use XDG config/data/cache locations under `ox`. Keep configuration, the local
overlay and lock records in config/data. Build sources, logs, failed staging
and rebuildable binaries live in the cache. Cache-root movement or a compiler
prefix change causes a rebuild instead of an implicit relocation attempt.

For the first runner, cache complete solved environments at their final absolute
path, keyed before building by the resolved inputs. Build packages in dependency
order at that path, publish a completion marker last, and keep the prefix at
that location. This avoids installing to a staging prefix and renaming it after
absolute paths have been embedded. A crash leaves an incomplete environment
that is rebuilt under the same path while holding its lock.

This trades some duplicate package builds between different solves for a smaller
correct implementation. Source downloads and Dune's local cache can still be
shared. Add package-level binary reuse only after tests demonstrate correctness
for compiler artifacts, native stubs, META/dune-package files and runtime data.
D10 supplies metadata, indexing and locks now. Its current staging executor is
not the initial ox install policy.

Cache keys include schema version, platform/architecture, compiler fingerprint,
absolute install root, effective package metadata, source checksums, patch and
extra-file contents, dependency keys and build-affecting flags/environment.
Use SHA256 with unambiguous field encoding. Record host C-toolchain and system
library inputs, and provide explicit invalidation when they change.

Use a process lock plus an Eio mutex for cache mutation. Publish source downloads
through verified temporary files and atomic rename. Do not hardlink mutable
build inputs to canonical cached files. Track complete installed manifests.
An environment in use needs a lease so cleanup cannot remove runtime data.
An initial coarse cache lock is acceptable until parallel behavior is tested.
No S3 configuration, upload code, remote binary cache, or public registry index.

## Implementation increments after review

1. Add `avsm/ox`: opam workspace discovery, package/source grouping, XDG layout,
   repository snapshots, local overlay precedence, explicit OxCaml selection,
   solver and `ox plan`/`run -n`. Validate this monorepo and a fork as fixtures.
   Keep the isolated opam CLI backend until there is evidence for replacing it.
2. Implement one package runner through the complete-environment cache, source
   verification, subprocess environments, `--with`, argument forwarding and
   deterministic binary lookup. Reuse selected oi modules after reducing their
   registry/toolchain dependencies instead of importing its CLI wholesale.
3. Expose the same resolver/cache through `ox sync`, `ox env`, environment-mode
   `ox run -- COMMAND` and `ox build`. Test local package overrides, dirty forks,
   shared Dune build contexts and dependency-environment reuse after source edits.
4. Add script dependency parsing, script-content cache keys, offline warm runs,
   concurrent invocations, explicit cache inspection and safe cleanup.
5. Add compiler bootstrapping after the runner and workspace paths pass.

## Acceptance checks

- A clean cache resolves through OxCaml metadata and builds/runs a small tool.
- This monorepo and a fresh fork can resolve and build a selected application
  using their opam files, without a second dependency declaration.
- Local packages override upstream packages by name, including several packages
  from one checkout, and a changed source tree cannot reuse a stale binary.
- Source-only edits preserve a valid dependency environment and incremental
  workspace build. Dependency edits invalidate the solve.
- The second run succeeds offline without rebuilding or resolving remotely.
- Local overlay edits override upstream metadata and invalidate affected results.
- Different compiler builds, prefixes and platforms cannot share binary entries.
- A compiled script using an OxCaml feature runs with its declared dependencies.
- Native stubs and runtime data remain usable on both macOS and Linux.
- Two concurrent processes requesting one environment publish it once.
- Interrupted downloads/builds leave no usable completion marker.
- Arguments, working directory, exit status and signals survive the runner.
- No path reaches S3, oi's default registry or reporepo publication machinery.

The immediate review decisions are the package/source grouping for this
monorepo, the complete-environment cache tradeoff and starting with the existing
switch. Both keep the first ox implementation small
while leaving compiler bootstrapping and finer binary reuse as later work.
