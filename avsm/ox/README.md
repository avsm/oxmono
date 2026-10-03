# ox

Run binaries from opam packages using OxCaml and a local day10 cache. Ox
resolves opam metadata in-process, fetches sources, builds the compiler and
dependency closure, and executes the binary. It does not invoke the opam CLI
or create switches.

## Requirements

An installed `ox` executable, Git, tar, patch, make, a C/C++ toolchain and
system dependencies required by the selected recipes are needed. The OxCaml
compiler recipe also requires autoconf. On a new machine, the default
`oxcaml` package builds the compiler from source into the day10 cache. No
existing OCaml compiler installation is required to run `ox`.

To build the `ox` executable in this workspace, use the repository's OxCaml
build environment:

```sh
dune build @avsm/ox/all
dune exec -- ox run --from . yamlcat -- --help
dune exec -- ox run utop -- -version
```

`--from` accepts a local checkout or a Git URL. Only committed files are used.
It supplies package definitions from that snapshot before other repositories.
Use `--ref COMMIT` to select a revision. Git URL sources are cached, so
subsequent runs can work offline. `--refresh` fetches cached sources and
repositories before resolving. It does not update a caller-owned local
checkout.

```sh
ox run --from https://github.com/OWNER/oxmono.git --ref COMMIT yamlcat
ox run --with yamlrw yamlcat -- --help
ox run --overlay /path/to/opam-overlay utop
ox run -n --from . yamlcat
```

`--with PACKAGE` is repeatable and accepts opam package atoms. Use it when the
binary name differs from its package or has ambiguous ownership. Stamped Dune
`public_name` declarations provide automatic mappings where package ownership
is unambiguous. Otherwise a package name is also assumed to be its binary
name. An unconstrained root from a stamped repository selects its exact
snapshot version, even when an upstream repository contains a newer release.

Arguments after `--`, the working directory, exit status and signals are
preserved. Build diagnostics go to stderr. A dry run resolves and lists selected packages without fetching package
sources or building them. Repository metadata may be cloned.

## Snapshot versions

```sh
ox stamp . --output /tmp/ox-overlay
ox stamp . --ref COMMIT --source https://github.com/OWNER/oxmono.git \
  --output /tmp/shareable-overlay
```

Each project-root `.opam` file becomes an ordinary opam repository entry. The
base version comes from the opam file, then `dune-project`, then `0.0.0`.
Versions have the form `BASE+ox.COUNT.COMMIT`, where COUNT is the repository
revision count and COMMIT is the first twelve characters of the commit hash.
Complete Git history is required. All projects share that suffix, preserving
same-version dependencies between packages with the same base version.

Source URLs pin the full commit and record the Dune project subdirectory.
Internal dependency atoms select the exact corresponding snapshot package,
retaining filters such as `with-test`. A generated solver constraint also
prevents external dependencies from selecting upstream versions of local
package names. Only the requested dependency closure is installed. Constraints
referring to a dependency's base version are translated to its snapshot
version. Local `pin-depends` entries are replaced by these exact dependencies.
External `pin-depends` entries are retained in exports. To run those packages,
provide pinned package definitions through an explicit overlay. Build and
install commands remain those in the opam files.

The output directory must be new. Empty opam placeholders are reported and
skipped. Duplicate package names are errors. Executable discovery reads
literal Dune declarations. Use `--with` for generated or included
declarations.

`ox run --from` manages its own immutable stamped repositories.
Explicit `ox stamp` defaults to `$XDG_DATA_HOME/ox/overlay`. Use distinct `--output`
directories to export further snapshots, then select them with `--overlay`.

## Repositories and cache

Metadata order is the source snapshot, explicit overlays, the local data
directory's `overlay`, `oxcaml/opam-repository`, and `ocaml/opam-repository`.
The latter supplies packages absent from the OxCaml overlay. Repeated
`--repository PATH-OR-GIT-URL` replaces the two default repositories.

Data lives in `${XDG_DATA_HOME:-~/.local/share}/ox`. Build environments and
source downloads live in `${XDG_CACHE_HOME:-~/.cache}/ox`. Override these with
`--data-dir` and `--cache-dir`. The cache must be on a local filesystem.

Each package has a day10 layer and a permanent installation prefix. Keys
include effective opam metadata, source contents, patches, dependency layers,
platform, C compiler version, common build flags and the absolute cache root.
Different programs reuse matching dependency layers. Generated `.install` and
`.config` files are handled locally. Build logs and resolved recipes are kept
under the cache. A run prefix combines the selected layers and exported
package environments.

Moving the cache causes a rebuild. Keep its original prefixes available:
compiled artifacts may contain absolute paths. Use `--cache-tag TAG` after
changing external system libraries or custom build tools. Use `--refresh` to
resolve again and fetch mutable source references. Checksummed archives and
Git sources pinned to full commits remain reusable.

Metadata and build mutations hold process locks. Failed builds publish no
completed layer or request receipt and are retried in a fresh build tree.
Successful requests retain only their ordered layer list and runtime
environment. Day10 metadata supplies each package's dependency layers for
prefix restoration. Warm
execution neither fetches nor solves. Missing installation prefixes are
reconstructed from completed layers. Dependency files are detached before
installers can modify them. File content and modes determine the installed
delta. Deleting dependency files is rejected.

`--toolchain PACKAGE` selects another OxCaml toolchain package atom. Compilers
and applications use the same opam recipe and day10 build path. Custom
compiler definitions can be provided through an overlay.

The runner retains the OxCaml repository's patch guards for external packages.
For explicitly stamped packages it generates guard metadata without constraints
on those local names. Their opam recipes are the selected patched definitions.
Opam recipes in this workspace must describe all build dependencies and work
from their project subdirectory. Ox does not infer undeclared sibling
dependencies. A local snapshot must contain the OxCaml adaptations its
packages require.

## Scope and tests

The runner supports installed binaries and committed source snapshots. Script
execution, dirty-worktree builds, environment activation, incremental workspace
builds and cache cleanup remain future work. System dependencies are not
installed automatically. Source backends are Git and tar archives. Git
submodules currently require an explicit source archive.

```sh
dune runtest avsm/ox --force
```

Tests use temporary Git repositories and put a failing `opam` executable on
PATH. They cover stamping, fork precedence, default toolchain builds, native
and bytecode execution with C stubs, concurrent builds, offline layer
restoration, runtime environment updates, source refresh, checksums, argument
and signal forwarding, failed-build retry and damaged receipt rejection.
The compiler integration test uses an overlay recipe that copies local OxCaml
artifacts to avoid bootstrapping on every test run. Set
`OX_TEST_COMPILER_PREFIX` to select the fixture's compiler installation.

A clean-cache validation built OxCaml 5.2.0minus39 from its repository recipe
and ran this monorepo's `yamlcat`. Snapshot `551112fee9a2`, containing the
`origin/minus39` merge, reused the compiler and external dependency layers.
With cached sources and run prefixes removed, the runner restored layers and
successfully processed YAML offline. Validation is on macOS arm64. Linux
execution remains unverified.
