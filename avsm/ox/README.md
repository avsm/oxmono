# ox

Build projects and run binaries using OxCaml and a local day10 cache. Ox
resolves opam metadata in-process, fetches sources, builds the compiler and
dependency closure, and executes the binary. It does not invoke the opam CLI
or create switches.

```sh
ox run --from=https://github.com/avsm/oxmono#minus39 -- yamlcat --help
```

This clones the `minus39` branch, stamps its committed opam files, builds
`yamlcat` and its dependencies with OxCaml, then runs `yamlcat --help`.
Subsequent runs reuse the local cache. Add `--refresh` before `--` to fetch
branch updates.

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
dune exec -- ox run --from . -- yamlcat --help
dune exec -- ox run utop -- -version
```

`--from` accepts a local checkout or a Git URL. Only committed files are used.
It supplies package definitions from that snapshot before other repositories.
Append `#BRANCH`, `#TAG` or `#COMMIT` to select a revision. Without a revision,
the default is `HEAD`. An explicit `--ref REV` overrides the fragment.
Git URL sources are cached, so subsequent runs can work offline.
`--refresh` fetches cached sources and
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

Put ox options before `--`, followed by the binary and its arguments.
`ox run BINARY -- ARG...` also works. The working directory, exit status and
signals are preserved. Build diagnostics go to stderr. A dry run resolves and
lists selected packages without fetching package sources or building them.
Repository metadata may be cloned.

## Build, test and develop

Package builds use the same solver, source fetcher and day10 executor as
`ox run`. Libraries do not need an executable:

```sh
ox build yamlrw
ox build --from=https://github.com/avsm/oxmono#minus39 yamlrw ox
ox test --from . yamlrw
ox show --from . ox
ox build --from . --all
ox build --fetch --from . yamlrw
ox build --depext --from . yamlrw
```

Multiple roots and `--all` are solved together in one compatible environment.
`--all` selects every stamped package, including vendors. Package builds print
the assembled installation prefix. `--fetch` prepares sources without building.
`--depext` prints the system packages required on the current platform without
installing them. `ox show` and `--dry-run` resolve without fetching package
sources or building.

`ox test` enables `with-test` dependencies and actions for the requested
packages, then runs their build, `run-test` and install commands in fresh
writable prefixes. Dependency layers are reused. A successful test run is
never cached.

Inside a Git checkout, omit package arguments to build the editable project:

```sh
cd avsm/ox
ox build
ox test
ox build --deps-only
ox build --depext
ox exec -- dune exec -- ox --help
eval "$(ox env)"
```

Ox discovers project-root opam files beneath the current directory. At the
Git root it selects non-vendor projects. `ox build --local ox` selects a local
package explicitly. Working-tree commands include uncommitted and untracked
files, excluding ignored untracked files. `--from` always uses committed
snapshots and cannot be combined with `--local`.

Local definitions take precedence over repository definitions. Dependencies
absent from the checkout are built from the OxCaml and ordinary opam
repositories, retaining the OxCaml patch guards. If an external package needs
a local library, day10 builds that prerequisite from the working tree too.
Source edits invalidate those layers. Remaining local packages are left for
Dune to build incrementally in the checkout's `_build` directory.

Ox runs scoped Dune `@PROJECT/all` or `@PROJECT/runtest` aliases from the Git
root. Tests use `--force`. `--profile` defaults to `release`, and `-j` controls
each recipe and the local Dune build. `--deps-only` prepares the environment
without running Dune. Local package metadata must declare its dependencies,
including test dependencies. Declare base versions for libraries whose users
specify version bounds.

`ox exec -- COMMAND ARG...` preserves the current directory and runs a command
with the project's dependencies. `ox env` prints POSIX shell exports for the
same environment. Add repeatable `--with PACKAGE` options to either command
to build a package environment instead. No workspace configuration file is
required.

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
provide an overlay containing the pinned definitions and consuming metadata
with those pins resolved into ordinary dependencies. Build and install
commands remain those in the opam files.

The output directory must be new. Empty opam placeholders are reported and
skipped. Duplicate package names are errors. Executable discovery reads
literal Dune declarations. Use `--with` for generated or included
declarations.

`ox run --from` manages its own immutable stamped repositories.
Explicit `ox stamp` defaults to `$XDG_DATA_HOME/ox/overlay`. Use distinct `--output`
directories to export further snapshots, then select them with `--overlay`.

## Distribution packages

```sh
ox dist pkg --from=https://github.com/avsm/oxmono#minus39 \
  --distros=debian-13,fedora-44 -o ./packages -- yamlcat
sh ./packages/build.sh
```

This follows oi's source bundle workflow. Ox resolves each target's opam
metadata, fetches the compiler and dependency sources, and exports the build
plan through `D10ir.Makefile`. Osdist generates the native packaging files.
The generated build uses GNU make and shell recipes. It requires neither ox
nor the opam CLI, and includes its compiler sources.

`ox dist pkg` generates files by default. Add `--build` to run the generated
Docker Compose driver immediately. The driver tries every selected target
and exits unsuccessfully if any build fails. It builds the container images,
then runs them to compile and write packages under `artefacts/<tag>/`.
Source export requires Python 3. Container builds require Docker Compose.

The output directory must be new. Its layout is:

```text
packages/
  bundle/<tag>/<package>-<version>.tar.gz
  bundle/<tag>/<package>-<version>.tar.gz.sha256
  bundle/<tag>/<package>-<version>.osdist.json
  <tag>/Dockerfile
  <tag>/debian/              # Debian and Ubuntu
  <tag>/<package>.spec       # RPM
  compose.yaml
  build.sh
  artefacts/<tag>/
```

Each build context contains its source archive. Each archive contains a
Makefile, `build.sh`, resolved recipes, opam metadata and unpacked sources.
Sources, patches and extra sources are fetched and checked during export.
System dependencies and action filters are evaluated for the target Linux
distribution. Bundles omit the exporting host's build environment and cache
paths. Repeated exports of identical inputs produce identical archive hashes.

The default target is `debian-13`. `--distros` also accepts `ubuntu-24.04`,
`ubuntu-26.04`, `fedora-44` and `alpine-static`, separated by commas. Unknown
tags are errors. `--arch` selects `x86_64` (default) or `aarch64` and sets the
Docker platform. `--from`, `--ref`, `--repository`, `--overlay`, `--toolchain`,
`--refresh`, `--with` and `-j` work as for `ox run`.

Package metadata comes from the first requested opam package. Use `--pkg-name`,
`--pkg-version` and `--maintainer` to override it. Versions starting with a
letter receive a `0~` prefix. Hyphens become dots for RPM compatibility.
Snapshot versions retain their `+ox` revision suffix. `SOURCE_DATE_EPOCH`
sets the packaging changelog date when supplied.

As in oi, installation copies the requested packages' `bin`, `sbin` and
`share` files. This supports native applications whose OCaml dependencies are
linked into their executables. Programs requiring a bytecode interpreter,
private shared libraries or paths baked into their build prefix need additional
packaging support. System shared libraries are handled by the native package
tools. Generated scalar package configuration is read from the bundled build's
installed dependencies. Action filters must be resolvable at export time. Alpine builds also require the project's recipes to honour
`OI_STATIC=1` and an OxCaml toolchain that supports musl.

Tests build and install an exported native fixture after removing its checkout
and cache. They check target filters, patches, substitutions, symlinks, package
metadata, archive reproducibility and driver failure propagation. A real
`yamlcat` Debian export resolves 114 nodes including OxCaml 5.2.0minus39.
Container builds and native package installation remain unverified.

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

Ox prepares opam recipes after dependencies have built. The shared
`D10ir.Direct` executor runs them with its permanent-prefix policy, preserving
the recipe PATH. D10 owns writable prefix assembly, installation and layer
capture. Existing layers remain reusable. Prefixes made by older ox builds
are reconstructed once to adopt d10's completion markers.

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

The runner supports package builds, tests, installed binaries, committed source
snapshots and editable Dune workspaces. Script dependency headers, automatic
external pins, independent batch builds and cache cleanup remain future work.
Package builds execute sequentially. System dependencies are not
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
They also cover library-only builds, uncached test execution, shell environments,
editable Dune projects and external packages with local prerequisites.
The compiler integration test uses an overlay recipe that copies local OxCaml
artifacts to avoid bootstrapping on every test run. Set
`OX_TEST_COMPILER_PREFIX` to select the fixture's compiler installation.

A clean-cache validation built OxCaml 5.2.0minus39 from its repository recipe
and ran this monorepo's `yamlcat`. Snapshot `551112fee9a2`, containing the
`origin/minus39` merge, reused the compiler and external dependency layers.
With cached sources and run prefixes removed, the runner restored layers and
successfully processed YAML offline. Validation is on macOS arm64. Linux
execution remains unverified.
