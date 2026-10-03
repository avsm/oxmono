# ox

Run binaries from opam packages with an existing OxCaml compiler. Ox clones
sources, installs the dependency closure, and keeps the resulting environment
in a local per-user cache. It uses the opam CLI for solving and building.

## Requirements

Use opam 2.5 or newer, Git, a C toolchain, and an installed OxCaml opam
switch. Select that switch or pass `--compiler-prefix /path/to/switch`. Ox
reads the compiler switch and copies its compiler artifacts into private
environments. It does not install packages into the selected switch.

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
preserved. Build diagnostics go to stderr. A dry run prepares compiler
metadata and shows opam's installation actions. It does not build application
packages.

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
External pins are retained. Build and install commands remain those in the
opam files.

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

A cache entry is a complete environment at its final absolute path. Its key
includes compiler artifact hashes, compiler configuration, metadata and extra
file contents, source snapshot identities, solver roots, platform, the C
compiler version and common build flags. Changing the cache path rebuilds the
environment. Use `--cache-tag TAG` after changing external system libraries,
custom tool executables or mutable external source references. These inputs
cannot all be detected automatically.

Metadata and build mutations hold process locks. Failed builds have no
completion marker and are retried in a fresh environment. Completed entries
contain `ox.locked`, a full frozen opam switch export. Warm execution does not
fetch or solve. Completed environments are retained, including their runtime
data. There is no remote binary cache, upload path, S3 configuration or
relocation.

The supplied compiler metadata fixes the compiler version and preserves its
conflicts and environment variables. Optional `oxcaml-*-patches` and guard
packages from the original switch are not declared installed. The runner
retains the OxCaml repository's patch guards for external packages. For
explicitly stamped packages it generates guard metadata without constraints on
those local names. Their opam recipes are the selected patched definitions.
Opam recipes in this workspace must describe all build dependencies and work
from their project subdirectory. Ox does not infer undeclared sibling
dependencies. A local snapshot must contain the OxCaml adaptations its
packages require.

## Scope and tests

This first implementation runs installed binaries. It does not yet provide
compiler bootstrapping, script execution, dirty-worktree builds, environment
activation, incremental workspace builds or cache cleanup. Keep the original
compiler installation available, since embedded compiler paths may refer to
it.

```sh
dune runtest avsm/ox --force
```

The integration test uses temporary local Git repositories and a private opam
root. It checks source stamping, fork precedence, native and bytecode
dependency builds, concurrent invocations, offline reuse,
argument/cwd/exit/signal handling, C stubs, patch guards, refresh,
failed-build retry and damaged cache rejection. It requires Python 3 and the
tools above. Set `OX_TEST_COMPILER_PREFIX` to choose the compiler under test.

The runner has also built this monorepo's `yamlcat` and patched `bytesrw` from
commit `863001fa385480fde4306c5b1300942cebef9dbf`, using the OxCaml and ordinary
opam repository checkouts for external dependencies. Validation ran on macOS
arm64 with the `5.2.0+ox` switch. Linux execution remains unverified.
