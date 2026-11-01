# A forkable OxMono workspace and release pipeline

Status: design with confirmed ownership, platforms and overlay policy,
2026-11-01. This plan changes no source locations.
Implementation should proceed as separately reviewable migrations.

## Recommended direction

Use three source categories, with package discovery shared by development,
containers and releases:

- `vendor/`: upstream imports and their OxCaml ports, with verified provenance.
- `wip/`: shared projects developed here, not yet released through the public
  opam repository.
- `u/<owner>/`: independently maintained applications and personal integrations.

Move `avsm/` to `u/avsm/` and the complete `taposaur/` project to
`u/etap/taposaur/`. Give each owner a Dockerfile and a small build manifest.
Promote `avsm/ox` to `wip/ox` as the shared management CLI, preserving the
`ox` binary and `ox.lib` library. Extend its existing build and solver machinery
for owner selection, containers and source-overlay releases.

Generate one ordinary opam repository containing shared packages and the
patched vendor packages needed to install them. Produce deterministic source
archives under `dist/`, then publish those bytes to configurable storage.
Keep package extraction separate from upload and repository publication.

Use simple OxCaml package versions: `BASE+ox`, then `BASE+ox2`, `BASE+ox3`
for subsequent revisions of that base. Keep commit hashes and release IDs in
manifests. Run our own source-package overlay over the pinned OxCaml and
public opam repositories, with the compiler fixed at minus39. The immediate
deliverable is usable Linux binaries and per-user containers, not published
binary packages. Source publication remains a later phase.

## What is here today

The inventory covers every immediate source directory:

| Area | Observed contents | Consequence |
| --- | --- | --- |
| `vendor/` | 54 directories, all represented in `upstreams.json` with recorded revisions | Extend the existing provenance system |
| `bleeding/` | 52 directories, 96 non-example/test opam files, including imports | Classify before renaming |
| `avsm/` | 13 directories, 12 Dune projects, 13 opam files | One personal area, with some shared-tool extraction |
| `taposaur/` | One Dune project, one opam package, 22 component directories | Move intact, not as 22 separate packages |
| `example/` | One project with Fetch, Proffer and localhost examples | Shared documentation examples |
| `samoht/` | Empty, with no tracked files | Remove its stale README entry |
| root | Workspace configuration and dependency-only `oxmono.opam` | Keep a small, owner-neutral entry point |

Counts distinguish projects, packages and directories. They are not a release
manifest: examples, disabled packages, nested projects and imported subsets
need explicit classification.

Existing machinery worth retaining:

- `vendor/upstreams.json`, `vendor/check-upstreams.py` and its offline tests.
- Package-local `OXMONO.md`, `OXMONO.json`, `VENDORED.md` and
  `oxmono/upstream.json` records.
- `avsm/ox/lib/stamp.ml` discovers committed packages, detects duplicate names,
  stamps versions, records project tree hashes and creates an opam overlay.
- `avsm/ox/lib/workspace.ml` selects local build roots. Today a root invocation
  includes all non-vendor projects, which is too broad for independent users.
- `avsm/ox/lib/dist.ml` already produces source bundles and Linux packaging
  contexts. Those bundles include compiler/dependency sources, unlike the
  small per-project archives proposed here.
- `bleeding/atp` already has Tangled artifact types, listing code and fixture
  tests. A release uploader still needs implementation and verification.
- `bleeding/osrel`, `d10` and `osdist` provide platform and packaging support.

## Placement policy

Provenance and release status are separate axes. An unreleased imported
upstream project still belongs in `vendor/` when this tree maintains its port.
A project developed here can belong in `wip/` even if it also has an external
Git mirror. A homepage alone does not establish the direction of maintenance.

Use registry fields for `origin_kind`, `maintainers`, `release_status` and
`export_policy`. A successful snapshot export does not itself mean the project
has graduated from `wip/` to a maintained public release.

Recommended dependency direction:

```text
u/<owner>  -->  wip  -->  vendor  -->  external packages
    \--------------------^
```

Shared projects must not depend on personal trees. User applications can
depend on shared projects and their own applications, but should not require
another user's checkout. Promote a shared library before establishing that
dependency. Imported projects that need substantial new shared functionality
should expose it as a separate extension rather than quietly reversing the
vendor dependency direction.

Directory ownership does not namespace opam or Dune library names. Detect
duplicate package names, public library names and installed binary names.
A fork may replace a shared package intentionally, but there must be one
selected definition, not two directories shadowing each other.

### Target layout

```text
AGENTS.md
README.md
PLAN.md
TODO.md
dune
dune-project
dune-workspace
oxmono.opam                 # optional full-workspace developer dependencies
workspace.json              # classification and export exceptions
toolchain.lock.json         # compiler, repositories and container base pins
release.json                # publisher and transport configuration
vendor/
  upstreams.json
  <upstream>/
wip/
  <shared-project>/
  ox/                       # shared management CLI and ox.lib
u/
  avsm/
    user.json
    Dockerfile
    Dockerfile.dockerignore
    arod/
    bushel/
    ...
  etap/
    user.json
    Dockerfile
    Dockerfile.dockerignore
    taposaur/               # all existing component subdirectories retained
examples/                   # rename existing example/ during layout migration
docs/
tools/                      # thin bootstrap and CI entry points
dist/                       # generated, ignored by Git and Dune
  sources/
  releases/
  opam-repository/
```

Keep `(vendored_dirs vendor)` at the root. Mark generated `dist/` and
non-source tooling/configuration directories appropriately so extracted
archives and generated package metadata cannot become accidental workspace
projects. Do not leave source-directory symlink aliases for old paths:
Dune must see each package only once.

### All current bleeding directories

These are proposed destinations. Ports marked "verify" must recover or
confirm provenance before release, not invent a commit.

| Current directory | Proposed destination or treatment |
| --- | --- |
| agentkit | `wip/agentkit`; retain its multiple packages and backends |
| apple-mlx-embeddings | `wip/apple-mlx-embeddings`; explicit macOS capability |
| apple-speech | `wip/apple-speech`; explicit macOS capability |
| apubt | `wip/apubt` |
| atp | `wip/atp`; one source project, many opam packages |
| bloscz | `wip/bloscz` |
| bytesrw-eio | `wip/bytesrw-eio`; separate extension to Bytesrw |
| cbort | `wip/cbort` |
| d10 | `vendor/oi/d10`; retain its recorded scoped oi import |
| ds4 | `wip/ds4`; register embedded engine sources and CPU/CUDA/Metal support |
| ecosystems | `wip/ecosystems` |
| eioext | `wip/eioext`; upstream candidates, not an imported Eio tree |
| fetch | `wip/fetch`; correct its stale Taposaur source attribution |
| finger | `wip/finger` |
| frontmatter | `wip/frontmatter` |
| gitops | `wip/gitops` |
| hf | `wip/hf` |
| httpz | `wip/httpz`; retain and improve embedded-source provenance |
| idk | `wip/idk`; IDKit is maintained here until an upstream is designated |
| imap | `wip/imap`; repair its cross-project oracle fixture dependency |
| immich | `wip/immich` |
| jmap | `wip/jmap`; maintained here; explicitly account for disabled jmap-mosaic |
| json-pointer | `vendor/ocaml-json-pointer`; examples are not release roots |
| jsonwt | `wip/jsonwt` |
| karakeep | `wip/karakeep` |
| linkedin | `wip/linkedin` |
| maildir | `wip/maildir` |
| matrix | `wip/matrix`; maintained here; package remains matrix-chat |
| memento | `wip/memento` |
| mqttz | `wip/mqttz`; retain provenance of its inherited codec |
| nx-oxcaml | `vendor/nx-oxcaml`; recorded upstream port, paired with Nx |
| openapi | `wip/openapi` |
| openrouter | `wip/openrouter` |
| osdist | `vendor/oi/osdist` |
| osm | `wip/osm` |
| osrel | `vendor/oi/osrel` |
| owntracks | `wip/owntracks`; retained import history, actively maintained here |
| peertube | `wip/peertube` |
| proffer | `wip/proffer` |
| sekrit | `wip/sekrit` |
| spindle | `wip/spindle`; generic shared CI service |
| sqlite3 | `vendor/sqlite3`; verify upstream base and embedded SQLite version |
| tessera | `wip/tessera` |
| tomlt | `wip/tomlt` |
| webfinger | `wip/webfinger` |
| xdge | `wip/xdge` |
| yamlrw | `wip/yamlrw` |
| yamlt | `wip/yamlt` |
| zarrz | `wip/zarrz` |
| zotero-translation | `wip/zotero-translation` |
| zstdz | `wip/zstdz` |
| zulip | `wip/zulip`; maintained here; retain historical provenance |

The oi group retains three independently buildable Dune projects under one
upstream provenance entry. Its source mapping must describe the original oi
subdirectories and the local rearrangement.

Keep `wip/matrix` separate from existing `vendor/matrix`. The latter is
terminal infrastructure imported from Mosaic, not the Matrix chat SDK.
Matrix, JMAP, IDKit and Zulip stay first-party until the owner explicitly
designates an upstream. Preserve their historical provenance.

### All current avsm directories

| Current directory | Recommendation |
| --- | --- |
| arod | `u/avsm/arod` |
| bushel | `u/avsm/bushel` |
| crowthebot | `u/avsm/crowthebot` |
| dooit | `u/avsm/dooit` |
| ox | `wip/ox`; shared management CLI and reusable `ox.lib` |
| recoilnet | `u/avsm/recoilnet`; operations data, excluded from images/archives |
| retrieval | Promote to `wip/retrieval`; generic storage and retrieval library |
| siftbot | `u/avsm/siftbot` |
| sortal | `u/avsm/sortal` |
| srcsetter | `u/avsm/srcsetter` initially; promote if another owner needs it |
| termanil | `u/avsm/termanil` |
| tessabot | `u/avsm/tessabot` |
| zarr-perma-proxy | `u/avsm/zarr-perma-proxy`, including zarrtop |

Bushel and Sortal are reusable but still coherent owner-maintained products.
There is no need to move every reusable application into shared infrastructure.
If another user's project needs their libraries, make that promotion explicit.

Move `taposaur` as a unit, retaining `archive`, `audit`, `audit_cli`, `bench`,
`bin`, `catalog`, `core`, `document`, `download`, `indexing`, `mirrors`,
`paper`, `reader`, `regeneration`, `review`, `review_run`, `review_store`,
`services`, `store`, `test`, `ui` and `web` beneath `u/etap/taposaur`.
Ownership placement should not rewrite authorship or license notices.

### Existing vendor directories

Retain all 54 existing imports. The manifest is their primary index:

```text
0install-solver asn1-combinators base64 bytesrw ca-certs checkseum
cmarkit console cstruct decompress digestif domain-name duration eio
eqaf gmap hmap htmlit ipaddr jsonfeed jsonm jsont k7 kdf lwt-dllist
matrix mirage-crypto mtime multibase nx ocaml-apple-fm ocaml-codec
ocaml-dockerfile ocaml-proj ocaml-uri ocamlgraph opam opam-0install
opam-file-format optint patch progress ptime sha sitemap sqlite-vec
swhid_core syndic tls uring uunf x509 xmlm zarith
```

Keep partial imports and deliberate rewrites clearly identified. For example,
Uriz is not a byte-for-byte copy of ocaml-uri, and Nx contains scoped bundled
sources. An unchanged import needed for the OxCaml dependency closure is also
valid in `vendor/`; do not invent a patch requirement.

Extend provenance to record canonical URL, exact base commit, imported paths,
local path mapping, archive checksum where applicable, license, patch notes,
verification date and validation commands. Prefer public clone URLs where
available; do not require another contributor's SSH identity to reproduce a
public import.

Embedded sources need the same accountability. HTTPz currently documents
unknown base commits for some inherited components. Record those as unknown
until verified. DS4, SQLite and other bundled native sources need component
entries too. Move separable upstream components into `vendor/` when they can
be independently packaged. Do not break a source unit merely to move its C
files: transitional embedded imports must be explicit in provenance and exports.

## Discovery and fork independence

Keep `dune-project` and generated `.opam` files as the package/dependency
source of truth. Do not introduce a second hand-maintained dependency graph.

`workspace.json` supplies only what cannot be inferred: category/ownership
exceptions, release units, embedded imports, export exclusions with reasons,
platform requirements and extra tracked files required for extraction.
Discover Dune projects recursively, associate packages with their project,
and reject duplicate names. Never release every `*.opam` file found by a glob:
there are nested examples and currently disabled packages.

Each `u/<owner>/user.json` declares application roots and any shared CLI
packages to bundle. Auto-discover that owner's project-root packages by
default; allow explicit exclusions with reasons. The generated inventory
lists public executables and helper binaries with their providing packages.

Fork workflow:

1. Fork the repository and add `u/alice/` from a documented template.
2. Add a Dune project, user manifest and Dockerfile. No central owner list
   needs editing; CI discovers `u/*/user.json`.
3. Build that user's dependency closure, not every project in the workspace.
4. Merge changes to `wip/`, `vendor/` and shared tooling from the parent fork.
   Keep owner commits separate from shared commits.
5. Promote cross-owner dependencies into shared projects through reviewed
   changes. Do not solve shared-library conflicts by copying libraries into
   each user directory.

A selected build must exclude other owners' source roots from the effective
Dune workspace, not merely avoid their aliases. Otherwise duplicate packages
or broken stanzas in an unrelated fork can still disrupt discovery. Generate
a selected staging workspace for containers and isolated CI, and use an
equivalent package/source selection for local owner builds.

External repositories, compiler revision and support libraries must be locked.
A compiler-version label alone does not pin the OxCaml toolchain or compatible
Bonsai/Base/Async packages. Retain `oxmono.opam` for "build everything" work,
but generate per-owner external dependency sets for the normal workflow.

## Per-user containers

Provide actual checked-in `u/avsm/Dockerfile` and `u/etap/Dockerfile` files.
Use shared generation/build code so they remain small and consistent:

```sh
docker build -f u/avsm/Dockerfile -t oxmono-avsm .
docker build -f u/etap/Dockerfile -t oxmono-etap .
docker run --rm -it oxmono-avsm zarrtop --cache-dir /data/zarr
docker run --rm oxmono-etap taposaur --help
```

The context is the repository root, even though the Dockerfile is in the user
directory. Docker supports Dockerfile-specific ignore files, which take
precedence over the root ignore file. Use generated allowlists to expose only
the selected source closure, build configuration and necessary assets.
[Docker build-context reference](https://docs.docker.com/build/concepts/context/).

Local Docker builds can use filesystem discovery and must not require `.git`
in the build context. Official images should instead use a selected context
exported from a committed revision, carrying a source manifest and OCI revision
labels. Mark working-tree images as development builds. Do not infer a clean
source identity from a manually supplied build argument.

Use stages for a pinned toolchain, selected dependency build, owner build/test,
and runtime assembly. Build application install targets, not arbitrary test,
example or benchmark executables. Discover installed files through Dune/opam
install manifests, retaining helper binaries, shared libraries, plugins,
licenses and package data. Copying only `_build/default/**/main.exe` would
break several applications.

The avsm inventory must include at least arod, bushel, crowthebot, dooit, ox,
siftbot and its worker/review/config helpers, sortal, srcsetter, termanil and
its worker, tessabot, zarr-perma-proxy and zarrtop. The etap inventory includes
taposaur, taposaur-consolidate, taposaur-web, taposaur-publishers and
taposaur.audit-cli. Treat these as audit expectations; derive the authoritative
set from installation metadata.

Runtime images should:

- Contain all selected Linux-supported user commands, with commands on PATH.
- Include dynamic system-library closure, certificates, timezone information,
  terminal support and assets such as PROJ data where needed.
- Run without root, with writable XDG directories on explicit volumes.
- Contain no development caches, personal datasets, model weights or secrets.
- Execute the requested command directly, preserving signals and exit codes.
  An optional default command can list available binaries.
- Start no collection of services implicitly; add separate Compose examples
  later for applications that should run together.

Linux Docker cannot supply Apple frameworks or Metal, even on a Mac host.
Declare CPU/CUDA/macOS support explicitly. Build portable applications with
their supported Linux backends and document unavailable optional features.
A genuinely macOS-only executable must appear in the exclusion report, with
a documented native macOS route outside the initial deliverables. Never claim
a Linux image contains functional Apple services because a stub linked
successfully.

The initial platform matrix is Linux amd64 and arm64, using glibc. Test both
natively where available. CUDA images and native macOS bundles are deferred.
Derive native build and runtime dependencies from package metadata plus
reviewed platform overrides; do not assume the existing global apt list
is the runtime closure.

### Binary linking and delivery

Use native OCaml executables with their required system shared libraries in
small glibc-based runtime images. Also support a per-owner install directory
with `bin/`, assets and a recorded system-library inventory for compatible
Linux hosts. A loose executable is not promised to run on arbitrary distros.
No Debian/RPM packages or public binary-release service are needed initially.

Fully static linking is an optional later optimisation, evaluated per binary.
The inspected OxCaml overlay explicitly says musl is unsupported. Do not base
the initial build on Alpine or assume that adding `-static` suffices. C/C++
dependencies such as BLAS, compression and DS4, external helper programs,
certificates and data files all need an inventory even with static linking.
Start with an agreed glibc runtime image and smoke-test the complete bundle
on both architectures, including DNS, TLS and helper discovery.

## Shared management through ox

Promote the entire existing `avsm/ox` project to `wip/ox`, preserving its `ox`
package, binary and `ox.lib` library names. Owner bundles can include `ox` as
a shared CLI. Do not introduce a competing `oxmono` binary or a second solver,
cache, metadata parser or build executor.

The existing implementation supplies:

| Existing component | Reuse |
| --- | --- |
| `Stamp` | Discover packages, Dune projects and binaries; rewrite dependency formulas |
| `Repository` / `Solve` | Ordered overlays, package solving and snapshot constraints |
| `Source` / `Recipe` / `Build` / `Runner` | Fetch sources, execute recipes, cache day10 layers and assemble prefixes |
| `Workspace` | Build working trees with prepared dependencies and scoped Dune aliases |
| `Dist` | Source bundles, platform/dependency inventories and Docker build plumbing |

Keep `ox build`, `test`, `show`, `run`, `env` and `exec`. Add owner selection
to those existing workflows. Preserve `ox dist pkg` for existing callers, but
use its lower-level machinery for install trees and runtime images without
requiring Debian/RPM generation. Keep the public `ox.lib` interfaces cohesive;
split modules internally as responsibilities grow rather than create an
owner-specific dependency for shared infrastructure.

The main gaps to implement are:

- Owner manifests and staging workspaces which exclude unrelated owner trees.
  Current `Workspace` selects scoped aliases but still operates at the Git root.
- Checked-in compiler/repository locks, with exact minus39 selection. Today
  `--toolchain oxcaml-compiler.5.2.0minus39` can select that compiler explicitly;
  the default toolchain is the less restrictive `oxcaml` meta-package.
- A release policy using `+ox` revisions and deterministic subtree archives.
  Existing `ox stamp` uses `BASE+ox.COUNT.COMMIT` and Git subdirectory sources.
- Exportable guard metadata. Current `Repository.constrain` removes local
  package atoms from upstream guards and adds an internal `ox-local-snapshot`
  solver root that rejects other versions of those names. This supports ox's
  snapshot execution, but is not the standalone opam overlay guard design.
  Share the new guard-policy implementation between ox and overlay exports.
- Per-owner install-tree and runtime-image output, with helper, library and
  asset inventories and smoke tests on Linux amd64/arm64.

Use Eio capabilities for filesystem, process, network and clocks. Publication
should accept an explicit validated release manifest. Keep pure opam formula
and version transformations separate from I/O. Reuse existing opam-format,
gitops, archive and checksum facilities rather than a shell/regex metadata
rewriter. Preserve the existing stamp workflow until its callers migrate.

Proposed additions, not commands implemented by this plan:

```sh
ox inventory --check
ox build --user avsm
ox test --user etap
ox dist bundle --user avsm --output dist/avsm
ox dist image --user etap --arch arm64
ox release plan --ref COMMIT --id 20261101-1
ox release export --plan RELEASE-PLAN --output dist
ox release verify --manifest MANIFEST
ox release publish --manifest MANIFEST --transport https
```

Bootstrap must not require an already installed `ox` executable. Provide a
small script using a pinned supported switch or builder image to build `ox`
and its closure. Once installed, ox can build its compiler/dependencies through
day10 without an opam CLI or switch. Test bootstrap from a fresh clone without
the author's opam switch, home-directory configuration or binary cache.

### Source units, packages and scope

A source archive normally contains one complete Dune project. Every opam
package emitted from that project references the same archive. Examples
include `fetch`, `atp`, `idk` and `sqlite3`. Directories and opam packages are
not interchangeable release units.

The default release includes all enabled shared `wip/` packages and required
ported `vendor/` packages. Offer an all-vendors mode and optional owner
releases. A single repository does not have to mirror every external opam
dependency: its declared pinned upstream repositories provide that remainder.

Account for every discovered package as exported, platform-restricted or
excluded with a reason. Empty placeholders such as the disabled jmap-mosaic
package must not become apparently installable releases. An exclusion that
breaks the selected dependency closure is an error.

Recognize `bleeding/` as an input alias while migrating old commits. New
releases use `wip/` and `vendor/`. Reject duplicate old/new definitions rather
than exporting both.

Concrete extraction problems found during the inventory:

- IMAP's oracle tests copied a sibling JMAP harness. The suite has now moved
  into JMAP and uses its project-local harness through a test-only IMAP dependency.
- Nx-oxcaml's I/O test read Nx's pristine source through a sibling path. The
  test now lives in Nx and uses its C backend.
- sqlite-vec copied SQLite's header from a sibling checkout. SQLite now
  installs that header, and sqlite-vec resolves it through the package.
- Crow's CLI tests invoked Sekrit through a sibling source path. They now
  resolve its installed executable.
- HTTPz now installs the media interface referenced by its public re-export,
  preserving nested modules and portability for installed consumers.
- IMAP and JMAP CLI entry points now use the current Console terminal API
  and declare their Console dependency.
- HTTPz, DS4 and SQLite have bundled sources or data needing their own notices.
- The affected projects now retain standalone build profiles. Audit the
  remaining projects for reliance on the parent `dune-workspace`.

For a multi-package source unit, test each package with Dune's package
selection (`-p PACKAGE` or `--only-packages PACKAGE`). For example, JMAP's
IMAP tests must use the same installed `mail-flag` package as installed IMAP,
rather than building a second local copy of it in the test build.

Make each unit self-contained: package shared test support or place traced
fixtures inside the consuming unit. Explicit export extras are acceptable,
but must be tracked, hashed and placed inside the archive. Reject unexplained
reads of parent directories and undeclared host paths. Include standalone
build configuration, licenses and required generated assets.

### Versioning

Use the overlay's simple `+ox` convention for every exported source package.
For example, Digestif starts at `1.4.0+ox`; changed source or packaging under
the same base becomes `1.4.0+ox2`. Preserve the upstream or declared first-party
base. Do not add date, publisher or commit components to package versions.
If a version already exists with different bytes in our overlay or the pinned
OxCaml overlay, allocate the next unused revision rather than overwrite it.
Do not append `+ox` repeatedly to versions already carrying an OxCaml suffix.

Release manifests can retain `YYYYMMDD-N` IDs for grouping artifacts, independent
of package versions. Record full commits and hashes there. Re-running the same
plan must reproduce its metadata and bytes without allocating new revisions.
Keep the existing personal `ox stamp` behavior until its callers migrate.

The suffix is part of opam's version ordering, not ignored SemVer metadata.
Test comparisons using the repository's opam library. Do not assume a suffix
satisfies an exact upstream-version constraint.
[Opam version-ordering reference](https://opam.ocaml.org/doc/Manual.html#Version-ordering).

Base versions must come from explicit package/project metadata or a verified
upstream release, not an unparsed `git describe` string. First-party projects
with compatibility bounds need a meaningful declared base rather than relying
on `0.0.0`.

For a coherent snapshot, constrain internal dependencies to the corresponding
exported versions while retaining boolean alternatives, optional dependency
semantics, platform filters and build/test/doc flags. Review `conflicts`,
`depopts`, `pin-depends` and `= :version` relationships as well as `depends`.
The current stamper is a starting point, not proof that every formula is
handled. Diagnose unsatisfied bounds instead of dropping them to make a solve
succeed.

Every exported library/application package must require
`oxcaml-compiler = "5.2.0minus39"`. The switch name `5.2.0+ox` and the
`oxcaml.latest` meta-package alone do not constrain the compiler to minus39.
Pin both external repository commits and resolved dependency versions.

Integrate the upstream guard mechanism, not just version suffixes. Extend the
allowlist in each applicable `oxcaml-P-patches.enabled` package with our
validated versions of `P`, preserving its conflicts and post-dependency chain.
For locally patched packages whose upstream guards permit ordinary upstream
versions, add an overlay-specific guard to prevent incompatible mixtures in
our switches. New guards must be reachable from an installed switch root.
Never remove `oxcaml-patch-guards` or edit a guard to allow arbitrary versions.
See [the inspected guard design](docs/oxcaml-overlay.md) for exact examples.

Repository priority selects metadata for identical names and versions; it is
not an ABI or snapshot constraint. Reject duplicate package/version entries
with different bytes. Forks using simple `+oxN` versions must coordinate
revision allocation or use separate, explicitly locked repository sets.

### Export and repository structure

```text
dist/
  sources/<publisher>/<release-id>/<unit>-<tree-id>.tar.gz
  releases/<publisher>/<release-id>/manifest.json
  releases/<publisher>/<release-id>/checksums.txt
  releases/<publisher>/<release-id>/verification.json
  opam-repository/
    repo
    packages/<package>/<package>.<version>/opam
```

The cumulative opam repository retains earlier versions. Start from the last
published repository revision and merge immutable entries, rather than
replacing it with only the current release. Serialize publication or use a
compare-and-swap Git update so concurrent releases cannot drop each other's
entries. Ignore generated `dist/` locally, but persist release records and the
published repository outside the ephemeral build directory.

A release manifest
maps packages to source units, source commit and subtree hashes, generated
versions, archive checksums, compiler/dependency locks, exporter version,
export transformations and publication locations.

The generated repository follows standard opam package layout and uses HTTP
source URLs with checksums. It must be usable by ordinary opam, without the
custom ox client.
[Opam repository reference](https://opam.ocaml.org/doc/Manual.html#Repositories).

Export algorithm:

1. Resolve one full committed revision and an explicit release configuration.
   Refuse accidental working-tree publication. Development exports may be
   separate and clearly non-publishable.
2. Discover projects and packages, compute their dependency closure, validate
   classification, and produce the reviewable release plan.
3. Extract each selected project from that commit into isolated staging.
   Use the equivalent of `git archive COMMIT:PATH` for its committed tree,
   then normalize the final staged archive after metadata transformations.
   Include required tracked extras and provenance. Do not copy the checkout,
   `_build`, `_opam`, user configuration or ignored local state.
4. Apply documented metadata transformations in staging. Freeze generated
   opam metadata so Dune cannot regenerate conflicting versions/constraints.
   Verify any embedded application version matches the intended release.
   Do not modify the source checkout.
5. Create deterministic archives: fixed entry order, owner/group, timestamps
   derived from the plan, modes and compression settings. Preserve executable
   bits and safe symlinks. Reject escaping links, unresolved submodules and
   unmaterialized LFS pointers. Record the exporter recipe version.
6. Hash the finished bytes, emit repository metadata and manifests, and verify
   fresh standalone builds. A source archive should not contain its own final
   download URL/checksum, avoiding a self-reference in its hash.
7. Atomically install the verified local release into `dist/`. Retrying must
   either reuse identical output or report a conflict.

Start with whole-release cohorts for simplicity. Reuse unchanged package
versions later only when the source, export recipe, compiler expectations and
internal dependency selection are all unchanged. Subtree hash alone is not
enough when dependency metadata changes.

### Publication

Keep public download URLs separate from write destinations and credentials.
Configuration supplies publisher identity, public archive base URL, repository
location, HTTPS/static upload destination, and optional Tangled repository/PDS
identity. Use Sekrit or CI secret injection for credentials, never tracked
configuration or Docker build arguments.

For static hosting, support a local mirror/upload command behind a typed
transport interface. Upload immutable archives and checksums first. Fetch
them back through their public URLs and verify their hashes. Publish the
opam repository update last, so no visible metadata points to missing files.
Existing package/version bytes must never be overwritten.

For Tangled, reuse the existing ATP client, artifact types and tests, extending
them with an uploader. Official instructions upload a PDS blob and create an
artifact record attached to an annotated Git tag. Validate the destination
server's limits and resolve a stable, anonymously downloadable URL before
emitting opam metadata. Do not confuse the tag's whole-repository source
archive with the extracted subtree archives.
[Tangled artifact publication](https://docs.tangled.org/spindles#publish-a-release-artifact).

Make upload retries idempotent using recorded artifact identities and checksums.
Publishing may leave unreferenced uploaded files after a failure, but must not
publish a partial repository. Preserve an immutable release record and support
restoring the prior repository revision. Replicated HTTPS mirrors are desirable
even when Tangled is the primary artifact destination.

## Verification and CI

Use one generated dependency graph to select affected projects and reverse
dependencies. Ordinary owner changes test that owner and its closure. Shared
changes test affected shared projects and selected owners. Vendor changes also
run explicit upstream test aliases: root recursive aliases skip vendored tests.

Acceptance gates:

- Inventory covers every source directory and discovered package, with no
  duplicate names or unexplained exclusions.
- No shared project depends on `u/`. No owner image reads another owner tree.
- A new example user builds without editing a central user list.
- All renamed paths work in scripts, Dune rules, fixtures, Docker contexts,
  docs, gitignore entries, ox discovery and provenance tooling.
- Each owner image's installed binary/asset inventory matches its manifest.
  Smoke tests exercise helper discovery as well as top-level `--help`.
- Linux amd64 and arm64 CI distinguish unsupported features from real passes.
- Solver tests reject stock/incorrect compilers and incompatible upstream
  libraries, and accept each generated minus39 package closure.
- Export the same commit/configuration twice and compare archive bytes.
- Unpack every archive away from the monorepo. Install using only the generated
  repository and declared external repositories, with an empty build cache.
- Test each enabled package independently, including multi-package source
  projects, test/doc filters, virtual libraries and compiler constraints.
- Build an owner image with only the selected owner/shared source closure.
- Verify public downloads without credentials before repository publication.
- Exercise interrupted uploads, duplicate IDs, fork collisions, version
  ordering, old `bleeding/` commits and missing provenance.
- Run allocation checks under `release-check` where supported. Keep optional
  live-service tests opt-in and report what was actually exercised.

## Suggested implementation order

1. **Inventory and metadata.** Add the classification schema and checker.
   Reconcile upstream records, establish the package/binary inventory, recover
   uncertain bases, and list standalone-extraction failures.
2. **Mechanical layout migration.** Rename the user and shared trees, move the
   identified imports, update every active path reference, and remove the
   empty samoht entry. Preserve package names. Test before semantic changes.
3. **Independent owner builds.** Introduce user manifests, selected workspaces,
   compiler/dependency locks and the fork template. Promote ox to `wip/ox`
   and extend its existing workflows without breaking existing commands.
4. **Containers.** Add the two owner Dockerfiles, source-context allowlists,
   runtime inventories and Linux amd64/arm64 CI.
5. **Local releases.** Implement plan/export/verify, deterministic project
   archives, version-policy tests and the cumulative local opam repository.
   Demonstrate clean installs of several small packages, then every supported
   shared package and both owner closures.
6. **Publication.** Add configurable static hosting first, then Tangled using
   the same verified release manifest. Add public read-back verification and
   repository publication ordering.
7. **Optimisation.** Add incremental releases, remote build caches and richer
   platform matrices only after standalone correctness is established.

## Confirmed decisions and remaining configuration

Confirmed on 2026-11-01:

- Matrix, JMAP, IDKit and Zulip remain maintained here under `wip/` until the
  owner designates an upstream.
- Initial targets are Linux amd64 and arm64. Prioritise runnable binaries and
  per-user containers. Binary package publication is out of scope for now.
- Maintain our own source opam overlay, use `BASE+ox` / `BASE+oxN` package
  versions, pin minus39 and integrate the upstream patch guards.

Proceed with glibc-based runtime images and bundled dependencies. Fully static
linking is not an initial acceptance requirement. Shared packages and required
patched vendors enter the overlay; user applications opt in when needed.
Hosting URLs and upload credentials can wait until local source releases work.
An upstream or graduation change requires an explicit owner decision.

## Implementation status

The `oxrel` branch contains the layout migration, owner manifests and templates,
locked toolchain inputs, owner build/bundle/image commands and deterministic
source release plan/export/verify commands. Guard extensions and the exported
snapshot policy preserve compatibility constraints outside ox's own solver.

Owner isolation, bundle relocation, deterministic archives, guard retention,
version collisions and checksum verification have regression coverage. An
exported Bytesrw archive builds independently under `release-check`. Projects
need declared release base versions before export. Linux
images still require execution on a host with a Docker daemon. The macOS owner
build encountered a network failure fetching the MLX wheel. Public hosting,
uploads, full standalone package certification, execution of architecture CI
and unknown
SQLite provenance remain release gates rather than claimed completed work.
