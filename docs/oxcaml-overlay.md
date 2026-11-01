# OxCaml overlay compatibility

Inspected `https://github.com/oxcaml/opam-repository.git` at commit
`afddc8337950dbc02863fe30ae58735a1cf2719f` on 2026-11-01. The research clone
is `/tmp/oxmono-oxcaml-opam-repository`; it is not a vendored library or a
permanent dependency path. Implementation must pin the repository revision
in the build lock, alongside the public opam repository and our overlay.

## Compiler selection

The supported compiler is `oxcaml-compiler.5.2.0minus39`. The inspected
package builds compiler commit `2515546fea38e21e8143cc41db663bd56efc8d06`
and reports release `5.2.0minus-39`. The current local switch also has
`oxcaml-compiler.5.2.0minus39` installed.

The switch label `5.2.0+ox` is not a compiler pin. Nor is `oxcaml.latest`:
its current dependency admits versions from `5.2.0minus39~` up to, but not
including, `5.4.0-ox8~`. Exported OxMono libraries and applications should
require the exact compiler version until another cohort has been validated:

```opam
depends: [
  "oxcaml-compiler" {= "5.2.0minus39"}
]
```

These constraints supplement each package's real dependency formula. They do
not replace dependency locks or checksums for the compiler and library sources.

Sources: [compiler recipe](https://github.com/oxcaml/opam-repository/blob/afddc8337950dbc02863fe30ae58735a1cf2719f/packages/oxcaml-compiler/oxcaml-compiler.5.2.0minus39/opam),
[current-release marker](https://github.com/oxcaml/opam-repository/blob/afddc8337950dbc02863fe30ae58735a1cf2719f/packages/oxcaml/oxcaml.latest/opam).

## How the guards work

The compiler has a post-dependency on `oxcaml-patch-guards.ox`. This starts a
chain of alternatives for patched packages. For a package `P`:

- `oxcaml-P.guard` conflicts with the incompatible ordinary versions of `P`
  and with `oxcaml-P-patches`. It allows the switch to exist without `P`.
- `oxcaml-P-patches.enabled` conflicts with the guard and depends on an
  explicit list of permitted versions of `P`, using a build dependency.
- Both alternatives carry the next post-dependency in the chain and a
  post-dependency on `oxcaml`. Compiler bounds may disable older cohorts.

Thus installing a blocked `P` forces the patches alternative, which forces an
allowlisted version. A package's `+ox` suffix is a convention, not a predicate
the solver recognises as compatibility evidence. Guard chains do not install
all the libraries they cover.

For example, the inspected Cmarkit guard conflicts with every `cmarkit`
version. Its patches alternative allows only `cmarkit.0.3.0+ox`. Exporting our
newer Cmarkit with a `+ox` suffix alone would therefore be unsatisfiable. Our
overlay must explicitly extend the patches package's allowlist.

Eio is different: its guard conflicts only with versions below `1.4`, because
upstream considers newer Eio releases compatible without its patches. That
policy does not establish compatibility with our locally modified Eio APIs.
Our own snapshot constraints must still select our tested port.

Sources: [root guard](https://github.com/oxcaml/opam-repository/blob/afddc8337950dbc02863fe30ae58735a1cf2719f/packages/oxcaml-patch-guards/oxcaml-patch-guards.ox/opam),
[Cmarkit guard](https://github.com/oxcaml/opam-repository/blob/afddc8337950dbc02863fe30ae58735a1cf2719f/packages/oxcaml-cmarkit/oxcaml-cmarkit.guard/opam),
[Cmarkit allowlist](https://github.com/oxcaml/opam-repository/blob/afddc8337950dbc02863fe30ae58735a1cf2719f/packages/oxcaml-cmarkit-patches/oxcaml-cmarkit-patches.enabled/opam),
[Eio guard](https://github.com/oxcaml/opam-repository/blob/afddc8337950dbc02863fe30ae58735a1cf2719f/packages/oxcaml-eio/oxcaml-eio.guard/opam).

## Our overlay

Use repository priority: our overlay, pinned OxCaml overlay, pinned public
opam repository. Package versions start at `BASE+ox`, then advance to
`BASE+ox2`, `BASE+ox3` as source or packaging changes. Keep the original base,
and account for already allocated upstream-overlay versions. Never overwrite
a source package/version with different bytes. Keep dates and commits in
release manifests rather than the package version.

Generate metadata using opam's structured types, with these rules:

1. Add the exact minus39 dependency to exported library/application packages.
2. Rewrite dependencies within the exported set to their chosen versions,
   preserving alternatives, filters, optional dependencies and conflicts.
3. Extend applicable upstream patches allowlists with the generated versions.
   Preserve their existing chain, filters and conflicts. These are explicit,
   reviewed metadata overrides at the upstream guard name/version, not source
   package re-publications. Record both upstream and resulting metadata hashes.
4. Every exported package requires an `oxmono-snapshot` policy package. Its
   conflicts reject every other version of packages in that release closure.
   Unused packages may remain absent. The policy version hashes the source
   commit and selected package versions, so distinct closures cannot collide.
5. Test every generated closure with the solver, then build it from extracted
   sources. Reject stock OCaml, minus38, minus40, incompatible upstream ports
   and exact-version conflicts. Test both minimal and full owner installs.

Current local package names overlapping upstream guard metadata are `cmarkit`,
`eio`, `eio_linux`, `eio_main`, `eio_posix`, `opam-core`, `opam-format` and
`zarith`. Discover this intersection from the locked inputs rather than
hard-coding it in the exporter.

Do not run upstream `layout.sh` directly on our overlay. It regenerates and
removes `oxcaml-*` metadata and contains upstream-specific version exceptions.
Use it as a reference for the guard semantics, not as our publication command.
The exporter extends these markers and records their output checksums and
the input guard tree hash. An ordinary opam dry-run regression checks compiler
and local-version rejection independently of ox's internal solver.

Source: [guard generator](https://github.com/oxcaml/opam-repository/blob/afddc8337950dbc02863fe30ae58735a1cf2719f/layout.sh).

## Integration with ox

Use the existing `ox` binary and `ox.lib` for management. Its `Repository` and
`Solve` modules already process ordered repositories and opam formulas, and
its runner builds the selected closure through day10 without invoking opam.
The current `--toolchain` option accepts `oxcaml-compiler.5.2.0minus39`.

Current `Repository.constrain` handles stamped local packages by removing
those names from upstream guard dependencies/conflicts and adding an internal
`ox-local-snapshot` solver root that conflicts with other versions of the
local names. That invariant is specific to ox's prepared solve. Reusing the
rewritten guard files alone in a published overlay would lose the protection.
Implement the exportable guard policy once, and use it in both ox's solver
setup and source-overlay generation. Test the resulting overlay through an
ordinary opam solve as well as ox's in-process solver.

## Linux binaries

Initial targets are Linux amd64 and arm64. The inspected upstream README
explicitly lists musl distributions as unsupported and says OxCaml requires
glibc. This rules out treating an Alpine/musl build as the simple route to
fully static binaries for the initial delivery.

Use glibc-based build/runtime images, native OCaml executables, and their
required shared libraries and data files. Inventory external helpers as well
as ELF dependencies. Smoke-test DNS, HTTPS certificate discovery, assets and
helper execution on each architecture. Static linking can be evaluated later
for individual commands without delaying usable containers and install trees.
This review does not establish that every command can be fully statically
linked, or that the Linux images have already been built.

Source: [platform limitations](https://github.com/oxcaml/opam-repository/blob/afddc8337950dbc02863fe30ae58735a1cf2719f/README.md#known-issues).
