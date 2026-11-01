# Working in OxMono

See [README.md](README.md) for layout and setup. Public contracts belong in
package READMEs and `.mli` files. Keep package plans beside their code and use
[TODO.md](TODO.md) as an index, not an implementation journal.

## Writing

Write concise, complete sentences. Document values as `[f x] is ...` or
`[f x] does ...`, naming arguments and stating defaults and caller obligations.
Comments explain invariants or constraints the code cannot express. Avoid
historical narration, em dashes and semicolons joining clauses. Keep new prose
near 80 columns. Add short user-visible entries to the package changelog.

## Build and verification

Use the repository's OxCaml switch. Scope builds and tests to changed packages
and affected consumers. For example, from the repository root:

```sh
dune build @avsm/siftbot/all
dune build --force @avsm/siftbot/runtest
dune build @avsm/siftbot/fmt
```

Use `--root .` in a worktree if Dune would otherwise select the parent
workspace. Run relevant checks before committing and report any unavailable
checks. Do not require an unrelated whole-workspace build to pass.

The `dev` and `release` profiles disable allocation checks. Use
`--profile release-check` to check `[@zero_alloc]` annotations. Measure runtime
allocations separately when making performance claims.

Formatting versions vary by project. Check the applicable `.ocamlformat` and
the installed formatter. If the OxCaml formatter understands the code but its
version differs, use `ocamlformat --no-version-check` on changed files and
report that substitution. Avoid unrelated reformatting.

Never regenerate goldens merely to make a failure pass. Inspect behavior
changes. For Arod serving-path changes, use
[render_capture.sh](avsm/arod/test/render_capture.sh) before and after and
compare outputs, respecting the script's documented clock-dependent output.

## OxCaml and Eio

Prove mode claims with the compiler and tests against the current dependency
interfaces. Distinguish a portable function from a value safe to capture in a
portable closure. A capture test must use a module-level value inside
`let (f @ portable) = fun () -> ...`.

A floating `@@ portable` in an interface applies to following declarations.
Annotate external values in type position, for example
`external f : ty @@ portable = "%prim"`. Optional callback modes need an
explicit type at the definition, and returned partial applications may need
an explicit result mode in the interface.

Do not assume module-level collections cross modes. Use a suitable kinded
collection, immutable data, a local factory or explicit resource ownership.
Do not add unchecked mode casts to silence an error. Check the current vendored
interfaces before declaring a dependency nonportable.

Pass capabilities into libraries. Keep child fibers within their owning
switch and join them before releasing resources. Prefer Eio I/O and the
shared `eioext` helpers. Keep blocking work off the scheduler. Preserve
cancellation and exception context. Use monotonic clocks for elapsed time and
deadlines, wall clocks for timestamps.

## Vendoring

Follow [vendor/README.md](vendor/README.md). Record exact revisions and import
scope in `vendor/upstreams.json`, with local patches and re-vendoring checks
beside the sources. Workspace libraries shadow installed libraries, so check
affected consumers and shared type identities.

Recursive test aliases skip vendored directories. Invoke their tests explicitly
with `@@vendor/PACKAGE/PATH/runtest` or run the test executable. Keep regression
guards outside `vendor/` too. Use pristine-source differentials for behavior-
preserving patches and document intentional differences.

## Commits

Work on a branch. Use one commit per self-contained change, with a one-line
imperative subject and no trailers or sign-off. Separate unrelated mechanical
changes from behavior changes.
