# OxMono import

Imported `bd31810f35740a4c595ba33e9900e3524d7d142a` from `/home/avsm2/src/git/avsm/ocaml-zulip`.
[oxmono/upstream.json](oxmono/upstream.json) records the repository and base.
The upstream layout, package name `zulip`, examples and tests are retained.
The source checkout is not modified.

## Local adaptations

The build targets OxCaml 5.2.0+ox and the monorepo's Jsont 0.4 API.
Dune opens a private compatibility module with the OCaml 5.5 List, Option and
Result helpers used by upstream. JSON defaults use factories and retained
callbacks have checked portable interfaces. Portable map functors and a few
eta expansions preserve the original computations.

The pure `zulip` library remains independent of HTTPz and Eio. The Eio
client uses HTTPz's unboxed date API with a length check before constructing
its bounded span. The server fixtures in `test/integration/` remain opt-in.

## Build and test

From the oxmono root:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check \
  @bleeding/zulip/all
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check \
  --force bleeding/zulip
```

## Update from upstream

1. Read the recorded revision. Review the source checkout's changes since that
   commit. Export them with `git diff --binary BASE..NEW` in that checkout.
2. Check the patch with `git apply --check --directory=bleeding/zulip PATCH`
   from oxmono, then apply it with the same directory prefix. Resolve conflicting
   hunks against `git show BASE:path` in the source checkout, preserving the
   adaptations listed above and the local `compat/` and `oxmono/` directories.
3. Run the scoped build and tests. Dune regenerates the opam file from the
   locally adapted `dune-project`. Update the recorded revision only after the
   complete upstream change has been integrated.

Upstream documentation and CI describe the standalone OCaml 5.5 build.
This file records the monorepo-specific commands and differences.
