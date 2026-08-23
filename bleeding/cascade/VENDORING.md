# Vendoring cascade

This is cascade 1.1.0, from the opam release at
https://github.com/samoht/cascade/releases/download/1.1.0/cascade-1.1.0.tbz,
sha256 `c303651e043ed8d807d3d6f2c3745233e792ce7c30d562c9717d284e1bf5ac6a`,
verified against `packages/cascade/cascade.1.1.0/opam` in opam-repository.

Nothing in this workspace calls cascade directly. It is here because
`bleeding/tw` requires it, and `tw` on `main` requires `>= 1.1.0`.

1.1.0 is a breaking release over 1.0.0: `Css.hex` raises rather than returning
opaque black, `Tree_diff.t` gains `layer_order`, the optimizer's profiling API
is per-run, `Apply.Make(_).compute` takes a parsed sheet, and the `memtrace`
dependency is gone. None of that reaches what `tw` uses, which was checked by
building the consumer rather than by reading the changelog.

## What differs from upstream

Four hunks, one of them library code.

* Three eta-expansions, in `lib/diff/css_compare.ml`, `lib/diff/tree_diff.ml`
  and `bin/cmd_diff.ml`. OxCaml infers the partial application
  `List.iter (Buffer.add_string b)` as stack-local and rejects it. Grep for
  `Buffer.add_string` applied to one argument before declaring a re-vendor
  done: only the first of the three shows up in a build of the consumer, and
  the other two need `bin/` and a repo-wide build to surface.
* `dune` and `lib/dune`: each `(mdx ...)` stanza gains `unix`, for the reason
  given in `bleeding/tw/VENDORING.md`.
* `lib/info/dune`: `-w -58`. The switch's `dune-build-info` ships no `cmx` for
  the link-time `Build_info_data` module, so warning 58 fires on every build.
* `fuzz/` is dropped. It needs `alcobar`, which is not in the switch.
* `cascade.opam` is the file dune regenerates in this workspace.

## Re-vendoring

    curl -LO https://github.com/samoht/cascade/releases/download/X/cascade-X.tbz
    shasum -a 256 cascade-X.tbz          # against opam-repository
    tar xf cascade-X.tbz
    rm -rf bleeding/cascade && cp -R cascade-X bleeding/cascade
    rm -rf bleeding/cascade/fuzz

Re-apply the four hunks, then verify through the consumer.
`@bleeding/cascade/all` cannot be built in this switch, for the mdx reason
above, and that failure predates this copy.

    dune build @avsm/bushel/all

Then follow the stylesheet check in `bleeding/tw/VENDORING.md`.
