## jsonfeed - JSON Feed 1.1 for OCaml

This is jsonfeed v1.1.0, vendored unpatched from
https://tangled.sh/@anil.recoil.org/ocaml-jsonfeed.

It is here only to keep the link consistent. `vendor/ptime` shadows the
installed ptime for the whole workspace, and two libraries named `ptime`
cannot go into one executable, so every workspace dependency of ptime has to
be built from source alongside it. jsonfeed is the one such package that was
not already vendored.

The sources are the ones opam installed, copied verbatim. `dune`,
`dune-project` and `jsonfeed.opam` are written for this workspace, and the
opam file records why the copy exists. Dune's generated `jsonfeed__.ml` is
not copied, since dune regenerates it.

Nothing here is annotated for portability. `Jsonfeed.t` is built from
`Jsont.json`, which does not cross portability, so annotating the interface
would buy nothing until jsont is annotated.

### Re-vendoring checklist

1. Copy the `.ml` and `.mli` files from the new release over this directory,
   keeping `dune`, `dune-project`, `jsonfeed.opam`, `LICENSE.md` and this
   file. Do not copy `jsonfeed__.ml`.
2. Update the version in `jsonfeed.opam` and in the first line of this file.
3. `dune build @avsm/sortal/all @avsm/sortal/runtest @avsm/arod/all`.

This copy can be dropped when `vendor/ptime` is dropped, and not before.
