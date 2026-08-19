## sitemap - XML sitemap generation for OCaml

This is sitemap v1.0, vendored unpatched from
https://github.com/avsm/ocaml-sitemap.

It is here only to keep the link consistent. `vendor/xmlm` shadows the
installed xmlm for the whole workspace, and two libraries named `xmlm` cannot
go into one executable, so every workspace dependency of xmlm has to be built
from source alongside it. sitemap is the one such package that was not already
vendored.

`sitemap.ml` and `sitemap.mli` are byte identical to the ones in the v1.0
release tarball, which are also the ones opam installed. `dune`,
`dune-project` and `sitemap.opam` are written for this workspace, and the opam
file records why the copy exists.

Nothing here is annotated for portability. That waits on `vendor/xmlm`, since
`Sitemap.output_url` and `Sitemap.output_urlset` take an `Xmlm.output` and
`Sitemap.output` writes through one.

### Re-vendoring checklist

1. Copy `lib/sitemap.ml` and `lib/sitemap.mli` from the new release over this
   directory, keeping `dune`, `dune-project`, `sitemap.opam`, `LICENSE.md` and
   this file.
2. Update the version in `sitemap.opam` and in the first line of this file.
3. `dune build @vendor/sitemap/all @avsm/arod/all @avsm/arod/runtest`.

This copy can be dropped when `vendor/xmlm` is dropped, and not before.
