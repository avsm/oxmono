## sitemap - XML sitemap generation for OCaml

This is sitemap v1.0, vendored unpatched from
https://github.com/avsm/ocaml-sitemap.

It is here only to keep the link consistent. `vendor/xmlm` shadows the
installed xmlm for the whole workspace, and two libraries named `xmlm` cannot
go into one executable, so every workspace dependency of xmlm has to be built
from source alongside it. sitemap is the one such package that was not already
vendored.

`sitemap.ml` and `sitemap.mli` are byte identical to the ones in the v1.0
release tarball, which are also the ones opam installed.

Nothing here is annotated for portability. That waits on `vendor/xmlm`, since
`Sitemap.output_url` and `Sitemap.output_urlset` take an `Xmlm.output` and
`Sitemap.output` writes through one.

### What differs from the upstream distribution

* `dune` and `dune-project` are written for this workspace. Upstream keeps the
  sources in `lib/` and generates its opam file from `dune-project`. This copy
  is flat and its opam file is checked in.
* `sitemap.opam` is the generated upstream file with `license: "ISC"` restored
  from the release's own opam metadata, the `ptime` and `odoc` `with-doc`
  dependencies dropped since no documentation is built here, `dune` raised to
  the workspace's 3.21, and a paragraph in the description recording why the
  copy exists.
* Upstream's `CHANGES.md`, `README.md`, `.github/` and `.gitignore` are not
  vendored. The release this copy came from is named in `sitemap.opam` and
  here.

### Re-vendoring checklist

1. Copy `lib/sitemap.ml` and `lib/sitemap.mli` from the new release over this
   directory, keeping `dune`, `dune-project`, `sitemap.opam`, `LICENSE.md` and
   this file.
2. Update the version in `sitemap.opam` and in the first line of this file.
3. `dune build @avsm/arod/all @avsm/arod/runtest`, which is where the
   consumers are. Do not add `@vendor/sitemap/all`. The root `dune` declares
   `(vendored_dirs vendor)`, so dune skips aliases there: such a build exits 0
   having compiled nothing, which reads as a pass and is not one.

This copy can be dropped when `vendor/xmlm` is dropped, and not before.
